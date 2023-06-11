#ifndef LA_OBJECT_POOL_H
#define LA_OBJECT_POOL_H

#include <concepts>
#include <functional>
#include <list>
#include <memory>
#include <mutex>
#include <type_traits>

namespace la {

template<class T, typename Allocator>
class pooled_ptr;

/**
 * @brief Pool of reusable objects.
 * When nececarry creates new objects using factory function.
 *
 * @tparam T
 * @tparam Allocator an allocator that is used for internal pointer lists and for new objects if not using custom factory and deleter.
 * @tparam Shared    if true enables copy construction and assignment.
 */
template<class T, typename Allocator = std::allocator<T>, bool Shared = false>
class pool {
    friend class pooled_ptr<T, Allocator>;

    class pool_;
    using pool_allocator_t = typename std::allocator_traits<Allocator>::template rebind_alloc<pool_>;
    using pool_ptr_t       = std::conditional_t<Shared, std::shared_ptr<pool_>, pool_*>;

public:
    using value_type     = T;
    using pointer        = pooled_ptr<T, Allocator>;
    using allocator_type = Allocator;

    /**
     * @brief Construct a new pool object.
     *
     * @param args arguments passed to operator new when creating new object.
     */
    template<typename... Args>
        requires std::constructible_from<T, Args...> && (std::copy_constructible<Args> && ...)
    explicit pool(Args&&... args) {
        auto allocator      = allocator_type{};
        auto pool_allocator = static_cast<pool_allocator_t>(allocator);
        auto pool_ptr       = pool_allocator.allocate(1);
        new (pool_ptr) pool_(allocator, pool_allocator, std::forward<Args>(args)...);

        if constexpr (Shared) {
            m_pool = std::shared_ptr<pool_>(
                pool_ptr, [](pool_* ptr) { ptr->abandon(); }, allocator);
        } else {
            m_pool = pool_ptr;
        }
    };

    /**
     * @brief Construct a new pool object.
     *
     * @param args arguments passed to operator new when creating new object.
     * @param allocator
     */
    template<typename... Args>
        requires std::constructible_from<T, Args...> && (std::copy_constructible<Args> && ...)
    explicit pool(allocator_type allocator, Args&&... args) {
        auto pool_allocator = static_cast<pool_allocator_t>(allocator);
        auto pool_ptr       = pool_allocator.allocate(1);
        new (pool_ptr) pool_(allocator, pool_allocator, std::forward<Args>(args)...);

        if constexpr (Shared) {
            m_pool = std::shared_ptr<pool_>(
                pool_ptr, [](pool_* ptr) { ptr->abandon(); }, allocator);
        } else {
            m_pool = pool_ptr;
        }
    };

    /**
     * @brief Construct a new pool object
     *
     * @param factory must allocate memory, construct object and return a pointer to the constructed object.
     * @param deleter must destruct object and deallocate memory pointed to by ptr. Invoked as `T* ptr; deleter(ptr);`.
     */
    template<typename F, typename D>
        requires std::constructible_from<std::function<T*()>, F> &&
                 std::constructible_from<std::function<void(T*)>, D>
    pool(F&& factory, D&& deleter, allocator_type allocator = allocator_type{}) {
        auto pool_allocator = static_cast<pool_allocator_t>(allocator);
        auto pool_ptr       = pool_allocator.allocate(1);
        new (pool_ptr) pool_(std::forward<F>(factory), std::forward<D>(deleter), allocator, pool_allocator);

        if constexpr (Shared) {
            m_pool = std::shared_ptr<pool_>(
                pool_ptr, [](pool_* ptr) { ptr->abandon(); }, allocator);
        } else {
            m_pool = pool_ptr;
        }
    };

    ~pool() {
        if constexpr (!Shared) {
            m_pool->abandon();
        }
    }

    pool(pool&&) noexcept            = default;
    pool& operator=(pool&&) noexcept = default;

    pool(const pool&)            = delete;
    pool& operator=(const pool&) = delete;

    pool(const pool&)
        requires Shared
    = default;
    pool& operator=(const pool&)
        requires Shared
    = default;

    /**
     * @brief Creates objects using factory and inserts them into pool.
     *
     * @param insert_n number of objects to insert.
     */
    void populate(std::size_t insert_n) {
        m_pool->populate(insert_n);
    }

    /**
     * @brief Acquires object from pool. If no free objects are available creates new using factory.
     *
     * @return pooled_ptr<T> smart pointer to the object.
     */
    [[nodiscard]] auto acquire() -> pointer {
        return m_pool->acquire();
    };
    /**
     * @brief Attempts to acquire a free object from pool. If no free objects are available returns empty pooled_ptr.
     *
     * @return pooled_ptr<T> smart pointer to the object. Empty if no free objects are available.
     */
    [[nodiscard]] auto acquire_free() -> pointer {
        return m_pool->acquire_free();
    };

    /**
     * @brief
     *
     * @return std::size_t number of objects in pool.
     */
    [[nodiscard]] auto size() const -> std::size_t {
        return m_pool->size();
    }
    /**
     * @brief
     *
     * @return std::size_t number of free objects in pool.
     */
    [[nodiscard]] auto free_size() const -> std::size_t {
        return m_pool->free_size();
    }

private:
    class pool_ {
        using list_allocator_t = typename std::allocator_traits<Allocator>::template rebind_alloc<T*>;

    public:
        pool_(pool_&&)                 = delete;
        pool_(const pool_&)            = delete;
        pool_& operator=(pool_&&)      = delete;
        pool_& operator=(const pool_&) = delete;

        ~pool_() = default;

        template<typename... Args>
            requires std::constructible_from<T, Args...> && (std::copy_constructible<Args> && ...)
        explicit pool_(allocator_type allocator, pool_allocator_t pool_allocator, Args&&... args)
        : m_allocator(allocator)
        , m_pool_allocator(pool_allocator)
        , m_object_list(static_cast<list_allocator_t>(allocator))
        , m_free_list(static_cast<list_allocator_t>(allocator))
        , m_factory([&, ... args = std::forward<Args>(args)] {
            auto* ptr = m_allocator.allocate(1);
            return new (ptr) T(args...);
        })
        , m_deleter([&](T* ptr) {
            ptr->~T();
            m_allocator.deallocate(ptr, 1);
        }){};

        template<typename F, typename D>
            requires std::constructible_from<std::function<T*()>, F> &&
                         std::constructible_from<std::function<void(T*)>, D>
        pool_(F&& factory, D&& deleter, allocator_type allocator, pool_allocator_t pool_allocator)
        : m_allocator(allocator)
        , m_pool_allocator(pool_allocator)
        , m_object_list(static_cast<list_allocator_t>(allocator))
        , m_free_list(static_cast<list_allocator_t>(allocator))
        , m_factory(std::forward<F>(factory))
        , m_deleter(std::forward<D>(deleter)){};


        [[nodiscard]] auto acquire() -> pointer {
            std::scoped_lock lock(pool_mutex);
            if (!m_free_list.empty()) {
                T* l_ptr = m_free_list.back();
                m_free_list.pop_back();
                return {this, l_ptr};
            }
            T* l_new_ptr = m_factory();
            m_object_list.push_back(l_new_ptr);
            return {this, l_new_ptr};
        };

        [[nodiscard]] auto acquire_free() -> pointer {
            std::scoped_lock lock(pool_mutex);
            if (!m_free_list.empty()) {
                T* l_ptr = m_free_list.back();
                m_free_list.pop_back();
                return {this, l_ptr};
            }
            return {};
        };

        void populate(std::size_t insert_n) {
            std::scoped_lock lock(pool_mutex);
            for (std::size_t i = 0; i < insert_n; i++) {
                T* l_new_ptr = m_factory();
                m_object_list.push_back(l_new_ptr);
                m_free_list.push_back(l_new_ptr);
            }
        }

        [[nodiscard]] auto size() const -> std::size_t {
            std::scoped_lock lock(pool_mutex);
            return m_object_list.size();
        }

        [[nodiscard]] auto free_size() const -> std::size_t {
            std::scoped_lock lock(pool_mutex);
            return m_free_list.size();
        }

        void release(T* object_ptr) {
            std::scoped_lock lock(pool_mutex);
            m_free_list.push_back(object_ptr);
            if (m_abandoned) {
                m_deleter(object_ptr);
                if (m_free_list.size() == m_object_list.size()) {
                    destroy();
                }
            }
        };

        void abandon() {
            std::scoped_lock lock(pool_mutex);
            m_abandoned = true;
            for (auto* i_object_ptr: m_free_list) {
                m_deleter(i_object_ptr);
            }
            if (m_free_list.size() == m_object_list.size()) {
                destroy();
            }
        }

        [[nodiscard]] auto allocator() {
            return m_allocator;
        }

    private:
        [[no_unique_address]] allocator_type   m_allocator{};
        [[no_unique_address]] pool_allocator_t m_pool_allocator{};

        std::list<T*, list_allocator_t> m_object_list;
        std::list<T*, list_allocator_t> m_free_list;

        const std::function<T*()>     m_factory;
        const std::function<void(T*)> m_deleter;

        bool m_abandoned = false;

        mutable std::mutex pool_mutex;

        void destroy() {
            auto pool_allocator = m_pool_allocator;
            this->~pool_();
            pool_allocator.deallocate(this, 1);
        };
    };

    pool_ptr_t m_pool;
};

template<class T, typename Allocator = std::allocator<T>>
using shared_pool = pool<T, Allocator, true>;

/**
 * @brief Smart pointer that manages an object from pool<>.
 * A pooled_ptr may also manage no objects.
 * If pooled_ptr manages an object, pooled_ptr releases object back into the pool when it goes out of scope.
 * pooled_ptr is move constructable and move assignable, but neigher copy constructable nor copy assignable.
 *
 * @tparam T
 */
template<class T, typename Allocator>
class pooled_ptr {
    friend class pool<T, Allocator>;
    friend void swap(pooled_ptr& first, pooled_ptr& second) {
        using std::swap;
        swap(first.m_pool_ptr, second.m_pool_ptr);
        swap(first.m_data, second.m_data);
    }

public:
    using element_type = T;
    using pointer      = T*;
    /**
     * @brief Construct a new pooled_ptr object that manages no objects.
     *
     */
    pooled_ptr() = default;
    pooled_ptr(pooled_ptr&& other) noexcept {
        swap(*this, other);
    };
    pooled_ptr& operator=(pooled_ptr&& other) noexcept {
        swap(*this, other);
        return *this;
    };

    ~pooled_ptr() {
        release();
    }

    pooled_ptr(const pooled_ptr&)            = delete;
    pooled_ptr& operator=(const pooled_ptr&) = delete;

    /**
     * @brief Constructs and returns a shared_ptr that manages an object currently managed by *this.
     * Manages no objects after return.
     *
     * @return std::shared_ptr<T>
     */
    explicit operator std::shared_ptr<T>() {
        if (m_pool_ptr != nullptr) {
            auto ptr = std::shared_ptr<T>(
                m_data,
                [parent_pool = m_pool_ptr](T* ptr) { parent_pool->release(ptr); },
                m_pool_ptr->allocator());
            m_pool_ptr = nullptr;
            m_data     = nullptr;
            return ptr;
        }
        return {};
    }
    //NOLINTBEGIN(google-explicit-constructor, hicpp-explicit-conversions)
    /**
     * @brief Returns true if pooled_ptr manages an object.
     *
     * @return true Manages object.
     * @return false Manages no object.
     */
    operator bool() const noexcept {
        return (m_pool_ptr != nullptr);
    }
    //NOLINTEND(google-explicit-constructor, hicpp-explicit-conversions)

    /**
     * @brief Returns a pointer to the managed object.
     *
     * @return T*
     */
    [[nodiscard]] auto get() const -> pointer {
        return m_data;
    }
    /**
     * @brief Dereferences pointer to the managed object.
     *
     * @return T&
     */
    [[nodiscard]] auto operator*() const -> element_type& {
        return *m_data;
    }
    /**
     * @brief Dereferences pointer to the managed object.
     *
     * @return T*
     */
    [[nodiscard]] auto operator->() const -> pointer {
        return m_data;
    }

    /**
     * @brief Releases managed object back into the pool. Manages no objects afterwards.
     *
     */
    void release() {
        if (m_pool_ptr != nullptr) {
            m_pool_ptr->release(m_data);
            m_pool_ptr = nullptr;
            m_data     = nullptr;
        }
    }

private:
    using pool_t = typename pool<T, Allocator>::pool_;

    pooled_ptr(pool_t* parent_pool, T* object_ptr)
    : m_pool_ptr(parent_pool)
    , m_data(object_ptr){};

    pool_t* m_pool_ptr = nullptr;
    T*      m_data     = nullptr;
};
};    // namespace la

#endif
