#ifndef LA_OBJECT_POOL_H
#define LA_OBJECT_POOL_H

#include <concepts>
#include <cstddef>
#include <functional>
#include <memory>
#include <mutex>
#include <type_traits>
#include <utility>

namespace mtmu {

template<class T>
class pooled_ptr;

namespace detail_ {
template<typename T>
class pool_;

template<typename T>
class pool_alloc_manager {
public:
    pool_alloc_manager() = default;

    pool_alloc_manager(const pool_alloc_manager& other)                = delete;
    pool_alloc_manager(pool_alloc_manager&& other) noexcept            = delete;
    pool_alloc_manager& operator=(const pool_alloc_manager& other)     = delete;
    pool_alloc_manager& operator=(pool_alloc_manager&& other) noexcept = delete;

    virtual ~pool_alloc_manager() = default;

    [[nodiscard]] virtual auto allocate_buffer(std::size_t) -> T** = 0;

    virtual void deallocate_buffer(T**, std::size_t) = 0;

    [[nodiscard]] virtual auto allocate(std::size_t) -> T* = 0;

    virtual void deallocate(T*, std::size_t) = 0;

    virtual void destroy() = 0;

    virtual auto pool_ptr() const -> pool_<T>* = 0;
};

template<typename T, typename Allocator>
class pool_alloc_manager_ final : public pool_alloc_manager<T> {
    using alloc_traits   = std::allocator_traits<Allocator>;
    using pool_alloc_t   = typename alloc_traits::template rebind_alloc<pool_<T>>;
    using buffer_alloc_t = typename alloc_traits::template rebind_alloc<T*>;

public:
    using manager_alloc_t = typename alloc_traits::template rebind_alloc<pool_alloc_manager_<T, Allocator>>;

    pool_alloc_manager_() = delete;

    pool_alloc_manager_(manager_alloc_t core_allocator, Allocator allocator)
    : m_this_allocator(std::move(core_allocator))
    , m_allocator(std::move(allocator))
    , m_buffer_allocator(static_cast<buffer_alloc_t>(m_allocator))
    , m_pool_allocator(static_cast<pool_alloc_t>(m_allocator))
    , m_pool_ptr(m_pool_allocator.allocate(1)){};

    pool_alloc_manager_(const pool_alloc_manager_& other)     = delete;
    pool_alloc_manager_(pool_alloc_manager_&& other) noexcept = delete;

    pool_alloc_manager_& operator=(const pool_alloc_manager_& other)     = delete;
    pool_alloc_manager_& operator=(pool_alloc_manager_&& other) noexcept = delete;

    ~pool_alloc_manager_() final = default;

    void destroy() final {
        m_pool_ptr->~pool_<T>();
        m_pool_allocator.deallocate(m_pool_ptr, 1);
        auto this_alloc = m_this_allocator;
        this->~pool_alloc_manager_();
        this_alloc.deallocate(this, 1);
    }

    [[nodiscard]] auto allocate_buffer(std::size_t n) -> T** final {
        return std::allocator_traits<buffer_alloc_t>::allocate(m_buffer_allocator, n);
    };

    void deallocate_buffer(T** ptr, std::size_t n) final {
        std::allocator_traits<buffer_alloc_t>::deallocate(m_buffer_allocator, ptr, n);
    };

    [[nodiscard]] auto allocate(std::size_t n) -> T* final {
        return alloc_traits::allocate(m_allocator, n);
    };

    void deallocate(T* ptr, std::size_t n) final {
        return alloc_traits::deallocate(m_allocator, ptr, n);
    };

    auto pool_ptr() const -> pool_<T>* final {
        return m_pool_ptr;
    };

private:
    [[no_unique_address]] manager_alloc_t m_this_allocator;
    [[no_unique_address]] Allocator       m_allocator;
    [[no_unique_address]] buffer_alloc_t  m_buffer_allocator;
    [[no_unique_address]] pool_alloc_t    m_pool_allocator;

    pool_<T>* m_pool_ptr;
};    // namespace detail_


template<typename T>
class pool_ {
    using value_type = T;
    using pointer    = pooled_ptr<value_type>;

public:
    pool_(pool_&&)                 = delete;
    pool_(const pool_&)            = delete;
    pool_& operator=(pool_&&)      = delete;
    pool_& operator=(const pool_&) = delete;

    ~pool_() = default;

    template<typename Allocator, typename... Args>
    static auto allocate_pool(Allocator allocator, Args&&... args) {
        using manager_alloc_t = typename pool_alloc_manager_<T, Allocator>::manager_alloc_t;

        auto  core_allocator = static_cast<manager_alloc_t>(allocator);
        auto* core_ptr       = std::allocator_traits<manager_alloc_t>::allocate(core_allocator, 1);
        new (core_ptr) pool_alloc_manager_<T, Allocator>(core_allocator, allocator);
        auto* pool_ptr = core_ptr->pool_ptr();
        new (pool_ptr)
            pool_(static_cast<pool_alloc_manager<value_type>*>(core_ptr), std::forward<Args>(args)...);
        return pool_ptr;
    };

    template<typename... Args>
        requires std::constructible_from<T, Args...> && (std::copy_constructible<Args> && ...)
    explicit pool_(pool_alloc_manager<value_type>* core_ptr, Args&&... args)
    : m_core(core_ptr)
    , m_factory([&, ... args = std::forward<Args>(args)] {
        auto* ptr = m_core->allocate(1);
        return new (ptr) T(args...);
    })
    , m_deleter([&](T* ptr) {
        ptr->~T();
        m_core->deallocate(ptr, 1);
    }){};

    template<typename F, typename D>
        requires std::constructible_from<std::function<T*()>, F> &&
                     std::constructible_from<std::function<void(T*)>, D>
    pool_(pool_alloc_manager<value_type>* core_ptr, F&& factory, D&& deleter)
    : m_core(core_ptr)
    , m_factory(std::forward<F>(factory))
    , m_deleter(std::forward<D>(deleter)){};


    [[nodiscard]] auto acquire() -> pointer {
        std::scoped_lock lock_t(tail_pool_mutex);
        if (m_head_idx != m_tail_idx) {
            auto data  = m_data[m_tail_idx];
            m_tail_idx = (m_tail_idx + 1) % m_capacity;
            return {this, data};
        }
        std::scoped_lock lock_h(head_pool_mutex);

        auto* inserted = insert_no_mutex(1);
        m_tail_idx     = (m_tail_idx + 1) % m_capacity;
        return {this, inserted};
    };

    [[nodiscard]] auto acquire_free() -> pointer {
        std::scoped_lock lock_t(tail_pool_mutex);
        if (m_head_idx != m_tail_idx) {
            auto data  = m_data[m_tail_idx];
            m_tail_idx = (m_tail_idx + 1) % m_capacity;
            return {this, data};
        }
        return {};
    };

    inline void populate(std::size_t insert_n) {
        std::scoped_lock lock_h(head_pool_mutex);
        std::scoped_lock lock_t(tail_pool_mutex);
        insert_no_mutex(insert_n);
    }

    [[nodiscard]] auto size() const -> std::size_t {
        return m_size;
    }

    [[nodiscard]] auto free_size() const -> std::size_t {
        if (m_head_idx > m_tail_idx) {
            return m_head_idx - m_tail_idx;
        }
        if (m_head_idx < m_tail_idx) {
            auto len_tail = m_capacity - m_tail_idx;
            return m_head_idx + len_tail;
        }
        return 0;
    }

    void release(T* object_ptr) {
        std::scoped_lock lock_h(head_pool_mutex);
        if (m_abandoned) {
            m_deleter(object_ptr);
            ++m_deleted;
            if (m_deleted == m_size)
                m_core->destroy();
        } else {
            m_data[m_head_idx] = object_ptr;
            m_head_idx         = (m_head_idx + 1) % m_capacity;
        }
    };

    void abandon() {
        std::scoped_lock lock_h(head_pool_mutex);
        std::scoped_lock lock_t(tail_pool_mutex);
        m_abandoned = true;
        while (m_tail_idx != m_head_idx) {
            m_deleter(m_data[m_tail_idx]);
            ++m_deleted;
            m_tail_idx = (m_tail_idx + 1) % m_capacity;
        }
        m_core->deallocate_buffer(m_data, m_capacity);
        if (m_deleted == m_size)
            m_core->destroy();
    }

private:
    pool_alloc_manager<T>* m_core;

    std::function<T*()>     m_factory;
    std::function<void(T*)> m_deleter;

    std::size_t m_head_idx = 0;
    std::size_t m_tail_idx = 0;

    std::size_t m_size     = 0;
    std::size_t m_deleted  = 0;
    std::size_t m_capacity = 1;

    T** m_data = nullptr;

    bool m_abandoned = false;

    mutable std::mutex tail_pool_mutex;
    mutable std::mutex head_pool_mutex;

    auto insert_no_mutex(std::size_t insert_n) -> T* {
        if (m_size + insert_n >= m_capacity) {
            auto  new_capacity = insert_n > 1 ? m_capacity + insert_n : m_capacity * 2;
            auto* tmp_buf      = m_core->allocate_buffer(new_capacity);
            if (m_head_idx > m_tail_idx) {
                std::copy(m_data + m_tail_idx, m_data + m_tail_idx, tmp_buf);
            } else if (m_head_idx < m_tail_idx) {
                auto len_tail = m_capacity - m_tail_idx;
                std::copy(m_data + m_tail_idx, m_data + m_capacity - 1, tmp_buf);
                std::copy(m_data, m_data + m_head_idx, tmp_buf + len_tail);
            }
            if (m_data != nullptr)
                m_core->deallocate_buffer(m_data, m_capacity);

            m_data     = tmp_buf;
            m_capacity = new_capacity;
            m_tail_idx = 0;
            m_head_idx = free_size();
        }
        auto old_head = m_head_idx;
        for (uint i = 0; i < insert_n; ++i) {
            m_data[m_head_idx] = m_factory();

            m_head_idx = (m_head_idx + 1) % m_capacity;
            ++m_size;
        }
        return (m_data[old_head]);
    }
};

class separator {};
}    // namespace detail_


template<class T, typename Allocator, bool Shared = false, typename... Args>
inline auto allocate_pool(Allocator allocator, Args&&... args) {
    return pool<T, Shared>(allocator, detail_::separator{}, std::forward(args)...);
}

/**
 * @brief Pool of reusable objects.
 * When necessary creates new objects using factory function.
 * Uses heap allocated internal pool that persists until the last object is returned.
 *
 * @tparam T
 * @tparam Shared  if true enables copy construction and assignment.
 */
template<class T, bool Shared = false>
class pool {
    friend class pooled_ptr<T>;

    using pool_ptr_t = std::conditional_t<Shared, std::shared_ptr<detail_::pool_<T>>, detail_::pool_<T>*>;

public:
    using value_type = T;
    using pointer    = pooled_ptr<value_type>;

    /**
     * @brief Construct a new pool object. called by allocate_pool() for better interface.
     *
     * @param allocator allocator used for allcation of internal control block and new elements.
     * @param args arguments passed to operator new when creating new object.
     */
    template<typename Allocator, typename... Args>
        requires std::constructible_from<T, Args...> && (std::copy_constructible<Args> && ...)
    explicit pool(Allocator allocator, detail_::separator /*unused*/, Args&&... args) {
        auto pool_ptr = detail_::pool_<T>::allocate_pool(allocator, std::forward<Args>(args)...);
        if constexpr (Shared) {
            m_pool = pool_ptr_t(pool_ptr, [](detail_::pool_<T>* ptr) { ptr->abandon(); });
        } else {
            m_pool = pool_ptr;
        }
    };

    /**
     * @brief Construct a new pool object.
     *
     * @param args arguments passed to operator new when creating new object.
     */
    template<typename... Args>
        requires std::constructible_from<T, Args...> && (std::copy_constructible<Args> && ...)
    explicit pool(Args&&... args) {
        auto pool_ptr = detail_::pool_<T>::allocate_pool(std::allocator<T>{}, std::forward<Args>(args)...);
        if constexpr (Shared) {
            m_pool = pool_ptr_t(pool_ptr, [](detail_::pool_<T>* ptr) { ptr->abandon(); });
        } else {
            m_pool = pool_ptr;
        }
    };

    /**
     * @brief Construct a new pool object
     *
     * @param factory must allocate memory, construct object and return a pointer to the constructed object.
     * @param deleter must destruct object and deallocate memory pointed to by ptr. Invoked as `T* ptr; deleter(ptr);`.
     * @param allocator allocator used for allcation of internal control block.
     */
    template<typename F, typename D, typename Allocator = std::allocator<T>>
        requires std::constructible_from<std::function<T*()>, F> &&
                 std::constructible_from<std::function<void(T*)>, D>
    pool(F&& factory, D&& deleter, Allocator allocator = Allocator{}) {
        auto pool_ptr = detail_::pool_<T>::allocate_pool(    //
            allocator,
            std::forward<F>(factory),
            std::forward<D>(deleter));

        if constexpr (Shared) {
            m_pool = pool_ptr_t(
                pool_ptr, [](detail_::pool_<T>* ptr) { ptr->abandon(); }, allocator);
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
     * @brief Creates new objects using factory and inserts them into pool.
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
    pool_ptr_t m_pool;
};
template<class T>
using shared_pool = pool<T, true>;

/**
 * @brief Smart pointer that manages an object from pool<>.
 * A pooled_ptr may manage no objects.
 * If pooled_ptr manages an object, pooled_ptr releases object back into the pool when it goes out of scope.
 * pooled_ptr is move constructable and move assignable, but neigher copy constructable nor copy assignable.
 *
 * @tparam T
 */
template<class T>
class pooled_ptr {
    friend class detail_::pool_<T>;
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
        return make_shared();
    }

    /**
     * @brief Constructs and returns a shared_ptr that manages an object currently managed by *this.
     * Manages no objects after return.
     *
     * @return std::shared_ptr<T>
     */
    template<typename Allocator = std::allocator<T>>
    auto make_shared(Allocator allocator = Allocator{}) -> std::shared_ptr<T> {
        if (m_pool_ptr != nullptr) {
            auto ptr = std::shared_ptr<T>(
                m_data, [parent_pool = m_pool_ptr](T* ptr) { parent_pool->release(ptr); }, allocator);
            m_pool_ptr = nullptr;
            m_data     = nullptr;
            return ptr;
        }
        return {};
    }

    //NOLINTBEGIN(*explicit*)
    /**
     * @brief Returns true if pooled_ptr manages an object.
     *
     * @return true Manages object.
     * @return false Manages no object.
     */
    operator bool() const noexcept {
        return (m_pool_ptr != nullptr);
    }
    //NOLINTEND(*explicit*)

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
    using pool_t = typename detail_::pool_<T>;

    pooled_ptr(pool_t* parent_pool, pointer object_ptr)
    : m_pool_ptr(parent_pool)
    , m_data(object_ptr){};

    pool_t* m_pool_ptr = nullptr;
    pointer m_data     = nullptr;
};

};    // namespace mtmu

#endif
