#ifndef LA_OBJECT_POOL_H
#define LA_OBJECT_POOL_H

#include <concepts>
#include <functional>
#include <list>
#include <memory>
#include <mutex>

namespace la {

template<class T, typename Allocator>
class pooled_ptr;

/**
 * @brief Pool of reusable objects.
 * When nececarry creates new objects using factory function.
 *
 * @tparam T
 * @tparam Allocator An allocator that is used for internal pointer lists and for new objects if not using custom factory and deleter.
 */
template<class T, typename Allocator = std::allocator<T>>
class pool
{
    friend class pooled_ptr<T, Allocator>;

    using list_allocator_type =
        typename std::allocator_traits<Allocator>::template rebind_alloc<T*>;
    class pool_;
    using pool_allocator_type_ =
        typename std::allocator_traits<Allocator>::template rebind_alloc<pool_>;

public:
    using value_type     = T;
    using pointer        = pooled_ptr<T, Allocator>;
    using allocator_type = Allocator;

    /**
     * @brief Construct a new pool object.
     *
     * @param args Arguments passed to operator new when creating new object.
     */
    template<typename... Args>
        requires std::constructible_from<T, Args...> && (std::copy_constructible<Args> && ...)
    explicit pool(Args&&... args)
    {
        auto alloc  = pool_allocator_type_();
        auto l_base = alloc.allocate(1);
        auto drain  = pool_drain_(alloc);

        m_pool = new (l_base) pool_(drain, std::forward<Args>(args)...);
    };

    /**
     * @brief Construct a new pool object.
     *
     * @param args Arguments passed to operator new when creating new object.
     * @param allocator
     */
    template<typename... Args>
        requires std::constructible_from<T, Args...> && (std::copy_constructible<Args> && ...)
    explicit pool(allocator_type allocator, Args&&... args)
    {
        auto alloc  = static_cast<pool_allocator_type_>(allocator);
        auto l_base = alloc.allocate(1);
        auto drain  = pool_drain_(alloc);

        m_pool = new (l_base) pool_(drain, allocator, std::forward<Args>(args)...);
    };

    /**
     * @brief Construct a new pool object
     *
     * @param factory Must allocate memory, construct object and return a pointer to the constructed object.
     * @param deleter Must destruct object and deallocate memory pointed to by ptr. Invoked as `T* ptr; deleter(ptr);`.
     */
    template<typename F, typename D>
        requires std::constructible_from<std::function<T*()>, F> &&
                 std::constructible_from<std::function<void(T*)>, D>
    pool(F&& factory, D&& deleter, allocator_type allocator = allocator_type{})
    {
        auto alloc  = static_cast<pool_allocator_type_>(allocator);
        auto l_base = alloc.allocate(1);
        auto drain  = pool_drain_(alloc);

        m_pool = new (l_base)
            pool_(drain, std::forward<F>(factory), std::forward<D>(deleter), allocator);
    };

    ~pool()
    {
        m_pool->abandon();
    }

    pool(pool&&)                 = delete;
    pool(const pool&)            = delete;
    pool& operator=(pool&&)      = delete;
    pool& operator=(const pool&) = delete;

    /**
     * @brief Creates objects using factory and inserts them into pool.
     *
     * @param insert_n Number of objects to insert.
     */
    void populate(std::size_t insert_n)
    {
        m_pool->populate(insert_n);
    }

    /**
     * @brief Acquires object from pool. If no free objects are available creates new using factory.
     *
     * @return pooled_ptr<T> Smart pointer to the object.
     */
    [[nodiscard]] auto acquire() -> pointer
    {
        return m_pool->acquire();
    };
    /**
     * @brief Attempts to acquire a free object from pool. If no free objects are available returns empty pooled_ptr.
     *
     * @return pooled_ptr<T> Smart pointer to the object. Empty if no free objects are available.
     */
    [[nodiscard]] auto acquire_free() -> pointer
    {
        return m_pool->acquire_free();
    };

    /**
     * @brief
     *
     * @return std::size_t Number of objects in pool.
     */
    [[nodiscard]] inline std::size_t size() const
    {
        return m_pool->size();
    }
    /**
     * @brief
     *
     * @return std::size_t Number of free objects in pool.
     */
    [[nodiscard]] inline std::size_t free_size() const
    {
        return m_pool->free_size();
    }

private:
    class pool_drain_
    {
    public:
        explicit pool_drain_(pool_allocator_type_ allocator = pool_allocator_type_{})
        : m_allocator(allocator){};

        void operator()(pool_* ptr)
        {
            ptr->~pool_();
            m_allocator.deallocate(ptr, 1);
        };

        [[no_unique_address]] pool_allocator_type_ m_allocator;
    };

    class pool_
    {
    public:
        pool_(pool_&&)                 = delete;
        pool_(const pool_&)            = delete;
        pool_& operator=(pool_&&)      = delete;
        pool_& operator=(const pool_&) = delete;

        ~pool_() = default;

        template<typename... Args>
            requires std::constructible_from<T, Args...> && (std::copy_constructible<Args> && ...)
        explicit pool_(pool_drain_ drain, Args&&... args)
        : m_drain(drain)
        , m_factory([&, ... args = std::forward<Args>(args)] {
            auto* ptr = m_allocator.allocate(1);
            return new (ptr) T(args...);
        })
        , m_deleter([&](T* ptr) {
            ptr->~T();
            m_allocator.deallocate(ptr, 1);
        }){};

        template<typename... Args>
            requires std::constructible_from<T, Args...> && (std::copy_constructible<Args> && ...)
        explicit pool_(pool_drain_ drain, allocator_type allocator, Args&&... args)
        : m_drain(drain)
        , m_allocator(allocator)
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
        pool_(pool_drain_    drain,
              F&&            factory,
              D&&            deleter,
              allocator_type allocator = allocator_type{})
        : m_drain(drain)
        , m_allocator(allocator)
        , m_factory(std::forward<F>(factory))
        , m_deleter(std::forward<D>(deleter)){};


        [[nodiscard]] auto acquire() -> pointer
        {
            std::scoped_lock lock(pool_mutex);
            if (!m_free_list.empty())
            {
                T* l_ptr = m_free_list.back();
                m_free_list.pop_back();
                return {this, l_ptr};
            }
            T* l_new_ptr = m_factory();
            m_object_list.push_back(l_new_ptr);
            return {this, l_new_ptr};
        };

        [[nodiscard]] auto acquire_free() -> pointer
        {
            std::scoped_lock lock(pool_mutex);
            if (!m_free_list.empty())
            {
                T* l_ptr = m_free_list.back();
                m_free_list.pop_back();
                return {this, l_ptr};
            }
            return {};
        };

        void populate(std::size_t insert_n)
        {
            std::scoped_lock lock(pool_mutex);
            for (std::size_t i = 0; i < insert_n; i++)
            {
                T* l_new_ptr = m_factory();
                m_object_list.push_back(l_new_ptr);
                m_free_list.push_back(l_new_ptr);
            }
        }

        [[nodiscard]] inline std::size_t size() const
        {
            std::scoped_lock lock(pool_mutex);
            return m_object_list.size();
        }

        [[nodiscard]] inline std::size_t free_size() const
        {
            std::scoped_lock lock(pool_mutex);
            return m_free_list.size();
        }

        void release(T* object_ptr)
        {
            std::scoped_lock lock(pool_mutex);
            m_free_list.push_back(object_ptr);
            if (m_abandoned)
            {
                m_deleter(object_ptr);
                if (m_free_list.size() == m_object_list.size())
                {
                    auto drain = m_drain;
                    drain(this);
                }
            }
        };

        void abandon()
        {
            std::scoped_lock lock(pool_mutex);
            m_abandoned = true;
            for (auto* i_object_ptr : m_free_list)
            {
                m_deleter(i_object_ptr);
            }
            if (m_free_list.size() == m_object_list.size())
            {
                auto drain = m_drain;
                drain(this);
            }
        }

    private:
        [[no_unique_address]] allocator_type m_allocator{};
        [[no_unique_address]] pool_drain_    m_drain;

        std::list<T*, list_allocator_type> m_object_list;
        std::list<T*, list_allocator_type> m_free_list;

        const std::function<T*()>     m_factory;
        const std::function<void(T*)> m_deleter;

        bool m_abandoned = false;

        mutable std::mutex pool_mutex;
    };

    pool_* m_pool;
};

/**
 * @brief Smart pointer that manages an object from pool<>.
 * A pooled_ptr may also manage no objects.
 * If pooled_ptr manages an object, pooled_ptr releases object back into the parent pool when it goes out of scope.
 * pooled_ptr is move constructable and move assignable, but neigher copy constructable nor copy assignable.
 *
 * @tparam T
 */
template<class T, typename Allocator>
class pooled_ptr
{
    friend class pool<T, Allocator>;
    friend void swap(pooled_ptr& first, pooled_ptr& second)
    {
        using std::swap;
        swap(first.m_parent_pool, second.m_parent_pool);
        swap(first.m_data, second.m_data);
    }

public:
    using element_type = T;
    /**
     * @brief Construct a new pooled_ptr object that manages no objects.
     *
     */
    pooled_ptr() = default;
    pooled_ptr(pooled_ptr&& other) noexcept
    {
        swap(*this, other);
    };
    pooled_ptr& operator=(pooled_ptr&& other) noexcept
    {
        swap(*this, other);
        return *this;
    };

    ~pooled_ptr()
    {
        this->release();
    }

    pooled_ptr(const pooled_ptr&)            = delete;
    pooled_ptr& operator=(const pooled_ptr&) = delete;

    /**
     * @brief Constructs and returns a shared_ptr that manages an object currently managed by *this.
     * Manages no objects after return.
     *
     * @return std::shared_ptr<T>
     */
    explicit operator std::shared_ptr<T>()
    {
        if (m_parent_pool != nullptr)
        {
            std::shared_ptr<T> tmp_ptr(m_data, [parent_pool = m_parent_pool](T* ptr) {
                parent_pool->release(ptr);
            });
            m_parent_pool = nullptr;
            m_data        = nullptr;
            return tmp_ptr;
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
    operator bool() const noexcept
    {
        return (m_parent_pool != nullptr);
    }
    //NOLINTEND(google-explicit-constructor, hicpp-explicit-conversions)

    /**
     * @brief Returns a pointer to the managed object.
     *
     * @return T*
     */
    [[nodiscard]] T* get() const
    {
        return m_data;
    }
    /**
     * @brief Dereferences pointer to the managed object.
     *
     * @return T&
     */
    [[nodiscard]] T& operator*() const
    {
        return *m_data;
    }
    /**
     * @brief Dereferences pointer to the managed object.
     *
     * @return T*
     */
    [[nodiscard]] T* operator->() const
    {
        return m_data;
    }

    /**
     * @brief Releases managed object back into the parent pool. Manages no objects afterwards.
     *
     */
    void release()
    {
        if (m_parent_pool != nullptr)
        {
            m_parent_pool->release(m_data);
            m_parent_pool = nullptr;
            m_data        = nullptr;
        }
    }

private:
    using pool_t = typename pool<T, Allocator>::pool_;

    pooled_ptr(pool_t* parent_pool, T* object_ptr)
    : m_parent_pool(parent_pool)
    , m_data(object_ptr){};

    pool_t* m_parent_pool = nullptr;
    T*      m_data        = nullptr;
};
};    // namespace la

#endif
