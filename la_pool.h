#ifndef LA_OBJECT_POOL_H
#define LA_OBJECT_POOL_H

#include <concepts>
#include <functional>
#include <list>
#include <memory>
#include <mutex>

namespace la {

template<class T>
class pooled_ptr;

/**
 * @brief Pool of reusable objects.
 * When nececarry creates new objects using factory function. By default factory uses operator new.
 *
 * @tparam T
 */
template<class T>
class pool
{
    friend class pooled_ptr<T>;

public:
    /**
     * @brief Construct a new pool object.
     *
     * @param args Arguments passed to operator new when creating new object.
     */
    template<typename... Args>
        requires std::constructible_from<T, Args...>
    explicit pool(Args&&... args)
    : m_factory([... args = std::forward<Args>(args)] { return new T(args...); }){};

    /**
     * @brief Construct a new pool object.
     *
     * @param factory Must allocate memory, construct object and return a pointer to the constructed object.
     * Object is deleted using `[](T* ptr){ delete ptr; }`
     */
    template<typename F>
        requires std::constructible_from<std::function<T*()>, F>
    explicit pool(F&& factory)
    : m_factory(std::forward<F>(factory))
    , m_deleter([](T* ptr) { delete ptr; }){};

    /**
     * @brief Construct a new pool object
     *
     * @param factory Must allocate memory, construct object and return a pointer to the constructed object.
     * @param deleter Must destruct object and deallocate memory pointed to by ptr. Invoked as `T* ptr; deleter(ptr);`.
     */
    template<typename F, typename D>
        requires std::constructible_from<std::function<T*()>, F> &&
                     std::constructible_from<std::function<void(T*)>, D>
    explicit pool(F&& factory, D&& deleter)
    : m_factory(std::forward<F>(factory))
    , m_deleter(std::forward<D>(deleter)){};

    ~pool()
    {
        clear();
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
        std::scoped_lock lock(pool_mutex);
        for (std::size_t i = 0; i < insert_n; i++)
        {
            T* l_new_ptr = m_factory();
            m_object_list.push_back(l_new_ptr);
            m_free_list.push_back(l_new_ptr);
        }
    }

    /**
     * @brief Acquires object from pool. If no free objects are available creates new using factory.
     *
     * @return pooled_ptr<T> Smart pointer to the object.
     */
    [[nodiscard]] pooled_ptr<T> acquire()
    {
        std::scoped_lock lock(pool_mutex);
        if (!m_free_list.empty())
        {
            T* l_ptr = m_free_list.back();
            m_free_list.pop_back();
            return pooled_ptr(this, l_ptr);
        }
        T* l_new_ptr = m_factory();
        m_object_list.push_back(l_new_ptr);
        return pooled_ptr(this, l_new_ptr);
    };
    /**
     * @brief Attempts to acquire a free object from pool. If no free objects are available returns empty pooled_ptr.
     *
     * @return pooled_ptr<T> Smart pointer to the object. Empty if no free objects are available.
     */
    [[nodiscard]] pooled_ptr<T> acquire_free()
    {
        std::scoped_lock lock(pool_mutex);
        if (!m_free_list.empty())
        {
            T* l_ptr = m_free_list.back();
            m_free_list.pop_back();
            return pooled_ptr(this, l_ptr);
        }
        return {};
    };

    /**
     * @brief
     *
     * @return std::size_t Number of objects in pool.
     */
    [[nodiscard]] inline std::size_t size() const
    {
        std::scoped_lock lock(pool_mutex);
        return m_object_list.size();
    }
    /**
     * @brief
     *
     * @return std::size_t Number of free objects in pool.
     */
    [[nodiscard]] inline std::size_t free_size() const
    {
        std::scoped_lock lock(pool_mutex);
        return m_free_list.size();
    }

    void clear()
    {
        std::scoped_lock lock(pool_mutex);
        for (auto* i_object_ptr : m_object_list)
        {
            delete i_object_ptr;
        }
        m_object_list.clear();
        m_free_list.clear();
    }

private:
    void release(T* object_ptr)
    {
        std::scoped_lock lock(pool_mutex);
        m_free_list.push_back(object_ptr);
    };

    std::list<T*> m_object_list{};
    std::list<T*> m_free_list{};

    const std::function<T*()>     m_factory;
    const std::function<void(T*)> m_deleter;

    mutable std::mutex pool_mutex;
};

/**
 * @brief Smart pointer that manages an object from pool<>.
 * A pooled_ptr may also manage no objects.
 * If pooled_ptr manages an object, pooled_ptr releases object back into the parent pool when it goes out of scope.
 * pooled_ptr is move constructable and move assignable, but neigher copy constructable nor copy assignable.
 *
 * @tparam T
 */
template<class T>
class pooled_ptr
{
    friend class pool<T>;
    friend void swap(pooled_ptr& first, pooled_ptr& second)
    {
        using std::swap;
        swap(first.m_parent_pool, second.m_parent_pool);
        swap(first.m_data, second.m_data);
    }

public:
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
    // NOLINTBEGIN(google-explicit-constructor, hicpp-explicit-conversions)
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
    inline void release()
    {
        if (m_parent_pool != nullptr)
        {
            m_parent_pool->release(m_data);
            m_parent_pool = nullptr;
            m_data        = nullptr;
        }
    }

private:
    pooled_ptr(pool<T>* parent_pool, T* object_ptr)
    : m_parent_pool(parent_pool)
    , m_data(object_ptr){};

    pool<T>* m_parent_pool = nullptr;
    T*       m_data        = nullptr;
};
};    // namespace la

#endif
