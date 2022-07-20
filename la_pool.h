#ifndef LA_OBJECT_POOL_H
#define LA_OBJECT_POOL_H

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
     * @tparam Args
     * @param args Arguments passed to operator new when creating new object.
     */
    template<typename... Args>
    explicit pool(Args&&... args)
    : m_factory([args...] { return new T(args...); }){};

    /**
     * @brief Construct a new pool object.
     *
     * @param factory Creates object and returns pointer to created object.
     */
    explicit pool(std::function<T*()> factory)
    : m_factory(std::move(factory)){};

    // template<typename... Args>
    // explicit pool(Args&&... args)
    // : m_factory([... args = std::forward<Args>(args)]{
    //     return new T(args...);
    // }){};

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
     * @brief Tries to acquire a free object from pool. If no free objects are available returns empty pooled_ptr.
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

    const std::function<T*()> m_factory;

    mutable std::mutex pool_mutex;
};

/**
 * @brief Smart pointer that manages object from pool<>.
 * pooled_ptr may alternatively manage no object.
 * If pooled_ptr manages an object, pooled_ptr releases object back into pool when it goes out of scope.
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
     * @brief Construct a new pooled_ptr object that manages no object.
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

    /**
     * @brief Returns true if pooled_ptr manages an object.
     *
     * @return true Manages object.
     * @return false Manages no object.
     */
    // NOLINTNEXTLINE(google-explicit-constructor, hicpp-explicit-conversions)
    operator bool() const noexcept
    {
        return (m_parent_pool != nullptr);
    }

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
     * @brief Releases managed object back into pool. Manages no object afterwards.
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
