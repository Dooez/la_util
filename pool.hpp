#ifndef LA_OBJECT_POOL_H
#define LA_OBJECT_POOL_H

#include <concepts>
#include <memory>
#include <mutex>

namespace mtmu {

template<class T>
class pooled_ptr;

namespace detail_ {

template<typename Allocator, typename T>
concept allocator_of = std::same_as<typename Allocator::value_type, T>;

template<typename F, typename T>
concept factory_of = requires(F&& factory, T* placement_ptr) {
                         { factory(placement_ptr) } -> std::same_as<T*>;
                     };

template<typename T, typename Allocator, typename F>
    requires factory_of<F, T>
class pool_ctrl_block_;

/**
 * @brief Manages a ring buffer of pointers to elements. Maximum size is capacity - 1.
 * When inserting exactly 1 element causes size to be equal to capacity, capacity is doubled.
 * Otherwise capacity is increased by the number of inserted elements.
 * Allocation, construction and deallocation of data and buffer are managed by the derived class.
 * Control block should be allocated on heap.
 * After calling abandon() control block waits for all managed elements to be released and calls destroy().
 */
template<typename T>
class pool_ctrl_block {
protected:
    pool_ctrl_block() = default;

public:
    using value_type = T;
    using pointer    = pooled_ptr<value_type>;

    pool_ctrl_block(const pool_ctrl_block& other)                = delete;
    pool_ctrl_block(pool_ctrl_block&& other) noexcept            = delete;
    pool_ctrl_block& operator=(const pool_ctrl_block& other)     = delete;
    pool_ctrl_block& operator=(pool_ctrl_block&& other) noexcept = delete;

    virtual ~pool_ctrl_block() = default;

    void populate(std::size_t insert_n) {
        std::scoped_lock lock_h(head_pool_mutex);
        std::scoped_lock lock_t(tail_pool_mutex);
        insert_no_mutex(insert_n);
    }

    [[nodiscard]] auto acquire() -> pointer {
        std::scoped_lock lock_t(tail_pool_mutex);
        if (m_head_idx != m_tail_idx) {
            auto* data = m_data[m_tail_idx];    //NOLINT(*pointer*)
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
            auto data  = m_data[m_tail_idx];    //NOLINT(*pointer*)
            m_tail_idx = (m_tail_idx + 1) % m_capacity;
            return {this, data};
        }
        return {};
    };

    void release(T* object_ptr) {
        std::scoped_lock lock_h(head_pool_mutex);
        if (m_abandoned) {
            delete_val(object_ptr);
            ++m_deleted;
            if (m_deleted == m_size)
                destroy();
        } else {
            m_data[m_head_idx] = object_ptr;    //NOLINT(*pointer*)
            m_head_idx         = (m_head_idx + 1) % m_capacity;
        }
    };

    void abandon() {
        std::scoped_lock lock_h(head_pool_mutex);
        std::scoped_lock lock_t(tail_pool_mutex);
        m_abandoned = true;
        while (m_tail_idx != m_head_idx) {
            delete_val(m_data[m_tail_idx]);    //NOLINT(*pointer*)
            ++m_deleted;
            m_tail_idx = (m_tail_idx + 1) % m_capacity;
        }
        deallocate_buffer(m_data, m_capacity);
        if (m_deleted == m_size)
            destroy();
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

private:
    std::size_t m_head_idx = 0;
    std::size_t m_tail_idx = 0;

    std::size_t m_size     = 0;
    std::size_t m_deleted  = 0;
    std::size_t m_capacity = 1;

    T** m_data = nullptr;

    bool m_abandoned = false;

    std::mutex tail_pool_mutex;
    std::mutex head_pool_mutex;

    [[nodiscard]] virtual auto new_val() -> T*                     = 0;
    virtual void               delete_val(T* ptr)                  = 0;
    [[nodiscard]] virtual auto allocate_buffer(std::size_t) -> T** = 0;
    virtual void               deallocate_buffer(T**, std::size_t) = 0;
    virtual void               destroy()                           = 0;

    auto insert_no_mutex(std::size_t insert_n) -> T* {
        if (m_size + insert_n >= m_capacity) {
            auto  new_capacity = insert_n > 1 ? m_capacity + insert_n : m_capacity * 2;
            auto* tmp_buf      = allocate_buffer(new_capacity);
            if (m_head_idx > m_tail_idx) {
                std::copy(m_data + m_tail_idx, m_data + m_tail_idx, tmp_buf);
            } else if (m_head_idx < m_tail_idx) {
                auto len_tail = m_capacity - m_tail_idx;
                std::copy(m_data + m_tail_idx, m_data + m_capacity - 1, tmp_buf);
                std::copy(m_data, m_data + m_head_idx, tmp_buf + len_tail);
            }
            if (m_data != nullptr)
                deallocate_buffer(m_data, m_capacity);

            m_data     = tmp_buf;
            m_capacity = new_capacity;
            m_head_idx = free_size();
            m_tail_idx = 0;
        }
        auto old_head = m_head_idx;
        for (uint i = 0; i < insert_n; ++i) {
            m_data[m_head_idx] = new_val();    //NOLINT(*pointer*)

            m_head_idx = (m_head_idx + 1) % m_capacity;
            ++m_size;
        }
        return (m_data[old_head]);    //NOLINT(*pointer*)
    }
};

/**
 * @brief Manages allocation and deallocation of data in pool.
 * Holds a copy of its own allocator. When destroy() is called,
 * uses the copy of allocator to self-destruct and deallocate.
 */
template<typename T, typename Allocator, typename F>
    requires factory_of<F, T>
class pool_ctrl_block_ final : public pool_ctrl_block<T> {
    using alloc_traits   = std::allocator_traits<Allocator>;
    using buffer_alloc_t = typename alloc_traits::template rebind_alloc<T*>;

public:
    using ctrl_block_alloc_t =
        typename alloc_traits::template rebind_alloc<pool_ctrl_block_<T, Allocator, F>>;

    pool_ctrl_block_() = delete;

    pool_ctrl_block_(ctrl_block_alloc_t this_allocator, Allocator allocator, F factory)
    : m_this_allocator(std::move(this_allocator))
    , m_allocator(std::move(allocator))
    , m_buffer_allocator(static_cast<buffer_alloc_t>(m_allocator))
    , m_factory(factory){};

    pool_ctrl_block_(const pool_ctrl_block_& other)     = delete;
    pool_ctrl_block_(pool_ctrl_block_&& other) noexcept = delete;

    pool_ctrl_block_& operator=(const pool_ctrl_block_& other)     = delete;
    pool_ctrl_block_& operator=(pool_ctrl_block_&& other) noexcept = delete;

    ~pool_ctrl_block_() final = default;

    void destroy() final {
        auto this_alloc = m_this_allocator;
        std::allocator_traits<ctrl_block_alloc_t>::destroy(this_alloc, this);
    }

    [[nodiscard]] auto allocate_buffer(std::size_t n) -> T** final {
        return std::allocator_traits<buffer_alloc_t>::allocate(m_buffer_allocator, n);
    };

    void deallocate_buffer(T** ptr, std::size_t n) final {
        std::allocator_traits<buffer_alloc_t>::deallocate(m_buffer_allocator, ptr, n);
    };

    [[nodiscard]] auto new_val() -> T* final {
        return m_factory(alloc_traits::allocate(m_allocator, 1));
    };

    void delete_val(T* ptr) final {
        alloc_traits::destroy(m_allocator, ptr);
    };

private:
    [[no_unique_address]] ctrl_block_alloc_t m_this_allocator;
    [[no_unique_address]] Allocator          m_allocator;
    [[no_unique_address]] buffer_alloc_t     m_buffer_allocator;

    F m_factory;
};

template<typename T, typename Allocator, typename... Args>
static auto allocate_ctrl_block(Allocator allocator, Args&&... args) {
    auto factory = [... args = std::forward<Args>(args)](T* placement_ptr) {
        return new (placement_ptr) T(args...);
    };
    using ctrl_block_alloc_t = typename pool_ctrl_block_<T, Allocator, decltype(factory)>::ctrl_block_alloc_t;

    auto  ctrl_block_alloc = static_cast<ctrl_block_alloc_t>(allocator);
    auto* ctrl_block_ptr   = std::allocator_traits<ctrl_block_alloc_t>::allocate(ctrl_block_alloc, 1);
    new (ctrl_block_ptr)
        pool_ctrl_block_<T, Allocator, decltype(factory)>(ctrl_block_alloc, allocator, factory);
    return ctrl_block_ptr;
};

template<typename T, typename Allocator, typename F>
    requires factory_of<F, T>
static auto allocate_ctrl_block(Allocator allocator, F&& factory) {
    using ctrl_block_alloc_t = typename pool_ctrl_block_<T, Allocator, F>::ctrl_block_alloc_t;

    auto  ctrl_block_alloc = static_cast<ctrl_block_alloc_t>(allocator);
    auto* ctrl_block_ptr   = std::allocator_traits<ctrl_block_alloc_t>::allocate(ctrl_block_alloc, 1);
    new (ctrl_block_ptr)
        pool_ctrl_block_<T, Allocator, F>(ctrl_block_alloc, allocator, std::forward<F>(factory));
    return ctrl_block_ptr;
};

}    // namespace detail_

/**
 * @brief Pool of reusable objects.
 * When necessary creates new elements using provided or generated factory.
 * Uses heap allocated internal control block that persists until the last element is released.
 *
 * @tparam T
 * @tparam Shared if true enables copy construction and assignment.
 */
template<class T, bool Shared = false>
class pool {
    friend class pooled_ptr<T>;
    friend void swap(pool& first, pool& second) {
        using std::swap;
        swap(first.m_ctrl_block, second.m_ctrl_block);
    };
    using ctrl_block_ptr_t = std::
        conditional_t<Shared, std::shared_ptr<detail_::pool_ctrl_block<T>>, detail_::pool_ctrl_block<T>*>;

public:
    using value_type = T;
    using pointer    = pooled_ptr<value_type>;

    /**
     * @param args arguments passed to operator new when creating data elements.
     */
    template<typename... Args>
        requires std::constructible_from<T, Args...> && (std::copy_constructible<Args> && ...)
    explicit pool(Args&&... args) {
        auto ctrl_block_ptr =
            detail_::allocate_ctrl_block<T>(std::allocator<T>{}, std::forward<Args>(args)...);
        if constexpr (Shared) {
            m_ctrl_block =
                ctrl_block_ptr_t(ctrl_block_ptr, [](detail_::pool_ctrl_block<T>* ptr) { ptr->abandon(); });
        } else {
            m_ctrl_block = ctrl_block_ptr;
        }
    };

    /**
     * @param allocator allocator used for allocation of internal control block and data elements.
     * @param args arguments passed to operator new when creating data elements.
     */
    template<typename Allocator, typename... Args>
        requires std::constructible_from<T, Args...> &&
                 (std::copy_constructible<Args> && ...) && detail_::allocator_of<Allocator, T>
    explicit pool(std::allocator_arg_t /*unused*/, Allocator allocator, Args&&... args) {
        auto ctrl_block_ptr = detail_::allocate_ctrl_block<T>(allocator, std::forward<Args>(args)...);
        if constexpr (Shared) {
            m_ctrl_block = ctrl_block_ptr_t(
                ctrl_block_ptr, [](detail_::pool_ctrl_block<T>* ptr) { ptr->abandon(); }, allocator);
        } else {
            m_ctrl_block = ctrl_block_ptr;
        }
    };


    /**
     * @param factory invoked to construct object in place `ptr = factory(placement_ptr)`.
     * @param allocator allocator used for allcation of internal control block and data elements.
     */
    template<typename F, typename Allocator = std::allocator<T>>
        requires detail_::factory_of<F, T> && detail_::allocator_of<Allocator, T>
    //NOLINTNEXTLINE(*forwarding*) const pool& would not satify factory_of<T>
    explicit pool(F&& factory, Allocator&& allocator = Allocator{}) {
        auto ctrl_block_ptr =
            detail_::allocate_ctrl_block<T>(std::forward<Allocator>(allocator), std::forward<F>(factory));
        if constexpr (Shared) {
            m_ctrl_block =
                ctrl_block_ptr_t(ctrl_block_ptr, [](detail_::pool_ctrl_block<T>* ptr) { ptr->abandon(); });
        } else {
            m_ctrl_block = ctrl_block_ptr;
        }
    };

    ~pool() {
        if constexpr (!Shared) {
            if (m_ctrl_block != nullptr) {
                m_ctrl_block->abandon();
            }
        }
    }
    pool(pool&& other) noexcept {
        swap(*this, other);
    };
    pool& operator=(pool&& other) noexcept {
        swap(*this, other);
    };

    pool(const pool&)            = delete;
    pool& operator=(const pool&) = delete;

    pool(const pool&)
        requires Shared
    = default;
    pool& operator=(const pool&)
        requires Shared
    = default;

    /**
     * @brief Creates new data elements and inserts them into pool.
     *
     * @param insert_n number of elements to insert.
     */
    void populate(std::size_t insert_n) {
        m_ctrl_block->populate(insert_n);
    }

    /**
     * @brief Acquires an element from pool. If no free elements are available creates a single new element.
     *
     * @return pooled_ptr<T> smart pointer to the element.
     */
    [[nodiscard]] auto acquire() -> pointer {
        return m_ctrl_block->acquire();
    };
    /**
     * @brief Attempts to acquire a free element from pool. If no free element are available returns an empty pooled_ptr.
     *
     * @return pooled_ptr<T> smart pointer to the element. Empty if no free elements are available.
     */
    [[nodiscard]] auto acquire_free() -> pointer {
        return m_ctrl_block->acquire_free();
    };

    /**
     * @brief Returns the total number of elements in the pool.
     * Does not lock mutexes, possibly inaccurate.
     *
     * @return std::size_t number of objects in pool.
     */
    [[nodiscard]] auto size() const -> std::size_t {
        return m_ctrl_block->size();
    }
    /**
     * @brief Returns the number of free elements in the pool.
     * Does not lock mutexes, possibly inaccurate.
     *
     * @return std::size_t number of free objects in pool.
     */
    [[nodiscard]] auto free_size() const -> std::size_t {
        return m_ctrl_block->free_size();
    }

private:
    ctrl_block_ptr_t m_ctrl_block{};
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
    using pool_t = typename detail_::pool_ctrl_block<T>;
    friend pool_t;
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
     * *this manages no objects after return.
     *
     * @return std::shared_ptr<T>
     */
    explicit operator std::shared_ptr<T>() {
        return to_shared_ptr();
    }

    /**
     * @brief Constructs and returns a shared_ptr that manages an object currently managed by *this.
     *  *this manages no objects after return.
     * @param allocator allocator passed to shared_ptr for internal allocations.
     */
    template<typename Allocator = std::allocator<T>>
    auto to_shared_ptr(Allocator allocator = Allocator{}) -> std::shared_ptr<T> {
        if (m_pool_ptr != nullptr) {
            auto ptr = std::shared_ptr<T>(
                m_data, [parent_pool = m_pool_ptr](T* ptr) { parent_pool->release(ptr); }, allocator);
            m_pool_ptr = nullptr;
            m_data     = nullptr;
            return ptr;
        }
        return {};
    }


    /**
     * @brief Returns true if pooled_ptr manages an object.
     *
     * @return true manages object.
     * @return false manages no object.
     */
    operator bool() const noexcept {    //NOLINT(*explicit*)
        return (m_pool_ptr != nullptr);
    }

    /**
     * @brief Returns the pointer to the managed object.
     */
    [[nodiscard]] auto get() const -> pointer {
        return m_data;
    }
    /**
     * @brief Dereferences pointer to the managed object.
     */
    [[nodiscard]] auto operator*() const -> element_type& {
        return *m_data;
    }
    /**
     * @brief Accesses members of the managed object.
     */
    [[nodiscard]] auto operator->() const -> pointer {
        return m_data;
    }

    /**
     * @brief Releases managed object back into the pool. *this manages no objects afterwards.
     */
    void release() {
        if (m_pool_ptr != nullptr) {
            m_pool_ptr->release(m_data);
            m_pool_ptr = nullptr;
            m_data     = nullptr;
        }
    }

private:
    pooled_ptr(pool_t* parent_pool, pointer object_ptr)
    : m_pool_ptr(parent_pool)
    , m_data(object_ptr){};

    pool_t* m_pool_ptr = nullptr;
    pointer m_data     = nullptr;
};

};    // namespace mtmu

#endif
