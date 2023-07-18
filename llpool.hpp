#ifndef LA_OBJECT_POOL_H
#define LA_OBJECT_POOL_H

#include <atomic>
#include <concepts>
#include <memory>
#include <mutex>

namespace mtmu::ll {

using uZ  = std::size_t;
using u32 = uint32_t;

namespace detail_ {
template<typename T>
class pool_releaser;
}    // namespace detail_

template<typename T>
using pooled_ptr = std::unique_ptr<T, detail_::pool_releaser<T>>;

namespace detail_ {

template<typename Allocator, typename T>
concept allocator_of = std::same_as<typename Allocator::value_type, T>;

template<typename F, typename T>
concept factory_of = requires(F&& factory, T* placement_ptr) {
                         { factory(placement_ptr) } -> std::same_as<T*>;
                     };

/**
 * @brief Manages a ring buffer of pointers to elements. Maximum size is capacity - 1.
 * When inserting exactly 1 element causes size to be equal to capacity, capacity is doubled.
 * Otherwise capacity is increased by the number of inserted elements.
 * Allocation, construction and deallocation of data and buffer are managed by the derived class.
 * Control block should be allocated on heap.
 * After calling abandon() control block waits for all managed elements to be released and calls destroy().
 */
template<typename T>
class pool_ctrl_block_common {
protected:
    pool_ctrl_block_common() = default;

public:
    using value_type = T;
    using pointer    = pooled_ptr<value_type>;

    pool_ctrl_block_common(const pool_ctrl_block_common& other)                = delete;
    pool_ctrl_block_common(pool_ctrl_block_common&& other) noexcept            = delete;
    pool_ctrl_block_common& operator=(const pool_ctrl_block_common& other)     = delete;
    pool_ctrl_block_common& operator=(pool_ctrl_block_common&& other) noexcept = delete;

    virtual ~pool_ctrl_block_common() = default;

    [[nodiscard]] auto try_acquire() -> pointer {
        auto tail = m_tail.load(std::memory_order_acquire);
        auto head = m_head.load(std::memory_order_acquire);
        if (tail == head)
            return {};
        while (!m_tail.compare_exchange_strong(
            tail, tail + 1, std::memory_order_acq_rel, std::memory_order_acquire)) {
            if (tail == m_head.load(std::memory_order_acquire))
                return {};
        }
        tail %= m_ring_size;
        //NOLINTBEGIN(*pointer*)
        auto ptr = m_ring_buffer[tail].load(std::memory_order_acquire);
        while (ptr == nullptr) {
            ptr = m_ring_buffer[tail].load(std::memory_order_acquire);
        }
        m_ring_buffer[tail].store(nullptr, std::memory_order_release);
        //NOLINTEND(*pointer*)
        return {ptr, pool_releaser(this)};
    };

    void release(T* object_ptr) {
        if (m_abandoned.load(std::memory_order_acquire)) {
            object_ptr->~T();
            auto deleted = m_deleted.fetch_add(1, std::memory_order_acq_rel);
            if (m_deleted + 1 + (m_head - m_tail) == m_size)
                cleanup();
            return;
        }
        auto head = m_head.fetch_add(1, std::memory_order_acq_rel);
        head %= m_ring_size;
        //NOLINTNEXTLINE(*pointer*)
        while (!m_ring_buffer[head].compare_exchange_strong(
            nullptr, object_ptr, std::memory_order_acq_rel, std::memory_order_relaxed)) {};
    };

    inline void abandon() {
        m_abandoned.store(true, std::memory_order_release);
        if ((m_head - m_tail) == m_size)
            cleanup();
    }

    [[nodiscard]] auto size() const -> uZ {
        return m_size;
    }

    [[nodiscard]] auto free_size() const -> uZ {
        return m_head - m_tail;
    }

private:
    uZ m_size      = 0;
    uZ m_ring_size = 1;


    std::atomic<u32> m_head;
    std::atomic<u32> m_tail;

    std::atomic<T*>* m_ring_buffer;
    std::atomic<uZ>  m_deleted = 0;

    std::atomic<bool> m_abandoned = false;

    virtual void destroy() = 0;

    void cleanup() {
        auto tail = m_tail.load(std::memory_order_acquire);
        auto head = m_tail.load(std::memory_order_acquire);
        while (tail != head) {
            //NOLINTBEGIN(*pointer*)
            auto ptr = m_ring_buffer[tail % m_ring_size].load(std::memory_order_acquire);
            while (ptr == nullptr) {
                ptr = m_ring_buffer[tail % m_ring_size].load(std::memory_order_acquire);
            }
            //NOLINTEND(*pointer*)
            ptr->~T();
            ++tail;
        }
    }
};

template<typename T>
class pool_releaser {
    friend void swap(pool_releaser& first, pool_releaser& second) {
        using std::swap;
        swap(first.parent_pool_ptr, second.parent_pool_ptr);
    }
    friend class pool_ctrl_block_common<T>;

    explicit pool_releaser(pool_ctrl_block_common<T>* parent)
    : parent_pool_ptr(parent){};

public:
    pool_releaser() = default;

    pool_releaser(pool_releaser&& other) noexcept {
        swap(*this, other);
    };
    pool_releaser& operator=(pool_releaser&& other) noexcept {
        swap(*this, other);
        return *this;
    };

    pool_releaser(const pool_releaser& other)            = delete;
    pool_releaser& operator=(const pool_releaser& other) = delete;

    ~pool_releaser() = default;

    void operator()(T* ptr) {
        if (ptr != nullptr)
            parent_pool_ptr->release(ptr);
    }

private:
    pool_ctrl_block_common<T>* parent_pool_ptr = nullptr;
};

/**
 * @brief Manages allocation and deallocation of data in pool.
 * Holds a copy of its own allocator. When destroy() is called,
 * uses the copy of allocator to self-destruct and deallocate.
 */
template<typename T, typename Allocator, typename F>
    requires factory_of<F, T>
class pool_ctrl_block final : public pool_ctrl_block_common<T> {
    using alloc_traits   = std::allocator_traits<Allocator>;
    using buffer_alloc_t = typename alloc_traits::template rebind_alloc<T*>;

public:
    using ctrl_block_alloc_t = typename alloc_traits::template rebind_alloc<pool_ctrl_block<T, Allocator, F>>;

    pool_ctrl_block() = delete;

    pool_ctrl_block(ctrl_block_alloc_t this_allocator, Allocator allocator, F factory)
    : m_this_allocator(std::move(this_allocator))
    , m_allocator(std::move(allocator))
    , m_buffer_allocator(static_cast<buffer_alloc_t>(m_allocator))
    , m_factory(factory){};

    pool_ctrl_block(const pool_ctrl_block& other)     = delete;
    pool_ctrl_block(pool_ctrl_block&& other) noexcept = delete;

    pool_ctrl_block& operator=(const pool_ctrl_block& other)     = delete;
    pool_ctrl_block& operator=(pool_ctrl_block&& other) noexcept = delete;

    ~pool_ctrl_block() final = default;

    void destroy() final {
        auto this_alloc = m_this_allocator;
        std::allocator_traits<ctrl_block_alloc_t>::destroy(this_alloc, this);
    }

    [[nodiscard]] auto allocate_buffer(uZ n) -> T** final {
        return std::allocator_traits<buffer_alloc_t>::allocate(m_buffer_allocator, n);
    };

    void deallocate_buffer(T** ptr, uZ n) final {
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
    using ctrl_block_alloc_t = typename pool_ctrl_block<T, Allocator, decltype(factory)>::ctrl_block_alloc_t;

    auto  ctrl_block_alloc = static_cast<ctrl_block_alloc_t>(allocator);
    auto* ctrl_block_ptr   = std::allocator_traits<ctrl_block_alloc_t>::allocate(ctrl_block_alloc, 1);
    new (ctrl_block_ptr)
        pool_ctrl_block<T, Allocator, decltype(factory)>(ctrl_block_alloc, allocator, factory);
    return ctrl_block_ptr;
};

template<typename T, typename Allocator, typename F>
    requires factory_of<F, T>
static auto allocate_ctrl_block(Allocator allocator, F&& factory) {
    using ctrl_block_alloc_t = typename pool_ctrl_block<T, Allocator, F>::ctrl_block_alloc_t;

    auto  ctrl_block_alloc = static_cast<ctrl_block_alloc_t>(allocator);
    auto* ctrl_block_ptr   = std::allocator_traits<ctrl_block_alloc_t>::allocate(ctrl_block_alloc, 1);
    new (ctrl_block_ptr)
        pool_ctrl_block<T, Allocator, F>(ctrl_block_alloc, allocator, std::forward<F>(factory));
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
    friend void swap(pool& first, pool& second) {
        using std::swap;
        swap(first.m_ctrl_block, second.m_ctrl_block);
    };
    using ctrl_block_ptr_t = std::conditional_t<Shared,
                                                std::shared_ptr<detail_::pool_ctrl_block_common<T>>,
                                                detail_::pool_ctrl_block_common<T>*>;

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
            m_ctrl_block = ctrl_block_ptr_t(ctrl_block_ptr,
                                            [](detail_::pool_ctrl_block_common<T>* ptr) { ptr->abandon(); });
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
                ctrl_block_ptr, [](detail_::pool_ctrl_block_common<T>* ptr) { ptr->abandon(); }, allocator);
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
            m_ctrl_block = ctrl_block_ptr_t(ctrl_block_ptr,
                                            [](detail_::pool_ctrl_block_common<T>* ptr) { ptr->abandon(); });
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
        return *this;
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
    void populate(uZ insert_n) {
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
     * @return uZ number of objects in pool.
     */
    [[nodiscard]] auto size() const -> uZ {
        return m_ctrl_block->size();
    }
    /**
     * @brief Returns the number of free elements in the pool.
     * Does not lock mutexes, possibly inaccurate.
     *
     * @return uZ number of free objects in pool.
     */
    [[nodiscard]] auto free_size() const -> uZ {
        return m_ctrl_block->free_size();
    }

private:
    ctrl_block_ptr_t m_ctrl_block = nullptr;
};
template<class T>
using shared_pool = pool<T, true>;
};    // namespace mtmu::ll

#endif
