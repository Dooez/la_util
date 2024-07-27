#ifndef LA_OBJECT_POOL_H
#define LA_OBJECT_POOL_H

#include <algorithm>
#include <atomic>
#include <concepts>
#include <cstdint>
#include <memory>
#include <type_traits>

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
concept placement_ctor = requires(F&& p_ctor, T* placement_ptr) {
    { p_ctor(placement_ptr) } -> std::same_as<T*>;
};

enum class pool_memory_management {
    managed,
    memory_leak,
    terminate
};

/**
 * @brief Manages a ring buffer of pointers to elements. Maximum size is capacity - 1.
 * When inserting exactly 1 element causes size to be equal to capacity, capacity is doubled.
 * Otherwise capacity is increased by the number of inserted elements.
 * Allocation, construction and deallocation of data and buffer are managed by the derived class.
 * Control block should be allocated on heap.
 * After calling abandon() control block waits for all managed elements to be released and calls destroy().
 */
template<typename T, bool Managed>
class pool_ctrl_block_common {
protected:
    using value_t      = T;
    using atomic_ptr_t = std::atomic<value_t*>;    // Potentially overalign
    using atomic_cnt_t = std::atomic<uZ>;          // Potentially overalign
public:
    using pointer = pooled_ptr<value_t>;

    pool_ctrl_block_common(uZ size, uZ ring_size, atomic_ptr_t* ring, atomic_cnt_t* counters);

    pool_ctrl_block_common(const pool_ctrl_block_common& other)                = delete;
    pool_ctrl_block_common(pool_ctrl_block_common&& other) noexcept            = delete;
    pool_ctrl_block_common& operator=(const pool_ctrl_block_common& other)     = delete;
    pool_ctrl_block_common& operator=(pool_ctrl_block_common&& other) noexcept = delete;

    virtual ~pool_ctrl_block_common() = default;

    [[nodiscard]] auto try_acquire() -> pointer {
        auto tail = m_tail.load(std::memory_order_acquire);
        if (tail == m_head.load(std::memory_order_acquire))
            return {};
        while (!m_tail.compare_exchange_strong(tail, tail + 1, std::memory_order_acq_rel)) {
            if (tail == m_head.load(std::memory_order_acquire))
                return {};
        }
        tail %= m_ring_size;
        auto ptr = m_ring_buffer[tail].load(std::memory_order_acquire);
        while (true) {
            if (ptr == nullptr) {
                ptr = m_ring_buffer[tail].load(std::memory_order_acquire);
                continue;
            }
            if (m_ring_buffer[tail].compare_exchange_strong(ptr, nullptr, std::memory_order_acq_rel))
                break;
        }
        return {ptr, pool_releaser(this)};
    };

    void release(T* object_ptr) {
        if constexpr (Managed) {
            if (m_abandoned.load(std::memory_order_acquire)) {
                object_ptr->~T();
                auto deleted = m_deleted.fetch_add(1, std::memory_order_acq_rel);
                if (deleted + 1 +
                        (m_head.load(std::memory_order_acquire) - m_tail.load(std::memory_order_acquire)) ==
                    m_size)
                    cleanup();
                return;
            }
        }
        auto head = m_head.fetch_add(1, std::memory_order_acq_rel);
        head %= m_ring_size;
        auto ptr = m_ring_buffer[head].load(std::memory_order_acquire);
        while (true) {
            if (ptr != nullptr) {
                ptr = m_ring_buffer[head].load(std::memory_order_acquire);
                continue;
            }
            if (m_ring_buffer[head].compare_exchange_strong(ptr, object_ptr, std::memory_order_acq_rel))
                break;
        }
    };

    [[nodiscard]] auto size() const -> uZ {
        return m_size;
    }

    [[nodiscard]] auto free_size() const -> uZ {
        auto head = m_head.load(std::memory_order_acquire);
        auto tail = m_tail.load(std::memory_order_acquire);
        return head - tail;
    }

private:
    uZ            m_size;
    uZ            m_ring_size;
    atomic_ptr_t* m_ring_buffer;

    atomic_cnt_t m_head;
    atomic_cnt_t m_tail;

    using deleted_t = std::conditional_t<Managed, std::atomic<bool>, decltype([] {})>;
    [[no_unique_address]] deleted_t m_deleted{};
    using abandoned_t = std::conditional_t<Managed, std::atomic<bool>, decltype([] {})>;
    [[no_unique_address]] abandoned_t m_abandoned{};


    void cleanup() {
        auto tail = m_tail.load(std::memory_order_acquire);
        auto head = m_tail.load(std::memory_order_acquire);
        while (tail != head) {
            auto ptr = m_ring_buffer[tail % m_ring_size].load(std::memory_order_acquire);
            while (ptr == nullptr)
                ptr = m_ring_buffer[tail % m_ring_size].load(std::memory_order_acquire);
            ptr->~T();
            ++tail;
        }
    }
};

/**
 * @brief Manages allocation and deallocation of data in pool.
 * Holds a copy of its own allocator. When destroy() is called,
 * uses the copy of allocator to self-destruct and deallocate.
 */
template<typename T, typename Allocator, bool Managed>
class pool_ctrl_block {
    using value_t      = T;
    using ctrl_block_t = pool_ctrl_block_common<value_t, Managed>;
    using atomic_ptr_t = ctrl_block_t::atomic_ptr_t;
    using atomic_cnt_t = ctrl_block_t::atomic_cnt_t;

    using allocator_t  = std::allocator_traits<Allocator>::template rebind_alloc<std::byte>;
    using alloc_traits = std::allocator_traits<allocator_t>;

public:
    pool_ctrl_block() = delete;

    explicit pool_ctrl_block(
        const allocator_t& allocator, uZ size, uZ ring_size, atomic_ptr_t* ring, atomic_cnt_t* counters)
    : m_allocator(allocator)
    , m_ctrl_block(size, ring_size, ring, counters) {};

    pool_ctrl_block(const pool_ctrl_block& other)     = delete;
    pool_ctrl_block(pool_ctrl_block&& other) noexcept = delete;

    pool_ctrl_block& operator=(const pool_ctrl_block& other)     = delete;
    pool_ctrl_block& operator=(pool_ctrl_block&& other) noexcept = delete;

    ~pool_ctrl_block() = default;

    void destroy() {
        /*auto this_alloc = m_this_allocator;*/
        /*std::allocator_traits<ctrl_block_alloc_t>::destroy(this_alloc, this);*/
    }

private:
    static constexpr auto alignment = std::max({alignof(pool_ctrl_block),    //
                                                alignof(atomic_ptr_t),
                                                alignof(atomic_cnt_t),
                                                alignof(value_t)});

    static constexpr auto next_pow_2(std::uint64_t v) {
        v--;
        v |= v >> 1U;
        v |= v >> 2U;
        v |= v >> 4U;
        v |= v >> 8U;
        v |= v >> 16U;
        v |= v >> 32U;
        v++;
        return v;
    }

    static constexpr auto calc_size(uZ pool_size, uZ ring_size) -> uZ {
        constexpr auto ctrl_align = alignof(pool_ctrl_block);
        constexpr auto aptr_align = alignof(atomic_ptr_t);
        constexpr auto acnt_align = alignof(atomic_cnt_t);
        constexpr auto data_align = alignof(value_t);

        constexpr auto ctrl_size = sizeof(pool_ctrl_block);
        constexpr auto ctrl_size_al =
            ctrl_size + (ctrl_size % aptr_align > 0 ? aptr_align - ctrl_size % aptr_align : 0);

        auto ctrl_ring_size = ctrl_size_al + ring_size * sizeof(atomic_ptr_t);
        auto ctrl_ring_size_al =
            ctrl_ring_size + (ctrl_ring_size % acnt_align > 0 ? acnt_align - ctrl_ring_size % acnt_align : 0);

        auto non_data_size = ctrl_ring_size + (pool_size + 1) * sizeof(atomic_cnt_t);
        auto non_data_size_al =
            non_data_size + (non_data_size % data_align > 0 ? data_align - non_data_size % data_align : 0);

        return non_data_size_al + pool_size * sizeof(value_t);
    }


    static auto alloc(uZ pool_size, const Allocator& allocator, auto placement_ctor) {
        auto  ring_size    = next_pow_2(pool_size);
        auto  raw_size     = calc_size(pool_size, ring_size);
        auto  storage_size = raw_size + alignment - 1;    // ensure alignment possible
        auto  new_alloc    = static_cast<allocator_t>(allocator);
        auto* storage      = alloc_traits::allocate(new_alloc, storage_size);
        auto* ptr          = storage;

        auto  space    = storage_size;
        auto* ctrl_ptr = reinterpret_cast<pool_ctrl_block*>(std::align(alignof(pool_ctrl_block),    //
                                                                       1,
                                                                       ptr,
                                                                       space));
        if (ctrl_ptr == nullptr)
            return;
        auto* ring_ptr = reinterpret_cast<atomic_ptr_t*>(std::align(alignof(atomic_ptr_t),    //
                                                                    ring_size,
                                                                    ptr,
                                                                    space));
        if (ring_ptr == nullptr)
            return;
        auto* cnt_ptr = reinterpret_cast<atomic_cnt_t*>(std::align(alignof(atomic_cnt_t),    //
                                                                   pool_size + 1,
                                                                   ptr,
                                                                   space));
        if (cnt_ptr == nullptr)
            return;
        auto* data_ptr = reinterpret_cast<value_t*>(std::align(alignof(value_t),    //
                                                               pool_size,
                                                               ptr,
                                                               space));
        if (data_ptr == nullptr)
            return;

        uZ created = 0;
        try {
            auto* x = new (ctrl_ptr) pool_ctrl_block(new_alloc, pool_size, ring_size, ring_ptr, cnt_ptr);
            for (; created < pool_size; ++created) {
                placement_ctor(data_ptr[created]);
            }
        } catch (...) {
            for (uZ i = 0; i < created; ++i) {
                data_ptr[i]->~value_t();
            }
            ctrl_ptr->~pool_ctrl_block();
            alloc_traits::deallocate(new_alloc, storage, storage_size);
            throw;
        }
    };

    allocator_t  m_allocator;
    ctrl_block_t m_ctrl_block;
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
    requires placement_ctor<F, T>
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
        requires detail_::placement_ctor<F, T> && detail_::allocator_of<Allocator, T>
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
