#ifndef LA_OBJECT_POOL_H
#define LA_OBJECT_POOL_H

#include <algorithm>
#include <atomic>
#include <concepts>
#include <cstdint>
#include <cstdlib>
#include <exception>
#include <memory>
#include <type_traits>

namespace mtmu::ll {

using uZ  = std::size_t;
using u32 = uint32_t;

enum class pool_memory_management {
    managed,
    memory_leak,
    terminate
};
template<typename T, typename Allocator, pool_memory_management MemManagement>
class pool;

namespace detail_ {
template<typename T>
class pool_releaser;
}    // namespace detail_

template<typename T, bool Managed>
class pooled_ptr;

template<typename T, bool Managed>
class arc_pooled_ptr;

namespace detail_ {

template<typename Allocator, typename T>
concept allocator_of = std::same_as<typename Allocator::value_type, T>;

template<typename F, typename T>
concept placement_ctor = requires(F&& p_ctor, T* placement_ptr) {
    { p_ctor(placement_ptr) } -> std::same_as<T*>;
};
template<typename T, uZ align>
class alignas(std::max(align, alignof(T))) aligned : public T {
    using T::T;
};
constexpr uZ align_n = 64;


/**
 * @brief Manages a ring buffer of pointers to elements. Maximum size is capacity - 1.
 * When inserting exactly 1 element causes size to be equal to capacity, capacity is doubled.
 * Otherwise capacity is increased by the number of inserted elements.
 * Allocation, construction and deallocation of data and buffer are managed by the derived class.
 * Control block should be allocated on heap.
 * After calling abandon() control block waits for all managed elements to be released and calls destroy().
 */
template<typename T, bool Managed>
struct pool_ctrl_block_common {
    using value_t = T;
    /*using atomic_ptr_t = std::atomic<value_t*>;*/
    /*using atomic_cnt_t = std::atomic<uZ>;*/
    using atomic_ptr_t = aligned<std::atomic<value_t*>, align_n>;
    using atomic_cnt_t = aligned<std::atomic<uZ>, align_n>;
    using atomic_flg_t = aligned<std::atomic<bool>, align_n>;

    static_assert(atomic_ptr_t::is_always_lock_free);
    static_assert(atomic_cnt_t::is_always_lock_free);
    static_assert(atomic_flg_t::is_always_lock_free);

    using deallocate_fptr_t = std::conditional_t<Managed, void (*)(void*), decltype([] {})>;
    using rawptr_t          = std::conditional_t<Managed, void*, decltype([] {})>;

    using pointer = pooled_ptr<value_t, Managed>;

    pool_ctrl_block_common(uZ            size,    //
                           uZ            ring_size,
                           value_t*      data_begin,
                           atomic_ptr_t* ring,
                           atomic_cnt_t* counters) noexcept
        requires(!Managed)
    : m_size(size)
    , m_ring_size(ring_size)
    , m_data_begin(data_begin)
    , m_ring_buffer(ring)
    , m_counters(counters) {};

    pool_ctrl_block_common(uZ                size,
                           uZ                ring_size,
                           value_t*          data_begin,
                           atomic_ptr_t*     ring,
                           atomic_cnt_t*     counters,
                           deallocate_fptr_t deallocate_fptr,
                           rawptr_t          rawptr) noexcept
        requires(Managed)
    : m_size(size)
    , m_ring_size(ring_size)
    , m_data_begin(data_begin)
    , m_ring_buffer(ring)
    , m_counters(counters)
    , m_destroy_fptr(deallocate_fptr)
    , m_owner_ptr(rawptr) {};

    pool_ctrl_block_common(const pool_ctrl_block_common& other)                = delete;
    pool_ctrl_block_common(pool_ctrl_block_common&& other) noexcept            = delete;
    pool_ctrl_block_common& operator=(const pool_ctrl_block_common& other)     = delete;
    pool_ctrl_block_common& operator=(pool_ctrl_block_common&& other) noexcept = delete;

    virtual ~pool_ctrl_block_common() = default;

    [[nodiscard]] auto try_acquire() noexcept -> pointer {
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
        return {ptr, this};
    };

    void release(T* object_ptr) noexcept {
        if constexpr (Managed) {
            if (m_abandoned.load(std::memory_order_acquire)) {
                object_ptr->~T();
                auto deleted = m_deleted.fetch_add(1, std::memory_order_acq_rel);
                if (deleted + 1 +
                        (m_head.load(std::memory_order_acquire) - m_tail.load(std::memory_order_acquire)) ==
                    m_size)
                    m_destroy_fptr(m_owner_ptr);
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

    [[nodiscard]] auto free_size() const noexcept -> uZ {
        auto head = m_head.load(std::memory_order_acquire);
        auto tail = m_tail.load(std::memory_order_acquire);
        return head - tail;
    }

    [[nodiscard]] auto is_full() const noexcept -> bool {
        auto head = m_head.load(std::memory_order_acquire);
        auto tail = m_tail.load(std::memory_order_acquire);
        return head - tail == m_size                                //
               && head == m_head.load(std::memory_order_acquire)    //
               && tail == m_tail.load(std::memory_order_acquire);
    }

    void abandon() noexcept
        requires(Managed)
    {
        m_abandoned.store(true, std::memory_order_release);
    }

    void cleanup() noexcept {
        auto tail = m_tail.load(std::memory_order_acquire);
        auto head = m_tail.load(std::memory_order_acquire);
        while (tail != head) {
            auto ptr = m_ring_buffer[tail % m_ring_size].load(std::memory_order_acquire);
            while (ptr == nullptr)
                ptr = m_ring_buffer[tail % m_ring_size].load(std::memory_order_acquire);
            ptr->~T();
            ++tail;
        }

        for (uZ i = 0; i < m_ring_size; ++i)
            m_ring_buffer[i].~atomic_ptr_t();
        for (uZ i = 0; i < m_size; ++i)
            m_counters[i].~atomic_cnt_t();
    }

    uZ            m_size;
    uZ            m_ring_size;
    value_t*      m_data_begin;
    atomic_ptr_t* m_ring_buffer;
    atomic_cnt_t* m_counters;

    atomic_cnt_t m_head;
    atomic_cnt_t m_tail;

    using deleted_t   = std::conditional_t<Managed, atomic_cnt_t, decltype([] {})>;
    using abandoned_t = std::conditional_t<Managed, atomic_flg_t, decltype([] {})>;
    /*using deleted_t   = std::conditional_t<true, atomic_cnt_t, decltype([] {})>;*/
    /*using abandoned_t = std::conditional_t<true, atomic_flg_t, decltype([] {})>;*/

    [[no_unique_address]] deleted_t         m_deleted{};
    [[no_unique_address]] abandoned_t       m_abandoned{};
    [[no_unique_address]] deallocate_fptr_t m_destroy_fptr;
    [[no_unique_address]] rawptr_t          m_owner_ptr;
};

/**
 * @brief Manages allocation and deallocation of data in pool.
 * Holds a copy of its own allocator. When destroy() is called,
 * uses the copy of allocator to self-destruct and deallocate.
 */
template<typename T,
         typename Allocator,
         pool_memory_management MemManagement = pool_memory_management::managed>
class pool_ctrl_block {
    static constexpr bool managed = MemManagement == pool_memory_management::managed;

    using value_t        = T;
    using common_block_t = pool_ctrl_block_common<value_t, managed>;
    using atomic_ptr_t   = common_block_t::atomic_ptr_t;
    using atomic_cnt_t   = common_block_t::atomic_cnt_t;
    using pointer        = common_block_t::pointer;

    using allocator_t  = std::allocator_traits<Allocator>::template rebind_alloc<std::byte>;
    using alloc_traits = std::allocator_traits<allocator_t>;

    friend class pool<T, Allocator, MemManagement>;

    explicit pool_ctrl_block(const allocator_t& allocator,
                             std::byte*         storage,
                             uZ                 storage_size,
                             uZ                 size,
                             uZ                 ring_size,
                             value_t*           data_begin,
                             atomic_ptr_t*      ring,
                             atomic_cnt_t*      element_counters)
        requires(managed)
    : m_allocator(allocator)
    , m_storage_ptr(storage)
    , m_storage_size(storage_size)
    , m_ctrl_block(size, ring_size, data_begin, ring, element_counters, destroy_and_deallocate_erased, this) {
    };
    explicit pool_ctrl_block(const allocator_t& allocator,
                             std::byte*         storage,
                             uZ                 storage_size,
                             uZ                 size,
                             uZ                 ring_size,
                             value_t*           data_begin,
                             atomic_ptr_t*      ring,
                             atomic_cnt_t*      element_counters)
        requires(!managed)
    : m_allocator(allocator)
    , m_storage_ptr(storage)
    , m_storage_size(storage_size)
    , m_ctrl_block(size, ring_size, data_begin, ring, element_counters) {};

    /*pool_ctrl_block_common(uZ            size,    //*/
    /*                       uZ            ring_size,*/
    /*                       value_t*      data_begin,*/
    /*                       atomic_ptr_t* ring,*/
    /*                       atomic_cnt_t* counters) noexcept*/

public:
    pool_ctrl_block() = delete;

    pool_ctrl_block(const pool_ctrl_block& other)     = delete;
    pool_ctrl_block(pool_ctrl_block&& other) noexcept = delete;

    pool_ctrl_block& operator=(const pool_ctrl_block& other)     = delete;
    pool_ctrl_block& operator=(pool_ctrl_block&& other) noexcept = delete;

    ~pool_ctrl_block() = default;

    auto try_acquire() noexcept -> pointer {
        return m_ctrl_block.try_acquire();
    }

    void increment() noexcept {
        m_counter.fetch_add(1, std::memory_order_acq_rel);
    }
    void decrement() noexcept {
        auto rem = m_counter.fetch_sub(1, std::memory_order_acq_rel);
        if (rem == 1) {
            if constexpr (managed) {
                m_ctrl_block.abandon();
                return;
            } else {
                using enum pool_memory_management;
                if (m_ctrl_block.is_full()) {
                    destroy_and_deallocate();
                    return;
                }
                if constexpr (MemManagement == memory_leak) {
                    return;
                } else if constexpr (MemManagement == terminate) {
                    std::terminate();
                }
            }
        }
    }

private:
    void destroy_and_deallocate() noexcept {
        m_ctrl_block.cleanup();

        auto  allocator    = m_allocator;
        auto* storage      = m_storage_ptr;
        auto  storage_size = m_storage_size;
        this->~pool_ctrl_block();
        alloc_traits::deallocate(allocator, storage, storage_size);
    }
    static void destroy_and_deallocate_erased(void* ptr) noexcept {
        reinterpret_cast<pool_ctrl_block*>(ptr)->destroy_and_deallocate();
    }

    static constexpr auto next_pow_2(std::uint64_t v) noexcept {
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

    static constexpr auto calc_size(uZ pool_size, uZ ring_size) noexcept -> uZ {
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

        auto non_data_size = ctrl_ring_size + pool_size * sizeof(atomic_cnt_t);
        auto non_data_size_al =
            non_data_size + (non_data_size % data_align > 0 ? data_align - non_data_size % data_align : 0);

        return non_data_size_al + pool_size * sizeof(value_t);
    }

    template<typename U>
    static auto align(uZ count, void*& ptr, uZ& space) -> U* {
        auto  l_ptr     = ptr;
        auto  l_space   = space;
        auto  byte_size = count * sizeof(U);
        auto* ret       = reinterpret_cast<U*>(std::align(alignof(U), byte_size, l_ptr, l_space));
        if (ret != nullptr) {
            l_ptr = reinterpret_cast<std::byte*>(l_ptr) + byte_size;
            l_space -= byte_size;
        }
        ptr   = l_ptr;
        space = l_space;
        return ret;
    }

    static auto allocate(uZ pool_size, const Allocator& allocator, auto placement_ctor) -> pool_ctrl_block* {
        constexpr auto alignment = std::max({alignof(pool_ctrl_block),    //
                                             alignof(atomic_ptr_t),
                                             alignof(atomic_cnt_t),
                                             alignof(value_t)});

        auto  ring_size    = next_pow_2(pool_size);
        auto  raw_size     = calc_size(pool_size, ring_size);
        auto  storage_size = raw_size + alignment - 1;    // ensure alignment possible
        auto  new_alloc    = static_cast<allocator_t>(allocator);
        auto* storage      = alloc_traits::allocate(new_alloc, storage_size);

        void* ptr      = storage;
        auto  space    = storage_size;
        auto* ctrl_ptr = align<pool_ctrl_block>(1, ptr, space);
        if (ctrl_ptr == nullptr)
            return nullptr;

        auto* ring_ptr = align<atomic_ptr_t>(ring_size, ptr, space);
        if (ring_ptr == nullptr)
            return nullptr;

        auto* cnt_ptr = align<atomic_cnt_t>(pool_size, ptr, space);
        if (cnt_ptr == nullptr)
            return nullptr;

        auto* data_ptr = align<T>(pool_size, ptr, space);
        if (data_ptr == nullptr)
            return nullptr;

        bool ctrl_constructed   = false;
        uZ   n_elem_constructed = 0;
        uZ   n_cnt_constructed  = 0;
        uZ   n_ptr_constructed  = 0;
        try {
            new (ctrl_ptr) pool_ctrl_block(new_alloc,    //
                                           storage,
                                           storage_size,
                                           pool_size,
                                           ring_size,
                                           data_ptr,
                                           ring_ptr,
                                           cnt_ptr);

            ctrl_constructed = true;
            for (; n_ptr_constructed < ring_size; ++n_ptr_constructed)
                new (ring_ptr + n_ptr_constructed) atomic_ptr_t{};
            for (; n_cnt_constructed < pool_size; ++n_cnt_constructed)
                new (cnt_ptr + n_cnt_constructed) atomic_cnt_t{};
            for (; n_elem_constructed < pool_size; ++n_elem_constructed)
                placement_ctor(data_ptr + n_elem_constructed);

        } catch (...) {
            for (uZ i = 0; i < n_cnt_constructed; ++i)
                cnt_ptr[i].~atomic_cnt_t();
            for (uZ i = 0; i < n_ptr_constructed; ++i)
                ring_ptr[i].~atomic_ptr_t();
            for (uZ i = 0; i < n_elem_constructed; ++i)
                data_ptr[i].~value_t();

            if (ctrl_constructed)
                ctrl_ptr->~pool_ctrl_block();
            alloc_traits::deallocate(new_alloc, storage, storage_size);
            throw;
        }
        return ctrl_ptr;
    };

    [[no_unique_address]] allocator_t m_allocator;
    std::byte*                        m_storage_ptr;
    uZ                                m_storage_size;
    common_block_t                    m_ctrl_block;
    atomic_cnt_t                      m_counter;
};

}    // namespace detail_

template<typename T, bool Managed>
class pooled_ptr {
    using ctrl_block_t = detail_::pool_ctrl_block_common<T, Managed>;
    using value_t      = T;
    friend ctrl_block_t;

    pooled_ptr(value_t* data_ptr, ctrl_block_t* ctrl_block_ptr)
    : m_data_ptr(data_ptr)
    , m_ctrl_block_ptr(ctrl_block_ptr) {};

public:
    pooled_ptr()                             = default;
    pooled_ptr(const pooled_ptr&)            = delete;
    pooled_ptr& operator=(const pooled_ptr&) = delete;

    friend void swap(pooled_ptr& lhs, pooled_ptr& rhs) noexcept {
        std::swap(lhs.m_ctrl_block_ptr, rhs.m_ctrl_block_ptr);
    }
    pooled_ptr(pooled_ptr&& other) noexcept
    : m_data_ptr(other.m_data_ptr)
    , m_ctrl_block_ptr(other.m_ctrl_block_ptr) {
        other.m_ctrl_block_ptr = nullptr;
    };
    pooled_ptr& operator=(pooled_ptr&& other) noexcept {
        swap(*this, other);
        return *this;
    };
    ~pooled_ptr() {
        if (m_ctrl_block_ptr != nullptr)
            m_ctrl_block_ptr->release(m_data_ptr);
    };
    auto make_arc_ptr() noexcept -> arc_pooled_ptr<T, Managed> {
        uZ    offest         = m_data_ptr - m_ctrl_block_ptr->m_data_begin;
        auto* counter_ptr    = m_ctrl_block_ptr->m_counters[offest];
        auto* ctrl_block_ptr = m_ctrl_block_ptr;
        auto* data_ptr       = m_data_ptr;
        m_ctrl_block_ptr     = nullptr;
        m_data_ptr           = nullptr;
        return {ctrl_block_ptr, counter_ptr, data_ptr};
    }
    explicit operator bool() const noexcept {
        return m_ctrl_block_ptr != nullptr;
    }
    auto operator->() noexcept -> value_t* {
        return m_data_ptr;
    }
    auto operator*() noexcept -> value_t& {
        return *m_data_ptr;
    }

private:
    value_t*      m_data_ptr;
    ctrl_block_t* m_ctrl_block_ptr{};
};

template<typename T, bool Managed>
class arc_pooled_ptr {
    using value_t      = T;
    using ctrl_block_t = detail_::pool_ctrl_block_common<value_t, Managed>;
    using atomic_cnt_t = ctrl_block_t::atomic_cnt_t;

    friend class pooled_ptr<T, Managed>;
    friend ctrl_block_t;

    arc_pooled_ptr(ctrl_block_t* ctrl_block_ptr, atomic_cnt_t* counter_ptr, value_t* data_ptr) noexcept
    : m_ctrl_block_ptr(ctrl_block_ptr)
    , m_counter_ptr(counter_ptr)
    , m_data_ptr(data_ptr) {};

public:
    arc_pooled_ptr() noexcept = default;

    arc_pooled_ptr(arc_pooled_ptr&& other) noexcept
    : m_ctrl_block_ptr{other.m_ctrl_block_ptr}
    , m_counter_ptr{other.m_counter_ptr}
    , m_data_ptr{other.m_data_ptr} {
        other.m_ctrl_block_ptr = nullptr;
    };
    arc_pooled_ptr(const arc_pooled_ptr& other) noexcept
    : m_ctrl_block_ptr(other.m_ctrl_block_ptr)
    , m_counter_ptr(other.m_counter_ptr)
    , m_data_ptr(other.m_data_ptr) {
        m_counter_ptr->fetch_add(1, std::memory_order_acq_rel);
    };

    arc_pooled_ptr& operator=(arc_pooled_ptr&& other) noexcept {
        using std::swap;
        swap(m_data_ptr, other.m_data_ptr);
        swap(m_ctrl_block_ptr, other.m_ctrl_block_ptr);
        swap(m_counter_ptr, other.m_counter_ptr);
        return *this;
    };
    arc_pooled_ptr& operator=(const arc_pooled_ptr& other) noexcept {
        if (m_ctrl_block_ptr != nullptr) {
            auto refs = m_counter_ptr->fetch_sub(1, std::memory_order_acq_rel);
            if (refs == 1)
                m_ctrl_block_ptr->release(m_data_ptr);
        }
        m_ctrl_block_ptr = other.m_ctrl_block_ptr;
        m_counter_ptr    = other.m_counter_ptr;
        m_data_ptr       = other.m_data_ptr;
        m_counter_ptr->fetch_add(1, std::memory_order_acq_rel);
        return *this;
    }

    ~arc_pooled_ptr() {
        if (m_ctrl_block_ptr == nullptr)
            return;
        auto refs = m_counter_ptr->fetch_sub(1, std::memory_order_acq_rel);
        if (refs == 1)
            m_ctrl_block_ptr->release(m_data_ptr);
    };

    explicit arc_pooled_ptr(pooled_ptr<T, Managed>&& ptr) noexcept
    : arc_pooled_ptr(ptr.make_arc_ptr()) {};

    explicit operator bool() const noexcept {
        return m_ctrl_block_ptr != nullptr;
    }
    auto operator->() noexcept -> value_t* {
        return m_data_ptr;
    }
    auto operator*() noexcept -> value_t& {
        return *m_data_ptr;
    }

private:
    value_t*      m_data_ptr;
    ctrl_block_t* m_ctrl_block_ptr{};
    atomic_cnt_t* m_counter_ptr;
};

template<typename T,
         typename Allocator                   = std::allocator<T>,
         pool_memory_management MemManagement = pool_memory_management::managed>
class pool {
    static constexpr bool managed = MemManagement == pool_memory_management::managed;

    using ctrl_block_t = detail_::pool_ctrl_block<T, Allocator, MemManagement>;

public:
    using value_type = T;
    using pointer    = pooled_ptr<T, managed>;

    pool() = delete;

    pool(uZ pool_size, const Allocator& allocator, const auto& placement_ctor)
    : m_ctrl_ptr{ctrl_block_t::allocate(pool_size, allocator, placement_ctor)} {};

    pool(pool&&)                 = delete;
    pool(const pool&)            = delete;
    pool& operator=(pool&&)      = delete;
    pool& operator=(const pool&) = delete;
    ~pool() {
        if (m_ctrl_ptr != nullptr)
            m_ctrl_ptr->decrement();
    };

    [[nodiscard]] auto try_acquire() -> pointer {
        return m_ctrl_ptr->try_acquire();
    }


private:
    ctrl_block_t* m_ctrl_ptr{};
};

};    // namespace mtmu::ll

#endif
