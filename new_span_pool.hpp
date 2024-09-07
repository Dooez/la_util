#pragma once

#include <algorithm>
#include <atomic>
#include <concepts>
#include <cstdint>
#include <cstdlib>
#include <exception>
#include <iostream>
#include <memory>
#include <mutex>
#include <type_traits>

namespace mtmu::ll3 {

using uZ  = std::size_t;
using u32 = uint32_t;

template<typename T, typename Allocator>
class span_pool;

template<typename T, bool Managed>
class p_span;

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

template<typename T>
class span_pool_ctrl_block {
    enum class status_t : u32 {
        normal,
        abandoned,
        transferred,
        cleaned_up,
    };

    using cnt_t        = u32;
    using value_t      = T;
    using atomic_ptr_t = aligned<std::atomic<T*>, align_n>;
    using size_t       = std::atomic<cnt_t>;

    using size_ptr_t        = const size_t*;
    using next_block_t      = std::atomic<span_pool_ctrl_block*>;
    using prev_block_t      = span_pool_ctrl_block*;
    using deallocate_fptr_t = auto (*)(void*) -> void;
    using owner_ptr_t       = void*;
    using storage_ptr_t     = std::byte*;


public:
    struct head_t {
        cnt_t    value{};
        status_t status{};

        bool operator==(const head_t& other) const {
            return *reinterpret_cast<std::uint64_t*>(this) == *reinterpret_cast<std::uint64_t*>(&other);
        }
    };
    /*using atomic_head_t = aligned<std::atomic<head_t>, align_n>;*/
    /*using atomic_tail_t = aligned<std::atomic<cnt_t>, align_n>;*/
    using atomic_head_t = std::atomic<head_t>;
    using atomic_tail_t = std::atomic<cnt_t>;

    static_assert(atomic_head_t::is_always_lock_free);
    static_assert(atomic_tail_t::is_always_lock_free);
    using pointer = p_span<value_t, true>;

    span_pool_ctrl_block()                                       = delete;
    span_pool_ctrl_block(span_pool_ctrl_block&&)                 = delete;
    span_pool_ctrl_block(const span_pool_ctrl_block&)            = delete;
    span_pool_ctrl_block& operator=(span_pool_ctrl_block&&)      = delete;
    span_pool_ctrl_block& operator=(const span_pool_ctrl_block&) = delete;
    ~span_pool_ctrl_block()                                      = default;

    span_pool_ctrl_block(cnt_t             initial_count,
                         cnt_t             ring_size,
                         atomic_ptr_t*     ring_buffer,
                         size_ptr_t        size_ptr,
                         uZ                span_size,
                         prev_block_t      prev_block,
                         deallocate_fptr_t destoy_fptr,
                         owner_ptr_t       owner,
                         storage_ptr_t     storage_ptr,
                         uZ                storage_size,
                         value_t*          data_end)
    : m_head(head_t{.value = initial_count, .status = status_t::normal})
    , m_ring_size(ring_size)
    , m_ring_buffer(ring_buffer)
    , m_size_ptr(size_ptr)
    , m_span_size(span_size)
    , m_prev_block(prev_block)
    , m_destroy_fptr(destoy_fptr)
    , m_owner_ptr(owner)
    , m_storage_ptr(storage_ptr)
    , m_storage_size(storage_size)
    , m_data_end(data_end) {};

    void release(value_t* object_ptr) {
        auto head = m_head.load(std::memory_order_acquire);
        switch (head.status) {
        [[likely]] case status_t::normal:
            break;
        case status_t::transferred: {
            auto next_block = m_next_block.load(std::memory_order_acquire);
            while (next_block == nullptr)
                next_block = m_next_block.load(std::memory_order_acquire);
            next_block->release(object_ptr);
            return;
        }
        case status_t::abandoned:
            [[fallthrough]];
        case status_t::cleaned_up:
            std::cout << "releasing into cleaned_up block\n";
            object_ptr->~T();
            auto deleted = 1 + m_deleted.fetch_add(1, std::memory_order_acq_rel);
            if ((deleted + (head.value - m_tail.load(std::memory_order_acquire))) ==
                m_size_ptr->load(std::memory_order_acquire)) {
                while (head.status != status_t::cleaned_up)
                    head = m_head.load(std::memory_order_acquire);
                m_destroy_fptr(m_owner_ptr);
            }
            return;
        }

        while (!m_head.compare_exchange_strong(head,    //
                                               {head.value + 1, status_t::normal},
                                               std::memory_order_acq_rel)) {
            switch (head.status) {
            [[likely]] case status_t::normal:
                break;
            case status_t::transferred: {
                auto next_block = m_next_block.load(std::memory_order_acquire);
                while (next_block == nullptr)
                    next_block = m_next_block.load(std::memory_order_acquire);
                next_block->release(object_ptr);
                return;
            }
            case status_t::abandoned:
                [[fallthrough]];
            case status_t::cleaned_up:
                object_ptr->~T();
                auto deleted = 1 + m_deleted.fetch_add(1, std::memory_order_acq_rel);
                if ((deleted + (head.value - m_tail.load(std::memory_order_acquire))) ==
                    m_size_ptr->load(std::memory_order_acquire)) {
                    while (head.status != status_t::cleaned_up)
                        head = m_head.load(std::memory_order_acquire);
                    m_destroy_fptr(m_owner_ptr);
                }
                return;
            };
        }
        auto head_v = head.value % m_ring_size;
        auto ptr    = m_ring_buffer[head_v].load(std::memory_order_acquire);
        while (true) {
            if (ptr != nullptr) {
                ptr = m_ring_buffer[head_v].load(std::memory_order_acquire);
                continue;
            }
            if (m_ring_buffer[head_v].compare_exchange_strong(ptr, object_ptr, std::memory_order_acq_rel))
                break;
        }
    }

    [[nodiscard]] auto try_acquire() noexcept -> pointer {
        auto tail = m_tail.load(std::memory_order_acquire);
        if (tail == m_head.load(std::memory_order_acquire).value)
            return {};
        while (!m_tail.compare_exchange_strong(tail, tail + 1, std::memory_order_acq_rel)) {
            if (tail == m_head.load(std::memory_order_acquire).value)
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
        return {this, ptr, m_span_size};
    };

    [[nodiscard]] auto data_end() -> value_t*& {
        return m_data_end;
    }

    void transfer(span_pool_ctrl_block* next_block_ptr) {
        auto head = m_head.load(std::memory_order_acquire);
        while (!m_head.compare_exchange_strong(head,    //
                                               {head.value, status_t::transferred},
                                               std::memory_order_acq_rel)) {}
        m_next_block.store(next_block_ptr, std::memory_order_release);
        auto tail = m_tail.load(std::memory_order_acquire);
        if (tail == head.value)
            return;
        while (true) {
            while (!m_tail.compare_exchange_strong(tail, tail + 1, std::memory_order_release)) {
                if (tail == head.value)
                    return;
            }
            auto ptr = m_ring_buffer[tail].load(std::memory_order_acquire);
            while (true) {
                if (ptr == nullptr) {
                    ptr = m_ring_buffer[tail].load(std::memory_order_acquire);
                    continue;
                }
                if (m_ring_buffer[tail].compare_exchange_strong(ptr, nullptr, std::memory_order_acq_rel))
                    break;
            }
            next_block_ptr->release(ptr);
        }
    }

    // Marks the pool as abandoned and returns the number of elements destroyed.
    [[nodiscard]] auto abandon() -> cnt_t {
        auto head = m_head.load(std::memory_order_acquire);
        while (!m_head.compare_exchange_strong(head,    //
                                               {head.value, status_t::abandoned},
                                               std::memory_order_acq_rel)) {}
        auto tail   = m_tail.load(std::memory_order_acquire);
        auto i_tail = tail;
        while (i_tail != head.value) {
            auto ptr = m_ring_buffer[i_tail % m_ring_size].load(std::memory_order_acquire);
            while (ptr == nullptr)
                ptr = m_ring_buffer[i_tail % m_ring_size].load(std::memory_order_acquire);
            for (uZ i = 0; i < m_span_size; ++i)
                ptr[i].~T();
            ++i_tail;
        }
        m_head.store({head.value, status_t::cleaned_up}, std::memory_order_release);
        return head.value - tail;
    }

public:
    atomic_head_t m_head;
    atomic_tail_t m_tail;
    cnt_t         m_ring_size;
    atomic_ptr_t* m_ring_buffer;
    size_ptr_t    m_size_ptr;
    uZ            m_span_size;

    std::atomic<cnt_t> m_deleted{};
    next_block_t       m_next_block{};
    prev_block_t       m_prev_block;
    deallocate_fptr_t  m_destroy_fptr;
    owner_ptr_t        m_owner_ptr;
    std::byte*         m_storage_ptr;
    uZ                 m_storage_size;
    value_t*           m_data_end;
};

template<typename T, typename Allocator, typename PlaceCtor>
class span_pool_manager;

template<typename T, typename Allocator>
class span_pool_manager_common {
    using allocator_t  = Allocator;
    using alloc_traits = std::allocator_traits<allocator_t>;

    template<typename T_, typename Allocator_>
    friend class ll3::span_pool;

public:
    span_pool_manager_common()                                           = delete;
    span_pool_manager_common(span_pool_manager_common&&)                 = delete;
    span_pool_manager_common(const span_pool_manager_common&)            = delete;
    span_pool_manager_common& operator=(span_pool_manager_common&&)      = delete;
    span_pool_manager_common& operator=(const span_pool_manager_common&) = delete;
    virtual ~span_pool_manager_common()                                  = default;

    using cnt_t        = u32;
    using value_t      = T;
    using atomic_ptr_t = aligned<std::atomic<T*>, align_n>;
    using size_t       = std::atomic<cnt_t>;
    using ctrl_block_t = span_pool_ctrl_block<T>;
    using span         = ctrl_block_t::pointer;

    span_pool_manager_common(allocator_t&& alloc, uZ span_size, uZ span_count, ctrl_block_t* ctrl_ptr)
    : m_allocator(std::move(alloc))
    , m_span_size(span_size)
    , m_initialized_count(span_count)
    , m_total_count(span_count)
    , m_ctrl_ptr(ctrl_ptr) {};

    virtual void emplace(T* ptr) = 0;

    template<typename F>
    [[nodiscard]] static auto make_manager(F&&       placment_ctor,
                                           Allocator allocator,
                                           uZ        span_size,
                                           uZ        span_count) -> span_pool_manager_common* {
        using place_ctor_t       = std::remove_cvref_t<F>;
        using pool_mngr_t        = span_pool_manager<value_t, Allocator, place_ctor_t>;
        constexpr auto alignment = std::max({alignof(ctrl_block_t),    //
                                             alignof(atomic_ptr_t),
                                             alignof(value_t),
                                             alignof(pool_mngr_t)});

        auto  ring_size         = next_pow_2(span_count);
        auto  ctrl_storage_size = calc_storage_size(span_size, span_count, ring_size);
        auto  storage_size      = ctrl_storage_size + sizeof(pool_mngr_t) + alignment - 1;
        auto* storage_ptr       = alloc_traits::allocate(allocator, storage_size);

        void* ptr   = storage_ptr;
        auto  space = storage_size;

        auto* mngr_ptr = align<pool_mngr_t>(1, ptr, space);
        if (mngr_ptr == nullptr)
            return nullptr;

        auto* ctrl_ptr = align<ctrl_block_t>(1, ptr, space);
        if (ctrl_ptr == nullptr)
            return nullptr;

        auto ring_ptr = align<atomic_ptr_t>(ring_size, ptr, space);
        if (ring_ptr == nullptr)
            return nullptr;

        auto data_ptr = align<value_t>(span_size * span_count, ptr, space);
        if (data_ptr == nullptr)
            return nullptr;

        bool mngr_constructed   = false;
        bool ctrl_constructed   = false;
        uZ   n_elem_constructed = 0;
        uZ   n_ptr_constructed  = 0;
        try {
            /*span_pool_manager(*/
            /*    allocator_t&& alloc, uZ span_size, uZ span_count, ctrl_block_t* ctrl_ptr, F&& placement_ctor)*/

            new (mngr_ptr) pool_mngr_t(allocator,    //
                                       span_size,
                                       span_count,
                                       ctrl_ptr,
                                       std::forward<F>(placment_ctor));
            mngr_constructed = true;
            /*span_pool_ctrl_block(cnt_t initial_count,*/
            /*                     cnt_t             ring_size,*/
            /*                     atomic_ptr_t      ring_buffer,*/
            /*                     size_ptr_t        size_ptr,*/
            /*                     prev_block_t      prev_block,*/
            /*                     deallocate_fptr_t destoy_fptr,*/
            /*                     owner_ptr_t       owner,*/
            /*                     storage_ptr_t     storage_ptr,*/
            /*                     uZ                storage_size,*/
            /*                     value_t*          data_end)*/
            new (ctrl_ptr) ctrl_block_t(span_count,
                                        ring_size,
                                        ring_ptr,
                                        &(mngr_ptr->m_initialized_count),
                                        span_size,
                                        nullptr,
                                        &destroy_erased,
                                        mngr_ptr,
                                        storage_ptr,
                                        storage_size,
                                        data_ptr + span_size * span_count);
            ctrl_constructed = true;
            for (; n_elem_constructed < span_size * span_count; ++n_elem_constructed)
                mngr_ptr->emplace(data_ptr + n_elem_constructed);
            for (; n_ptr_constructed < ring_size; ++n_ptr_constructed)
                new (ring_ptr + n_ptr_constructed) atomic_ptr_t{data_ptr + n_ptr_constructed * span_size};

        } catch (...) {
            for (uZ i = 0; i < n_ptr_constructed; ++i)
                ring_ptr[i].~atomic_ptr_t();
            for (uZ i = 0; i < n_elem_constructed; ++i)
                data_ptr[i].~value_t();
            if (ctrl_constructed)
                ctrl_ptr->~ctrl_block_t();
            if (mngr_constructed)
                mngr_ptr->~span_pool_manager_common();

            alloc_traits::deallocate(allocator, storage_ptr, storage_size);
            throw;
        }
        return mngr_ptr;
    }

    void abandon() {
        auto n_destroyed = m_ctrl_ptr.load(std::memory_order_acquire)->abandon();
        if (n_destroyed == m_initialized_count.load())
            destroy();
    }

private:
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

    template<typename U>
    [[nodiscard]] inline static auto align(uZ count, void*& ptr, uZ& space) -> U* {
        auto* l_ptr     = ptr;
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

    // Must be externally protected by m_resize_mutex;
    auto allocate_and_emplace(uZ span_count, uZ ring_size, uZ emplace_count = 0) -> ctrl_block_t* {
        constexpr auto alignment = std::max({alignof(ctrl_block_t),    //
                                             alignof(atomic_ptr_t),
                                             alignof(value_t)});

        auto  raw_size     = calc_storage_size(m_span_size, span_count, ring_size);
        auto  storage_size = raw_size + alignment - 1;
        auto* storage_ptr  = alloc_traits::allocate(m_allocator, storage_size);

        auto* ptr   = static_cast<void*>(storage_ptr);
        auto  space = storage_size;

        auto ctrl_ptr = align<ctrl_block_t>(1, ptr, space);
        if (ctrl_ptr == nullptr)
            return nullptr;

        auto ring_ptr = align<atomic_ptr_t>(ring_size, ptr, space);
        if (ring_ptr == nullptr)
            return nullptr;

        auto data_ptr = align<value_t>(m_span_size * span_count, ptr, space);
        if (data_ptr == nullptr)
            return nullptr;

        bool ctrl_constructed   = false;
        uZ   n_elem_constructed = 0;
        uZ   n_ptr_constructed  = 0;
        try {
            /*span_pool_ctrl_block(cnt_t             initial_count,*/
            /*                     cnt_t             ring_size,*/
            /*                     atomic_ptr_t*     ring_buffer,*/
            /*                     size_ptr_t        size_ptr,*/
            /*                     uZ                span_size,*/
            /*                     prev_block_t      prev_block,*/
            /*                     deallocate_fptr_t destoy_fptr,*/
            /*                     owner_ptr_t       owner,*/
            /*                     storage_ptr_t     storage_ptr,*/
            /*                     uZ                storage_size,*/
            /*                     value_t*          data_end)*/
            new (ctrl_ptr) ctrl_block_t(emplace_count,
                                        ring_size,
                                        ring_ptr,
                                        &m_initialized_count,
                                        m_span_size,
                                        m_ctrl_ptr,
                                        &destroy_erased,
                                        this,
                                        storage_ptr,
                                        storage_size,
                                        data_ptr + m_span_size * emplace_count);

            ctrl_constructed = true;
            for (; n_elem_constructed < m_span_size * emplace_count; ++n_elem_constructed)
                emplace(data_ptr + n_elem_constructed);
            for (; n_ptr_constructed < emplace_count; ++n_ptr_constructed)
                new (ring_ptr + n_ptr_constructed) atomic_ptr_t{data_ptr + m_span_size * n_ptr_constructed};
            for (; n_ptr_constructed < ring_size; ++n_ptr_constructed)
                new (ring_ptr + n_ptr_constructed) atomic_ptr_t{};

        } catch (...) {
            for (uZ i = 0; i < n_ptr_constructed; ++i)
                ring_ptr[i].~atomic_ptr_t();
            for (uZ i = 0; i < n_elem_constructed; ++i)
                data_ptr[i].~value_t();

            if (ctrl_constructed)
                ctrl_ptr->~ctrl_block_t();
            alloc_traits::deallocate(m_allocator, storage_ptr, storage_size);
            throw;
        }
        advance(ctrl_ptr->data_end(), emplace_count);
        m_total_count += span_count;
        m_initialized_count.fetch_add(emplace_count, std::memory_order_acq_rel);
        return ctrl_ptr;
    }

    static auto calc_storage_size(uZ span_size, uZ span_count, uZ ring_size) -> uZ {
        constexpr auto ctrl_align = alignof(ctrl_block_t);
        constexpr auto aptr_align = alignof(atomic_ptr_t);
        constexpr auto data_align = alignof(value_t);

        constexpr auto ctrl_size = sizeof(ctrl_block_t);
        constexpr auto ctrl_size_al =
            ctrl_size + (ctrl_size % aptr_align > 0 ? aptr_align - ctrl_size % aptr_align : 0);

        auto ctrl_ring_size = ctrl_size_al + ring_size * sizeof(atomic_ptr_t);
        auto ctrl_ring_size_al =
            ctrl_ring_size + (ctrl_ring_size % data_align > 0 ? data_align - ctrl_ring_size % data_align : 0);


        return ctrl_ring_size_al + span_count * span_size * sizeof(value_t);
    }

    void resize(uZ new_size) {
        auto init_count = m_initialized_count.load(std::memory_order_acquire);
        if (new_size <= init_count)
            return;
        auto lock  = std::scoped_lock(m_resize_mutex);
        init_count = m_initialized_count.load(std::memory_order_acquire);
        if (new_size <= init_count)
            return;
        if (new_size > m_total_count) {
            auto  emplace_count   = m_total_count - init_count;
            auto  new_block_count = new_size - m_total_count;
            auto* new_ctrl_ptr = allocate_and_emplace(new_block_count, next_pow_2(new_size), new_block_count);
            if (new_ctrl_ptr == nullptr)
                throw std::runtime_error("Could not allocate new pool storage.");

            auto old_ctrl_ptr = m_ctrl_ptr.load(std::memory_order_acquire);
            m_ctrl_ptr.store(new_ctrl_ptr, std::memory_order_release);
            old_ctrl_ptr.transfer(new_ctrl_ptr);

            auto data_end = old_ctrl_ptr->data_end();
            for (uZ i = 0; i < emplace_count; ++i) {
                emplace(data_end);
                new_ctrl_ptr->release(data_end);
                advance(data_end);
            }
            advance(old_ctrl_ptr->data_end(), emplace_count);
            m_initialized_count.fetch_add(emplace_count);
            return;
        }
        auto emplace_count = static_cast<cnt_t>(new_size) - init_count;
        auto ctrl_ptr      = m_ctrl_ptr;
        auto data_end      = ctrl_ptr->data_end();
        for (uZ i = 0; i < emplace_count; ++i) {
            emplace(data_end);
            ctrl_ptr->release(data_end);
            advance(data_end);
        }
        advance(ctrl_ptr->data_end(), emplace_count);
        m_initialized_count.store(init_count + emplace_count, std::memory_order_release);
    }

    [[nodiscard]] auto try_acquire() -> span {
        return m_ctrl_ptr.load(std::memory_order_acquire)->try_acquire();
    }

    [[nodiscard]] auto acquire() -> span {
        auto* ctrl_ptr   = m_ctrl_ptr.load(std::memory_order_acquire);
        auto  pooled_ptr = ctrl_ptr->try_acquire();
        if (pooled_ptr)
            return pooled_ptr;

        auto lock  = std::scoped_lock(m_resize_mutex);
        ctrl_ptr   = m_ctrl_ptr.load(std::memory_order_acquire);
        pooled_ptr = ctrl_ptr->try_acquire();
        if (pooled_ptr)
            return pooled_ptr;

        if (m_total_count > m_initialized_count.load(std::memory_order_acquire)) {
            auto ptr = ctrl_ptr->data_end();
            emplace(ptr);
            advance(ctrl_ptr->data_end());
            m_initialized_count.fetch_add(1, std::memory_order_acq_rel);
            pooled_ptr = {ctrl_ptr, ptr, m_span_size};
            return pooled_ptr;
        }

        auto  new_count    = m_total_count == 0 ? 2 : m_total_count * 2;
        auto* new_ctrl_ptr = allocate_and_emplace(new_count - m_total_count, next_pow_2(new_count));
        if (new_ctrl_ptr == nullptr)
            throw std::runtime_error("Could not allocate new pool storage.");
        auto ptr = new_ctrl_ptr->data_end();
        emplace(ptr);
        advance(new_ctrl_ptr->data_end());
        pooled_ptr = {new_ctrl_ptr, ptr, m_span_size};
        m_initialized_count.fetch_add(1, std::memory_order_acq_rel);
        m_ctrl_ptr.store(new_ctrl_ptr, std::memory_order_release);
        ctrl_ptr->transfer(new_ctrl_ptr);
        return pooled_ptr;
    }

    void advance(T*& data_end, cnt_t count = 1) {
        data_end += m_span_size * count;
    }

    void destroy() {
        auto alloc    = m_allocator;
        auto ctrl_ptr = m_ctrl_ptr.load(std::memory_order_acquire);
        this->~span_pool_manager_common();
        while (ctrl_ptr != nullptr) {
            auto ring_size    = ctrl_ptr->m_ring_size;
            auto ring_buffer  = ctrl_ptr->m_ring_buffer;
            auto storage      = ctrl_ptr->m_storage_ptr;
            auto storage_size = ctrl_ptr->m_storage_size;
            auto prev_ptr     = ctrl_ptr->m_prev_block;
            for (uZ i = 0; i < ring_size; ++i) {
                ring_buffer[i].~atomic_ptr_t();
            }
            alloc_traits::deallocate(alloc, storage, storage_size);
            ctrl_ptr = prev_ptr;
        }
    }

    static void destroy_erased(void* this_ptr) {
        reinterpret_cast<span_pool_manager_common*>(this_ptr)->destroy();
    };


    allocator_t                m_allocator;
    uZ                         m_span_size;
    std::atomic<cnt_t>         m_initialized_count;
    cnt_t                      m_total_count;
    std::atomic<ctrl_block_t*> m_ctrl_ptr;
    std::mutex                 m_resize_mutex{};
};

template<typename T, typename Allocator, typename PlaceCtor>
class span_pool_manager : span_pool_manager_common<T, Allocator> {
    friend class span_pool_manager_common<T, Allocator>;
    using allocator_t      = Allocator;
    using manager_common_t = span_pool_manager_common<T, allocator_t>;
    using ctrl_block_t     = manager_common_t::ctrl_block_t;

public:
    span_pool_manager()                                    = delete;
    span_pool_manager(span_pool_manager&&)                 = delete;
    span_pool_manager(const span_pool_manager&)            = delete;
    span_pool_manager& operator=(span_pool_manager&&)      = delete;
    span_pool_manager& operator=(const span_pool_manager&) = delete;
    virtual ~span_pool_manager()                           = default;

    template<typename F>
    span_pool_manager(
        allocator_t alloc, uZ span_size, uZ span_count, ctrl_block_t* ctrl_ptr, F&& placement_ctor)
    : manager_common_t(std::move(alloc), span_size, span_count, ctrl_ptr)
    , m_place_ctor(std::forward<F>(placement_ctor)){};

    void emplace(T* placement_ptr) override {
        m_place_ctor(placement_ptr);
    }

private:
    PlaceCtor m_place_ctor;
};

}    // namespace detail_
template<typename T, typename Allocator = std::allocator<T>>
class span_pool {
    using allocator_t = typename std::allocator_traits<Allocator>::template rebind_alloc<std::byte>;
    using manager_t   = detail_::span_pool_manager_common<T, allocator_t>;

public:
    using pointer = typename manager_t::span;

    span_pool()                            = delete;
    span_pool(span_pool&&)                 = delete;
    span_pool(const span_pool&)            = delete;
    span_pool& operator=(span_pool&&)      = delete;
    span_pool& operator=(const span_pool&) = delete;
    ~span_pool() {
        m_manager_ptr->abandon();
    };

    template<typename F>
        requires detail_::placement_ctor<F, T>
    span_pool(F&& placement_ctor, uZ span_size, uZ span_count = 0, const Allocator& allocator = {})
    : m_manager_ptr(manager_t::make_manager(std::forward<F>(placement_ctor),    //
                                            static_cast<allocator_t>(allocator),
                                            span_size,
                                            span_count)) {
        if (m_manager_ptr == nullptr)
            throw std::runtime_error("Could not allocate storage for span pool");
    };
    explicit span_pool(uZ span_size, uZ span_count = 0, const Allocator& allocator = {})
    : m_manager_ptr(manager_t::make_manager([](T* ptr) { return new (ptr) T{}; },    //
                                            static_cast<allocator_t>(allocator),
                                            span_size,
                                            span_count)) {
        if (m_manager_ptr == nullptr)
            throw std::runtime_error("Could not allocate storage for span pool");
    };

    [[nodiscard]] auto try_acquire() -> pointer {
        return m_manager_ptr->try_acquire();
    }
    [[nodiscard]] auto acquire() -> pointer {
        return m_manager_ptr->acquire();
    }
    void resize(uZ new_size);
    void reserve(uZ new_capacity);


private:
    manager_t* m_manager_ptr;
};

template<typename T, bool Managed>
class p_span {
    friend class detail_::span_pool_ctrl_block<T>;
    template<typename T_, typename Alloc_>
    friend class detail_::span_pool_manager_common;

    p_span(detail_::span_pool_ctrl_block<T>* pool_ptr, T* data_ptr, uZ size)
    : m_pool_ptr(pool_ptr)
    , m_data_ptr(data_ptr)
    , m_size(size) {};

public:
    p_span() = default;
    p_span(p_span&& other) noexcept
    : m_pool_ptr(other.m_pool_ptr)
    , m_data_ptr(other.m_data_ptr) {
        other.m_pool_ptr = nullptr;
    };
    p_span(const p_span&) = delete;
    p_span& operator=(p_span&& other) noexcept {
        if (m_pool_ptr != nullptr)
            m_pool_ptr->release(m_data_ptr);
        m_pool_ptr       = other.m_pool_ptr;
        m_data_ptr       = other.m_data_ptr;
        other.m_pool_ptr = nullptr;
        return *this;
    };
    p_span& operator=(const p_span&) = delete;

    ~p_span() {
        if (m_pool_ptr != nullptr)
            m_pool_ptr->release(m_data_ptr);
    };


    explicit operator bool() const {
        return m_pool_ptr != nullptr;
    }


private:
    detail_::span_pool_ctrl_block<T>* m_pool_ptr{};
    T*                                m_data_ptr;
    uZ                                m_size;
};

}    // namespace mtmu::ll3
