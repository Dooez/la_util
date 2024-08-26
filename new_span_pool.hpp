#pragma once

#include <algorithm>
#include <atomic>
#include <concepts>
#include <cstdint>
#include <cstdlib>
#include <exception>
#include <memory>
#include <mutex>
#include <type_traits>

namespace mtmu::ll3 {

using uZ  = std::size_t;
using u32 = uint32_t;

template<typename T, typename Allocator>
class pool;

template<typename T, bool Managed>
class pooled_ptr;

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
class pool_ctrl_block_common {
    enum class status_t {
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
    using next_block_t      = std::atomic<pool_ctrl_block_common*>;
    using prev_block_t      = pool_ctrl_block_common*;
    using deallocate_fptr_t = auto (*)(void*) -> void;
    using owner_ptr_t       = void*;
    using storage_ptr_t     = std::byte*;

    struct head_t {
        cnt_t    value{};
        status_t status{};
    };
    using atomic_head_t = aligned<std::atomic<head_t>, align_n>;
    using atomic_tail_t = aligned<std::atomic<cnt_t>, align_n>;

    static_assert(atomic_head_t::is_always_lock_free);
    static_assert(atomic_tail_t::is_always_lock_free);

public:
    using pointer = pooled_ptr<value_t, true>;

    pool_ctrl_block_common()                                         = delete;
    pool_ctrl_block_common(pool_ctrl_block_common&&)                 = delete;
    pool_ctrl_block_common(const pool_ctrl_block_common&)            = delete;
    pool_ctrl_block_common& operator=(pool_ctrl_block_common&&)      = delete;
    pool_ctrl_block_common& operator=(const pool_ctrl_block_common&) = delete;
    ~pool_ctrl_block_common()                                        = default;

    pool_ctrl_block_common(cnt_t             ring_size,
                           atomic_ptr_t      ring_buffer,
                           size_ptr_t        size_ptr,
                           prev_block_t      prev_block,
                           deallocate_fptr_t destoy_fptr,
                           owner_ptr_t       owner,
                           storage_ptr_t     storage_ptr,
                           value_t*          data_end)
    : m_ring_size{ring_size}
    , m_ring_buffer{ring_buffer}
    , m_size_ptr{size_ptr}
    , m_prev_block{prev_block}
    , m_destroy_fptr{destoy_fptr}
    , m_owner_ptr{owner}
    , m_storage_ptr{storage_ptr}
    , m_data_end(data_end) {};

    void release(value_t* object_ptr) {
        auto head = m_head.load(std::memory_order_acquire);
        switch (head.status) {
            using enum status_t;
        [[likely]] case normal:
            break;
        case transferred: {
            auto next_block = m_next_block.load(std::memory_order_acquire);
            while (next_block == nullptr)
                next_block = m_next_block.load(std::memory_order_acquire);
            next_block->release(object_ptr);
            return;
        }
        case abandoned:
            [[fallthrough]];
        case cleaned_up:
            object_ptr->~T();
            auto deleted = 1 + m_deleted.fetch_add(1, std::memory_order_acq_rel);
            if ((deleted + (head.value - m_tail.load(std::memory_order_acquire))) ==
                m_size_ptr->load(std::memory_order_acquire)) {
                while (head.status != cleaned_up)
                    head = m_head.load(std::memory_order_acquire);
                m_destroy_fptr(m_owner_ptr);
            }
            return;
        }

        while (!m_head.compare_exchange_strong(head,    //
                                               {head.value + 1, status_t::normal},
                                               std::memory_order_acq_rel)) {
            switch (head.status) {
                using enum status_t;
            [[likely]] case normal:
                break;
            case transferred: {
                auto next_block = m_next_block.load(std::memory_order_acquire);
                while (next_block == nullptr)
                    next_block = m_next_block.load(std::memory_order_acquire);
                next_block->release(object_ptr);
                return;
            }
            case abandoned:
                [[fallthrough]];
            case cleaned_up:
                object_ptr->~T();
                auto deleted = 1 + m_deleted.fetch_add(1, std::memory_order_acq_rel);
                if ((deleted + (head.value - m_tail.load(std::memory_order_acquire))) == m_size_ptr) {
                    while (head.status != cleaned_up)
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
        return {ptr, this};
    };

    [[nodiscard]] auto data_end() -> value_t*& {
        return m_data_end;
    }

    void tranasfer(pool_ctrl_block_common* next_block_ptr) {
        auto head = m_head.load(std::memory_order_acquire);
        while (!m_head.compare_exchange_strong(head,    //
                                               {head.value, status_t::transferred},
                                               std::memory_order_acq_rel)) {}
        m_next_block.store(std::memory_order_release);
        auto tail = m_tail.load(std::memory_order_acquire);
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
        auto tail = m_tail.load(std::memory_order_acquire);
        while (tail != head.value) {
            auto ptr = m_ring_buffer[tail % m_ring_size].load(std::memory_order_acquire);
            while (ptr == nullptr)
                ptr = m_ring_buffer[tail % m_ring_size].load(std::memory_order_acquire);
            ptr->~T();
            ++tail;
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

    std::atomic<cnt_t> m_deleted{};
    next_block_t       m_next_block{};
    prev_block_t       m_prev_block;
    deallocate_fptr_t  m_destroy_fptr;
    owner_ptr_t        m_owner_ptr;
    std::byte*         m_storage_ptr;
    uZ                 m_storage_size;
    value_t*           m_data_end;
};

template<typename T>
struct pool_types {
protected:
    using cnt_t        = u32;
    using value_t      = T;
    using atomic_ptr_t = aligned<std::atomic<T*>, align_n>;
    using size_t       = std::atomic<cnt_t>;
};


template<typename T, typename Allocator>
class pool_ctrl_block_er {
    using allocator_t  = Allocator;
    using alloc_traits = std::allocator_traits<allocator_t>;

public:
    pool_ctrl_block_er()                                     = default;
    pool_ctrl_block_er(pool_ctrl_block_er&&)                 = default;
    pool_ctrl_block_er(const pool_ctrl_block_er&)            = default;
    pool_ctrl_block_er& operator=(pool_ctrl_block_er&&)      = default;
    pool_ctrl_block_er& operator=(const pool_ctrl_block_er&) = default;
    ~pool_ctrl_block_er()                                    = default;

    virtual void emplace(T* ptr) = 0;

    using cnt_t          = u32;
    using value_t        = T;
    using atomic_ptr_t   = aligned<std::atomic<T*>, align_n>;
    using size_t         = std::atomic<cnt_t>;
    using common_block_t = pool_ctrl_block_common<T>;
    using pointer        = common_block_t::pointer;

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
    auto allocate_and_emplace(uZ span_count, uZ ring_size, uZ emplace_count = 0) -> common_block_t* {
        constexpr auto alignment = std::max({alignof(common_block_t),    //
                                             alignof(atomic_ptr_t),
                                             alignof(value_t)});

        auto  raw_size     = calc_storage_size(m_span_size, span_count, ring_size);
        auto  storage_size = raw_size + alignment - 1;
        auto* storage_ptr  = alloc_traits::allocate(m_allocator, storage_size);

        auto* ptr   = static_cast<void*>(storage_ptr);
        auto  space = storage_size;

        auto ctrl_ptr = align<common_block_t>(1, ptr, space);
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
            new (ctrl_ptr) common_block_t(ring_size,    //
                                          ring_ptr,
                                          &m_initialized_count,
                                          m_ctrl_ptr,
                                          &destroy_erased,
                                          this,
                                          storage_ptr);

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
                ctrl_ptr->~common_block_t();
            alloc_traits::deallocate(m_allocator, storage_ptr, storage_size);
            throw;
        }
        advance(ctrl_ptr->data_end(), emplace_count);
        m_total_count += span_count;
        m_initialized_count.fetch_add(emplace_count, std::memory_order_acq_rel);
        return ctrl_ptr;
    }

    static auto calc_storage_size(uZ span_size, uZ span_count, uZ ring_size) -> uZ {
        constexpr auto ctrl_align = alignof(common_block_t);
        constexpr auto aptr_align = alignof(atomic_ptr_t);
        constexpr auto data_align = alignof(value_t);

        constexpr auto ctrl_size = sizeof(common_block_t);
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

    [[nodiscard]] auto try_acquire() -> pointer {
        return m_ctrl_ptr.load(std::memory_order_acquire)->try_acquire();
    }

    [[nodiscard]] auto acquire() -> pointer {
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
            pooled_ptr = {ptr, ctrl_ptr};
            return pooled_ptr;
        }

        auto* new_ctrl_ptr = allocate_and_emplace(m_total_count, next_pow_2(m_total_count * 2));
        if (new_ctrl_ptr == nullptr)
            throw std::runtime_error("Could not allocate new pool storage.");
        auto ptr = new_ctrl_ptr->data_end();
        emplace(ptr);
        advance(new_ctrl_ptr->data_end());
        pooled_ptr = {ptr, new_ctrl_ptr};
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
        this->~pool_ctrl_block_er();
        auto prev_ptr = ctrl_ptr->m_prev_block;
        while (ctrl_ptr != nullptr) {
            auto ring_size    = ctrl_ptr->m_ring_size;
            auto ring_buffer  = ctrl_ptr->m_ring_buffer;
            auto storage      = ctrl_ptr->m_storage;
            auto storage_size = ctrl_ptr->m_storage_size;
            for (uZ i = 0; i < ring_size; ++i) {
                ring_buffer[i]->~atomic_ptr_t();
            }
            alloc_traits::deallocate(alloc, storage, storage_size);
            ctrl_ptr = prev_ptr;
        }
    }

    static void destroy_erased(void* this_ptr) {
        reinterpret_cas<pool_ctrl_block_er*>(this_ptr)->destroy();
    };

    allocator_t                  m_allocator;
    uZ                           m_span_size{};
    std::atomic<cnt_t>           m_initialized_count{};
    cnt_t                        m_total_count{};
    std::atomic<common_block_t*> m_ctrl_ptr;
    std::mutex                   m_resize_mutex;
};


template<typename T, typename Allocator, typename PlaceCtor>
class pool_ctrl_block : pool_ctrl_block_er<T, Allocator> {
public:
    pool_ctrl_block()                                  = delete;
    pool_ctrl_block(pool_ctrl_block&&)                 = delete;
    pool_ctrl_block(const pool_ctrl_block&)            = delete;
    pool_ctrl_block& operator=(pool_ctrl_block&&)      = delete;
    pool_ctrl_block& operator=(const pool_ctrl_block&) = delete;
    ~pool_ctrl_block()                                 = default;

private:
};


}    // namespace detail_
}    // namespace mtmu::ll3
