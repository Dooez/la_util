#include "pool.hpp"
#include "test_base.h"

#include <iostream>
#include <type_traits>
#include <typeinfo>
template<typename T, std::align_val_t Alignment = std::align_val_t{64}>
class aligned_allocator {
public:
    using value_type      = T;
    using is_always_equal = std::true_type;

    aligned_allocator() = default;

    template<typename U>
    explicit aligned_allocator(const aligned_allocator<U, Alignment>& /*unused*/) noexcept {};

    aligned_allocator(const aligned_allocator&)     = default;
    aligned_allocator(aligned_allocator&&) noexcept = default;

    ~aligned_allocator() = default;

    aligned_allocator& operator=(const aligned_allocator&)     = default;
    aligned_allocator& operator=(aligned_allocator&&) noexcept = default;

    [[nodiscard]] auto allocate(std::size_t n) -> value_type* {
        return reinterpret_cast<value_type*>(::operator new[](n * sizeof(value_type), Alignment));
    }

    void deallocate(value_type* p, std::size_t) {
        ::operator delete[](reinterpret_cast<void*>(p), Alignment);
    }

    template<typename U>
    struct rebind {
        using other = aligned_allocator<U, Alignment>;
    };

private:
};

template<typename T>
int test_pool(T&& pool) {
    using pool_t = std::remove_cvref_t<T>;
    using ptr_t  = typename pool_t::pointer;
    ptr_t ptr{};

    auto acq0 = pool.acquire_free();
    if (acq0) {
        pool_t::value_type::cout_name();
        return 1;
    }
    auto acq1 = pool.acquire();
    if (!acq1) {
        pool_t::value_type::cout_name();
        return 1;
    }
    acq0 = pool.acquire_free();
    if (acq0) {
        pool_t::value_type::cout_name();
        return 1;
    }
    acq1.release();
    if (acq1) {
        pool_t::value_type::cout_name();
        return 1;
    }
    acq0 = pool.acquire_free();
    if (!acq0) {
        pool_t::value_type::cout_name();
        return 1;
    }
    pool.populate(1);
    if (pool.size() != 2 || pool.free_size() != 1) {
        pool_t::value_type::cout_name();
        return 1;
    }
    auto sh_ptr = static_cast<std::shared_ptr<typename decltype(acq0)::element_type>>(acq0);
    if (acq0) {
        pool_t::value_type::cout_name();
        return 1;
    }
    if constexpr (std::copy_constructible<pool_t>) {
        auto pool_cpy = pool_t(pool);

        ptr       = pool_cpy.acquire();
        auto acq1 = pool.acquire();

    } else {
        ptr = pool.acquire();
    }
    auto sz  = pool.size();
    auto szf = pool.free_size();
    {
        std::array<typename pool_t::pointer, 137> pointers{};
        for (uint i = 0; i < 512; ++i) {
            pointers.at(i % pointers.size()) = std::move(pool.acquire());
        }
        for (uint i = 0; i < 4096; ++i) {
            if (std::rand() % 2 == 0) {
                pointers.at(i % pointers.size()) = pool.acquire();
            } else {
                pointers.at(i % pointers.size()).release();
            }
        }
    }

    if (pool.size() - pool.free_size() != sz - szf) {
        acq0 = pool.acquire();
        pool_t::value_type::cout_name();
        return 1;
    }
    acq0 = pool.acquire();
    return 0;
}

template<typename T>
int test_pool_ctor(T&& object) {
    using value_t = std::remove_reference_t<T>;

    int ret = 0;
    if constexpr (std::constructible_from<value_t>) {
        auto pool = mtmu::pool<value_t>();
        ret += test_pool(pool);

        auto al_pool = mtmu::pool<value_t>();
        ret += test_pool(al_pool);
    }
    if constexpr (std::copy_constructible<value_t>) {
        auto obj  = value_t(0);
        auto pool = mtmu::pool<value_t>(obj);
        ret += test_pool(pool);

        auto al_pool_al = mtmu::pool<value_t>(obj);
        ret += test_pool(al_pool_al);


        auto factory = [&obj]() { return new value_t(obj); };
        auto deleter = [](value_t* ptr) { delete ptr; };

        auto pool_fd = mtmu::pool<value_t>(factory, deleter);
        ret += test_pool(pool_fd);


        auto al_pool_fd = mtmu::pool<value_t>(factory, deleter, aligned_allocator<value_t>{});
    }
    return ret;
}

template<typename... Types>
int test_pools(Types&&... args) {
    return (test_pool_ctor(args) + ...);
}

int main() {
    auto dummy = test::create_test_tuple<test::member_selector{}>();
    auto ret   = std::apply([](auto&&... args) { return test_pools(args...); }, *dummy);

    return ret;
}
