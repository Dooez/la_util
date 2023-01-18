#include "la_pool.h"
#include "test_base.h"

#include <iostream>
#include <typeinfo>

template<typename T>
int test_pool(T&& pool)
{
    auto acq0 = pool.acquire_free();
    if (acq0)
    {
        return 1;
    }
    auto acq1 = pool.acquire();
    if (!acq1)
    {
        return 1;
    }
    acq0 = pool.acquire_free();
    if (acq0)
    {
        return 1;
    }
    acq1.release();
    if (acq1)
    {
        return 1;
    }
    acq0 = pool.acquire_free();
    if (!acq0)
    {
        return 1;
    }
    pool.populate(1);
    if (pool.size() != 2 || pool.free_size() != 1)
    {
        return 1;
    }
    auto sh_ptr = static_cast<std::shared_ptr<typename decltype(acq0)::element_type>>(acq0);
    if (acq0)
    {
        return 1;
    }
    return 0;
}

template<typename T>
int test_pool_ctor(T&& object)
{
    using value_t = std::remove_reference_t<T>;

    int ret = 0;
    if constexpr (std::constructible_from<value_t>)
    {
        auto pool = la::pool<value_t>();
        ret += test_pool(pool);
    }
    if constexpr (std::copy_constructible<value_t>)
    {
        auto obj  = value_t(0);
        auto pool = la::pool<value_t>(obj);
        ret += test_pool(pool);

        auto factory = [&obj]() { return new value_t(obj); };
        auto deleter = [](value_t* ptr) { delete ptr; };

        auto pool_f = la::pool<value_t>(factory);
        ret += test_pool(pool_f);

        auto pool_fd = la::pool<value_t>(factory, deleter);
        ret += test_pool(pool_fd);
    }
    return ret;
}

template<typename... Types>
int test_pools(Types&&... args)
{
    return (test_pool_ctor(args) + ...);
}

int main()
{
    auto dummy = test::create_test_tuple<true, true, true, true, true>();
    auto ret = std::apply([](auto&&... args) { return test_pools(args...); }, *dummy);

    return ret;
}
