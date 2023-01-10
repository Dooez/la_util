#include "la_pool.h"
#include "test_base.h"

#include <iostream>
class C_movconable
{
public:
    C_movconable() = default;
    explicit C_movconable(int input)
    : m_data(input){};
    ~C_movconable() = default;

    C_movconable(C_movconable&& other) noexcept
    : m_data(other.m_data)
    {
        std::cout << "move constructed " << typeid(*this).name() << "\n";
    };

    C_movconable& operator=(C_movconable&& other)      = delete;
    C_movconable(const C_movconable& other)            = delete;
    C_movconable& operator=(const C_movconable& other) = delete;

private:
    int m_data = 0;
};

class C_movable
{
public:
    C_movable() = default;
    explicit C_movable(int input)
    : m_data(input){};
    ~C_movable() = default;

    C_movable(C_movable&& other) noexcept
    : m_data(other.m_data)
    {
        std::cout << "move constructed " << typeid(*this).name() << "\n";
    };
    C_movable& operator=(C_movable&& other) noexcept
    {
        m_data = other.m_data;
        return *this;
    }

    C_movable(const C_movable& other)            = delete;
    C_movable& operator=(const C_movable& other) = delete;

private:
    int m_data = 0;
};

class C_copyconable
{
public:
    C_copyconable() = default;
    explicit C_copyconable(int input)
    : m_data(input){};
    ~C_copyconable() = default;

    C_copyconable(const C_copyconable& other)
    : m_data(other.m_data)
    {
        std::cout << "copy constructed " << typeid(*this).name() << "\n";
    };

    C_copyconable& operator=(const C_copyconable& other) = delete;
    C_copyconable(C_copyconable&& other)                 = delete;
    C_copyconable& operator=(C_copyconable&& other)      = delete;

private:
    int m_data = 0;
};

class C_copyable
{
public:
    C_copyable() = default;
    explicit C_copyable(int input)
    : m_data(input){};
    ~C_copyable() = default;

    C_copyable(const C_copyable& other)
    : m_data(other.m_data)
    {
        std::cout << "copy constructed " << typeid(*this).name() << "\n";
    };
    C_copyable& operator=(const C_copyable& other)
    {
        m_data = other.m_data;
        return *this;
    }

    C_copyable(C_copyable&& other)            = delete;
    C_copyable& operator=(C_copyable&& other) = delete;

private:
    int m_data = 0;
};

class C_none
{
public:
    C_none() = delete;
    explicit C_none(int input)
    : m_data(input){};
    ~C_none() = default;

    C_none(const C_none& other)            = delete;
    C_none(C_none&& other)                 = delete;
    C_none& operator=(const C_none& other) = delete;
    C_none& operator=(C_none&& other)      = delete;

private:
    int m_data = 0;
};

class C_copymov
{
public:
    C_copymov() = default;
    explicit C_copymov(int input)
    : m_data(input){};
    ~C_copymov() = default;

    C_copymov(const C_copymov& other)
    : m_data(other.m_data)
    {
        std::cout << "copy constructed " << typeid(*this).name() << "\n";
    };
    C_copymov(C_copymov&& other) noexcept
    : m_data(other.m_data)
    {
        std::cout << "move constructed " << typeid(*this).name() << "\n";
    };
    C_copymov& operator=(const C_copymov& other)
    {
        m_data = other.m_data;
        return *this;
    }

    C_copymov& operator=(C_copymov&& other) noexcept
    {
        m_data = other.m_data;
        return *this;
    }

private:
    int m_data = 0;
};

template<typename... Types>
void test_pools(Types&&... pools)
{
    ((void)pools.acquire_free(), ...);
    ((void)pools.acquire(), ...);
    ((void)pools.acquire_free(), ...);
    ((void)pools.populate(1), ...);
    std::cout << "sizes:\n";
    ((std::cout << pools.size() << " "), ...);
    std::cout << "\n";
}

template<typename T>
void test_pool(T& o)
{
    auto pool = la::pool<std::remove_reference_t<decltype(o)>>();
    {
        auto acq0 = pool.acquire_free();
        auto acq1 = pool.acquire();
        acq0      = pool.acquire_free();
        acq1.release();
        acq0 = pool.acquire_free();
        pool.populate(1);
        auto sh_ptr = static_cast<std::shared_ptr<T>>(acq0);
    }
    std::cout << pool.size() << "\n";
}

template<uint I, typename... Types>
void apple(const std::tuple<Types...>& tuple)
{
    test_pool(std::get<I>(tuple));
    if constexpr (I < sizeof...(Types) - 1)
    {
        apple<I + 1>(tuple);
    }
}

int main()
{
    C_copymov sample(0);

    std::cout << "Testing pools\n";
    la::pool<C_copymov> pool(sample);
    la::pool<C_copymov> pool_factory([sample]() { return new C_copymov(sample); });
    la::pool<C_copymov> pool_factory_del([sample]() { return new C_copymov(sample); },
                                         [](C_copymov* ptr) { delete ptr; });
    test_pools(pool, pool_factory, pool_factory_del);

    // la::pool<C_none> none_p(C_none(0)); // Error, non-copiable construction args

    la::pool<C_copymov>     copymov_p(0);
    la::pool<C_copyconable> copycon_p(0);
    la::pool<C_movable>     mov_p(0);
    la::pool<C_movconable>  movcon_p(0);
    test_pools(copymov_p, copycon_p, mov_p, movcon_p);

    apple<0>(dummy_tuple{});
    return 0;
}
