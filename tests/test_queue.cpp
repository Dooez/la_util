#include "la_mt_queue.h"

#include <iostream>
#include <typeinfo>

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
    C_none() = default;
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
void test_queues(Types&&... Args)
{
    constexpr uint test_size = 2;
    std::cout << "Pushing into queues\n";
    for (uint i = 0; i < test_size; ++i)
    {
        (Args.push(typename std::remove_reference<decltype(Args)>::type::value_type(std::rand())),
         ...);
    }
    std::cout << "Popping queues\n";
    while ((static_cast<bool>(Args.pop()) || ...))
    {
    };
}

int main()
{
    la::mt_queue<C_copyconable> copycon_q{};
    la::mt_queue<C_copyable>    copy_q{};
    la::mt_queue<C_movconable>  movcon_q{};
    la::mt_queue<C_movable>     mov_q{};
    la::mt_queue<C_copymov>     normal_q{};
    // la::mt_queue<C_none>        none_q{}; // Compilation error

    std::cout << "Testing queues\n";
    test_queues(copycon_q, copy_q, movcon_q, mov_q, normal_q);
    std::cout << "Testing queues - done\n";
    
    return 0;
}