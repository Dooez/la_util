#include "la_mt_queue.h"
#include "test_base.h"

constexpr uint test_loop_n = 128;

template<typename T>
int test_queue(T&& queue)
{
    using value_t = typename std::remove_reference_t<decltype(queue)>::value_type;

    for (uint i = 0; i < test_loop_n; ++i)
    {
        queue.push(value_t(std::rand()));
    }
    if (queue.size() != test_loop_n)
    {
        return 1;
    }
    for (uint i = 0; i < test_loop_n; ++i)
    {
        auto v = queue.pop();
        if (!v)
        {
            return 1;
        }
    }
    auto v = queue.pop();
    if (v)
    {
        return 1;
    }
    if (queue.size() != 0)
    {
        return 1;
    }
    return 0;
}

template<typename T>
int test_queue_ctor(T&& obj)
{
    using value_t = std::remove_reference_t<T>;

    if constexpr (la::move_or_copy_constructable<value_t>)
    {
        auto queue = la::mt_queue<value_t>();
        return test_queue(queue);
    }
    return 0;
}

template<typename... Types>
int test_queues(Types&&... args)
{
    return (test_queue_ctor(args) + ...);
}

int main()
{
    auto ret = std::apply([](auto&&... args) { return test_queues(args...); }, dummy_def);
    ret += std::apply([](auto&&... args) { return test_queues(args...); }, dummy_no_def);
    return ret;

    return 0;
}