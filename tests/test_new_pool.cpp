#include "mtmu/new_span_pool.hpp"

#include <array>
#include <iostream>
#include <random>
#include <thread>

using pooled_t = std::vector<int>;
namespace mu   = mtmu::ll3;

using distrib_t                    = std::uniform_int_distribution<>;
constexpr int max_spans_per_thread = 128;
constexpr int max_span_size        = 16;
constexpr int iterations           = max_spans_per_thread * 2;
constexpr int n_threads            = 16;

template<typename T>
constexpr auto pool_makers = std::make_tuple(    //
    [](const auto& ctor) { return mu::span_pool<T>(ctor, 128); },
    [](const auto& ctor) { return mu::span_pool<T>(ctor, 128, 128); }
    //
);
template<typename T>
struct preallocated_pool_ctor {
    using pool_t = mu::span_pool<T>;
    std::random_device rd;           // a seed source for the random number engine*/
    std::mt19937       gen{rd()};    // mersenne_twister_engine seeded with rd()
                                     /*std::mutex                      distrib_mutex;*/
    distrib_t span_size_distr{1, max_span_size};
    distrib_t prealloc_distr{0, (max_spans_per_thread * n_threads)};

    auto operator()(auto&& elem_plcae_ctor) -> pool_t {
        return pool_t(elem_plcae_ctor, span_size_distr(gen), prealloc_distr(gen));
    }
};


template<typename T>
struct resized_pool_ctor {
    using pool_t = mu::span_pool<T>;
    std::random_device rd;           // a seed source for the random number engine*/
    std::mt19937       gen{rd()};    // mersenne_twister_engine seeded with rd()
                                     /*std::mutex                      distrib_mutex;*/
    static constexpr int max_resize = 128;

    distrib_t span_size_distr{1, max_span_size};
    distrib_t resize_distr{0, (max_spans_per_thread * n_threads)};

    auto operator()(auto&& elem_plcae_ctor) -> pool_t {
        auto pool = pool_t(elem_plcae_ctor, span_size_distr(gen));
        pool.resize(resize_distr(gen));
        return pool;
    }
};
template<typename T>
struct reserved_pool_ctor {
    using pool_t = mu::span_pool<T>;
    std::random_device rd;           // a seed source for the random number engine*/
    std::mt19937       gen{rd()};    // mersenne_twister_engine seeded with rd()
                                     /*std::mutex                      distrib_mutex;*/
    static constexpr int max_resize = 128;

    distrib_t span_size_distr{1, max_span_size};
    distrib_t reserve_distr{0, (max_spans_per_thread * n_threads)};

    auto operator()(auto&& elem_plcae_ctor) -> pool_t {
        auto pool = pool_t(elem_plcae_ctor, span_size_distr(gen));
        pool.reserve(reserve_distr(gen));
        return pool;
    }
};


int test_outlive(auto&& pool_ctor, auto&& elem_place_ctor) {
    using pool_t   = decltype(pool_ctor(elem_place_ctor));
    using pooled_t = pool_t::value_type;
    auto apool_ptr = std::atomic<pool_t*>();
    auto start     = std::atomic<bool>();
    auto ready     = std::atomic<std::size_t>();
    auto finished  = std::atomic<std::size_t>();
    auto exit      = std::atomic<bool>();

    std::random_device rd;           // a seed source for the random number engine*/
    std::mt19937       gen{rd()};    // mersenne_twister_engine seeded with rd()
    std::mutex         distrib_mutex;
    distrib_t          span_size_distr{1, max_spans_per_thread};

    auto worker = [&]() {
        auto outliving_storage = std::array<mu::pl_span<pooled_t, true>, max_spans_per_thread>();
        int  count;
        {
            auto lock = std::scoped_lock(distrib_mutex);
            count     = span_size_distr(gen);
        }
        ready.fetch_add(1);
        while (!start.load())
            std::this_thread::sleep_for(std::chrono::microseconds(50));
        auto pool_ptr = apool_ptr.load();
        for (int i = 0; i < count; ++i)
            outliving_storage[i] = pool_ptr->acquire();
        auto f = finished.fetch_add(1);
        while (finished.load() != n_threads)
            std::this_thread::sleep_for(std::chrono::microseconds(1));
    };

    auto threads = [&worker]<std::size_t... Is>(std::index_sequence<Is...>) {
        return std::array{std::jthread((void(Is), worker))...};
    }(std::make_index_sequence<n_threads>{});
    {
        auto pool = pool_ctor(elem_place_ctor);
        apool_ptr.store(&pool);
        while (ready.load() != n_threads)
            std::this_thread::sleep_for(std::chrono::microseconds(50));
        start.store(true);
        while (finished.load() != n_threads)
            std::this_thread::sleep_for(std::chrono::microseconds(1));
    }
    exit.store(true);
    return 0;
}

template<typename T>
int test_threads() {
    return 0;
}


int main() {
    for (int i = 0; i < 512; ++i) {
        /*std::cout << i << " ";*/
        test_outlive(preallocated_pool_ctor<pooled_t>(), [](pooled_t* ptr) { new (ptr) pooled_t{1}; });
        test_outlive(resized_pool_ctor<pooled_t>(), [](pooled_t* ptr) { new (ptr) pooled_t{1}; });
        test_outlive(reserved_pool_ctor<pooled_t>(), [](pooled_t* ptr) { new (ptr) pooled_t{1}; });
    }
    return 0;
}
