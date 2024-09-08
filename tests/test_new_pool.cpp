#include "new_span_pool.hpp"

#include <array>
#include <iostream>
#include <random>
#include <thread>

using pooled_t = std::vector<int>;
namespace mu   = mtmu::ll3;

constexpr int max        = 32;
constexpr int iterations = max * 2;
constexpr int n_threads  = 16;

template<typename T>
int test_outlive() {
    using pooled_t         = T;
    auto outliving_storage = std::array<mu::pl_span<pooled_t, true>, max>{};

    constexpr auto place_ctor = [](pooled_t* ptr) { return new (ptr) pooled_t(max); };

    auto pool = mu::span_pool<pooled_t>(place_ctor, 2);
    for (auto& pspan: outliving_storage)
        pspan = pool.acquire();
    return 0;
}


int main() {
    /*constexpr auto                  asize = 128;*/
    /*constexpr int                   max   = 32;*/
    /*std::random_device              rd;           // a seed source for the random number engine*/
    /*std::mt19937                    gen(rd());    // mersenne_twister_engine seeded with rd()*/
    /*std::mutex                      distrib_mutex;*/
    /*std::uniform_int_distribution<> distrib(0, max - 1);*/
    /**/
    /*constexpr int iterations = max * 2;*/
    /*constexpr int n_threads  = 16;*/
    /**/
    /*auto spool = mu::span_pool<pooled_t>(*/
    /*    [&](pooled_t* ptr) { return new (ptr) pooled_t(distrib(gen)); }, 128, max * n_threads);*/
    /*auto y = std::array<mu::pl_span<pooled_t, true>, asize>{};*/
    /*for (int i = 0; i < asize; ++i) {*/
    /*    y[i] = spool.acquire();*/
    /*}*/
    /**/
    /**/
    /*auto random_counts = std::array<std::vector<int>, n_threads>();*/
    /*for (auto& vec: random_counts) {*/
    /*    vec = std::vector<int>(iterations);*/
    /**/
    /*    for (auto& v: vec)*/
    /*        v = distrib(gen);*/
    /*}*/
    /**/
    /*int  repeats     = 2;*/
    /*auto make_pl_obj = [] {*/
    /*    thread_local std::random_device rd;           // a seed source for the random number engine*/
    /*    thread_local std::mt19937       gen(rd());    // mersenne_twister_engine seeded with rd()*/
    /*    std::uniform_int_distribution<> distrib(0, max - 1);*/
    /*    auto                            len = distrib(gen);*/
    /*    return std::vector<int>(len);*/
    /*};*/
    /*auto modify_pl_obj = [](pooled_t& obj) {*/
    /*    thread_local std::random_device rd;           // a seed source for the random number engine*/
    /*    thread_local std::mt19937       gen(rd());    // mersenne_twister_engine seeded with rd()*/
    /*    std::uniform_int_distribution<> distrib(0, max - 1);*/
    /*    for (auto& v: obj) {*/
    /*        auto val = distrib(gen);*/
    /*        v        = val;*/
    /*    }*/
    /*};*/
    /**/
    /*namespace chr    = std::chrono;*/
    /*auto start_point = chr::high_resolution_clock::now();*/
    /*for (int i = 0; i < repeats; ++i) {*/
    /*    auto start   = std::atomic<bool>(false);*/
    /*    auto threads = [&]<std::size_t... I>(std::index_sequence<I...>) {*/
    /*        return std::array{std::jthread(([&, index = I] {*/
    /*            while (!start.load(std::memory_order_acquire)) {*/
    /*                std::this_thread::sleep_for(chr::nanoseconds(50));*/
    /*            }*/
    /*            for (auto cnt: random_counts[index]) {*/
    /*                auto spans = std::array<mu::pl_span<pooled_t, true>, max>();*/
    /*                for (int i = 0; i < cnt; ++i) {*/
    /*                    spans[i] = spool.acquire();*/
    /*                    for (auto& v: spans[i]) {*/
    /*                        v = make_pl_obj();*/
    /*                        modify_pl_obj(v);*/
    /*                    }*/
    /*                }*/
    /*            }*/
    /*        }))...};*/
    /*    }(std::make_index_sequence<n_threads>{});*/
    /*    start.store(true, std::memory_order_release);*/
    /*}*/
    /*auto duration = chr::duration_cast<chr::milliseconds>(chr::high_resolution_clock::now() - start_point);*/
    /*std::cout << duration.count() << "ms\n";*/

    test_outlive<pooled_t>();
    return 0;
}
