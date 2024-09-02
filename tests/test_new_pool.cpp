#include "new_span_pool.hpp"

#include <array>
#include <iostream>
#include <random>
#include <thread>

int main() {
    namespace mu         = mtmu::ll3;
    constexpr auto asize = 128;

    auto spool = mu::span_pool<int>(1, asize);
    auto x     = std::array<mu::pooled_ptr<int, true>, asize>{};
    for (auto& p: x) {
        p = spool.acquire();
    }

    auto spool2 = mu::span_pool<int>(128);
    auto y      = std::array<mu::pooled_ptr<int, true>, asize>{};
    for (int i = 0; i < asize; ++i) {
        y[i] = spool2.acquire();
    }

    constexpr int max = 32;

    std::random_device              rd;           // a seed source for the random number engine
    std::mt19937                    gen(rd());    // mersenne_twister_engine seeded with rd()
    std::uniform_int_distribution<> distrib(1, max - 1);

    constexpr int iterations = max * 2;
    constexpr int n_threads  = 16;

    auto random_counts = std::array<std::vector<int>, n_threads>();
    for (auto& vec: random_counts) {
        vec = std::vector<int>(iterations);
        for (auto& v: vec)
            v = distrib(gen);
    }

    int repeats = 128;

    namespace chr    = std::chrono;
    auto start_point = chr::high_resolution_clock::now();
    for (int i = 0; i < repeats; ++i) {
        auto start   = std::atomic<bool>(false);
        auto threads = [&spool2, &random_counts, &start]<std::size_t... I>(std::index_sequence<I...>) {
            return std::array{std::jthread(([index = I, &spool2, &random_counts, &start] {
                while (!start.load(std::memory_order_acquire)) {
                    std::this_thread::sleep_for(chr::nanoseconds(50));
                }
                for (auto cnt: random_counts[index]) {
                    auto ptrs = std::array<mu::pooled_ptr<int, true>, max>();
                    for (int i = 0; i < cnt; ++i) {
                        ptrs[i] = spool2.acquire();
                    }
                }
            }))...};
        }(std::make_index_sequence<n_threads>{});
        start.store(true, std::memory_order_release);
    }
    auto duration = chr::duration_cast<chr::milliseconds>(chr::high_resolution_clock::now() - start_point);
    std::cout << duration.count() << "ms\n";

    return 0;
}
