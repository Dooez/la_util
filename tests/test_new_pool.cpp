#include "new_span_pool.hpp"

#include <array>

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


    return 0;
}
