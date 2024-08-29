#include "new_span_pool.hpp"

int main() {
    auto spool = mtmu::ll3::span_pool<int>(1, 128);
    auto x     = spool.acquire();

    return 0;
}
