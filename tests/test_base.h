#include <iostream>
#include <tuple>

template<bool Def_C, bool Cpy_C, bool Cpy_A, bool Mov_C, bool Mov_A>
class class_dummy
{
public:
    explicit class_dummy(int input)
    : m_data(input){};

    ~class_dummy() = default;

    class_dummy()
        requires Def_C
    = default;
    class_dummy(const class_dummy& other)
        requires Cpy_C
    = default;
    class_dummy& operator=(const class_dummy& other)
        requires Cpy_A
    {
        m_data = other.m_data;
        return *this;
    };

    class_dummy(class_dummy&& other) noexcept
        requires Mov_C
    = default;
    class_dummy& operator=(class_dummy&& other) noexcept
        requires Mov_A
    {
        m_data = other.m_data;
        return *this;
    }

private:
    int m_data = 0;
};

/**
 * @brief tuple of dummy classes with default constructor, int constructor and
 * all combinations of copy and move constructors and assignments.
 *
 */
using dummy_def_t = std::tuple<class_dummy<true, true, true, true, true>,
                               class_dummy<true, true, true, true, false>,
                               class_dummy<true, true, true, false, true>,
                               class_dummy<true, true, true, false, false>,
                               class_dummy<true, true, false, true, true>,
                               class_dummy<true, true, false, true, false>,
                               class_dummy<true, true, false, false, true>,
                               class_dummy<true, true, false, false, false>,
                               class_dummy<true, false, true, true, true>,
                               class_dummy<true, false, true, true, false>,
                               class_dummy<true, false, true, false, true>,
                               class_dummy<true, false, true, false, false>,
                               class_dummy<true, false, false, true, true>,
                               class_dummy<true, false, false, true, false>,
                               class_dummy<true, false, false, false, true>,
                               class_dummy<true, false, false, false, false>>;

/**
 * @brief tuple of dummy classes with no default constructor, int constructor and
 * all combinations of copy and move constructors and assignments.
 *
 */
using dummy_no_def_t     = std::tuple<class_dummy<false, true, true, true, true>,
                                  class_dummy<false, true, true, true, false>,
                                  class_dummy<false, true, true, false, true>,
                                  class_dummy<false, true, true, false, false>,
                                  class_dummy<false, true, false, true, true>,
                                  class_dummy<false, true, false, true, false>,
                                  class_dummy<false, true, false, false, true>,
                                  class_dummy<false, true, false, false, false>,
                                  class_dummy<false, false, true, true, true>,
                                  class_dummy<false, false, true, true, false>,
                                  class_dummy<false, false, true, false, true>,
                                  class_dummy<false, false, true, false, false>,
                                  class_dummy<false, false, false, true, true>,
                                  class_dummy<false, false, false, true, false>,
                                  class_dummy<false, false, false, false, true>,
                                  class_dummy<false, false, false, false, false>>;

inline auto dummy_def    = dummy_def_t{};
inline auto dummy_no_def = dummy_no_def_t{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
