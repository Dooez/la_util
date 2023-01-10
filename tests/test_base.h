#include <iostream>
#include <tuple>

template<bool Def_C, bool Cpy_C, bool Cpy_A, bool Mov_C, bool Mov_A>
class dummy
{
public:
    explicit dummy(int input)
    : m_data(input){};

    ~dummy() = default;

    dummy()
        requires Def_C
    = default;
    dummy(const dummy& other)
        requires Cpy_C
    : m_data(other.m_data)
    , m_copies(other.m_copies)
    , m_moves(other.m_moves)
    {
        ++m_copies;
    };
    dummy& operator=(const dummy& other)
        requires Cpy_A
    {
        m_data   = other.m_data;
        m_moves  = other.m_moves;
        m_copies = other.m_copies;
        ++m_copies;
        return *this;
    };

    dummy(dummy&& other) noexcept
        requires Mov_C
    : m_data(other.m_data)
    , m_copies(other.m_copies)
    , m_moves(other.m_moves)
    {
        ++m_moves;
    };
    dummy& operator=(dummy&& other) noexcept
        requires Mov_A
    {
        m_data   = other.m_data;
        m_moves  = other.m_moves;
        m_copies = other.m_copies;
        ++m_moves;
        return *this;
    }

private:
    int  m_data   = 0;
    uint m_moves  = 0;
    uint m_copies = 0;
};

using dummy_tuple = std::tuple<dummy<true, true, true, true, true>,
                               dummy<true, true, true, true, false>,
                               dummy<true, true, true, false, true>,
                               dummy<true, true, true, false, false>,
                               dummy<true, true, false, true, true>,
                               dummy<true, true, false, true, false>,
                               dummy<true, true, false, false, true>,
                               dummy<true, true, false, false, false>,
                               dummy<true, false, true, true, true>,
                               dummy<true, false, true, true, false>,
                               dummy<true, false, true, false, true>,
                               dummy<true, false, true, false, false>,
                               dummy<true, false, false, true, true>,
                               dummy<true, false, false, true, false>,
                               dummy<true, false, false, false, true>,
                               dummy<true, false, false, false, false>>;


template<uint I, typename... Types>
auto& extract(std::tuple<Types...> tuple)
{
    if constexpr (I == sizeof...(Types) - 1)
    {
        return std::get<I>(tuple);
    } else
    {
        return std::tuple_cat(std::get<I>(tuple), extract<I + 1>(tuple));
    }
}