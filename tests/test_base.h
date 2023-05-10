#include <iostream>
#include <memory>
#include <tuple>

namespace test {

namespace internal {

    inline bool should_print = false;
}

template<bool Def_C, bool Cpy_C, bool Cpy_A, bool Mov_C, bool Mov_A>
class class_dummy
{
public:
    using value_t = int;
    explicit class_dummy(value_t input)
    : m_data(input){};

    ~class_dummy() = default;

    class_dummy()
        requires Def_C
    = default;
    class_dummy(const class_dummy& other)
        requires Cpy_C
    : m_data(other.m_data)
    {
        if (internal::should_print)
        {
            std::cout << "copy ctor ";
            cout_name();
        }
    }
    class_dummy& operator=(const class_dummy& other)
        requires Cpy_A
    {
        m_data = other.m_data;
        if (internal::should_print)
        {
            std::cout << "copy ass ";
            cout_name();
        }
        return *this;
    };

    class_dummy(class_dummy&& other) noexcept
        requires Mov_C
    : m_data(other.m_data)
    {
        if (internal::should_print)
        {
            std::cout << "mov ctor ";
            cout_name();
        }
    }
    class_dummy& operator=(class_dummy&& other) noexcept
        requires Mov_A
    {
        m_data = other.m_data;
        if (internal::should_print)
        {
            std::cout << "mov ass ";
            cout_name();
        }
        return *this;
    }

    static void cout_name()
    {
        std::cout << "class_dummy<" << Def_C << ", " << Cpy_C << ", " << Cpy_A << ", " << Mov_C
                  << ", " << Mov_A << ">\n";
    }

    static value_t constructor_value()
    {
        return 0;
    }

private:
    value_t m_data = 0;
};

void enable_printing(bool enable = true)
{
    internal::should_print = enable;
}

namespace internal {

    template<bool Def_C, bool Cpy_C, bool Cpy_A, bool Mov_C, bool Test_Mov_A, bool Mov_A = true>
    auto test_mov_a()
    {
        if constexpr (Test_Mov_A)
        {
            return std::tuple<class_dummy<Def_C, Cpy_C, Cpy_A, Mov_C, true>*,
                              class_dummy<Def_C, Cpy_C, Cpy_A, Mov_C, false>*>(nullptr, nullptr);
        } else
        {
            return std::tuple<class_dummy<Def_C, Cpy_C, Cpy_A, Mov_C, Mov_A>*>(nullptr);
        }
    }
    template<bool Def_C,
             bool Cpy_C,
             bool Cpy_A,
             bool Test_Mov_C,
             bool Test_Mov_A,
             bool Mov_C = true,
             bool Mov_A = true>
    auto test_mov_c()
    {
        if constexpr (Test_Mov_C)
        {
            return std::tuple_cat(test_mov_a<Def_C, Cpy_C, Cpy_A, true, Test_Mov_A, Mov_A>(),
                                  test_mov_a<Def_C, Cpy_C, Cpy_A, false, Test_Mov_A, Mov_A>());
        } else
        {
            return test_mov_a<Def_C, Cpy_C, Cpy_A, Mov_C, Test_Mov_A, Mov_A>();
        }
    }

    template<bool Def_C,
             bool Cpy_C,
             bool Test_Cpy_A,
             bool Test_Mov_C,
             bool Test_Mov_A,
             bool Cpy_A = true,
             bool Mov_C = true,
             bool Mov_A = true>
    auto test_cpy_a()
    {
        if constexpr (Test_Cpy_A)
        {
            return std::tuple_cat(
                test_mov_c<Def_C, Cpy_C, true, Test_Mov_C, Test_Mov_A, Mov_C, Mov_A>(),
                test_mov_c<Def_C, Cpy_C, false, Test_Mov_C, Test_Mov_A, Mov_C, Mov_A>());
        } else
        {
            return test_mov_c<Def_C, Cpy_C, Cpy_A, Test_Mov_C, Test_Mov_A, Mov_C, Mov_A>();
        }
    }

    template<bool Def_C,
             bool Test_Cpy_C,
             bool Test_Cpy_A,
             bool Test_Mov_C,
             bool Test_Mov_A,
             bool Cpy_C = true,
             bool Cpy_A = true,
             bool Mov_C = true,
             bool Mov_A = true>
    auto test_cpy_c()
    {
        if constexpr (Test_Cpy_C)
        {
            return std::tuple_cat(
                test_cpy_a<Def_C, true, Test_Cpy_A, Test_Mov_C, Test_Mov_A, Cpy_A, Mov_C, Mov_A>(),
                test_cpy_a<Def_C,
                           false,
                           Test_Cpy_A,
                           Test_Mov_C,
                           Test_Mov_A,
                           Cpy_A,
                           Mov_C,
                           Mov_A>());
        } else
        {
            return test_cpy_a<Def_C,
                              Cpy_C,
                              Test_Cpy_A,
                              Test_Mov_C,
                              Test_Mov_A,
                              Cpy_A,
                              Mov_C,
                              Mov_A>();
        }
    }

    template<bool Test_Def_C,
             bool Test_Cpy_C,
             bool Test_Cpy_A,
             bool Test_Mov_C,
             bool Test_Mov_A,
             bool Def_C = true,
             bool Cpy_C = true,
             bool Cpy_A = true,
             bool Mov_C = true,
             bool Mov_A = true>
    auto test_def_c()
    {
        if constexpr (Test_Def_C)
        {
            return std::tuple_cat(test_cpy_c<true,
                                             Test_Cpy_C,
                                             Test_Cpy_A,
                                             Test_Mov_C,
                                             Test_Mov_A,
                                             Cpy_C,
                                             Cpy_A,
                                             Mov_C,
                                             Mov_A>(),
                                  test_cpy_c<false,
                                             Test_Cpy_C,
                                             Test_Cpy_A,
                                             Test_Mov_C,
                                             Test_Mov_A,
                                             Cpy_C,
                                             Cpy_A,
                                             Mov_C,
                                             Mov_A>());
        } else
        {
            return test_cpy_c<Def_C,
                              Test_Cpy_C,
                              Test_Cpy_A,
                              Test_Mov_C,
                              Test_Mov_A,
                              Cpy_C,
                              Cpy_A,
                              Mov_C,
                              Mov_A>();
        }
    }
    template<typename... Args>
    auto get_tup(Args... args)
    {
        using tuple_t = std::tuple<std::remove_pointer_t<Args>...>;

        auto ptr = new tuple_t{std::remove_pointer_t<Args>::constructor_value()...};
        return std::unique_ptr<tuple_t>(ptr);
    };

}    // namespace internal

/**
 * @brief Creates tuple with multiple dummy_class objects.
 * Tuple contains objects with different combnations of constructors and assignments.
 * Objects are constructed from dummy_class<...>::constructor_value();
 * If Test_<member> template argument is true, tuple will contain dummy classes with and without <member>.
 * If Test_<member> template argument is false, <member> template argument controls if dummy classes have <member>.
 * 
 * 
 * @tparam Test_Def_C Test default constructor
 * @tparam Test_Cpy_C Test copy constructor
 * @tparam Test_Cpy_A Test copy assignment
 * @tparam Test_Mov_C Test move constructor
 * @tparam Test_Mov_A Test move assignment
 * @tparam Def_C Default constructor
 * @tparam Cpy_C Copy constructor 
 * @tparam Cpy_A Copy assignment 
 * @tparam Mov_C Move constructor 
 * @tparam Mov_A Move assignment 
 * 
 * @return auto std::unique_ptr<std::tuple<...>> 
 */
template<bool Test_Def_C,
         bool Test_Cpy_C,
         bool Test_Cpy_A,
         bool Test_Mov_C,
         bool Test_Mov_A,
         bool Def_C = true,
         bool Cpy_C = true,
         bool Cpy_A = true,
         bool Mov_C = true,
         bool Mov_A = true>
auto create_test_tuple()
{
    auto ptr_tuple = internal::test_def_c<Test_Def_C,
                                          Test_Cpy_C,
                                          Test_Cpy_A,
                                          Test_Mov_C,
                                          Test_Mov_A,
                                          Def_C,
                                          Cpy_C,
                                          Cpy_A,
                                          Mov_C,
                                          Mov_A>();
    return std::apply([](auto&&... args) { return internal::get_tup(args...); }, ptr_tuple);
}
}    // namespace test