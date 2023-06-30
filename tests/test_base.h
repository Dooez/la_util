#include <iostream>
#include <memory>
#include <tuple>

namespace test {

/**
 * @brief Selects combinations of special member functions and their values.
 *
 */
struct member_selector {
    struct {
        bool def_ctor = true;
        bool cpy_ctor = true;
        bool cpy_asgn = true;
        bool mov_ctor = true;
        bool mov_asgn = true;
    } check;
    bool def_ctor = false;
    bool cpy_ctor = false;
    bool cpy_asgn = false;
    bool mov_ctor = false;
    bool mov_asgn = false;
};

namespace detail_ {

inline bool should_print = false;
}

template<member_selector Select>
class class_dummy {
public:
    using value_t = int;
    explicit class_dummy(value_t input)
    : m_data(input){};

    ~class_dummy() = default;

    class_dummy()
        requires(Select.def_ctor)
    = default;
    class_dummy(const class_dummy& other)
        requires(Select.cpy_ctor)
    : m_data(other.m_data) {
        if (detail_::should_print) {
            std::cout << "copy ctor ";
            cout_name();
        }
    }
    class_dummy& operator=(const class_dummy& other)
        requires(Select.cpy_asgn)
    {
        m_data = other.m_data;
        if (detail_::should_print) {
            std::cout << "copy ass ";
            cout_name();
        }
        return *this;
    };

    class_dummy(class_dummy&& other) noexcept
        requires(Select.mov_ctor)
    : m_data(other.m_data) {
        if (detail_::should_print) {
            std::cout << "mov ctor ";
            cout_name();
        }
    }
    class_dummy& operator=(class_dummy&& other) noexcept
        requires(Select.mov_asgn)
    {
        m_data = other.m_data;
        if (detail_::should_print) {
            std::cout << "mov ass ";
            cout_name();
        }
        return *this;
    }

    static void cout_name() {
        std::cout << "class_dummy<" << Select.def_ctor << ", " << Select.cpy_ctor << ", " << Select.cpy_asgn
                  << ", " << Select.mov_ctor << ", " << Select.mov_asgn << ">\n";
    }

    static value_t constructor_value() {
        return 0;
    }

private:
    value_t m_data = 0;
};

void enable_printing(bool enable = true) {
    detail_::should_print = enable;
}

namespace detail_ {


template<member_selector Select>
auto test_mov_a() {
    if constexpr (Select.check.mov_asgn) {
        constexpr member_selector sel_true = {
            .check    = Select.check,
            .def_ctor = Select.def_ctor,
            .cpy_ctor = Select.cpy_ctor,
            .cpy_asgn = Select.cpy_asgn,
            .mov_ctor = Select.mov_ctor,
            .mov_asgn = true,
        };
        constexpr member_selector sel_false = {
            .check    = Select.check,
            .def_ctor = Select.def_ctor,
            .cpy_ctor = Select.cpy_ctor,
            .cpy_asgn = Select.cpy_asgn,
            .mov_ctor = Select.mov_ctor,
            .mov_asgn = false,
        };
        return std::tuple<class_dummy<sel_true>*, class_dummy<sel_false>*>(nullptr, nullptr);
    } else {
        return std::tuple<class_dummy<Select>*>(nullptr);
    }
}
template<member_selector Select>
auto test_mov_c() {
    if constexpr (Select.check.mov_ctor) {
        constexpr member_selector sel_true = {
            .check    = Select.check,
            .def_ctor = Select.def_ctor,
            .cpy_ctor = Select.cpy_ctor,
            .cpy_asgn = Select.cpy_asgn,
            .mov_ctor = true,
            .mov_asgn = Select.mov_asgn,
        };
        constexpr member_selector sel_false = {
            .check    = Select.check,
            .def_ctor = Select.def_ctor,
            .cpy_ctor = Select.cpy_ctor,
            .cpy_asgn = Select.cpy_asgn,
            .mov_ctor = false,
            .mov_asgn = Select.mov_asgn,
        };
        return std::tuple_cat(test_mov_a<sel_true>(), test_mov_a<sel_false>());
    } else {
        return test_mov_a<Select>();
    }
}

template<member_selector Select>
auto test_cpy_a() {
    if constexpr (Select.check.cpy_asgn) {
        constexpr member_selector sel_true = {
            .check    = Select.check,
            .def_ctor = Select.def_ctor,
            .cpy_ctor = Select.cpy_ctor,
            .cpy_asgn = true,
            .mov_ctor = Select.mov_ctor,
            .mov_asgn = Select.mov_asgn,
        };
        constexpr member_selector sel_false = {
            .check    = Select.check,
            .def_ctor = Select.def_ctor,
            .cpy_ctor = Select.cpy_ctor,
            .cpy_asgn = false,
            .mov_ctor = Select.mov_ctor,
            .mov_asgn = Select.mov_asgn,
        };
        return std::tuple_cat(test_mov_c<sel_true>(), test_mov_c<sel_true>());
    } else {
        return test_mov_c<Select>();
    }
}

template<member_selector Select>
auto test_cpy_c() {
    if constexpr (Select.check.cpy_ctor) {
        constexpr member_selector sel_true = {
            .check    = Select.check,
            .def_ctor = Select.def_ctor,
            .cpy_ctor = true,
            .cpy_asgn = Select.cpy_asgn,
            .mov_ctor = Select.mov_ctor,
            .mov_asgn = Select.mov_asgn,
        };
        constexpr member_selector sel_false = {
            .check    = Select.check,
            .def_ctor = Select.def_ctor,
            .cpy_ctor = false,
            .cpy_asgn = Select.cpy_asgn,
            .mov_ctor = Select.mov_ctor,
            .mov_asgn = Select.mov_asgn,
        };
        return std::tuple_cat(test_cpy_a<sel_true>(), test_cpy_a<sel_false>());
    } else {
        return test_cpy_a<Select>();
    }
}

template<member_selector Select>
auto test_def_c() {
    if constexpr (Select.check.def_ctor) {
        constexpr member_selector sel_true = {
            .check    = Select.check,
            .def_ctor = true,
            .cpy_ctor = Select.cpy_ctor,
            .cpy_asgn = Select.cpy_asgn,
            .mov_ctor = Select.mov_ctor,
            .mov_asgn = Select.mov_asgn,
        };
        constexpr member_selector sel_false = {
            .check    = Select.check,
            .def_ctor = false,
            .cpy_ctor = Select.cpy_ctor,
            .cpy_asgn = Select.cpy_asgn,
            .mov_ctor = Select.mov_ctor,
            .mov_asgn = Select.mov_asgn,
        };
        return std::tuple_cat(test_cpy_c<sel_true>(), test_cpy_c<sel_false>());
    } else {
        return test_cpy_c<Select>();
    }
}
template<typename... Args>
auto get_tup(Args... args) {
    using tuple_t = std::tuple<std::remove_pointer_t<Args>...>;

    auto ptr = new tuple_t{std::remove_pointer_t<Args>::constructor_value()...};
    return std::unique_ptr<tuple_t>(ptr);
};

}    // namespace detail_


/**
 * @brief Creates tuple with multiple dummy_class objects.
 * Tuple contains objects with different combnations of constructors and assignments.
 * Objects are constructed from dummy_class<...>::constructor_value();
 * If member_selector.check.<member>  is true, tuple will contain dummy classes with and without <member>.
 * If member_selector.check.<member> template argument is false, member_selector.<member> controls if dummy classes have <member>.
 *
 *
 * @return auto std::unique_ptr<std::tuple<...>>
 */
template<member_selector Select>
auto create_test_tuple() {
    auto ptr_tuple = detail_::test_def_c<Select>();
    return std::apply([](auto&&... args) { return detail_::get_tup(args...); }, ptr_tuple);
}
}    // namespace test