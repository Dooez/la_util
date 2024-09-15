cmake_minimum_required(VERSION 3.16)
add_library(la_util INTERFACE)
target_include_directories(la_util INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})
set_target_properties(la_util PROPERTIES CXX_STANDARD 20 CXX_STANDARD_REQUIRED True)

if( CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR )
    enable_testing()
    project(la_util VERSION 0.2.0 LANGUAGES CXX)
    set(ADR_SAN -fsanitize=address -fno-omit-frame-pointer -g)
    set(UB_SAN -fsanitize=undefined -fno-omit-frame-pointer -g)
    set(THR_SAN -fsanitize=thread -fPIE -pie -fno-omit-frame-pointer -g)
    set(MEM_SAN -fsanitize=memory -fPIE -pie -fno-omit-frame-pointer -g)

    set(CMAKE_CXX_STANDARD 20)
    set(CMAKE_CXX_STANDARD_REQUIRED ON)

    include_directories( ${CMAKE_CURRENT_SOURCE_DIR}
        ${CMAKE_CURRENT_SOURCE_DIR}/tests)

    add_executable(pool tests/test_pool.cpp)
    add_test(pool pool)
    add_executable(queue tests/test_queue.cpp)
    add_test(queue queue)

    add_executable(new_pool tests/test_new_pool.cpp)
    target_link_libraries(new_pool la_util atomic)
    set_target_properties(new_pool PROPERTIES CXX_STANDARD 20 CXX_STANDARD_REQUIRED True)

    add_executable(new_pool_ub tests/test_new_pool.cpp)
    target_link_libraries(new_pool_ub la_util atomic)
    target_compile_options(new_pool_ub PUBLIC  ${UB_SAN})
    target_link_options(new_pool_ub PUBLIC  -fsanitize=undefined )
    add_test(NAME span_pool_ub COMMAND  new_pool_ub)
    set_tests_properties(span_pool_ub PROPERTIES "UBSAN_OPTIONS" "abort_on_errory=true")

    add_executable(new_pool_thr tests/test_new_pool.cpp)
    target_link_libraries(new_pool_thr la_util atomic)
    target_compile_options(new_pool_thr PUBLIC  ${THR_SAN})
    target_link_options(new_pool_thr PUBLIC  -fsanitize=thread )
    add_test(NAME span_pool_thr COMMAND  new_pool_thr)
    set_tests_properties(span_pool_ub PROPERTIES "TSAN_OPTIONS" "abort_on_errory=true")

    add_executable(new_pool_addr tests/test_new_pool.cpp)
    target_link_libraries(new_pool_addr la_util atomic)
    target_compile_options(new_pool_addr PUBLIC  ${ADR_SAN})
    target_link_options(new_pool_addr PUBLIC  -fsanitize=address )
    add_test(NAME span_pool_addr COMMAND new_pool_addr)
    set_tests_properties(span_pool_ub PROPERTIES "ASAN_OPTIONS" "abort_on_errory=true")

    add_executable(new_pool_mem tests/test_new_pool.cpp)
    target_link_libraries(new_pool_mem la_util atomic)
    target_compile_options(new_pool_mem PUBLIC  ${ADR_SAN})
    target_link_options(new_pool_mem PUBLIC  -fsanitize=address )
    add_test(NAME span_pool_mem COMMAND new_pool_mem)
    set_tests_properties(span_pool_ub PROPERTIES "MSAN_OPTIONS" "abort_on_errory=true")

endif()
