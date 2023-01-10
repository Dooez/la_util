cmake_minimum_required(VERSION 3.16)
if( CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR )
    enable_testing()
    project(la_util VERSION 0.2.0 LANGUAGES CXX)

    set(CMAKE_CXX_STANDARD 20)
    set(CMAKE_CXX_STANDARD_REQUIRED ON)

    include_directories( ${CMAKE_CURRENT_SOURCE_DIR}
        ${CMAKE_CURRENT_SOURCE_DIR}/tests)

    add_executable(pool tests/test_pool.cpp)
    add_test(pool pool)
    add_executable(queue tests/test_queue.cpp)
    add_test(queue queue)
else()
    add_library(la_util STATIC)
    target_include_directories(la_util INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})
    set_target_properties(la_util PROPERTIES CXX_STANDARD 20 CXX_STANDARD_REQUIRED True)
endif()