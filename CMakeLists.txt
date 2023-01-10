cmake_minimum_required(VERSION 3.16)
if( CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR )
    project(la_util VERSION 0.2.0 LANGUAGES CXX)

    include_directories( ${CMAKE_CURRENT_SOURCE_DIR})

    add_executable(tests
        tests/test_pool.cpp)
    set_target_properties(tests PROPERTIES CXX_STANDARD 20 CXX_STANDARD_REQUIRED True)

else()
    add_library(la_util STATIC)
    target_include_directories(la_util INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})
endif()