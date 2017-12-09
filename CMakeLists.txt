cmake_minimum_required(VERSION 2.6)

project(asmgen)

SET(EXTRA_CXX_COMPILE_FLAGS "-std=c++11 -I./src -I./test -O2 -Werror -Wall -pedantic")

SET(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} ${EXTRA_CXX_COMPILE_FLAGS}")

INCLUDE_DIRECTORIES(./src/)

SET(TEST_FILES test/main.cpp)

add_executable(all-tests ${TEST_FILES} ${MANUFACTURE_TEST_FILES})
