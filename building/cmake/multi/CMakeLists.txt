cmake_minimum_required (VERSION 2.6)

project (Multi)

include_directories(mylib)
add_definitions(-g -Wall)

add_subdirectory(mylib)
add_subdirectory(runner)



