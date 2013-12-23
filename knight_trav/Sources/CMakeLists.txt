cmake_minimum_required (VERSION 2.8)
project (knight_travail)
set (KNIGHT_TRAVAIL_VERSION_MAJOR 1)
set (KNIGHT_TRAVAIL_VERSION_MINOR 0)

include_directories(
  ${PROJECT_BINARY_DIR}
  Knight
)

# It's reasonable to expect Boost to be installed.
find_package(Boost REQUIRED COMPONENTS system program_options graph regex)
include_directories(${Boost_INCLUDE_DIR})
add_subdirectory(Config)
add_subdirectory(Knight)

link_directories(${Boost_LIBRARY_DIRS})
add_executable (knight_travail knight_travail.cpp)
set (KNIGHT_TRAVAIL_LIBS 
    Knight
    ${Boost_LIBRARIES})
target_link_libraries(knight_travail ${KNIGHT_TRAVAIL_LIBS})

# Build and run unit tests
enable_testing()
find_package(GTest REQUIRED)
file(GLOB_RECURSE TEST_SRCS "../Tests/*_test.cpp")
link_directories(${GTEST_LIBRARY_DIR})
add_executable(unittest-all ${TEST_SRCS})
include_directories(${GTEST_INCLUDE_DIR})
target_link_libraries(unittest-all ${KNIGHT_TRAVAIL_LIBS} ${GTEST_BOTH_LIBRARIES})
gtest_add_tests(unittest-all ${TEST_SRCS})

# Make sure the build is out of source
string(COMPARE EQUAL "${CMAKE_SOURCE_DIR}" "${CMAKE_BINARY_DIR}" insource)
if(insource)
  file(REMOVE CMakeCache.txt)
  file(REMOVE CMakeFiles)
  message(FATAL_ERROR "Please don't build in the source dir.")
endif(insource)


