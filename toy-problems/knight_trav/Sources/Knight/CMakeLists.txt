FILE(GLOB CPP_FILES "*.cpp")
include_directories(${Boost_INCLUDE_DIR})
add_library (Knight ${CPP_FILES})

