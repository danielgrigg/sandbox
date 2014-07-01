set (runner_src runner.cpp)
add_executable(runner ${runner_src})
target_link_libraries(runner mylib)

install (TARGETS runner DESTINATION bin)

