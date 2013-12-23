set (mylib_src mylib.cpp)

add_library(mylib SHARED ${mylib_src})

install (TARGETS mylib DESTINATION bin)
install (FILES MyLib.hpp DESTINATION include)

