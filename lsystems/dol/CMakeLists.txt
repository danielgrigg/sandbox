cmake_minimum_required(VERSION 2.6)
set(PROJ dol)
set(CMAKE_INSTALL_PREFIX ".." CACHE PATH "${PROJ} install prefix" FORCE)
project(${PROJ})
#link_directories(.)
add_definitions( -DBOOST_ALL_NO_LIB )
include_directories(.)
#source_group("${PROJ}" FILES ${PROJ}.cpp)
set(SOURCES)
set(SOURCES ${SOURCES} ${PROJ}.cpp)
add_executable(${PROJ} ${SOURCES})
target_link_libraries(${PROJ} general boost_regex) # optimized debug
target_link_libraries(${PROJ} lap) # optimized debug
install (TARGETS ${PROJ} DESTINATION bin)
