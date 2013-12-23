cmake_minimum_required(VERSION 2.6)
set(PROJ bullethello)
set(CMAKE_INSTALL_PREFIX ".." CACHE PATH "${PROJ} install prefix" FORCE)
project(${PROJ})
#link_directories(.)
add_definitions( -DBOOST_ALL_NO_LIB )
include_directories(.)
include_directories(/usr/local/include/bullet)
#source_group("${PROJ}" FILES ${PROJ}.cpp)
set(SOURCES)
set(SOURCES ${SOURCES} ${PROJ}.cpp)
add_executable(${PROJ} ${SOURCES})
#target_link_libraries(${PROJ} general boost_system) # optimized debug
target_link_libraries(${PROJ} general BulletDynamics) # optimized debug
target_link_libraries(${PROJ} general LinearMath) # optimized debug
target_link_libraries(${PROJ} general BulletCollision) # optimized debug
install (TARGETS ${PROJ} DESTINATION bin)
