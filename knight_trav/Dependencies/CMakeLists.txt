cmake_minimum_required (VERSION 2.8)
include(ExternalProject)
find_package(GTest QUIET)
if (NOT GTest_FOUND)
ExternalProject_Add(
  Gtest-1.6.0
  URL http://googletest.googlecode.com/files/gtest-1.6.0.zip
  URL_MD5 4577b49f2973c90bf9ba69aa8166b786
  DOWNLOAD_DIR ${PROJECT_SOURCE_DIR}/Downloads
  PATCH_COMMAND patch < ${PROJECT_SOURCE_DIR}/gtest-1.6.0.patch
  CMAKE_ARGS -DCMAKE_INSTALL_PREFIX:PATH=${KNIGHT_TRAVAIL_BINARY_DIR})
endif(NOT GTest_FOUND)
