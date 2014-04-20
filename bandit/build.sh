#!/usr/bin/env bash

set -e
set -u

ROOT=$PWD
[[ -d downloads ]] || mkdir downloads

cd downloads
if [[ ! -d cotire ]]; then
  git clone https://github.com/sakra/cotire.git cotire
fi

cd cotire
git checkout -f master
cd $ROOT
[[ -d cmake ]] || mkdir cmake
cp downloads/cotire/CMake/cotire.cmake cmake

[[ -d build ]] || mkdir build
cd build
GENERATOR_OPT=""
if [[ $# > 1 ]]; then
  GENERATOR_OPT="-G $1"
fi
cmake "$GENERATOR_OPT" ..
