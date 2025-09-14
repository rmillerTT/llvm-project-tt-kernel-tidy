#!/bin/bash
cd "$(dirname "$0")"
mkdir ../build
cd ../build
cmake -G Ninja -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra" ../llvm
ninja clang-tidy clangd
