#!/usr/bin/env sh

#export FC=nvfortran

cmake -S . -B build

cmake --build build

cp build/main .
