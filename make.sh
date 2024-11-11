#!/usr/bin/env sh

cmake -S . -B build

cmake --build build

cp build/main .
