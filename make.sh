#!/usr/bin/env sh

rm -rf build

MODE=${1:-serial}

if [ "$MODE" = "multicore" ] || [ "$MODE" = "gpu" ]; then
    export FC=nvfortran
fi

echo $MODE

cmake -DCMAKE_BUILD_TYPE=$MODE -S . -B build

cmake --build build

cp build/main .
