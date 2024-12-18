#!/bin/bash

function debug() {
    if [ $VERBOSE ]; then
        echo $1
    fi
}

function run_cadna {
    debug "Running CADNA ${1} ..."
    ./harmonic_cadna-${1} $2 | grep "RESULT"
}

function run_sr {
    debug "Running SR ${1}..."
    ./harmonic_sr-${1} $2
}

function run_verificarlo {
    debug "Running Verificarlo $1 ..."
    VFC_BACKENDS_LOGGER=False VFC_BACKENDS="libinterflop_mca_int.so" ./harmonic_verificarlo-$1 $2
}

function run_verrou_sr {
    debug "Running Verrou-SR $1 ..."
    valgrind --tool=verrou --rounding-mode=average -- ./harmonic-${1} $2 2>/dev/null
}

function run_verrou_cestac {
    debug "Running Verrou-CESTAC $1 ..."
    valgrind --tool=verrou --rounding-mode=random -- ./harmonic-${1} $2 2>/dev/null
}

function run_prism_sr {
    debug "Running PRISM-SR $1 ..."
    ./harmonic_prism_sr-${1} $2
}

function run_prism_ud {
    debug "Running PRISM-UD $1 ..."
    ./harmonic_prism_ud-${1} $2
}

function run_baseline_float {
    debug "Running Baseline $1 float..."
    ./harmonic-${1} $2
}

function run_baseline_double {
    debug "Running Baseline double..."
    ./harmonic_double-${1} $2
}

function run {
    TOOL=$1
    MODE=$2
    ITERATIONS=$3

    debug "Launching ${TOOL} ${MODE} ${ITERATIONS} ..."

    for i in {1..3}; do
        run_${TOOL} ${MODE} ${ITERATIONS} >>.result/${TOOL}-${MODE}-${ITERATIONS}.txt
    done
}

export -f run debug \
    run_cadna run_sr run_verificarlo \
    run_prism_sr run_prism_ud \
    run_baseline_float run_baseline_double \
    run_verrou_sr run_verrou_cestac

SIZES=$(python3 -c 'import numpy as np ; print(" ".join(map(lambda x : str(int(x)),np.logspace(2, 7, 100).tolist())))')

echo "Running Harmonic with different tools and modes..."

if [ $VERBOSE ]; then
    PROGRESS=""
else
    PROGRESS="--progress"
fi

rm -rf .result
mkdir -p .result

parallel --halt now,fail=1 ${PROGRESS} run \
    ::: cadna sr verificarlo prism_sr prism_ud baseline_float baseline_double verrou_sr verrou_cestac \
    ::: dbg perf \
    ::: $SIZES
