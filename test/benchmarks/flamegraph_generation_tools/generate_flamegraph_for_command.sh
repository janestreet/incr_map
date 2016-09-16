#!/bin/bash

# USAGE: ./generate_flamegraph_for_command [perf_box] [executable_name] [args] > file.svg
perf_box_to_use=$1
executable=$2
shift 2
args=$@

X_LIBRARY_INLINING=true, WITH_FRAME_POINTERS=true jenga -pr >/dev/null 2>&1 &&
    scp $executable $perf_box_to_use:~/executable &&
    ssh -t $perf_box_to_use "perf record -g ./executable $args 2> /dev/null && flamegraph 2>/dev/null; rm perf.data; rm executable"
