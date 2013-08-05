#!/usr/bin/env bash

function filter_bench_dat() {
    sed 's/Application.*//' | sed 's/2\*\([^,]*\)/\1, \1/' \
    | grep -v '#' | sed 's/\ \+/\ /'g | grep -v '^$' \
    | awk '{print $4 " " $7 " " $8}' | sort -n
}


#
# gen_data base dist bench num_particles
#
function compile_data() {
  #
  # base = /path/to/data
  #
  local base=$1

  #
  # dist: [replicated, systolic]
  #
  local dist=$2

  #
  # bench: [full_calculation, individual_operation, pair_operation]
  #
  local bench=$3

  #
  # num_particles: [512, 4096, 32768]
  #
  local num_particles=$4

  cat $base/$dist.$bench.$num_particles.*.false.false.bench.dat | filter_bench_dat > $base/$dist.$bench.$num_particles.normal.dat

  cat $base/$dist.$bench.$num_particles.*.true.false.bench.dat | filter_bench_dat > $base/$dist.$bench.$num_particles.nompi.dat

  cat $base/$dist.$bench.$num_particles.*.false.true.bench.dat | filter_bench_dat > $base/$dist.$bench.$num_particles.onlympi.dat

}
