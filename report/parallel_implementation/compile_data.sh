#!/usr/bin/env bash

function filter_bench_dat() {
    sed 's/Application.*//' | sed 's/2\*\([^,]*\)/\1, \1/g' \
    | grep -v '#' | sed 's/\ \+/\ /'g | grep -v '^$' \
    | awk '{print $4 " " $7 " " $8}' | sort -n
}


#
# gen_data base dist bench num_particles
#
function compile_data() {
  #
  # File pattern = files to process
  #
  local file_pattern=$@

  cat $file_pattern | filter_bench_dat

}

compile_data $@
