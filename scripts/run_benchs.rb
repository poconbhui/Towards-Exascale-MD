#!/usr/bin/env ruby

# Require bench to run is specified
raise "No bench type requested" if ARGV[0].nil?

@bench_name = ARGV[0].to_sym

require File.expand_path('./relpath', File.dirname(__FILE__))
require relpath('./mpiexec')
include MpiExec

# Add bench program directory to PATH
ENV["PATH"] = "#{ENV["PATH"]}:#{relpath('../bench/')}"

require relpath('../bench/list_of_benchs')

Benches[:strong_scaling][@bench_name].call
