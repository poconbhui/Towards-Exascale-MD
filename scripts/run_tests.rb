#!/usr/bin/env ruby

#
# scripts/run_tests.rb
#
# Run all the tests specified in test/list_of_tests.rb
#


# Hector qsub parameters
#PBS -l mppwidth=32
#PBS -N run_tests
#PBS -A d45
#PBS -l walltime=0:5:0


# Load relpath function
require File.expand_path('relpath', File.dirname(__FILE__))

# Load MpiExec module to get run function
require relpath('./mpiexec')

# Import run function into current scope
include MpiExec


# Add test program directory to PATH
ENV["PATH"] = "#{ENV["PATH"]}:#{relpath('../test')}"

# Execute requested tests
require relpath('../test/list_of_tests')
