#!/usr/bin/env ruby


#
# scripts/run_tests.rb
#
# Run all the tests specified in test/list_of_tests.rb
#


# HECToR qsub parameters
#
# This script can be submitted with "qsub ./scripts/run_tests.rb"
#
#PBS -l mppwidth=32
#PBS -N run_tests
#PBS -A d45
#PBS -l walltime=0:5:0
#


# Morar qsub parameters
#
# Unfortunately, Morar will try to execute this script
# with sh instead of ruby, so it can't be submitted
# as "qsub ./scripts/run_tests.rb", which would be nice.
#
# Instead, the parameters below should be supplied to qsub
# on the command line along with the "-b y" flag.
#
#$ -V
#$ -l h_rt=:5:
#$ -pe mpi 32
#$ -cwd
#


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
