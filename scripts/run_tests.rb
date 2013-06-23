#!/usr/bin/env ruby

#PBS -l mppwidth=32
#PBS -N bolt_par_job
#PBS -A d45
#PBS -l walltime=0:5:0

# Switch to current working directory
@PBS_O_WORKDIR = ENV["PBS_O_WORKDIR"] || Dir.pwd
Dir.chdir @PBS_O_WORKDIR

require File.expand_path('relpath', File.dirname(__FILE__))

require relpath('./mpiexec')

# Add test program directory to PATH
ENV["PATH"] = "#{ENV["PATH"]}:#{relpath('../test')}"

puts `echo PATH: $PATH`

include MpiExec
require relpath('../test/list_of_tests')
