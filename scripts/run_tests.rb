#!/usr/bin/env ruby

#
# scripts/run_tests.rb
#
# Run all the tests specified in test/list_of_tests.rb
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
