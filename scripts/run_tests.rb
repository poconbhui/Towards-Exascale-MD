#!/usr/bin/env ruby

require File.expand_path('relpath', File.dirname(__FILE__))
require relpath('./mpiexec')
include MpiExec

# Add test program directory to PATH
ENV["PATH"] = "#{ENV["PATH"]}:#{relpath('../test')}"

require relpath('../test/list_of_tests')
