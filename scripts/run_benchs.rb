#!/usr/bin/env ruby

#
# scripts/run_benchs.rb
#
# This program is used to provide a quick way of running sets of benchmarks
# as defined in bench/list_of_benches.rb
#
# It can be called as run_benchs.rb num_particles core_range distribution
#
# For a list of supported values, run run_benchs.rb with no arguments.
#
# Example usage:
#   run_benchs.rb 2^9 512 replicated
#

# Load the run function
require File.expand_path('./relpath', File.dirname(__FILE__))
require relpath('./mpiexec')
include MpiExec


# Add bench program directory to PATH
ENV["PATH"] = "#{ENV["PATH"]}:#{relpath('../bench/')}"


# Load then bench function
require relpath('../bench/bench.rb')


# Load the Benches variable, which should provide our benchmarks.
require relpath('../bench/list_of_benches.rb')


# Require the appropriate arguments have been passed
if ARGV.length < 3
    raise "\n\nUsage: run_benchs.rb num_particles core_range distribution\n" \
            "Particle ranges: #{Benches.keys.inspect}\n" \
            "Core ranges: #{Benches[Benches.keys[0]].keys.inspect}\n" \
            "Distributions: #{Distributions.inspect}\n"
end


# Set arguments to appropriate variables
@num_particles = ARGV[0].to_s
@core_range = ARGV[1].to_s
@distribution = ARGV[2].to_s


# Check @num_particles ok
benchmark = Benches[@num_particles]
if benchmark.nil?
    raise "Unsupported num_particles: #{@num_particles}"
end

# Check @core_range ok
benchmark = benchmark[@core_range]
if benchmark.nil?
    raise "Unsupported core_range: #{@core_range}"
end

# Check @distribution ok
benchmark = benchmark[@distribution]
if benchmark.nil?
    raise "Unsupported distribution: #{@distribution}"
end


# Run benchmark
benchmark.call
