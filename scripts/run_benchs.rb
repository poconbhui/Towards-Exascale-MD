#!/usr/bin/env ruby

require File.expand_path('./relpath', File.dirname(__FILE__))
require relpath('./mpiexec')
include MpiExec


# Add bench program directory to PATH
ENV["PATH"] = "#{ENV["PATH"]}:#{relpath('../bench/')}"

require relpath('../bench/bench.rb')
require relpath('../bench/list_of_benches.rb')


# Require bench to run is specified
if ARGV[0].nil? or ARGV[1].nil?
    raise "\n\nUsage: run_benchs.rb num_particles core_range\n" \
            "Particle ranges: #{Benches.keys.inspect}\n" \
            "Core ranges: #{Benches[Benches.keys[0]].keys.inspect}\n" \
            "Distributions: #{Distributions.inspect}\n"
end

@num_particles = ARGV[0].to_s
@core_range = ARGV[1].to_s
@distribution = ARGV[2].to_s


Benches[@num_particles][@core_range][@distribution].call
