#!/usr/bin/env ruby

require 'optparse'

#
# Define defaults
#
@FC = "gfortran"
@MPIFC = "mpif90"
@mpiexec = "mpiexec"

#
# Get command line arguments
#
OptionParser.new do |o|
    o.on('--FC=FC') {|fc| @FC=fc }
    o.on('--MPIFC=MPIFC') {|mpifc| @MPIFC=mpifc }
    o.on('--mpiexec=mpiexec') {|mpiexec| @mpiexec=mpiexec }
    o.parse!
end


def compile_in(filename, make_executable=false)
    # Read infile.in
    infile = File.read("#{filename}.in")

    # Make replacements
    infile.gsub!('@FC@', @FC)
    infile.gsub!('@MPIFC@', @MPIFC)
    infile.gsub!('@mpiexec@', @mpiexec)

    # Write infile
    File.open("#{filename}", "w") do |f|
        f.write(infile)
    end

    if make_executable
        File.chmod(0755, filename)
    end
end

compile_in("Makefile")
compile_in("scripts/exec.sh", true)
