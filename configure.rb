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
    o.banner =
        "Usage: configure.rb [--FC=<fortran compiler>] 
        [--MPIFC=<MPI fortran compiler>] [--mpiexec=<mpi execution program>]
        
        Configure the execution environment.
        Compile Makefile.in and scripts/exec.sh.in templates.
        
        After configuration, makefile may be used as normal, eg \"make all\".
        Scripts may be executed \"./scripts/exec.sh /path/to/script\".
        Alternatively, use \"source ./scripts/exec.sh\" to import
        configurations into current environment and simply execute
        scripts as \"/path/to/script\"."

    o.separator ""
    o.separator "Options:"

    o.on(
        '--FC=[gfortran, pgf90, crayftn]', [:gfortran, :pgf90, :crayftn],
        'FC option used to determine which flags',
        'to pass to MPIFC compiler.',
        'Only the listed compilers are supported.',
        'Default: gfortran.'
    ) {|fc| @FC=fc }

    o.separator ""

    o.on(
        '--MPIFC=MPIFC',
        'MPI fortran compiler to be used.',
        'Default: mpif90.'
    ) {|mpifc| @MPIFC=mpifc }

    o.separator ""

    o.on(
        '--mpiexec=[mpiexec, aprun]', [:mpiexec, :aprun],
        'MPI execution program to be used.',
        'Only listed execution programs supported.',
        'Default: mpiexec'
    ) {|mpiexec| @mpiexec=mpiexec }

    o.parse!
end


def compile_in(filename, make_executable=false)
    # Read infile.in
    infile = File.read("#{filename}.in")

    # Make replacements
    infile.gsub!('@FC@', @FC.to_s)
    infile.gsub!('@MPIFC@', @MPIFC.to_s)
    infile.gsub!('@mpiexec@', @mpiexec.to_s)

    # Write infile
    File.open("#{filename}", "w") do |f|
        f.write(infile)
    end

    if make_executable
        File.chmod(0755, filename)
    end
end

puts "Using:
    FC=#{@FC}
    MPIFC=#{@MPIFC}
    mpiexec=#{@mpiexec}"

compile_in("scripts/Makefile.inc")
compile_in("scripts/exec.sh", true)
