#!/usr/bin/env ruby

# ./configure.rb
#
# Example usage:
#
#   # For help
#   ./configure.rb --help
#
#   # Configure for use on HECToR
#   ./configure.rb --FC=crayftn --MPIFC=ftn --MPIEXEC=aprun
#
#   # Configure for use in a less exotic environment
#   ./configure.rb --FC=gfortran --MPIFC=mpif90 --MPIEXEC=mpiexec
#   ./configure.rb
#
# This script is use to configure some scripts in the scripts folder
# including mpiexec.rb and Makefile.inc that are used for building,
# testing and benchmarking the project.
#


# Include OptionParser
require 'optparse'


#
# Define defaults
#
@FC = "gfortran"
@MPIFC = "mpif90"
@MPIEXEC = "mpiexec"


#
# Get command line arguments
#
OptionParser.new do |o|

    #
    # Help banner. Use ./configure.rb --help to see this.
    #
    o.banner =
        "Usage: configure.rb [--FC=<fortran compiler>] 
        [--MPIFC=<MPI fortran compiler>] [--MPIEXEC=<mpi execution program>]
        
        Configure the execution environment.
        Compile scripts/Makefile.inc.in and scripts/mpiexec.rb.in templates.
        
        After configuration, makefile may be used as normal, eg \"make all\".
        Scripts should be usable from any directory, eg \"/path/to/script.rb\"."


    o.separator ""
    o.separator "Options:"


    # Parse --FC option
    o.on(
        '--FC=[gfortran, pgf90, crayftn]', [:gfortran, :pgf90, :crayftn],
        'FC option used to determine which flags',
        'to pass to MPIFC compiler.',
        'Only the listed compilers are supported.',
        'Default: gfortran.'
    ) {|fc| @FC=fc }

    o.separator ""


    # Parse --MPIFC option
    o.on(
        '--MPIFC=MPIFC',
        'MPI fortran compiler to be used.',
        'Default: mpif90.'
    ) {|mpifc| @MPIFC=mpifc }

    o.separator ""


    # Parse --MPIEXEC option
    o.on(
        '--MPIEXEC=[mpiexec, aprun]', [:mpiexec, :aprun],
        'MPI execution program to be used.',
        'Only listed execution programs supported.',
        'Default: mpiexec'
    ) {|mpiexec| @MPIEXEC=mpiexec }

    o.parse!
end


# DEF compile_in
#
# Example usage:
#   # Compile my_script.rb.in to my_script.rb
#   compile_in("my_script.rb")
#
#   # Compile my_script.rb.in to my_script.rb and make
#   # my_script.rb executable
#   compile_in("my_script.rb", true)
#
# Compile *.in templates, replacing @FC@, @MPIFC@ and @MPIEXEC@
# strings found in the templates with the values passed into
# this program.
#
# This also replaces the @PROJECT_ROOT@ string with the absolute
# path of the project root.
#
def compile_in(filename, make_executable=false)
    # Read infile.in
    infile = File.read("#{filename}.in")

    # Find project root (the directory this script is in)
    @PROJECT_ROOT = File.expand_path File.dirname(__FILE__)

    # Make replacements
    infile.gsub!('@FC@', @FC.to_s)
    infile.gsub!('@MPIFC@', @MPIFC.to_s)
    infile.gsub!('@MPIEXEC@', @MPIEXEC.to_s)
    infile.gsub!('@PROJECT_ROOT@', @PROJECT_ROOT.to_s)

    # Write infile
    File.open("#{filename}", "w") do |f|
        f.write(infile)
    end

    if make_executable
        File.chmod(0755, filename)
    end
end


#
# Output current configuration to screen
#
puts "Using:
    FC=#{@FC}
    MPIFC=#{@MPIFC}
    MPIEXEC=#{@MPIEXEC}"


#
# Compile the required template files
#
compile_in("scripts/Makefile.inc")
compile_in("scripts/mpiexec.rb")
compile_in("scripts/run_tests.qsub.sh")
