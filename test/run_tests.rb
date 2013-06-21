#!/usr/bin/env ruby

#PBS -l mppwidth=32
#PBS -N bolt_par_job
#PBS -A d45
#PBS -l walltime=0:5:0

# Switch to current working directory
@PBS_O_WORKDIR = ENV["PBS_O_WORKDIR"] || Dir.pwd
Dir.chdir @PBS_O_WORKDIR

require '../scripts/mpiexec'
include MpiExec

# Set MPI environment
MpiExec.mpienv = "cray"

run "./domain_distribution_type_test", :cores => 4
run "./grav_force_test", :cores => 4
run "./integration_test", :cores => 4
run "./lj_force_test", :cores => 4
run "./particle_type_test", :cores => 4
run "./replicated_distribution_type_test", :cores => 4
run "./serial_distribution_type_test", :cores => 4
run "./systolic_distribution_type_test", :cores => 4
