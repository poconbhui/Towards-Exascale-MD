# test/list_of_tests.rb
#
# Run all the tests here as part of the test suite.
#
# This is expected to be run by scripts/run_tests.rb
#
# The run command can be found in scripts/mpiexec.rb
#

run "particle_type.test", :mpi_procs => 4

run "grav_force.test"
run "integration.test"
run "lj_force.test"

run "serial_distribution_type.test", :mpi_procs => 1
run "replicated_distribution_type.test", :mpi_procs => 4
run "systolic_distribution_type.test", :mpi_procs => 4
run "shared_and_replicated_distribution_type.test",
    :mpi_procs=>4, :openmp_per_mpi=>8
