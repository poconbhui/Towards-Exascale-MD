run "particle_type.test", :cores => 4

run "grav_force.test"
run "integration.test"
run "lj_force.test"

run "serial_distribution_type.test", :mpi_procs => 1
run "replicated_distribution_type.test", :mpi_procs => 4
run "systolic_distribution_type.test", :mpi_procs => 4
run "shared_and_replicated_distribution_type.test",
    :mpi_procs=>4, :openmp_per_mpi=>8
