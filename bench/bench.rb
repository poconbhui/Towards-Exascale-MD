def bench(distribution_name, num_particles, num_reps, mpi_procs, openmp_per_mpi)
    distribution_name = distribution_name.to_s
    num_particles = num_particles.to_s
    num_reps = num_reps.to_s
    num_cores = num_cores.to_s

    run "bench #{distribution_name} #{num_particles} #{num_reps}",
      :mpi_procs => mpi_procs, :openmp_per_mpi => openmp_per_mpi
end
