def bench(distribution_name, bench_name, num_particles, num_reps, mpi_disabled, calculation_disabled, num_cores)
    distribution_name = distribution_name.to_s
    bench_name = bench_name.to_s
    num_particles = num_particles.to_s
    num_reps = num_reps.to_s
    num_cores = num_cores.to_s

    output = "#{distribution_name}.#{bench_name}.#{num_particles}.#{num_cores}.#{mpi_disabled}.#{calculation_disabled}.bench.dat"

    run "bench " \
      "#{distribution_name} #{bench_name} #{num_particles} #{num_reps} #{mpi_disabled} #{calculation_disabled} " \
      "> #{output}",
      :cores => num_cores
end
