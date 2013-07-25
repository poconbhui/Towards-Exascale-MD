def bench(distribution_name, bench_name, num_particles, num_reps, num_cores)
    distribution_name = distribution_name.to_s
    bench_name = bench_name.to_s
    num_particles = num_particles.to_s
    num_reps = num_reps.to_s
    num_cores = num_cores.to_s

    output = "#{distribution_name}.#{bench_name}.#{num_particles}.#{num_cores}.bench.dat"

    run "bench " \
      "#{distribution_name} #{bench_name} #{num_particles} #{num_reps} " \
      "> #{output}",
      :cores => num_cores
end
