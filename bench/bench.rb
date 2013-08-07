def bench(distribution_name, num_particles, num_reps, num_cores)
    distribution_name = distribution_name.to_s
    num_particles = num_particles.to_s
    num_reps = num_reps.to_s
    num_cores = num_cores.to_s

    run "bench #{distribution_name} #{num_particles} #{num_reps}",
      :cores => num_cores
end
