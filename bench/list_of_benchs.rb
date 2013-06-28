strong_block = Proc.new do |name|
  core_counts = [1, 2, 4, 8, 16, 32].reverse
  core_counts.each do |num_cores|
    num_particles = 10**5
    output = "#{name.to_s}_bench_#{num_particles}_#{num_cores}.out"

    run "calculation_only.bench " \
      "#{name.to_s} #{num_particles} 1 > #{output}",
      :cores => num_cores
  end
end

Benches = {
  :strong_scaling => strong_block
}
