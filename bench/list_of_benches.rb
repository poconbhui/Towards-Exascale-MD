Distributions = ["replicated", "systolic"]
BenchNames = ["individual_operation", "full_calculation", "pair_operation"]
MinReps = 5


#
# Run all benches for the given number of particles and cores
#
def bench_for(num_particles, core_counts)
    core_counts.each do |num_cores|
        BenchNames.each do |bench_name|
            Distributions.each do |distribution_name|
                bench(
                    distribution_name,
                    bench_name,
                    num_particles,
                    MinReps,
                    false, false,
                    num_cores
                )
                bench(
                    distribution_name,
                    bench_name,
                    num_particles,
                    MinReps,
                    true, false,
                    num_cores
                )
                bench(
                    distribution_name,
                    bench_name,
                    num_particles,
                    MinReps,
                    false, true,
                    num_cores
                )
            end
        end
    end
end


#
# Return a hash of the core ranged benches for a given number of particles
#
def bench_list_for(num_particles)
    {
        "serial" => Proc.new do
            BenchNames.each do |bench_name|
                bench(
                  "serial", bench_name, num_particles,
                  MinReps, false, false, 1
                )
            end
        end,

        "1-64" => Proc.new do
            cores = (0..6).map{|i| 2**i} # 1-128 in powers of 2

            bench_for(num_particles, cores)
        end,

        "128-1024" => Proc.new do
            cores = (7..10).map{|i| 2**i} # 128-1024 in powers of 2

            bench_for(num_particles, cores)
        end
    }
end


Benches = {
    "2^9"  => bench_list_for(2**9),
    "2^12" => bench_list_for(2**12),
    "2^15" => bench_list_for(2**15),
    "2^18" => bench_list_for(2**18),
    "2^21" => bench_list_for(2**21),
    "2^24" => bench_list_for(2**24)
}
