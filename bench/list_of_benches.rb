Distributions = ["replicated", "systolic"]
BenchNames = ["full_calculation", "individual_operation", "pair_operation"]
MinReps = 5


#
# Run all benches for the given number of particles and cores
#
def bench_for(num_particles, core_counts)
    Distributions.each do |distribution_name|
        BenchNames.each do |bench_name|
            core_counts.each do |num_cores|
                bench(
                    distribution_name,
                    bench_name,
                    num_particles,
                    MinReps,
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
                bench("serial", bench_name, num_particles, MinReps, 1)
            end
        end,

        "1-64" => Proc.new do
            cores = (0..6).map{|i| 2**i} # 1-128 in powers of 2

            bench_for(num_particles, cores)
        end,

        "128-512" => Proc.new do
            cores = (7..9).map{|i| 2**i} # 128-512 in powers of 2
            num_particles = 2**12

            bench_for(num_particles, cores)
        end,

        "1024-8192" => Proc.new do
            cores = (10..13).map{|i| 2**i} # 1024-8192 in powers of 2
            num_particles = 2**12

            bench_for(num_particles, cores)
        end,

        "16384" => Proc.new do
            cores = (14..14).map{|i| 2**i} # 16384-16384 in powers of 2

            bench_for(num_particles, cores)
        end
    }
end


Benches = {
    "2^12" => bench_list_for(2**12),
    "2^15" => bench_list_for(2**15),
    "2^18" => bench_list_for(2**18),
    "2^21" => bench_list_for(2**21),
    "2^24" => bench_list_for(2**24)
}
