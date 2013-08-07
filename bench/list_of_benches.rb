Distributions = ["replicated", "systolic"]
MinReps = 5


#
# Run all benches for the given number of particles and cores
#
def bench_for(num_particles, core_counts)

    Hash[ Distributions.map do |distribution_name|

        [ distribution_name, Proc.new do

            core_counts.each do |num_cores|
                bench(
                    distribution_name,
                    num_particles,
                    MinReps,
                    num_cores
                )
            end

        end ]

    end ]

end


#
# Return a hash of the core ranged benches for a given number of particles
#
def bench_list_for(num_particles)
    {
        "1" => {
            "serial" => Proc.new do
                BenchNames.each do |bench_name|
                    bench(
                      "serial", bench_name, num_particles,
                      MinReps, false, false, 1
                    )
                end
            end
        },

        # 1-64 in powers of 2
        "1-64" => bench_for(num_particles, (0..6).map{|i| 2**i}),

        # 128-1024 in powers of 2
        "128-1024" => bench_for(num_particles, (7..10).map{|i| 2**i}),

        # 2048-16384 in powers of 2
        "2048-16384" => bench_for(num_particles, (11..14).map{|i| 2**i})
    }
end


Benches = {
    "2^9"  => bench_list_for(2**9),
    "2^12" => bench_list_for(2**12),
    "2^15" => bench_list_for(2**15)
}
