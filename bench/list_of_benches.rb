# bench/list_of_benches.rb
#
# This script provides the Benches variable which is a 3d hash which
# organises benchmarks that can be run for different system particle
# counts, core ranges and distribution types.
#
# The Benches variable is expected to be used as
#   Benches[num_particles][core_range][distribution_type].call
#
# This is expected to be called in script/run_benchs.rb.
# It requires bench/bench.rb to be loaded before it is loaded.
#


# A list of the distributions allowed by bench.
Distributions = ["replicated", "systolic", "shared_and_replicated"]

# The minimum reps a distribution is expected to complete.
MinReps = 5


# DEF bench_for
#
# Return a hash from a Distribution to a Proc which loops over the given
# core_counts and runs bench with num_particles and each core count.
#
def bench_for(num_particles, core_counts)

    # Create Hash to return using the constructor
    # Hash[ [ [a,b], [c,d] ] ] -> {a=>b, c=>d}
    Hash[ Distributions.map do |distribution_name|

        # Create array [distribution_name, proc]
        [ distribution_name, Proc.new do

            # Make special case for shared_and_replicated distribution
            # which uses OMP_NUM_THREADS > 1.
            if distribution_name == "shared_and_replicated"
                core_counts.each do |num_cores|
                    if num_cores >= 8
                        bench(
                            distribution_name,
                            num_particles,
                            MinReps,
                            num_cores/8,
                            8
                        )
                    end
                end
            else
                core_counts.each do |num_cores|
                    bench(
                        distribution_name,
                        num_particles,
                        MinReps,
                        num_cores,
                        1
                    )
                end
            end

        end ]

    end ]

end


# DEF bench_list_for
#
# Return a Hash of the form
#   h[core_range][distribution]
# where the bench called will be set to the given num_particles
#
def bench_list_for(num_particles)
    {
        "1" => {
            "serial" => Proc.new do
                bench(
                  "serial", num_particles,
                  MinReps, 1, 1
                )
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


# Benches
#
# Generate the Benches hash which contains all allowed combinations
# for benchmarking.
#
Benches = {
    "2^9"  => bench_list_for(2**9),
    "2^12" => bench_list_for(2**12),
    "2^15" => bench_list_for(2**15)
}
