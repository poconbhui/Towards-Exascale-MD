#
# Expect including script to be of form
# distribution_name.bench_name.num_particles.graph_type
#

base_dir=$(dirname $(readlink -f $0))
script_name=$(basename $0)

info=( $(echo $script_name | sed 's/\./ /g') )

distribution_name=${info[0]}
bench_name=${info[1]}
num_particles=${info[2]}
graph_type=${info[3]}


data_prefix=$distribution_name.$bench_name.$num_particles

#
# Generate gnuplot file
#
echo Generating $base_dir/$data_prefix.plt.gnu

$base_dir/../gen_plotfile.sh \
    $base_dir/data/$data_prefix \
    $base_dir/$data_prefix \
    ${info[3]}
