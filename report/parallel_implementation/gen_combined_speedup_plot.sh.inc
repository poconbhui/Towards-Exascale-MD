#!/usr/bin/env bash


base_dir=$(dirname $(readlink -f $0))
script_name=$(basename $0)

info=( $(echo $script_name | sed 's/\./ /g') )

distribution_name=${info[0]}
bench_name=${info[1]}

data_prefix=$distribution_name.$bench_name


system_sizes="512 4096 32768"


data_base=$base_dir/data/$data_prefix
output_base=$base_dir/$data_prefix.logspeedup

output_tex=$output_base.plt.tex
output_gnu=$output_base.plt.gnu

function compile_N() {
    local N=$1
    echo $base_dir/../compile_data.sh $data_base.$N.\*.false.false.bench.dat
}


# Print stuff common to all plots
{
    echo '#!/usr/bin/env gnuplot'

    #
    # Set output terminal and files
    #
    echo set term epslatex color
    echo set output \"$output_tex\"

    #
    # Set tick styles
    #
    echo set logscale x 2
    echo set mxtics 2

    echo set logscale y
    echo set mytics 10

    echo 'set format y "$10^{%T}$"'

    echo 'set key left top'

    # Import common gnuplot styles
    echo 'load "'$base_dir'/../../gnuplot_styles.plt"'


    echo 'set ylabel "Speedup ($t(1)/t(P)$)"'
    echo 'set xlabel "Cores"'


    # Find lowest core time N
    function lowest_time_N() {
        local N="$1"

        $(compile_N $N) \
            | head -n 1 \
            | awk '{print $1*$3/$2}'
    }


    echo 'plot "<'$(compile_N 512)'" using 1:('$(lowest_time_N 512)'/($3/$2)) title "N=512" w lp ls 1, '\\
    echo '"<'$(compile_N 4096)'" using 1:('$(lowest_time_N 4096)'/($3/$2)) title "N=4096" w lp ls 2, '\\
    echo '"<'$(compile_N 32768)'" using 1:('$(lowest_time_N 32768)'/($3/$2)) title "N=32768" w lp ls 3, '\\
    echo 'x title "f(x) = x" ls 4'

} > $output_gnu
