#!/usr/bin/env bash


base_dir=$(dirname $(readlink -f $0))
script_name=$(basename $0)

info=( $(echo $script_name | sed 's/\./ /g') )

distribution_name=${info[0]}
bench_name=${info[1]}
num_particles=${info[2]}
graph_type=${info[3]}


data_prefix=$distribution_name.$bench_name.$num_particles




function gen_plotfile() {

    local data_base="$1"
    local output_base="$2"
    local graph_type="$3"

    local output_tex=$output_base.$graph_type.plt.tex
    local output_gnu=$output_base.$graph_type.plt.gnu

    local compile_normal=$(echo $base_dir/../compile_data.sh $data_base.\*.false.false.bench.dat)
    local compile_calc_only=$(echo $base_dir/../compile_data.sh $data_base.\*.true.false.bench.dat)
    local compile_mpi_only=$(echo $base_dir/../compile_data.sh $data_base.\*.false.true.bench.dat)


    # Print stuff common to all plots
    {
        echo '#!/usr/bin/env gnuplot'

        #
        # Set output terminal and files
        #
        echo set term epslatex color
        echo set output \"$output_tex\"
        #echo set term pngcairo enhanced
        #echo set output \"$output_base.$graph_type.plt.png\"
        #echo set term dumb

        #
        # Set tick styles
        #
        echo set logscale x 2
        echo set mxtics 2

        echo 'set format y "$10^{%L}$"'

        # Import common gnuplot styles
        cat ../gnuplot_styles.plt

    } > $output_gnu


    #
    # Do time plots
    #
    if [ "$graph_type" = "time" ] || [ "$graph_type" = "logtime" ] ; then

        #
        # Generate time plot
        #
        echo set xlabel \"Cores\"
        echo 'set ylabel "Time (s)"'


        #
        # Print linear time
        #
        if [ "$graph_type" = "time" ] ; then
            echo 'plot "<'$compile_normal'" using 1:($3/$2) title "total time" with lp ls 1, '\\

            echo '"<'$compile_calc_only'" using 1:($3/$2) title "calculation only" with lp ls 2, '\\

            echo '"<'$compile_mpi_only'" using 1:($3/$2) title "mpi only" with lp ls 3'
        fi


        #
        # Print logarithmic time
        #
        if [ "$graph_type" = "logtime" ] ; then
            # Set log scales on both axes.
            echo set logscale y 10

            echo 'plot "<'$compile_normal'" using 1:($3/$2) title "total time" with lp ls 1, '\\

            echo '"<'$compile_calc_only'" using 1:($3/$2) title "calculation only" with lp ls 2, '\\

            echo '"<'$compile_mpi_only'" using 1:($3/$2) title "mpi only" with lp ls 3'

        fi

    fi >> $output_gnu


    #
    # Do speedup plots
    #

    if [ "$graph_type" = "speedup" ] || [ "$graph_type" = "logspeedup" ] ; then


        # Find lowest core time
        lowest_time=$(
            $compile_normal \
            | head -n 1 \
            | awk '{print $1*$3/$2}'
        )


        echo set xlabel \"Cores\"
        echo 'set ylabel "Speedup (s(1)/s(p))"'

        echo 'f(x) = x'


        #
        # Print linear speedup graph
        #
        if [ "$graph_type" = "speedup" ] ; then
            echo 'plot "<'$compile_normal'" using 1:('$lowest_time'/($3/$2)) title "total time" with lp ls 1, '\\

            echo '"<'$compile_calc_only'" using 1:('$lowest_time'/($3/$2)) title "calculation only" with lp ls 2, '\\

            echo 'f(x) ti "f(x) = x" ls 4'
        fi


        #
        # Print logarithmic graphs
        #
        if [ "$graph_type" = "logspeedup" ] ; then

            # set logarithmix axes
            echo set logscale y 10

            echo 'plot "<'$compile_normal'" using 1:('$lowest_time'/($3/$2)) title "total time" with lp ls 1, '\\

            echo '"<'$compile_calc_only'" using 1:('$lowest_time'/($3/$2)) title "calculation only" with lp ls 2, '\\

            echo 'f(x) ti "f(x) = x" ls 4'
        fi

    fi >> $output_gnu

}


gen_plotfile \
    $base_dir/data/$data_prefix \
    $base_dir/$data_prefix \
    ${info[3]}
