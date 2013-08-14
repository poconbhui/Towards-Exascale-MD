#!/usr/bin/env bash



function gen_plotfile() {

    local data_base="$1"
    local output_base="$2"
    local graph_type="$3"

    local output_tex=$output_base.$graph_type.plt.tex
    local output_gnu=$output_base.$graph_type.plt.gnu


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
            echo plot \"$data_base.normal.dat\" using 1:'($3/$2)' title \"total time\" with lp ls 1, \\

            echo \"$data_base.nompi.dat\" using 1:'($3/$2)' title \"calculation only\" with lp ls 2, \\

            echo \"$data_base.onlympi.dat\" using 1:'($3/$2)' title \"mpi only\" with lp ls 3
        fi


        #
        # Print logarithmic time
        #
        if [ "$graph_type" = "logtime" ] ; then
            # Set log scales on both axes.
            echo set logscale y 10

            echo plot \"$data_base.normal.dat\" using 1:'($3/$2)' title \"total time\" with lp ls 1, \\

            echo \"$data_base.nompi.dat\" using 1:'($3/$2)' title \"calculation only\" with lp ls 2, \\

            echo \"$data_base.onlympi.dat\" using 1:'($3/$2)' title \"mpi only\" with lp ls 3

        fi

    fi >> $output_gnu


    #
    # Do speedup plots
    #

    if [ "$graph_type" = "speedup" ] || [ "$graph_type" = "logspeedup" ] ; then


        # Find lowest core time
        lowest_time=$(
            cat $data_base.normal.dat \
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
            echo plot \"$data_base.normal.dat\" using 1:'('$lowest_time'/($3/$2))' title \"total time\" with lp ls 1, \\

            echo \"$data_base.nompi.dat\" using 1:'('$lowest_time'/($3/$2))' title \"calculation only\" with lp ls 2, \\

            echo 'f(x) ti "f(x) = x" ls 4'
        fi


        #
        # Print logarithmic graphs
        #
        if [ "$graph_type" = "logspeedup" ] ; then

            # set logarithmix axes
            echo set logscale y 10

            echo plot \"$data_base.normal.dat\" using 1:'('$lowest_time'/($3/$2))' title \"total time\" with lp ls 1, \\

            echo \"$data_base.nompi.dat\" using 1:'('$lowest_time'/($3/$2))' title \"calculation only\" with lp ls 2, \\

            echo 'f(x) ti "f(x) = x" ls 4'
        fi

    fi >> $output_gnu

}
