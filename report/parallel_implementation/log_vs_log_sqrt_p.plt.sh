#!/usr/bin/env bash

# Cat gnuplot file into this file with .sh -> .gnu
cat > $(echo $(readlink -f $0) | sed 's/\.sh$/.gnu/') <<EOF
#!/usr/bin/env gnuplot

set term epslatex color
set output "$PWD/log_vs_log_sqrt_p.plt.tex"

load "../gnuplot_styles.plt"

set logscale x 2
set logscale y 10

set mxtics 2
set mytics 10

set format y "\$10^{%T}$"

set xrange [1:32768]

plot \
    (log(sqrt(x))+1)/sqrt(x) \
        title "$\\\\frac{1}{\\\\sqrt{P}}\\\\log{\\\\sqrt{P}}$" \
        ls 1, \
    log(sqrt(x)) \
        title "$\\\\log{\\\\sqrt{P}}$" \
        ls 2

EOF
