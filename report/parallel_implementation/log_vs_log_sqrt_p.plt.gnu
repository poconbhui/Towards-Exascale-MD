#!/usr/bin/env gnuplot

set term epslatex color
set output system('echo $PWD/log_vs_log_sqrt_p.plt.tex')

load "../gnuplot_styles.plt"

set logscale x 2
set logscale y 10

set mxtics 2
set mytics 10

set format y "$10^{%T}$"

set xrange [1:32768]
set yrange [0.01:100]

set xlabel "x"
set ylabel "f(x)"

set multiplot layout 2,1


f(x) = (log(sqrt(x))+1)/sqrt(x)
g(x) = log(sqrt(x))

set key spacing 1.5

set key left top
set key width 1
set key height 0.5

plot \
    f(x) title "$\\frac{1}{\\sqrt{x}}\\log{\\sqrt{x}}$" ls 1, \
    g(x) title "$\\log{\\sqrt{x}}$" ls 2

set key right bottom
set key width 0
set key height 0
set key samplen 2
set key spacing 1.25

plot \
    0.1*f(x) + g(x) \
        title "\\scriptsize $0.1\\times{}\\frac{1}{\\sqrt{x}}\\log{\\sqrt{x}} + \\log{\\sqrt{x}}$" ls 1, \
    1*f(x) + g(x) \
        title "\\scriptsize $1\\times{}\\frac{1}{\\sqrt{x}}\\log{\\sqrt{x}} + \\log{\\sqrt{x}}$" ls 2, \
    10*f(x) + g(x) \
    title "\\scriptsize $10\\times{}\\frac{1}{\\sqrt{x}}\\log{\\sqrt{x}} + \\log{\\sqrt{x}}$" ls 3

unset multiplot

