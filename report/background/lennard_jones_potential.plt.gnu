#!/usr/bin/env gnuplot

set term epslatex color
set output "background/lennard_jones_potential.plt.tex"

load "gnuplot_styles.plt"

epsilon = 1
delta   = 1

V(r) = 4*epsilon*( (delta/r)**12 - (delta/r)**6 )

set xrange [0:2.5]
set yrange [-1.5:2]

set ylabel 'V(r)/$\epsilon$'
set xlabel 'r/$\delta$'

plot \
        0 notitle ls 2, \
        -epsilon notitle ls 3, \
        '-' ls 4 noti with lines, \
        V(x) title '$V_{LJ}(r)/\epsilon$' ls 1 
1 -1.5
1 2
e
