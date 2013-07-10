#!/usr/bin/env gnuplot

set term epslatex color
set output "background/lennard_jones_potential.plt.tex"

epsilon = 1
delta   = 1

V(r) = 4*epsilon*( (delta/r)**12 - (delta/r)**6 )

set xrange [0:2.5]
set yrange [-1.5:2]

set ylabel 'V(r)/$\epsilon$'
set xlabel 'r/$\delta$'

set arrow from 1,graph(0,0) to 1,graph(1,1) nohead
plot V(x) title '$V_{LJ}(r)/\epsilon$' lw 2, 0 notitle, -epsilon notitle
