#!/usr/bin/env gnuplot

outfile = system('echo $PWD/replicated_systolic_comm_fits.plt.tex')

set term epslatex color
set output outfile

load "../gnuplot_styles.plt"

set logscale x 2
set logscale y 10

set mxtics 2
set mytics 10

set format y '$10^{%T}$'

set xlabel "Cores"
set ylabel "Time (s)"

set key right bottom
set key tmargin
set key spacing 1.5

#
# Define the fitting functions
#
f512(x) = a512*512*(log(sqrt(x))+1)/sqrt(x) + b512*log(x)
f4096(x) = a4096*4096*(log(sqrt(x))+1)/sqrt(x) + b4096*log(x)
f32768(x) = a32768*32768*(log(sqrt(x))+1)/sqrt(x) + b32768*log(x)

#
# Fit the functions
#
set fit errorvariables

fit f512(x) "<./compile_data.sh v1/data/replicated_systolic.pair_operation.512.*.false.true.bench.dat" u 1:($3/$2) via a512, b512
fit f4096(x) "<./compile_data.sh v1/data/replicated_systolic.pair_operation.4096.*.false.true.bench.dat" u 1:($3/$2) via a4096, b4096
fit f32768(x) "<./compile_data.sh v1/data/replicated_systolic.pair_operation.32768.*.false.true.bench.dat" u 1:($3/$2) via a32768, b32768

f_title(n, a, a_err, b, b_err) = \
    '\footnotesize $\
        ('.gprintf('%.2t',a).'\times{} 10^{'.gprintf('%T',a).'} \
        \pm{} \
        '.gprintf('%.0t',a_err).'\times{}10^{'.gprintf('%T', a_err).'}) \
        \frac{'.sprintf('%d',n).' (\log{\sqrt{x}}+1)}{\sqrt{x}} \
        + \
        ('.gprintf('%.2t',b).'\times{} 10^{'.gprintf('%T',b).'} \
        \pm{} \
        '.gprintf('%.0t',b_err).'\times{}10^{'.gprintf('%T', b_err).'}) \
        \log{\sqrt{x}}$'


plot \
    "<./compile_data.sh v1/data/replicated_systolic.pair_operation.512.*.false.true.bench.dat" u 1:($3/$2) \
        title '$N = 512$' w lp ls 1, \
    "<./compile_data.sh v1/data/replicated_systolic.pair_operation.4096.*.false.true.bench.dat" u 1:($3/$2) \
        title '$N = 4096$' w lp ls 2, \
    "<./compile_data.sh v1/data/replicated_systolic.pair_operation.32768.*.false.true.bench.dat" u 1:($3/$2) \
        title '$N = 32768$' w lp ls 3, \
    f512(x) title f_title(512, a512, a512_err, b512, b512_err) ls 4, \
    f4096(x) title f_title(4096, a4096, a4096_err, b4096, b4096_err) ls 5, \
    f32768(x) title f_title(32768, a32768, a32768_err, b32768, b32768_err) ls 6
