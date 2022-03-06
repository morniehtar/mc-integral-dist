#! /usr/bin/gnuplot

set term qt font "arial,12" enhanced #persist
set encoding utf8
set tmargin 1
set tics out
set grid
set nokey

plot "./mkintegr.dat" using 1:2 with points pt 1 lt rgb "#2C9CCD", \
"./solution.dat" using 1:2 smooth csplines lt rgb "red" linewidth 1.6

pause mouse close

