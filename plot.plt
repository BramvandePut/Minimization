# plot.plt

set pm3d map
set dgrid3d 100,100 qnorm 2

set xlabel "x"
set ylabel "y"
set zlabel "z"

m="data.txt"

set terminal x11 0

set title "SurfaceMap"

splot m with pm3d