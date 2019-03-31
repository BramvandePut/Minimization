# plot.plt

set pm3d at b
set ticslevel 2
set hidden3d
set isosample 10,10

set xlabel "x"
set ylabel "y"
set zlabel "z"

m="data.txt"
v="Vectors.txt"

set terminal x11 0

set title "SurfaceMap"

splot m with lines, v using 1:2:(5):3:4:(0) with vectors lw 2 lt -1

