# contour.plt

set view map
set isosample 40,40

unset key
set xlabel "x"
set ylabel "y"
set zlabel "z"
set autoscale

m="data.txt"
v="Vectors.txt"

set terminal x11 0

splot m with pm3d, v using 1:2:(5):3:4:(0) with vectors lw 2 lt -1
