# contour.plt

set view map
set isosample 40,40

unset key
set xlabel "x"
set ylabel "y"
set zlabel "z"
set autoscale

m="data.txt"

set terminal x11 0

splot m with pm3d
