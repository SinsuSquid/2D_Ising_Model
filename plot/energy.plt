set xlabel "Time"
set ylabel "Energy"
set grid

plot "./energy.dat" u 1:2 w l lc "#88B04B" t ""

