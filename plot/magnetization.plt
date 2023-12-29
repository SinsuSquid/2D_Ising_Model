set xlabel "Time"
set ylabel "Magnetization"
set grid

plot "./magnetization.dat" u 1:2 w l lc "#6868AB" t ""

