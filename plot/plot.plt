set xlabel "Temperature"
set ylabel "Magnetization"
set grid

set xrange [1:4]
set yrange [0:1]

while(1){
	plot "./plot.dat" u 1:(abs($2)) w l lc "#BE3455" t ""
	pause 3
	}

