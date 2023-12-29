# 2D Ising Model
2D Ising Model Monte-Carlo Simulation for R1215's Summer/Winter School

# How to Use
- Start a simulation to draw magnetization vs. temperature curve (edit parameters.in)
```
make
make run # this command will execute simulation with nohup!
gnuplot plot/plot.plt
```

- Start a simulation to track magnetization & energy during equilibration with specific temperature (edit dump.in)
```
make
./bin/dump
gnuplot -p plot/energy.plt
gnuplot -p plot/magnetization.plt
```

# If you're not teacher but reading this ...
- Hey, you might not be able to learn anything if you just copy & paste.
  - If you have any question, just ask teacher right next to you.

# If you're teacher
- If you have any question, DO NOT EVEN TRY TO REACH ME AFTER MY GRADUATION.
  - Edit this codebase as you want.
