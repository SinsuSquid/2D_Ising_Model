FC = ifort

all : main

main : 
	rm -f ./dump/*
	rm -f ./plot.dat
	rm -f ./magnetization.dat
	rm -f ./energy.dat
	rm -f ./main.out
	rm -f ./dump.out

	$(FC) ./src/global_variables.f90 \
	      ./src/silent_ranlux.f \
	      ./src/initialize.f90 \
	      ./src/tools.f90 \
	      ./src/core.f90 \
	      ./src/run.f90 \
	      ./src/main.f90 \
	      \
	      -o ./bin/main \
	      -module ./mod/

	$(FC) ./src/global_variables.f90 \
	      ./src/silent_ranlux.f \
	      ./src/initialize.f90 \
	      ./src/tools.f90 \
	      ./src/core.f90 \
	      ./src/run.f90 \
	      ./src/dump.f90 \
	      \
	      -o ./bin/dump \
	      -module ./mod/

run :
	nohup ./bin/main &> main.out &

debug :
	$(FC) ./src/global_variables.f90 \
	      ./src/initialize.f90 \
	      ./src/silent_ranlux.f \
	      ./src/tools.f90 \
	      ./src/core.f90 \
	      ./src/debug.f90 \
	      \
	      -o ./bin/debug \
	      -module ./mod/

	./bin/debug

clean :
	rm -f ./dump/*
	rm -f ./plot.dat
	rm -f ./magnetization.dat
	rm -f ./nohup.out
	rm -rf ./bin/*
