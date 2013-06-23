../src/%:
	make -C ../src $*

include ../Makefile.in

programs = serial.bench

bench_suite.mod.o = bench_suite.mod.o
bench_suite.mod = bench_suite.mod

distribution_type.mod= \
	../src/global_variables.mod \
	../src/particle_type.mod \
	../src/abstract_distribution_type.mod
distribution_type.mod.o= \
	../src/particle_type.mod.o \
	../src/abstract_distribution_type.mod.o

serial.bench: \
	$(bench_suite.mod.o) \
	$(distribution_type.mod.o) \
	../src/grav_force.mod.o \
	../src/integration.mod.o \
	../src/serial_distribution_type.mod.o
serial.bench.o: \
	$(bench_suite.mod.o) \
	$(distribution_type.mod) \
	../src/grav_force.mod \
	../src/integration.mod \
	../src/serial_distribution_type.mod
