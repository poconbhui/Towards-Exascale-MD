include ../scripts/Makefile.inc

../src/%.o: ../src/%.f90
	make -C ../src $@


programs = bench

bench_suite.mod.o = \
	../src/global_variables.mod.o \
        ../src/particle_type.mod.o \
    	../src/abstract_distribution_type.mod.o \
    	../src/integration.mod.o \
    	../src/lj_force.mod.o \
        bench_suite.mod.o
bench_suite.mod = \
	../src/global_variables.mod \
        ../src/particle_type.mod \
    	../src/abstract_distribution_type.mod \
    	../src/integration.mod \
    	../src/lj_force.mod \
        bench_suite.mod

distribution_type.mod= \
	../src/global_variables.mod \
	../src/particle_type.mod \
	../src/abstract_distribution_type.mod
distribution_type.mod.o= \
	../src/global_variables.mod.o \
	../src/particle_type.mod.o \
	../src/abstract_distribution_type.mod.o

bench: \
	$(bench_suite.mod.o) \
	$(distribution_type.mod.o) \
	../src/grav_force.mod.o \
	../src/integration.mod.o \
	../src/serial_distribution_type.mod.o \
        ../src/replicated_distribution_type.mod.o \
        ../src/domain_distribution_type.mod.o \
        ../src/systolic_distribution_type.mod.o
bench.o: \
	$(bench_suite.mod.o) \
	$(distribution_type.mod) \
	../src/grav_force.mod \
	../src/integration.mod \
	../src/serial_distribution_type.mod \
        ../src/replicated_distribution_type.mod \
        ../src/domain_distribution_type.mod \
        ../src/systolic_distribution_type.mod
