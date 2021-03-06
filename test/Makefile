include ../scripts/Makefile.inc

../src/%.o: ../src/%.f90
	make -C ../src $@


programs = \
	particle_type.test \
	integration.test \
	grav_force.test \
	lj_force.test \
	serial_distribution_type.test \
	replicated_distribution_type.test \
	systolic_distribution_type.test \
	shared_and_replicated_distribution_type.test \
	replicated_systolic_distribution_type.test


particle_type.test: ../src/global_variables.mod.o ../src/particle_type.mod.o test_suite.mod.o

integration.test: ../src/integration.mod.o ../src/global_variables.mod.o ../src/particle_type.mod.o test_suite.mod.o

force_test.mod.o = test_suite.mod.o force_test.mod.o ../src/global_variables.mod.o ../src/particle_type.mod.o ../src/abstract_distribution_type.mod.o
force_test.mod   = test_suite.mod force_test.mod ../src/global_variables.mod ../src/particle_type.mod ../src/abstract_distribution_type.mod

force_test.mod: $(force_test.mod)
force_test.mod.o: $(force_test.mod.o)

grav_force.test: $(force_test.mod.o) ../src/grav_force.mod.o

lj_force.test: $(force_test.mod.o) ../src/lj_force.mod.o


distribution_test.mod = \
	../src/global_variables.mod \
	../src/abstract_distribution_type.mod \
	../src/particle_type.mod \
        ../src/bench_flags.mod \
	test_suite.mod \
	distribution_test.mod
distribution_test.mod.o = \
        ../src/global_variables.mod.o \
	../src/abstract_distribution_type.mod.o \
	../src/particle_type.mod.o \
        ../src/bench_flags.mod.o \
	test_suite.mod.o \
	distribution_test.mod.o

distribution_test.mod: $(distribution_test.mod)
distribution_test.mod.o: $(distribution_test.mod.o)

serial_distribution_type.test: \
	$(distribution_test.mod.o) \
	../src/serial_distribution_type.mod.o

replicated_distribution_type.test: \
	$(distribution_test.mod.o) \
	../src/replicated_distribution_type.mod.o

systolic_distribution_type.test: \
	$(distribution_test.mod.o) \
	../src/systolic_distribution_type.mod.o

shared_and_replicated_distribution_type.test: \
	$(distribution_test.mod.o) \
	../src/replicated_distribution_type.mod.o \
	../src/shared_and_replicated_distribution_type.mod.o

replicated_systolic_distribution_type.test: \
	$(distribution_test.mod.o) \
	../src/replicated_systolic_distribution_type.mod.o
