../src/%:
	make -C ../src $*

include ../Makefile.in

programs = \
	particle_type.test \
	integration.test \
	grav_force.test \
	lj_force.test \
	serial_distribution_type.test \
	replicated_distribution_type.test \
	domain_distribution_type.test \
	systolic_distribution_type.test 


particle_type.test: ../src/particle_type.mod.o test_suite.mod.o
particle_type.test.o: ../src/particle_type.mod test_suite.mod

integration.test: ../src/integration.mod.o ../src/particle_type.mod.o test_suite.mod.o
integration.test.o: ../src/integration.mod ../src/particle_type.mod test_suite.mod

force_test.mod: test_suite.mod ../src/abstract_distribution_type.mod
force_test.mod.o = test_suite.mod.o force_test.mod.o ../src/particle_type.mod.o ../src/abstract_distribution_type.mod.o
force_test.mod   = test_suite.mod force_test.mod ../src/particle_type.mod ../src/abstract_distribution_type.mod

grav_force.test: ../src/grav_force.mod.o $(force_test.mod.o)
grav_force.test.o: ../src/grav_force.mod $(force_test.mod)

lj_force.test: ../src/lj_force.mod.o $(force_test.mod.o)
lj_force.test.o: ../src/lj_force.mod $(force_test.mod)


distribution_test.mod: test_suite.mod
distribution_test.mod.o = \
	../src/abstract_distribution_type.mod.o \
	../src/particle_type.mod.o \
	test_suite.mod.o \
	distribution_test.mod.o
distribution_test.mod = \
	../src/global_variables.mod \
	../src/abstract_distribution_type.mod \
	../src/particle_type.mod \
	test_suite.mod \
	distribution_test.mod

serial_distribution_type.test: \
	$(distribution_test.mod.o) \
	../src/serial_distribution_type.mod.o
serial_distribution_type.test.o: \
	$(distribution_test.mod) \
	../src/serial_distribution_type.mod

replicated_distribution_type.test: \
	$(distribution_test.mod.o) \
	../src/replicated_distribution_type.mod.o
replicated_distribution_type.test.o: \
	$(distribution_test.mod) \
	../src/replicated_distribution_type.mod

domain_distribution_type.test: \
	$(distribution_test.mod.o) \
	../src/domain_distribution_type.mod.o
domain_distribution_type.test.o: \
	$(distribution_test.mod) \
	../src/domain_distribution_type.mod

systolic_distribution_type.test: \
	$(distribution_test.mod.o) \
	../src/systolic_distribution_type.mod.o
systolic_distribution_type.test.o: \
	$(distribution_test.mod) \
	../src/systolic_distribution_type.mod
