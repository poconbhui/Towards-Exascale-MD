../src/%:
	make -C ../src $*

include ../Makefile.in

programs = \
	particle_type_test \
	integration_test \
	grav_force_test \
	lj_force_test \
	serial_distribution_type_test \
	replicated_distribution_type_test \
	domain_distribution_type_test \
	systolic_distribution_type_test 


particle_type_test: ../src/particle_type.mod.o test.mod.o
particle_type_test.o: ../src/particle_type.mod test.mod

integration_test: ../src/integration.mod.o ../src/particle_type.mod.o
integration_test.o: ../src/integration.mod ../src/particle_type.mod

force_test.mod: test.mod ../src/abstract_distribution_type.mod
force_test_mod_o = test.mod.o force_test.mod.o ../src/particle_type.mod.o ../src/abstract_distribution_type.mod.o
force_test_mod   = test.mod force_test.mod ../src/particle_type.mod ../src/abstract_distribution_type.mod

grav_force_test: ../src/grav_force.mod.o $(force_test_mod_o)
grav_force_test.o: ../src/grav_force.mod $(force_test_mod)

lj_force_test: ../src/lj_force.mod.o $(force_test_mod_o)
lj_force_test.o: ../src/lj_force.mod $(force_test_mod)


distribution_test.mod: test.mod
distribution_test_mod_o = \
	../src/abstract_distribution_type.mod.o \
	../src/particle_type.mod.o \
	test.mod.o \
	distribution_test.mod.o
distribution_test_mod = \
	../src/global_variables.mod \
	../src/abstract_distribution_type.mod \
	../src/particle_type.mod \
	test.mod \
	distribution_test.mod

serial_distribution_type_test: \
	$(distribution_test_mod_o) \
	../src/serial_distribution_type.mod.o
serial_distribution_type_test.o: \
	$(distribution_test_mod) \
	../src/serial_distribution_type.mod

replicated_distribution_type_test: \
	$(distribution_test_mod_o) \
	../src/replicated_distribution_type.mod.o
replicated_distribution_type_test.o: \
	$(distribution_test_mod) \
	../src/replicated_distribution_type.mod

domain_distribution_type_test: \
	$(distribution_test_mod_o) \
	../src/domain_distribution_type.mod.o
domain_distribution_type_test.o: \
	$(distribution_test_mod) \
	../src/domain_distribution_type.mod

systolic_distribution_type_test: \
	$(distribution_test_mod_o) \
	../src/systolic_distribution_type.mod.o
systolic_distribution_type_test.o: \
	$(distribution_test_mod) \
	../src/systolic_distribution_type.mod
