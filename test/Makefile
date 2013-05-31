tests = integration_test grav_force_test

include ../Makefile

integration_test: ../src/integration.mod.o
integration_test.o: ../src/integration.mod

grav_force_test: ../src/grav_force.mod.o force_test.mod.o test.mod.o
grav_force_test.o: ../src/grav_force.mod force_test.mod test.mod.o

force_test.mod: test.mod
