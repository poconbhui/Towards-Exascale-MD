sources = $(wildcard *.f90)
objects = $(patsubst %.f90, %.o, $(sources))


all: md
test: test/integration_test

md: $(objects)

particle_types.mod: global_variables.mod
distribution_types.mod: particle_types.mod
integration.mod: particle_types.mod

md.o: distribution_types.mod particle_types.mod serial.mod lj_force.mod grav_force.mod integration.mod

test/integration_test.o: integration.mod

%.mod: %.module.f90
	gfortran -fsyntax-only $<

%.o: %.f90
	gfortran -c -o $@ $<

# Run all tests
test:
	@for i in $<; do echo $$i; ./$$i; done

%: %.o
	gfortran -o $@ $^

clean_cmd = rm *.o *.mod MD test/*.o test/*.mod
clean:
	@ echo $(clean_cmd)
	-@ $(clean_cmd) 2>/dev/null || true
