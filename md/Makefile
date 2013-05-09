programs = md
sources = $(wildcard *.f90)
objects = $(patsubst %.f90, %.o, $(sources))
tests = test/integration_test


#
# Make targets
#
all: $(programs)
test: $(tests)
	@for i in $<; do echo $$i; ./$$i; done


#
# Executable object dependencies
#
md: $(objects)
md.o: distribution_types.mod particle_types.mod serial.mod lj_force.mod grav_force.mod integration.mod

test/integration_test: integration.mod.o
test/integration_test.o: integration.mod


#
# Module interdependencies
#
particle_types.mod: global_variables.mod
distribution_types.mod: particle_types.mod
integration.mod: particle_types.mod



#
# Module compilation rules
#
%.mod: %.mod.f90
	gfortran -fsyntax-only $<


#
# Fortran compilation rules
#
%.o: %.f90
	gfortran -c -o $@ $<


#
# Compile executables from object files
#
%: %.o
	gfortran -o $@ $^

#
# Cleanup rules
#
clean_cmd = rm *.o *.mod test/*.o test/*.mod $(programs) $(tests)
clean:
	@ echo $(clean_cmd)
	-@ $(clean_cmd) 2>/dev/null || true
