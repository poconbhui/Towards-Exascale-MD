
# Look in src for .mod files
FT_INCLUDES=-I../src
FT_FLAGS=-Wall -Wno-unused-dummy-argument
FC = mpif90


#
# Make targets
#
.PHONY: all
all: $(programs)
	$(if $^ ,, cd src; make all)

.PHONY: test
tests_execute=$(patsubst %, %_execute, $(tests))
test: $(tests_execute)
	$(if $^ ,, make all; cd test; make test)


#
# Execute executable
#
%_execute: %
	./$<


#
# Module compilation rules
#
%.mod: %.mod.f90
	$(FC) -fsyntax-only $< $(FT_INCLUDES) $(FT_FLAGS)


#
# Fortran compilation rules
#
%.mod.o: %.mod.f90 %.mod
	$(FC) -c -o $@ $< $(FT_INCLUDES) $(FT_FLAGS)

%.o: %.f90
	$(FC) -c -o $@ $< $(FT_INCLUDES) $(FT_FLAGS)


#
# Compile executables from object files
#
%: %.o
	$(FC) -o $@ $^ $(FT_INCLUDES) $(FT_FLAGS)


#
# Cleanup rules
#
temps = *.o *.mod
.PHONY: clean_temps
clean_temps:
	-rm *.o *.mod
.PHONY: clean_programs
clean_programs: clean_temps
	-rm $(programs)
.PHONY: clean_tests
clean_tests: clean_temps
	-rm $(tests)
.PHONY: clean
clean:
	$(if $(programs), make clean_programs, cd src && make clean_programs)
	$(if $(tests), make clean_tests, cd test && make clean_tests)
