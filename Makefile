
# Look in src for .mod files
FT_INCLUDES=-I../src

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
	gfortran -fsyntax-only $< $(FT_INCLUDES)


#
# Fortran compilation rules
#
%.mod.o: %.mod.f90 %.mod
	gfortran -c -o $@ $< $(FT_INCLUDES)

%.o: %.f90
	gfortran -c -o $@ $< $(FT_INCLUDES)


#
# Compile executables from object files
#
%: %.o
	gfortran -o $@ $^ $(FT_INCLUDES)

#
# Cleanup rules
#
clean_cmd = rm src/*.o src/*.mod test/*.o test/*.mod
clean:
	@ echo $(clean_cmd)
	-@ $(clean_cmd) 2>/dev/null || true
