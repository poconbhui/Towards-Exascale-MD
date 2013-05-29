
FT_INCLUDES=-I../src -J../src

#
# Make targets
#
.PHONY: all
all: $(programs)
	if [[ -z "$^" ]];then cd src; make all; fi

.PHONY: test
test: $(tests)
	if [[ -z "$^" ]];then cd test; make test; fi; \
	for i in $^; do echo $$i; ./$$i; done


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
	gfortran -o $@ $^

#
# Cleanup rules
#
clean_cmd = rm src/*.o src/*.mod test/*.o test/*.mod
clean:
	@ echo $(clean_cmd)
	-@ $(clean_cmd) 2>/dev/null || true
