sources = $(wildcard *.f90)
objects = $(patsubst %.f90, %.o, $(sources))


all: MD

particle_types.o: global_variables.o
distribution_types.o: particle_types.o
md.o : particle_types.o serial.o

%.o: %.f90
	gfortran -c -o $@ $<

MD: $(objects)
	gfortran -o $@ $^

clean_cmd = rm *.o *.mod MD
clean:
	@ echo $(clean_cmd)
	-@ $(clean_cmd) 2>/dev/null || true
