programs = md

sources = $(wildcard *.f90)
objects = $(patsubst %.f90, %.o, $(sources))


include ../Makefile


#
# Executable object dependencies
#
md: $(objects)
md.o: distribution.mod particle_types.mod serial.mod lj_force.mod grav_force.mod integration.mod

#
# Module interdependencies
#
particle_types.mod: global_variables.mod
distribution.mod: particle_types.mod
integration.mod: particle_types.mod
serial.mod: distribution.mod
