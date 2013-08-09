include ../scripts/Makefile.inc

programs = md


#
# Executable object dependencies
#
md: global_variables.mod.o particle_type.mod.o abstract_distribution_type.mod.o serial_distribution_type.mod.o lj_force.mod.o grav_force.mod.o integration.mod.o bench_flags.mod.o
#md.o: global_variables.mod particle_type.mod abstract_distribution_type.mod serial_distribution_type.mod lj_force.mod grav_force.mod integration.mod

#
# Module interdependencies
#
particle_type.mod: global_variables.mod
integration.mod: global_variables.mod particle_type.mod

distribution_type_mod = abstract_distribution_type.mod global_variables.mod particle_type.mod bench_flags.mod
abstract_distribution_type.mod: particle_type.mod global_variables.mod
serial_distribution_type.mod: $(distribution_type_mod)
replicated_distribution_type.mod: $(distribution_type_mod)
systolic_distribution_type.mod: $(distribution_type_mod)
