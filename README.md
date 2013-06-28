Towards Exascale MD
===================

This a dissertation project for the MSc. HPC in the University of Edinburgh run by EPCC.

This project aims to identify and potentially address some performance
scaling issues of some classical Molecular Dynamics (MD) algorithms.


Usage
-----

### config.sh ###

First, configure `config.sh` to whatever is most appropriate for your system.

The `mpiexec` environment variable determines which mpi execution program
will be used in scripts in the `scripts/` directory. For example, setting
`mpiexec=aprun` will use aprun to execute mpi code. Details of implementation
can be found in `scripts/mpiexec.rb`.

The `FC` environment variable determines which fortran compiler will be
used in Makefiles. Details can be found in `Makefile.in`. Setting
`FC=ftn` will assume the cray compiler is being used. Otherwise, it is assumed
that `mpif90` with `gfortran` is being used.

`config.sh` may be used in two ways: Either as `./config.sh ./my_program args`,
or alternatively as `source ./config.sh; ./my_program args`. The source method
can be used once in a shell to set the appropriate environment variables
and need never be used again.


### Compilation and Testing ###

To compile and test everything, the following procedure may be used:

    > source ./config.sh
    > make all
    > ./scripts/run_tests.rb


### Banchmarking ###

To date, appropriate benchmarks haven''t quite been decided yet.
