Towards Exascale MD
===================

This a dissertation project for the MSc.
HPC in the University of Edinburgh run by EPCC.

This project aims to identify and potentially address some performance
scaling issues of some classical Molecular Dynamics (MD) algorithms.


Usage
-----


### Quick Start ###

On Morar:

    ./configure.rb --FC=pgf90 --MPIFC=mpif90 --MPIEXEC=mpiexec
    make all
    make run_test

On Hector:

    ./configure.rb --FC=crayftn --MPIFC=ftn --MPIEXEC=aprun
    make all
    bolt -n 4 -s make run_test

On a standard machine:

    ./configure.rb --FC=gfortran --MPIFC=mpif90 --MPIEXEC=mpiexec
    make all
    make run_test



### configure.rb ###

To configure the package, run
`./configure.rb --FC=gfortran --MPIFC=mpif90 --MPIEXEC=mpiexec`.

The `FC` option may be set to whatever underlying compiler you are using.
Currently, only `gfortran`, `pgf90` and `crayftn` are supported.
This may be expanded by editing `scripts/Makefile.inc.in` and
adding to the clause defining flags based on the `$(FC)` variable.

This option is used for determining which flags to pass
to the `MPIFC` compiler.
There is no current limitation on the MPI compiler used,
assuming it passes arguments directly to the underlying compiler.

Use `MPIEXEC` to specify the MPI execution program to be used.
Currently, only `mpiexec` and `aprun` are supported.
This may be expanded by editing `scripts/mpiexec.rb`.


### Running scripts ###

Scripts in the `scripts/` directory rely on certain environment variables
being set.
The `configure.rb` scripts generates `scripts/exec.sh` which can be used
to set up these variables.
A user may either run a script using
`./scripts/exec.sh ./scripts/my_script.rb`, `./scripts/exec.sh my_script.rb`
or using `source ./scripts/exec.sh; ./scripts/...`.
The latter will add the necessary environment variables to your shell
so `sctiprs/exec.sh` doesn't need to be used every time a script is used.



### Compilation and Testing ###

To compile and test everything, the following procedure may be used:

    > ./configure.rb
    > make all
    > make run_test


### Benchmarking ###

Appropriate benchmarks have not yet been decided, but are being planned out.

Difficulties exist in defining appropriate weak scaling benchmarks
for an algorithm that scales as N^2.
