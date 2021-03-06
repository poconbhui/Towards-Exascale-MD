Towards Exascale MD
===================

This a dissertation project for the MSc.
HPC in the University of Edinburgh run by EPCC.

This project aims to identify and potentially address some performance
scaling issues of some classical Molecular Dynamics (MD) algorithms.


Usage
-----


### Quick Start ###

The test suite runs on 32 cores.

On Hector:

    cp examples/config.hector config
    ./configure.rb
    make all
    qsub ./examples/run_tests.qsub.sh

On Morar

    module load mpich2-pgi
    cp examples/config.morar
    ./configure.rb
    make all
    qsub ./examples/run_tests.qsub.sh

On a standard machine:

    cp examples/config.standard
    ./configure.rb
    make all
    ./scripts/run_tests.rb


This project requires a Fortran compiler capable of compiling
a reasonable set of the Fortran 2003 standard.


### configure.rb ###

To configure the package to compile using `gfortran` as the compiler,
`mpif90` as the MPI compiler and `mpiexec` as the MPI execution
program, run

    ./configure.rb --FC=gfortran --MPIFC=mpif90 --MPIEXEC=mpiexec

To save configurations, place them in a `config` file in the
project root. For some example `config` files, see the `examples/`
directory. With a `config` file set up, the project can be reconfigured
by running

    ./configure.rb

For help with configuration options, run

    ./configure.rb --help

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
This may be expanded by editing `scripts/mpiexec.rb.in`.


### Running scripts ###

After `configure.rb` has been run, scripts from the `scripts/` directory
should be ready to run.

    # Run the test suite
    ./scripts/run_tests.rb 

    # Run a set of benchmarks for 2^9 particles on the range of cores
    # 1-64 using the replicated data distribution
    ./scripts/run_benchs.rb 2^9 1-64 replicated


### Compilation and Testing ###

To compile and test everything, the following procedure may be used:

    ./configure.rb
    make all
    ./scripts/run_tests.rb


### Benchmarking ###

To see available benchmarks, run

    ./scripts/run_benchs.rb

The program `bench/bench` outlines a set of tests for a given distribution,
timing a full calculation time step,
several repetitions of the `individual_operation` method and
several repetitions of the `pair_operation` method.

These are done in several regimes. In one, both MPI communications and
calculations are enabled. In another, MPI communications are disabled
and calculations are enabled. In another, MPI communications are
enabled and calculations are disabled.
This allows for easy separation of MPI communication times from
calculation times, along with finding the time when both are done.

The script `bench/list_of_benches.rb` outlines an array of benchmarks
to be performed. Its primary use is to define reasonable particle
numbers to benchmark, reasonable grouping of core ranges to be
used in a submission script, and to list the distributions available
for testing.

Benchmarks can be run using `scripts/run_benchs.rb`.
Running `./scripts/run_benchs.rb` should provide a list of allowed
options.


### Structure ###

Parallel schemes are called distributions.
The replicated data scheme, for example, is implemented in
the `replicated_distribution` type, which can be found in the
`replicated_distribution_type` module, available in the file
`src/replicated_distribution_type.mod.f90`.

Distributions are defined by inheriting from the `abstract_distribution`
type.
This provides the `individual_operation` and `pair_operation`
interfaces which are used to implement an MD simulation.

An example MD application making use of these can be found in
`src/md.f90`.
The `bench/bench.f90` is also useful for seeing how these interfaces are used.
