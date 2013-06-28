23/2/13
-------
* Decided to use Fortran.
* Wrote basic serial Fortran code without tests.
* Wrote visualization tool that works for a small number of particles.
* Began reorganizing Fortran code with a mind towards being able to quickly
  implement a given parallel pattern.

* Tests need to be implemented so a given parallel code can be properly
  tested. A simple input/output test may not be sufficient for the case
  where a potential needs to be altered to suit the parallel scheme.
* Looking at the code, I think I only need to implement some
  particle-particle comparison function and reduction function.
  The latter can likely be implemented as a by product of the first.
  That is, the first can have an optional argument to run some
  reduction function while it runs the comparisons.

* It seems like implementing the parallel looping may be the very meat of
  the program. Any tests implemented would likely require either running
  the loop a second time or tightly coupling the tests with the force update
  routine.
  It looks like the only three routines needed are a particle comparison
  and update function, an individual particle update function, and a
  data merge function.

* Force calculations can be implemented straightforwardly with the
  comparison function.
* Potential updates can be implemented straightforwardly with the
  comparison function.
* Kinetic updates can be implemented as a special case of the comparison
  function where it is calculated only from the local particle,
  and the calculation is simply performed multiple times redundantly.
* Reductions, eg energy summation can be implemented in a slightly
  ridiculous manner. Values can be checked after they have been calculated.
  Eg in one timestep, find the potential and kinetic energies of the
  particle. During that individual update phase, update the energy.
  Then, on the next time around, the energy from the previous round
  will be available to check.
  This sort of fits with the general MD thing where you have access only to
  data from the previous timestep to guide the next.


28/6/13
-------
* Implemented functional programming oriented framework that should be
  usable by the user to broadly define an MD simulation
  by writing compatable modules for performing integrations,
  force updates, and map/reduces to extract information about the system.
* Currently, only the calculation end of the framework has been properly
  considered.
  Printing data is completely unoptimized. It simply dumps data
  to stdout, but does ensure every particle is printed only once.
  Future implementations should be expected to print to a file, even
  if in a hugely unpotimized manner.
* Implemented replicated, domain decomposed and systolic distribution
  systems with the framework.
* Implemented gravitational and Lennard-Jones force calculators for use
  with the framework.
* Implemented velocity verlet integrator for use with the framework.
* Wrote tests for all distribution systems and for grav, LJ and velocity
  verlet modules.
* Wrote some ruby scripts for easily specifying a series of programs
  and options to be run, along with methods of easily changing the mpi
  execution program used to run them.
* Began writing some benchmarking suites.
* Ensured project compiles with cray, pgi and gnu compilers.
