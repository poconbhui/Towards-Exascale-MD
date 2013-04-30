23/2
----
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
  ridiculous manner. Values can be checked after they've been calculated.
  Eg in one timestep, find the potential and kinetic energies of the
  particle. During that individual update phase, update the energy.
  Then, on the next time around, the energy from the previous round
  will be available to check.
  This sort of fits with the general MD thing where you have access only to
  data from the previous timestep to guide the next.
