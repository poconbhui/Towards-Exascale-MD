* Must be capable of performing simulation of systems interacting under
  particle-particle interactions.

* Must be capable of being parallelized.

* Should be easily able to change the method of parallelization.
** Parallelization schemes should allow for replicated data and
   for data to be split and recombined in any order.

* Should be able to output data in xyz format every n timesteps or n seconds.
** Output should be specifiable as either to file or to stdout.

* Should be easily timeable (measurable).

* Should be capable of implementing system tests to check the system is
  working while running.
** System tests should be easily switched on and off.

* Should be capable of generating data about the system being simulated.
** eg, should be able to find thermodynamic variables, energy, temperature.
