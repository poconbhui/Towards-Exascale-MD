* System will implement MD simulation using particle-particle force
  calculations and the velocity verlet integration algorithm.

* Will provide an abstract interface to the underlying parallelization
  scheme. This way, force and integration modules will only need to be
  written once, and the simulation should only need to be assembled
  once. It may be reordered later for efficiency.

* All particle data functions will be performed using a map/reduce
  paradigm. All mapping functions will be idempotent, should take in
  an expected number of particles and output a particle with an expected
  field set to just one element of a summation or multiplication added
  to it, to be combined in the reduction operation.
  The mapping operation should be expected to accept particles that
  have already been through several mapping operations and several
  reduction operations, or that have been through none at all.
  The reduction operation will be expected to accept particles directly
  from the coupled mapping operation, or ones that have already been through
  several reduction operations. This approach allows us to implement
  arbitrary replication and reduction in arbitrary parallelization schemes.
  All that is left to implement is the actual parallelization scheme.

* Particle-particle interactions will require particle comparison
  functionality. This will be provided through a mapper that accept two
  particles to be compared and should output based on the leftmost particle
  argument. This should be accompanied with a reducer.

* Data gathering and updating will require individual particle mapping and
  reducing. It will also require full system maps/reductions. Templating would
  be super nice for the system wide map/reduce. As we do not have this,
  we restrict data types to arrays of reals.
  For system wide map/reduce, a function accepting a particle and an array
  length and outputting an array of that length
  will be accepted for the map, along with the array length, and reductions
  will be a function that accepts that array and length and outputs an
  array of the same length. This array will be returned to the main program
  on every process.

* Data printing will be done with a function that accepts a particle and
  outputs a string to be output to file/stdout. It will also be done
  by passing a plain string to a printing function.
  The first is expected to implement printing particle data and the
  second is expected to implement printing system data.
  The parallelization scheme should implement the printing functionalities.
