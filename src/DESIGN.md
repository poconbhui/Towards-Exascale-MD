* Distribution Type
** Has function pointers for particular distribution functions
** Model for particle comparison is that left particle is compared
   to right one and should be updated accordingly. The left model
   may be compared to several right particles in parallel, so a
   function must be provided to reduce these particles together.
   The left particle may also have been reduced with several others
   before being used to compare to a right particle.
   So, a user should expect a left particle to be either a fresh particle
   or one that has already been merged several times, and they shouldn't
   make a distinction.
   
** Particle-Particle comparison
   (p(inout), p(in), compare(p(inout), p(in)), reduce(p(inout), p(in)))
** Individual Particle Updates
   (p(inout), update(p(inout)))
** Particle Reduce
   (real(inout), reduce(real(inout), p(in)))
** Particle Print
   (p(in), print(char(out), p(in)))

* MD
** Combines distribution and particle functions to generate a simulation
