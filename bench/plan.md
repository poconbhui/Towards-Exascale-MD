Test ideas
----------
* Strong scaling
* Weak scaling
* Maximum system size
** Particle count - Covered by weak scaling?
** Core count - Covered by weak/strong scaling?
* Long range forces - Grav
* Short range forces - LJ

* Run tests for 3 different initial system sizes
* Run strong scaling test for peak of weak scaling speedup

* Weak scaling does not necessarily make sense for O(N^2) algorithm,
  but it does seem a good fit for measuring the scaling of synchronization
  and load balancing algorithms.


Initial system sizes
====================
* Need to find sizes of systems used in MD codes, and generally how
  long a tick takes. It would be interesting to see if it can be matched
  with an O(N^2) algorithm on more cores.

* Maximum size executable by all distributions on 1 core/cores per node?
** 32 GB/node
** Particle size = (3*3*double + 1*double) = (3*3*8 + 8) = 80B
** => 32*(1024^3)/80 = 429496729.6 => 10^9 particles
** => ~23m * 10^6 = ~43 years
** => Need to run on ~10^6 cores (assuming linear speedup) to run in
      a feasible amount of time.

* 10^4 particles runs in short amount of time. (~10E-3s)
** However, may be useful for finding maximum performance that can
   be squeezed out of a small system.

* 10^5 particles runs in ~20 seconds.

* 100,000 particles.
** Serial distribution: 10^6 particles ~ 1413s (~23m)

* 10^7 untested.
** Likely only feasible starting at ~100 cores.



Strong Scaling
==============
* Start on one core.
* Start with given initial system size.
* Increase core count as 2^n, keep system size constant.

Weak Scaling
============
* Start on one core.
* Start with given initial system size.
* Increase core count as 2^n. Increase system size as 2^n.
* Really more suitable for an O(n) algorithm...
