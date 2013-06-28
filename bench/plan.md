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

Initial system sizes
====================
* 100,000 particles.
** Serial distribution: 10^6 particles ~ 1413s (~23m)

* Maximum size executable by all distributions on 1 core/cores per node.
** 32 GB/node
** Particle size = (3*3*double + 1*double) = (3*3*8 + 8) = 80B
** => 32*(1024^3)/80 = 429496729.6 => 10^9 particles
** => ~23m * 10^6 = ~43 years


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
