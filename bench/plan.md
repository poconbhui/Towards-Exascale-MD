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

* Initial system sizes:
    1000 particles.
    Maximum size executable by all distributions on 1 core/cores per node.

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
