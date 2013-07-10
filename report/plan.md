Introduce Molecular systems.
Introduce usefulness of modelling molecular systems.
Introduce other modelling methods (eg Monte Carlo, Molecular Modeling, MD).
Introduce usefulness of MD.
Introduce the basic problem.
Introduce limitations, errors, chaos.
Introduce numbers (system sizes, times).
Introduce general scheme (initialize, equalize, measure).

Introduce force fields.
Introduce numerical integrators.
Introduce some parallel schemes.
Introduce simplifications.
Note the error introduced by those schemes.
Note the limitations of those schemes.

Introduce our 3 parallel schemes: Replicated, domained, systoliced.
Present benchmarks of initial implementations.
Benchmarks should be strong scaling results on 10^4 - 10^7 particles for
lennard jones, including error compared to serial result (not possible for 10^7?).

Implement improvement techniques.
Replicated: MPI over node.
Domain: short range forces.
Systolic: Replicated systolic loop.
Benchmark, display.
