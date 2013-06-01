module particle_type
    use global_variables
    implicit none
    private

    public :: particle

    type particle
        real(p) :: pos(Ndim)
        real(p) :: vel(Ndim)
        real(p) :: force(Ndim)
        real(p) :: mass
    end type particle
    interface particle
        procedure constructor
    end interface particle

contains
    function constructor(pos, vel, force, mass)
        type(particle) :: constructor

        real(p) :: pos(size(constructor%pos))
        real(p) :: vel(size(constructor%vel))
        real(p) :: force(size(constructor%force))
        real(p) :: mass

        constructor%pos = pos
        constructor%vel = vel
        constructor%force = force
        constructor%mass = mass
    end function constructor

end module particle_type
