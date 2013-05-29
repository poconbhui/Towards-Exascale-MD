module particle_types
    use global_variables
    implicit none
    private

    public :: particle_type

    type particle_type
        REAL(p) :: pos(Ndim)
        REAL(p) :: vel(Ndim)
        REAL(p) :: force(Ndim)
        REAL(p) :: mass
    end type particle_type
    interface particle_type
        procedure constructor
    end interface particle_type

    contains
    function constructor(pos, vel, force, mass)
        type(particle_type) :: constructor
        REAL(p) :: pos(Ndim)
        REAL(p) :: vel(Ndim)
        REAL(p) :: force(Ndim)
        REAL(p) :: mass

        constructor%pos = pos
        constructor%vel = vel
        constructor%force = force
        constructor%mass = mass
    end function constructor

end module particle_types
