module grav_force
    use global_variables
    use particle_types
    implicit none

    REAL(p) :: G=1

    contains
    function grav_init(p1)
        type(particle_type), intent(in) :: p1

        type(particle_type) :: grav_init

        grav_init = p1
        grav_init%force = 0
    end function grav_init

    function grav_compare(p1, p2)
        type(particle_type), intent(in) :: p1
        type(particle_type), intent(in) :: p2

        type(particle_type) :: grav_compare

        REAL(p) :: d(Ndim)
        REAL(p) :: r
        REAL(p) :: d_unit(Ndim)

        REAL(p) :: force(Ndim)

        d = p2%pos - p1%pos
        r = sqrt(sum(d*d))
        d_unit = d/r

        if(r.LT.0.5) r=0.5

        force = G*p1%mass*p2%mass / r**2 * d_unit

        grav_compare = p1
        grav_compare%force = force
    end function grav_compare

    function grav_merge(p1, p2)
        type(particle_type), intent(in) :: p1
        type(particle_type), intent(in)    :: p2

        type(particle_type) :: grav_merge

        grav_merge = p1
        grav_merge%force = p1%force + p2%force
    end function grav_merge
end module grav_force
