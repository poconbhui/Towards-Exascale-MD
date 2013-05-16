module grav_force
    use global_variables
    use particle_types
    implicit none

    REAL(p) :: G=1

    contains
    subroutine grav_init(p1)
        type(particle_type), intent(inout) :: p1

        p1%force = 0
    end subroutine grav_init

    subroutine grav_compare(p1, p2)
        type(particle_type), intent(inout) :: p1
        type(particle_type), intent(in)    :: p2

        REAL(p) :: d(Ndim)
        REAL(p) :: r
        REAL(p) :: d_unit(Ndim)

        REAL(p) :: force(Ndim)

        d = p2%pos - p1%pos
        r = sqrt(sum(d*d))
        d_unit = d/r

        if(r.LT.0.5) r=0.5

        force = G*p1%mass*p2%mass / r**2 * d_unit

        p1%force = force
    end subroutine grav_compare

    subroutine grav_merge(p1, p2)
        type(particle_type), intent(inout) :: p1
        type(particle_type), intent(in)    :: p2

        p1%force = p1%force + p2%force
    end subroutine grav_merge
end module grav_force
