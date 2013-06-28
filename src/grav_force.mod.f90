! MODULE grav_force
!
! Module provides functions for use with a distribution type
! for calculating gravitational forces between bodies.
!
module grav_force
    use global_variables
    use particle_type
    implicit none


    ! The gravitational constant.
    REAL(p) :: G=1

contains


    ! FUNCTION grav_init
    !
    ! Initialize force to zero.
    !
    PURE function grav_init(p1, i)
        type(particle) :: grav_init

        type(particle), intent(in) :: p1
        integer, intent(in) :: i


        grav_init = p1
        grav_init%force = 0
    end function grav_init


    ! FUNCTION grav_compare
    !
    ! Compare the position of two particles and return a particle
    ! with the force set to the gravitational force between them.
    !
    ! F = G*m_1*m_2/(d^2) * r/|r|
    !
    PURE function grav_compare(p1, p2)
        type(particle) :: grav_compare

        type(particle), intent(in) :: p1
        type(particle), intent(in) :: p2


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


    ! FUNCTION grav_merge
    !
    ! Sum the partial forces of the particles.
    !
    PURE function grav_merge(p1, p2)
        type(particle) :: grav_merge

        type(particle), intent(in) :: p1
        type(particle), intent(in)    :: p2


        grav_merge = p1
        grav_merge%force = p1%force + p2%force
    end function grav_merge

end module grav_force
