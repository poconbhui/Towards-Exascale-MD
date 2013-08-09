! MODULE grav_force
!
! Module provides functions for use with a distribution type
! for calculating gravitational forces between bodies.
!
module grav_force
    use global_variables
    use particle_type
    use abstract_distribution_type
    implicit none


    ! The gravitational constant.
    real(p) :: G=1

    integer, private :: i
    real(p), parameter :: grav_reduction_init(Ndim) = 0

contains


    ! SUBROUTINE grav_pair_to_val
    !
    ! Compare the position of two particles and return an array
    ! representing the gravitational force on p1 due to p2.
    !
    ! F = G*m_1*m_2/(d^2) * r/|r|
    !
    PURE subroutine grav_pair_to_val(p1, p2, val)
        type(particle), intent(in) :: p1
        type(particle), intent(in) :: p2

        ! Expect size(val) = Ndim
        real(p), intent(out) :: val(:)


        REAL(p) :: d(Ndim)
        REAL(p) :: r
        REAL(p) :: d_unit(Ndim)
        REAL(p) :: force(Ndim)


        d = p2%pos - p1%pos
        r = sqrt(sum(d*d))
        d_unit = d/r

        if(r.LT.0.5) r=0.5

        force = G*p1%mass*p2%mass / r**2 * d_unit

        val = force
    end subroutine grav_pair_to_val


    ! FUNCTION grav_set_val
    !
    ! Set the force of the particle to the new total force.
    !
    PURE function grav_set_val(p1, force)
        type(particle) :: grav_set_val

        type(particle), intent(in) :: p1
        real(p), intent(in) :: force(Ndim)


        grav_set_val = p1
        grav_set_val%force = force
    end function grav_set_val

    PURE function grav_gen_reduce_op(dist)
        integer :: grav_gen_reduce_op

        class(abstract_distribution), intent(in) :: dist


        grav_gen_reduce_op = dist%sum
    end function grav_gen_reduce_op

end module grav_force
