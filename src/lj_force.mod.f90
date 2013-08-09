! MODULE LJ_force
!
! This module provides functions for calculating the Lennard-Jones
! force between two particles.
!
module LJ_force
    use global_variables
    use particle_type
    use abstract_distribution_type
    implicit none


    ! Delta and Epsilon parameters for the Lennard-Jones potential
    real(p), private :: del=1, eps=1


    ! REAL(p) LJ_reduction_init(Ndim)
    !
    ! This is the initial value for the LJ force reduction.
    ! It represents setting the particle force falus to zero.
    !
    integer, private :: i
    real(p), parameter :: LJ_reduction_init(Ndim) = 0

contains


    ! FUNCTION LJ_pair_to_val
    !
    ! Calculate the LJ force between two particles and return an
    ! array representing the force on p1 due to p2
    !
    PURE function LJ_pair_to_val(p1, p2, N)
        ! Expect N = Ndim
        integer, intent(in) :: N
        real(p) :: LJ_pair_to_val(N)

        type(particle), intent(in) :: p1
        type(particle), intent(in) :: p2


        real(p) :: d(Ndim)
        real(p) :: r
        real(p) :: d_unit(Ndim)
        real(p) :: force(Ndim)


        d = p2%pos - p1%pos
        r = sqrt(sum(d*d))
        d_unit = d/r

        if(r.LT.0.05) r=0.05

        force = -24*eps * ( 2*(del**12/r**13) - (del**6/r**7) ) * d_unit

        LJ_pair_to_val = force
    end function LJ_pair_to_val


    ! FUNCTION LJ_set_val
    !
    ! Set the force of the particle to the new reduced force.
    !
    PURE function LJ_set_val(p1, force, N)
        ! Expect N = Ndim
        integer, intent(in) :: N
        type(particle) :: LJ_set_val

        type(particle), intent(in) :: p1
        real(p), intent(in) :: force(N)


        LJ_set_val = p1
        LJ_set_val%force = force
    end function LJ_set_val


    PURE function LJ_gen_reduce_op(dist)
        integer :: LJ_gen_reduce_op

        class(abstract_distribution), intent(in) :: dist


        LJ_gen_reduce_op = dist%sum
    end function LJ_gen_reduce_op

end module LJ_force
