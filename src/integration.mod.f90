! MODULE integration
!
! This module provides functions for performing particle position updates.
!
module integration
    use global_variables
    use particle_type
    implicit none


    !
    ! The timestep to be used in the integration
    !
    real(p), private :: dt


contains


    ! SUBROUTINE integration_init
    !
    ! This subroutine initializes the integrators.
    ! This must be called before any other integration functions
    ! are called.
    !
    subroutine integration_init(time_step)
        REAL(p), intent(in) :: time_step

        dt = time_step
    end subroutine integration_init


    ! SUBROUTINE verlet_integrate_pt1
    !
    ! Perform the first part of the two integration steps.
    ! This is the first of three update steps in the velocity
    ! verlet algorithm, the second being the force update and the
    ! thisr being a further integration step.
    !
    PURE function verlet_integrate_pt1(p, i)
        type(particle) :: verlet_integrate_pt1

        type(particle), intent(in) :: p
        integer, intent(in) :: i


        verlet_integrate_pt1 = p
        verlet_integrate_pt1%pos = verlet_integrate_pt1%pos &
            + verlet_integrate_pt1%vel*dt &
            + 0.5 * ( &
                verlet_integrate_pt1%force/verlet_integrate_pt1%mass &
            ) * dt * dt
        verlet_integrate_pt1%vel = verlet_integrate_pt1%vel &
            + 0.5 * (verlet_integrate_pt1%force/verlet_integrate_pt1%mass) * dt
    end function verlet_integrate_pt1


    ! FUNCTION verlet_integrate_pt2
    !
    ! This performs the second integration of the velocity verlet
    ! algorithm, and is the third part of the algorithm.
    !
    PURE function verlet_integrate_pt2(p, i)
        type(particle) :: verlet_integrate_pt2

        type(particle), intent(in) :: p
        integer, intent(in) :: i


        verlet_integrate_pt2 = p
        verlet_integrate_pt2%vel = verlet_integrate_pt2%vel &
            + 0.5 * (verlet_integrate_pt2%force/verlet_integrate_pt2%mass) * dt
    end function verlet_integrate_pt2

end module integration
