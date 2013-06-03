module integration
    use global_variables
    use particle_type
    implicit none


    !
    ! The timestep to be used in the integration
    !
    real(p), private :: dt


    contains
    subroutine integration_init(time_step)
        REAL(p), intent(in) :: time_step

        dt = time_step
    end subroutine integration_init

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

    PURE function verlet_integrate_pt2(p, i)
        type(particle) :: verlet_integrate_pt2

        type(particle), intent(in) :: p
        integer, intent(in) :: i


        verlet_integrate_pt2 = p
        verlet_integrate_pt2%vel = verlet_integrate_pt2%vel &
            + 0.5 * (verlet_integrate_pt2%force/verlet_integrate_pt2%mass) * dt
    end function verlet_integrate_pt2
end module integration
