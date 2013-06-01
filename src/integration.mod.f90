module integration
    use global_variables
    use particle_type
    implicit none

    private :: dt
    REAL(p) :: dt

    contains
    subroutine integration_init(time_step)
        REAL(p), intent(in) :: time_step

        dt = time_step
    end subroutine integration_init

    function verlet_integrate_pt1(p)
        type(particle), intent(in) :: p
        type(particle) :: verlet_integrate_pt1

        verlet_integrate_pt1 = p
        verlet_integrate_pt1%pos = verlet_integrate_pt1%pos &
            + verlet_integrate_pt1%vel*dt &
            + 0.5 * ( &
                verlet_integrate_pt1%force/verlet_integrate_pt1%mass &
            ) * dt * dt
        verlet_integrate_pt1%vel = verlet_integrate_pt1%vel &
            + 0.5 * (verlet_integrate_pt1%force/verlet_integrate_pt1%mass) * dt
    end function verlet_integrate_pt1

    function verlet_integrate_pt2(p)
        type(particle), intent(in) :: p
        type(particle) :: verlet_integrate_pt2

        verlet_integrate_pt2 = p
        verlet_integrate_pt2%vel = verlet_integrate_pt2%vel &
            + 0.5 * (verlet_integrate_pt2%force/verlet_integrate_pt2%mass) * dt
    end function verlet_integrate_pt2
end module integration
