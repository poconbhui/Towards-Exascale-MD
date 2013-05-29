module integration
    use global_variables
    use particle_types
    implicit none

    private :: dt
    REAL(p) :: dt

    contains
    subroutine integration_init(time_step)
        REAL(p), intent(in) :: time_step

        dt = time_step
    end subroutine integration_init

    subroutine verlet_integrate_pt1(p)
        type(particle_type), intent(inout) :: p

        p%pos = p%pos + p%vel*dt + 0.5 * (p%force/p%mass) * dt * dt
        p%vel = p%vel + 0.5 * (p%force/p%mass) * dt
    end subroutine verlet_integrate_pt1

    subroutine verlet_integrate_pt2(p)
        type(particle_type), intent(inout) :: p

        p%vel = p%vel + 0.5 * (p%force/p%mass) * dt
    end subroutine verlet_integrate_pt2
end module integration
