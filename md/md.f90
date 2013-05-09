program MD
    use global_variables
    use particle_types
    use integration
    use grav_force
    use serial
    implicit none

    REAL(p) :: total_time
    REAL(p) :: current_time
    REAL(p) :: time_step

    INTEGER :: particle_count


    !
    ! Initialize random numbers
    !
    call random_seed


    !
    ! Initialize Simulation Parameters
    !
    total_time = 10
    current_time = 0
    time_step = 0.001

    particle_count = 10


    !
    ! Initialize modules
    !
    call distribution_init(particle_count)
    call integration_init(time_step)


    !
    ! Initialize particles
    !
    call individual_operation(initialize_particles)
    call pair_operation(grav_compare, grav_merge)

    !
    ! Perform Simulation
    !
    do while(current_time .LT. total_time)
        call individual_operation(verlet_integrate_pt1)

        call individual_operation(grav_init)
        call pair_operation(grav_compare, grav_merge)

        call individual_operation(verlet_integrate_pt2)

        ! Output particle data
        write(*,*) particle_count
        write(*,*) "Time: ", current_time
        call individual_operation(print_particles)

        current_time = current_time + time_step
    end do


    contains
    subroutine initialize_particles(p)
        type(particle_type), intent(inout) :: p

        call random_number(p%pos)
        p%vel = 0
        p%force = 0
        p%mass = 1
    end subroutine

    subroutine print_particles(p)
        type(particle_type), intent(inout) :: p

        write(*,*) p%pos, p%force
    end subroutine print_particles
end program MD
