program MD
    use global_variables
    use particle_types
    use integration
    use distribution
    use serial
    use grav_force
    implicit none

    REAL(p) :: total_time
    REAL(p) :: current_time
    REAL(p) :: time_step

    INTEGER :: particle_count

    character(len=30) :: distribution_name = "SERIAL"
    class(distribution_type), pointer :: dist

    !
    ! Define potential distribution types
    !
    type(serial_distribution), target :: serial_dist


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

    ! Select distribution module
    select case(distribution_name)
        case("SERIAL")
            serial_dist = serial_distribution(particle_count)
            dist => serial_dist
    end select

    call integration_init(time_step)



    !
    ! Initialize particles
    !
    call dist%individual_operation(initialize_particles)
    call dist%pair_operation(grav_compare, grav_merge)


    !
    ! Perform Simulation
    !
    do while(current_time .LT. total_time)
        call dist%individual_operation(verlet_integrate_pt1)

        call dist%individual_operation(grav_init)
        call dist%pair_operation(grav_compare, grav_merge)

        call dist%individual_operation(verlet_integrate_pt2)

        ! Output particle data
        write(*,*) particle_count
        write(*,*) "Time: ", current_time
        call dist%individual_operation(print_particles)

        current_time = current_time + time_step
    end do


    contains
    subroutine initialize_particles(p)
        type(particle_type), intent(inout) :: p

        ! initialize to zero
        p = particle_type(pos=0, vel=0, force=0, mass=1)

        ! randomize position
        call random_number(p%pos)
    end subroutine

    subroutine print_particles(p)
        type(particle_type), intent(inout) :: p

        write(*,*) p%pos, p%force
    end subroutine print_particles
end program MD
