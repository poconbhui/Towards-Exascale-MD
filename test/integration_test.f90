program integration_test
    use global_variables
    use integration
    use particle_types
    implicit none

    REAL(p), parameter :: PI = 3.14159265
    REAL(p) :: total_time = 1000
    REAL(p) :: time_step = 0.001
    REAL(p) :: accuracy = 0.001

    INTEGER :: total_steps
    INTEGER :: current_step

    type(particle_type) :: test_particle
    REAL(p) :: initial_position(Ndim)

    call random_seed

    call random_number(initial_position)

    !
    ! Initialize test particle
    !
    test_particle = particle_type(pos=initial_position, vel=0, force=0, mass=1)

    !
    ! Initialize integration module
    !
    call integration_init(time_step)

    total_steps = total_time/time_step
    do current_step=0, total_steps
        if( sqrt(sum((test_particle%pos - &
            initial_position*cos(current_step*time_step))**2)) &
            .GT. &
            accuracy &
        ) then
            !
            ! Output completion status
            !
            write(*,*) "Time:"
            write(*,*) "Current step:", current_step
            write(*,*) "Expected steps:", total_steps
            write(*,*) "Percentage completion:", &
                       100*current_step/real(total_steps), "%"
            write(*,*)

            ! Output current data
            write(*,*) "Position:"
            write(*,*) "Particle position:", test_particle%pos
            write(*,*) "Expected position:", &
                       initial_position*cos(current_step*time_step)
            write(*,*) "Difference:      ", &
                       test_particle%pos - &
                       initial_position*cos(current_step*time_step)
            write(*,*) "RMS:             ", sqrt(sum( &
                           ( &
                               test_particle%pos - &
                               initial_position*cos(current_step*time_step) &
                           )**2 &
                       ))
            write(*,*)

            return
        end if

        call verlet_integrate_pt1(test_particle)

        !
        ! Generate centripital force
        !
        call spring_force(test_particle)


        call verlet_integrate_pt2(test_particle)
    end do

    contains
    subroutine centripital_force(particle)
        type(particle_type), intent(inout) :: particle

        REAL(p) :: d(Ndim)
        REAL(p) :: d_unit(Ndim)
        REAL(p) :: r

        d = -particle%pos
        r = sqrt(sum(d*d))
        d_unit = d/r

        particle%force = particle%mass * particle%vel**2 / r * d_unit
    end subroutine centripital_force

    subroutine spring_force(particle)
        type(particle_type), intent(inout) :: particle

        REAL(p) :: k
        INTEGER :: i

        k = 1

        do i=1, Ndim
            particle%force(i) = -k*particle%pos(i)
        end do
    end subroutine spring_force

end program integration_test
