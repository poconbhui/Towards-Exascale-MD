program integration_test
    use global_variables
    use integration
    use particle_type
    implicit none

    REAL(p), parameter :: PI = 3.14159265
    REAL(p) :: total_time = 1000
    REAL(p) :: time_step = 0.001
    REAL(p) :: accuracy = 0.001

    INTEGER :: total_steps
    INTEGER :: current_step

    type(particle) :: test_particle
    REAL(p) :: initial_position(Ndim)

    call random_seed

    call random_number(initial_position)

    !
    ! Initialize test particle
    !
    test_particle = particle(pos=initial_position, vel=0, force=0, mass=1)

    !
    ! Initialize integration module
    !
    call integration_init(time_step)

    total_steps = int(total_time/time_step)
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

        test_particle = verlet_integrate_pt1(test_particle, 1)

        !
        ! Generate centripital force
        !
        call spring_force(test_particle)


        test_particle = verlet_integrate_pt2(test_particle, 1)
    end do

    contains
    subroutine spring_force(ptcl)
        type(particle), intent(inout) :: ptcl

        REAL(p) :: k


        k = 1

        ptcl%force = -k*ptcl%pos
    end subroutine spring_force

end program integration_test
