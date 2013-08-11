! PROGRAM integration_test
!
! Test that out Velocity Verlet integrator is working.
!
! This is done by using it to solve an analytically soluble system
! and comparing its output to the analytical solution.
!
! This module doesn't quite follow the same testing regime as the others
! as the testing framework isn't quite suited to how this test is laid out,
! but is should still output the same overall success or failure
! message as the others.
!
program integration_test
    use integration

    use test_suite

    use global_variables
    use particle_type
    implicit none


    real(p), parameter :: PI = 3.14159265
    real(p) :: total_time = 1000
    real(p) :: time_step = 0.001
    real(p) :: accuracy = 0.001

    integer :: total_steps
    integer :: current_step

    type(particle) :: test_particle
    real(p) :: initial_position(Ndim)


    !
    ! Set a random initial position
    !
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


    !
    ! Solve the system for the given number of time steps
    !
    total_steps = int(total_time/time_step)
    do current_step=0, total_steps

        !
        ! Test if current numerical solution is not close to the
        ! analytical solution
        !
        if( sqrt( sum( &
                ( &
                    test_particle%pos &
                    - initial_position*cos(current_step*time_step) &
                )**2 &
            ) ) &
            .GT. &
            accuracy &
        ) then

            !
            ! If here, the numerical solution is too far from the
            ! analytical solution.
            !


            !
            ! Output completion status
            !
            write(*,*) "Time:"
            write(*,*) "Current step:", current_step
            write(*,*) "Expected steps:", total_steps
            write(*,*) "Percentage completion:", &
                       100*current_step/real(total_steps), "%"
            write(*,*)

            !
            ! Output current data
            !
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


            !
            ! Generate error with the test module
            !
            call expect("Motion should follow solution", .FALSE.)


            !
            ! Die
            !
            if(end_test() .NE. 0) stop 1

        end if


        ! Do first half of integration
        test_particle = verlet_integrate_pt1(test_particle, 1)


        !
        ! Generate spring force
        !
        call spring_force(test_particle)


        ! Do second half of integration
        test_particle = verlet_integrate_pt2(test_particle, 1)
    end do


    if(end_test() .NE. 0) stop 1

contains


    ! SUBROUTINE spring_force
    !
    ! This routine generates a spring force on a particle of
    ! the form a = -kx.
    !
    ! This is useful because a single particle acting under a spring
    ! force is analytically solved as x_0*cos(kt) for a particle
    ! starting from rest at x_0.
    !
    subroutine spring_force(ptcl)
        type(particle), intent(inout) :: ptcl

        REAL(p) :: k


        k = 1

        ptcl%force = -k*ptcl%pos
    end subroutine spring_force

end program integration_test
