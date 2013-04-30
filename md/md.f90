program MD
    use global_variables
    use particle_types
    use serial
    implicit none

    REAL(p) :: total_time
    REAL(p) :: current_time
    REAL(p) :: time_step

    INTEGER :: particle_count

    total_time = 1
    current_time = 0
    time_step = 0.1

    particle_count = 20

    write(*,*) "Hello"

    call distribution_init(particle_count)

    do while(current_time .LT. total_time)
        call individual_operation(update_particles)

        call pair_operation(count_compairs, merge_count)

        call individual_operation(print_particles)

        current_time = current_time + time_step
    end do

    contains
    subroutine update_particles(p)
        type(particle_type), intent(inout) :: p

        p%pos = 0
    end subroutine

    subroutine count_compairs(p1, p2)
        type(particle_type), intent(inout) :: p1
        type(particle_type), intent(in)    :: p2

        p1%pos = p1%pos + 1
    end subroutine count_compairs

    subroutine merge_count(p1, p2)
        type(particle_type), intent(inout) :: p1
        type(particle_type), intent(in)    :: p2

        p1%pos = p1%pos + p2%pos
    end subroutine merge_count

    subroutine print_particles(p)
        type(particle_type), intent(inout) :: p

        write(*,*) p%pos
    end subroutine print_particles
end program MD
