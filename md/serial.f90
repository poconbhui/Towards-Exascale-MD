module serial
    use distribution
    use particle_types
    implicit none

    private :: num_particles
    INTEGER :: num_particles

    type(distribution_type) :: serial_distribution
    type(particle_type), allocatable     :: particles(:)

    contains
    subroutine distribution_init(particle_count)
        INTEGER :: particle_count

        num_particles = particle_count

        serial_distribution%pair_operation => pair_operation
        serial_distribution%individual_operation => individual_operation

        allocate(particles(num_particles))
    end subroutine distribution_init

    subroutine pair_operation(compare_func, merge_func)
        procedure(two_particle_interface) :: compare_func
        procedure(two_particle_interface) :: merge_func

        type(particle_type) :: tmp_particle
        INTEGER :: i, j

        do i=1, num_particles
            do j=1, num_particles
                if(i .EQ. j) cycle

                tmp_particle = particles(i)
                call compare_func(tmp_particle, particles(j))
                call merge_func(particles(i), tmp_particle)
            end do
        end do
    end subroutine pair_operation

    subroutine individual_operation(update_func)
        procedure(one_particle_interface) :: update_func

        INTEGER :: i

        do i=1, num_particles
            call update_func(particles(i))
        end do
    end subroutine individual_operation
        
end module serial
