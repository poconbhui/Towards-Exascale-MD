program systolic_distribution_test
    use test_suite
    use particle_type
    use distribution_test
    use systolic_distribution_type
    use global_variables
    use mpi
    implicit none

    integer :: num_particles
    type(systolic_distribution) :: dist

    integer :: exit_value
    integer :: ierr


    call MPI_Init(ierr)

    num_particles = 10
    dist = new_systolic_distribution(num_particles, MPI_COMM_WORLD)

    call distribution_module_test(dist, num_particles)


    exit_value = end_test()
    call MPI_Finalize(ierr)
    call exit(exit_value)

contains
    PURE subroutine print_particle(pi, i, string)
        type(particle), intent(in) :: pi
        integer, intent(in) :: i
        character(len=*), intent(out) :: string


        write(string,*) i, pi%pos(1), pi%vel(1)
    end subroutine print_particle

end program systolic_distribution_test
