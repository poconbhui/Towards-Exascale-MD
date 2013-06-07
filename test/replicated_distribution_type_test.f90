program replicated_distribution_test
    use particle_type
    use distribution_test
    use replicated_distribution_type
    use global_variables
    use mpi
    implicit none


    integer :: num_particles
    type(replicated_distribution) :: dist
    integer :: ierr


    call MPI_Init(ierr)

    num_particles = 10
    dist = replicated_distribution(num_particles, MPI_COMM_WORLD)

    call distribution_module_test(dist, num_particles)

    call MPI_Finalize(ierr)

contains
    PURE subroutine print_particle(pi, i, string)
        type(particle), intent(in) :: pi
        integer, intent(in) :: i
        character(len=*), intent(out) :: string


        write(string,*) i, pi%pos(1), pi%vel(1)
    end subroutine print_particle

end program replicated_distribution_test
