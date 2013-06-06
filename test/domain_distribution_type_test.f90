program domain_distribution_test
    use particle_type
    use distribution_test
    use domain_distribution_type
    use global_variables
    use mpi
    implicit none


    integer :: num_particles
    real(p) :: domain_size(Ndim)
    type(domain_distribution) :: dist
    integer :: ierr


    call MPI_Init(ierr)

    num_particles = 10
    domain_size = 10
    dist = domain_distribution(num_particles, domain_size, MPI_COMM_WORLD)

    call distribution_module_test(dist, num_particles)

    call MPI_Finalize

contains
    PURE subroutine print_particle(pi, i, string)
        type(particle), intent(in) :: pi
        integer, intent(in) :: i
        character(len=*), intent(out) :: string


        write(string,*) i, pi%pos(1), pi%vel(1)
    end subroutine print_particle

end program domain_distribution_test
