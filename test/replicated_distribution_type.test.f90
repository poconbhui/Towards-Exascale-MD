! PROGRAM replicated_disribution_test
!
! This program checks that the replicated_distribution type works
! as expected.
!
! It should be run with several MPI processes.
!
program replicated_distribution_test
    use replicated_distribution_type
    use mpi

    use test_suite

    use distribution_test
    implicit none


    integer :: num_particles
    type(replicated_distribution) :: dist

    integer :: exit_value
    integer :: ierr


    call MPI_Init(ierr)

    num_particles = 40
    dist = new_replicated_distribution(num_particles, MPI_COMM_WORLD)

    call distribution_module_test(dist, num_particles)


    exit_value = end_test()
    call MPI_Finalize(ierr)
    call exit(exit_value)

end program replicated_distribution_test
