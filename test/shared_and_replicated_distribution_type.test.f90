! PROGRAM shared_and_replicated_distribution_test
!
! This program tests that the shared_and_replicated_distribution type
! works as expected.
!
! This should be run with several MPI processes and OMP_NUM_THREADS set to
! something greater than 1.
!
program shared_and_replicated_distribution_test
    use shared_and_replicated_distribution_type
    use mpi

    use test_suite

    use distribution_test
    implicit none


    integer :: num_particles
    type(shared_and_replicated_distribution) :: dist

    integer :: exit_value
    integer :: ierr


    call MPI_Init(ierr)

    num_particles = 40
    dist = new_shared_and_replicated_distribution(num_particles, MPI_COMM_WORLD)

    call distribution_module_test(dist, num_particles)


    exit_value = end_test()
    call MPI_Finalize(ierr)

    if(exit_value .NE. 0) stop 1

end program shared_and_replicated_distribution_test
