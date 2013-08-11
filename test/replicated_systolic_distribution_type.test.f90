! PROGRAM replicated_systolic_distribution_test
!
! This program checks that the replicated_systolic_distribution type
! works as expected.
!
! This should be called with several MPI processes.
!
program replicated_systolic_distribution_test
    use replicated_systolic_distribution_type
    use mpi

    use test_suite

    use distribution_test
    implicit none


    integer :: num_particles
    type(replicated_systolic_distribution) :: dist

    integer :: exit_value
    integer :: ierr


    call MPI_Init(ierr)

    num_particles = 40
    dist = new_replicated_systolic_distribution(num_particles, MPI_COMM_WORLD)

    call distribution_module_test(dist, num_particles)


    exit_value = end_test()
    call MPI_Finalize(ierr)

    if(exit_value .NE. 0) stop 1

end program replicated_systolic_distribution_test
