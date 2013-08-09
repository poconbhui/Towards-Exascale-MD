program particle_type_test
    use global_variables
    use particle_type
    use mpi
    use test_suite
    implicit none

    type(particle) :: test_particle
    integer :: MPI_particle

    integer :: comm = MPI_COMM_WORLD
    integer :: rank
    integer :: nprocs

    integer :: exit_value
    integer :: ierror


    call MPI_Init(ierror)


    call MPI_Comm_rank(comm, rank, ierror)
    call MPI_Comm_size(comm, nprocs, ierror)


    !
    ! Test particle constructor
    !
    call describe("particle(...)")

    test_particle = particle(pos=1, vel=2, force=3, mass=4)

    call expect("%pos should be 1", all(test_particle%pos .EQ. 1))
    call expect("%vel should be 2", all(test_particle%vel .EQ. 2))
    call expect("%force should be 3", all(test_particle%force .EQ. 3))
    call expect("%mass should be 4", test_particle%mass .EQ. 4)


    !
    ! Test generation of MPI particle derived type
    !
    call describe("generate_MPI_particle")

    call generate_MPI_particle(MPI_particle)

    test_particle = particle(pos=rank, vel=2*rank, force=3*rank, mass=4*rank)

    ! Get particle from process 1
    call MPI_Bcast(test_particle, 1, MPI_particle, 1, comm, ierror)

    call expect("%pos should be 1", all(test_particle%pos .EQ. 1))
    call expect("%vel should be 2", all(test_particle%vel .EQ. 2))
    call expect("%force should be 3", all(test_particle%force .EQ. 3))
    call expect("%mass should be 4", test_particle%mass .EQ. 4)


    exit_value = end_test()
    call MPI_Finalize(ierror)
    call exit(exit_value)


end program particle_type_test
