program particle_type_test
    use global_variables
    use particle_type
    use mpi
    use test
    implicit none

    type(particle) :: test_particle
    integer :: MPI_particle

    integer :: comm = MPI_COMM_WORLD
    integer :: rank
    integer :: nprocs

    integer :: i
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
    call expect("%vel should be 1", all(test_particle%vel .EQ. 2))
    call expect("%force should be 1", all(test_particle%force .EQ. 3))
    call expect("%mass should be 1", test_particle%mass .EQ. 4)


    !
    ! Test generation of MPI particle derived type
    !
    call describe("generate_MPI_particle")

    call generate_MPI_particle(MPI_particle)

    test_particle = particle(pos=rank, vel=rank, force=rank, mass=rank)

    call MPI_Bcast(test_particle, 1, MPI_particle, 1, comm, ierror)

    call expect("%pos should be 1", all(test_particle%pos .EQ. 1))
    call expect("%vel should be 1", all(test_particle%vel .EQ. 1))
    call expect("%force should be 1", all(test_particle%force .EQ. 1))
    call expect("%mass should be 1", test_particle%mass .EQ. 1)


    call MPI_Finalize(ierror)
end program particle_type_test
