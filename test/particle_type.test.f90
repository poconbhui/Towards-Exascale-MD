! PROGRAM particle_type_test
!
! This program tests that the particle type has all the member variables
! expected and is settable using particle(pos=...,vel=..., ...).
!
! This program also tests that the MPI_particle derived types passes
! particles across MPI as expected.
!
! It should be called with several MPI processes to properly test MPI_particle.
!
program particle_type_test
    use particle_type
    use mpi

    use test_suite
    implicit none


    type(particle) :: test_particle
    integer :: MPI_particle

    integer :: comm = MPI_COMM_WORLD
    integer :: rank

    integer :: exit_value
    integer :: ierror


    call MPI_Init(ierror)




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

    !
    ! Generate the MPI_particle
    !
    call generate_MPI_particle(MPI_particle)

    ! Set particle to some rank specific configuration
    call MPI_Comm_rank(comm, rank, ierror)
    test_particle = particle(pos=rank, vel=2*rank, force=3*rank, mass=4*rank)

    ! Get particle from process 1
    call MPI_Bcast(test_particle, 1, MPI_particle, 1, comm, ierror)

    ! Check values are the same as expected on rank 1
    call expect("%pos should be 1", all(test_particle%pos .EQ. 1))
    call expect("%vel should be 2", all(test_particle%vel .EQ. 2))
    call expect("%force should be 3", all(test_particle%force .EQ. 3))
    call expect("%mass should be 4", test_particle%mass .EQ. 4)


    exit_value = end_test()
    call MPI_Finalize(ierror)
    call exit(exit_value)

end program particle_type_test
