module particle_type
    use global_variables
    use mpi
    implicit none

    private

    public :: particle
    public :: generate_MPI_particle

    type particle
        real(p) :: pos(Ndim)
        real(p) :: vel(Ndim)
        real(p) :: force(Ndim)
        real(p) :: mass
    end type particle
    interface particle
        procedure constructor
    end interface particle

contains
    function constructor(pos, vel, force, mass)
        type(particle) :: constructor

        real(p) :: pos(size(constructor%pos))
        real(p) :: vel(size(constructor%vel))
        real(p) :: force(size(constructor%force))
        real(p) :: mass

        constructor%pos = pos
        constructor%vel = vel
        constructor%force = force
        constructor%mass = mass
    end function constructor

    subroutine generate_mpi_particle(MPI_particle)
        integer, intent(out) :: MPI_particle

        logical, save :: cached_MPI_particle_exists = .FALSE.
        integer, save :: cached_MPI_particle

        integer, parameter :: num_params = 4
        integer :: block_lengths(num_params)
        integer(kind=MPI_ADDRESS_KIND) :: addresses(num_params+1)
        integer(kind=MPI_ADDRESS_KIND) :: displacements(num_params)
        integer :: type_list(num_params)

        type(particle) :: test_particle

        integer :: i
        integer :: ierror


        ! If a cached handle exists, use it and return early
        if(cached_MPI_particle_exists) then
            MPI_particle = cached_MPI_particle
            return
        end if


        ! Lengths of arrays
        block_lengths = (/ &
            size(test_particle%pos), &
            size(test_particle%vel), &
            size(test_particle%force), &
            1 & ! size(test_particle%mass)
        /)

        ! Types of arrays
        type_list = (/ MPI_REAL_P, MPI_REAL_P, MPI_REAL_P, MPI_REAL_P /)


        !
        ! Find byte displacement of arrays from start of derived type
        !
        call MPI_Get_address(test_particle,       addresses(1), ierror)
        call MPI_Get_address(test_particle%pos,   addresses(2), ierror)
        call MPI_Get_address(test_particle%vel,   addresses(3), ierror)
        call MPI_Get_address(test_particle%force, addresses(4), ierror)
        call MPI_Get_address(test_particle%mass,  addresses(5), ierror)

        do i=1, num_params
            displacements(i) = addresses(i+1) - addresses(1)
        end do


        !
        ! Generate the struct
        !
        call MPI_Type_create_struct( &
            num_params, block_lengths, displacements, type_list, &
            MPI_particle, &
            ierror &
        )

        call MPI_Type_commit(MPI_particle, ierror)


        ! Cache the handle for later calls
        cached_MPI_particle = MPI_particle
        cached_MPI_particle_exists = .TRUE.
    end subroutine generate_mpi_particle

end module particle_type
