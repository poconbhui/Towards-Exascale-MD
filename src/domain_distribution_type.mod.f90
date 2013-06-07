module domain_distribution_type
    use abstract_distribution_type
    use particle_type
    use global_variables
    use mpi
    implicit none

    private

    public :: domain_distribution

    type, EXTENDS(abstract_distribution) :: domain_distribution
        integer, private :: num_particles
        integer, private :: num_local_particles
        real(p), private :: domain_size(Ndim)

        type(particle), private, allocatable :: particles(:)

        integer, private :: rank
        integer, private :: nprocs
        integer, private :: comm

    contains
        procedure :: init
        procedure :: pair_operation
        procedure :: individual_operation
        procedure :: global_map_reduce
        procedure :: print_particles
        procedure :: print_string

        procedure, private :: get_foreign_list
    end type domain_distribution
    interface domain_distribution
        module procedure constructor
    end interface domain_distribution

contains
    function constructor(num_particles, domain_size, comm)
        type(domain_distribution) :: constructor
        integer, intent(in) :: num_particles
        real(p), intent(in) :: domain_size(Ndim)
        integer, intent(in) :: comm


        call constructor%init(num_particles, domain_size, comm)
    end function constructor

    subroutine init(this, num_particles, domain_size, comm)
        class(domain_distribution), intent(out) :: this
        integer, intent(in) :: num_particles
        real(p), intent(in) :: domain_size(Ndim)
        integer, intent(in) :: comm

        integer :: chunk_size
        integer :: comm_dup

        integer :: ierror


        call MPI_Comm_dup(comm, comm_dup, ierror)

        this%num_particles = num_particles
        this%domain_size   = domain_size
        this%comm          = comm_dup


        call MPI_Comm_rank(comm_dup, this%rank, ierror)
        call MPI_Comm_size(comm_dup, this%nprocs, ierror)

        ! Find initial particle distribution size
        chunk_size = ceiling(real(this%num_particles)/this%nprocs)
        this%num_local_particles = chunk_size
        if((this%rank+1)*chunk_size .GT. num_particles) then
            this%num_local_particles = this%num_local_particles &
                - ((this%rank+1)*chunk_size - num_particles)
        end if

        allocate(this%particles(this%num_local_particles))
    end subroutine init

    subroutine pair_operation(this, compare_func, merge_func)
        class(domain_distribution), intent(inout) :: this
        procedure(two_particle_function) :: compare_func
        procedure(two_particle_function) :: merge_func

        integer :: num_foreign_particles=0
        type(particle), allocatable :: foreign_list(:)

        type(particle) :: tmp_particle
        integer :: rank
        integer :: i, j


        do rank=0, this%nprocs-1
            call this%get_foreign_list( &
                rank, foreign_list, num_foreign_particles &
            )

            do i=1, this%num_local_particles
                do j=1, num_foreign_particles
                    if(rank .EQ. this%rank .AND. i .EQ. j) cycle

                    tmp_particle = compare_func( &
                        this%particles(i), foreign_list(j) &
                    )
                    this%particles(i) = merge_func( &
                        this%particles(i), tmp_particle &
                    )
                end do
            end do

        end do
    end subroutine pair_operation

    subroutine individual_operation(this, update_func)
        class(domain_distribution), intent(inout) :: this
        procedure(one_particle_function) :: update_func

        integer :: chunk_size
        integer :: chunk_start

        integer :: i


        chunk_size = ceiling(real(this%num_particles)/this%nprocs)
        chunk_start = this%rank*chunk_size+1

        do i=1, this%num_local_particles
            this%particles(i) = update_func(this%particles(i), chunk_start-1+i)
        end do
    end subroutine individual_operation

    subroutine global_map_reduce(this, map, reduce, reduce_value)
        class(domain_distribution), intent(inout) :: this
        procedure(global_map_function) :: map
        procedure(global_reduce_function) :: reduce
        real(p), intent(inout) :: reduce_value

        real(p) :: tmp_reduce_value
        integer :: i

        integer :: ierror


        do i=1, this%num_local_particles
            reduce_value = reduce( reduce_value, map(this%particles(i)) )
        end do

        tmp_reduce_value = reduce_value

        call MPI_Allreduce( &
            tmp_reduce_value, reduce_value, 1, &
            MPI_REAL_P, MPI_SUM, this%comm, &
            ierror &
        )
    end subroutine global_map_reduce

    subroutine print_particles(this, print_func)
        class(domain_distribution), intent(inout) :: this
        procedure(print_particle_function) :: print_func

        character(len=80) :: string
        integer :: i

        integer :: chunk_size
        integer :: chunk_start

        integer :: rank

        integer :: ierror


        chunk_size = ceiling(real(this%num_particles)/this%nprocs)
        chunk_start = this%rank*chunk_size+1

        do rank=0, this%nprocs-1
            call MPI_Barrier(this%comm, ierror)
            if(rank .EQ. this%rank) then
                do i=1, this%num_local_particles
                    call print_func( &
                        this%particles(i), &
                        chunk_start-1+i, &
                        string &
                    )
                    write(*,'(A)') string
                end do
            end if
        end do
        call MPI_Barrier(this%comm, ierror)
    end subroutine print_particles

    subroutine print_string(this, string)
        class(domain_distribution), intent(inout) :: this
        character(len=*), intent(in) :: string


        if(this%rank .EQ. 0) write(*,'(A)') string
    end subroutine print_string

    subroutine get_foreign_list( &
        this, rank, foreign_list, foreign_list_size &
    )
        class(domain_distribution), intent(inout) :: this
        integer, intent(in) :: rank
        type(particle), allocatable, intent(inout) :: foreign_list(:)
        integer, intent(out) :: foreign_list_size

        integer :: MPI_particle

        integer :: ierror


        call generate_MPI_particle(MPI_particle)

        foreign_list_size = 0
        if(rank .EQ. this%rank) foreign_list_size = this%num_local_particles

        call MPI_Bcast( &
            foreign_list_size, 1, MPI_INTEGER, rank, this%comm, ierror &
        )

        if(allocated(foreign_list)) deallocate(foreign_list)
        allocate(foreign_list(foreign_list_size))

        if(rank .EQ. this%rank) then
            foreign_list = this%particles
        end if

        call MPI_Bcast( &
            foreign_list, foreign_list_size, MPI_particle, &
            rank, this%comm, ierror &
        )
    end subroutine get_foreign_list


end module domain_distribution_type
