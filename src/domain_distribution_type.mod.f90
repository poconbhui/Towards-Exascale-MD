module domain_distribution_type
    use abstract_distribution_type
    use particle_type
    use global_variables
    use mpi
    implicit none

    private

    public :: domain_distribution

    type :: particle_list
        integer, allocatable :: id(:)
        real(p), allocatable :: pos(:,:)
        real(p), allocatable :: vel(:,:)
        real(p), allocatable :: force(:,:)
        real(p), allocatable :: mass(:)
    contains
        procedure get_particle
        procedure set_particle
        procedure resize
    end type

    type, EXTENDS(abstract_distribution) :: domain_distribution
        integer, private :: num_particles
        real(p), private :: domain_size(Ndim)

        integer, private :: num_local_particles
        type(particle_list), private :: particles

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
        integer :: i

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

        call this%particles%resize(this%num_local_particles)

        do i=1, this%num_local_particles
            this%particles%id(i) = this%rank*chunk_size + i
        end do
    end subroutine init

    subroutine pair_operation(this, compare_func, merge_func)
        class(domain_distribution), intent(inout) :: this
        procedure(two_particle_function) :: compare_func
        procedure(two_particle_function) :: merge_func

        integer :: num_foreign_particles=0
        type(particle_list) :: foreign_list

        type(particle) :: tmp_particle
        type(particle) :: particle_i, particle_j
        integer :: rank
        integer :: i, j

        integer :: ierror


        do rank=0, this%nprocs-1
            call this%get_foreign_list( &
                rank, foreign_list, num_foreign_particles &
            )

            do i=1, this%num_local_particles
                do j=1, num_foreign_particles
                    if(this%particles%id(i) .EQ. foreign_list%id(j)) cycle

                    call this%particles%get_particle(i, particle_i)
                    call foreign_list%get_particle(j, particle_j)

                    tmp_particle = compare_func( &
                        particle_i, particle_j &
                    )
                    call this%particles%set_particle( &
                        i, merge_func(particle_i, tmp_particle) &
                    )
                end do
            end do

        end do
    end subroutine pair_operation

    subroutine individual_operation(this, update_func)
        class(domain_distribution), intent(inout) :: this
        procedure(one_particle_function) :: update_func

        type(particle) :: particle_i
        integer :: i


        do i=1, this%num_local_particles
            call this%particles%get_particle(i, particle_i)
            call this%particles%set_particle( &
                i, update_func(particle_i, this%particles%id(i)) &
            )
        end do
    end subroutine individual_operation

    subroutine global_map_reduce(this, map, reduce, reduce_value)
        class(domain_distribution), intent(inout) :: this
        procedure(global_map_function) :: map
        procedure(global_reduce_function) :: reduce
        real(p), intent(inout) :: reduce_value

        real(p) :: tmp_reduce_value
        type(particle) :: particle_i
        integer :: i

        integer :: ierror


        do i=1, this%num_local_particles
            call this%particles%get_particle(i, particle_i)
            reduce_value = reduce( reduce_value, map(particle_i) )
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
        type(particle) :: particle_i
        integer :: i

        integer :: rank
        integer :: ierror


        do rank=0, this%nprocs-1
            call MPI_Barrier(this%comm, ierror)
            if(rank .EQ. this%rank) then
                do i=1, this%num_local_particles
                    call this%particles%get_particle(i, particle_i)
                    call print_func(particle_i, this%particles%id(i), string)
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
        type(particle_list), intent(inout) :: foreign_list
        integer, intent(out) :: foreign_list_size

        integer :: ierror


        foreign_list_size = 0
        if(rank .EQ. this%rank) foreign_list_size = this%num_local_particles

        call MPI_Bcast( &
            foreign_list_size, 1, MPI_INTEGER, rank, this%comm, ierror &
        )

        call foreign_list%resize(foreign_list_size)

        if(rank .EQ. this%rank) then
            foreign_list%id = this%particles%id
            foreign_list%pos = this%particles%pos
            foreign_list%vel = this%particles%vel
            foreign_list%force = this%particles%force
            foreign_list%mass = this%particles%mass
        end if

        call MPI_Bcast( &
            foreign_list%id, foreign_list_size, MPI_INTEGER, &
            rank, this%comm, ierror &
        )

        call MPI_Bcast( &
            foreign_list%pos, Ndim*foreign_list_size, MPI_REAL_P, &
            rank, this%comm, ierror &
        )

        call MPI_Bcast( &
            foreign_list%vel, Ndim*foreign_list_size, MPI_REAL_P, &
            rank, this%comm, ierror &
        )

        call MPI_Bcast( &
            foreign_list%force, Ndim*foreign_list_size, MPI_REAL_P, &
            rank, this%comm, ierror &
        )

        call MPI_Bcast( &
            foreign_list%mass, foreign_list_size, MPI_REAL_P, &
            rank, this%comm, ierror &
        )

    end subroutine get_foreign_list


    subroutine get_particle(this, i, p)
        class(particle_list), intent(inout) :: this
        integer, intent(in) :: i
        type(particle), intent(out) :: p


        p = particle( &
            pos=this%pos(:,i), &
            vel=this%vel(:,i), &
            force=this%force(:,i), &
            mass=this%mass(i) &
        )
    end subroutine get_particle

    subroutine set_particle(this, i, p)
        class(particle_list), intent(inout) :: this
        integer, intent(in) :: i
        type(particle), intent(in) :: p


        this%pos(:,i) = p%pos
        this%vel(:,i) = p%vel
        this%force(:,i) = p%force
        this%mass(i) = p%mass
    end subroutine set_particle


    subroutine resize(this, num_particles)
        class(particle_list), intent(inout) :: this
        integer, intent(in) :: num_particles


        if(allocated(this%id)) then
            deallocate( &
                this%id, &
                this%pos, &
                this%vel, &
                this%force, &
                this%mass &
            )
        end if

        allocate(this%id(num_particles))
        allocate(this%pos(Ndim,num_particles))
        allocate(this%vel(Ndim,num_particles))
        allocate(this%force(Ndim,num_particles))
        allocate(this%mass(num_particles))

    end subroutine resize

end module domain_distribution_type
