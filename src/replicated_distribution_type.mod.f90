module replicated_distribution_type
    use abstract_distribution_type
    use particle_type
    use global_variables
    use mpi
    implicit none

    private

    public :: replicated_distribution
    public :: new_replicated_distribution

    type, EXTENDS(abstract_distribution) :: replicated_distribution
        integer, private :: num_particles

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

        procedure, private :: sync_particles
        procedure, private :: get_chunk_data
    end type replicated_distribution

contains
    function new_replicated_distribution(particle_count, comm)
        type(replicated_distribution) :: new_replicated_distribution
        integer, intent(in) :: particle_count
        integer, intent(in) :: comm

        call new_replicated_distribution%init(particle_count, comm)
    end function new_replicated_distribution

    subroutine init(this, num_particles, comm)
        class(replicated_distribution), intent(out) :: this
        integer, intent(in) :: num_particles
        integer, intent(in) :: comm

        integer :: comm_dup
        integer :: ierror


        call MPI_Comm_dup(comm, comm_dup, ierror)

        this%num_particles = num_particles
        this%comm = comm_dup

        call MPI_Comm_rank(comm_dup, this%rank, ierror)
        call MPI_Comm_size(comm_dup, this%nprocs, ierror)

        allocate(this%particles(num_particles))
    end subroutine init

    subroutine pair_operation(this, compare_func, merge_func)
        class(replicated_distribution), intent(inout) :: this
        procedure(two_particle_function) :: compare_func
        procedure(two_particle_function) :: merge_func

        integer :: i_size, i_start, i_end
        type(particle) :: tmp_particle
        integer :: i, j


        call this%get_chunk_data(this%rank, i_size, i_start, i_end)

        do i=i_start, i_end
            do j=1, this%num_particles
                if(i .EQ. j) cycle

                tmp_particle = compare_func( &
                    this%particles(i), this%particles(j) &
                )
                this%particles(i) = merge_func(this%particles(i), tmp_particle)
            end do
        end do

        call this%sync_particles
    end subroutine pair_operation

    subroutine individual_operation(this, update_func)
        class(replicated_distribution), intent(inout) :: this
        procedure(one_particle_function) :: update_func

        integer :: i


        do i=1, this%num_particles
            this%particles(i) = update_func(this%particles(i), i)
        end do
    end subroutine individual_operation

    subroutine global_map_reduce(this, map, reduce, reduce_value)
        class(replicated_distribution), intent(inout) :: this
        procedure(global_map_function) :: map
        procedure(global_reduce_function) :: reduce
        real(p), intent(inout) :: reduce_value

        integer :: i


        do i=1, this%num_particles
            reduce_value = reduce( reduce_value, map(this%particles(i)) )
        end do
    end subroutine

    subroutine print_particles(this, print_func)
        class(replicated_distribution), intent(inout) :: this
        procedure(print_particle_function) :: print_func

        character(len=80) :: string
        integer :: i


        if(this%rank .EQ. 0) then
            do i=1, this%num_particles
                call print_func(this%particles(i), i, string)
                write(*,'(A)') string
            end do
        end if
    end subroutine print_particles

    subroutine print_string(this, string)
        class(replicated_distribution), intent(inout) :: this
        character(len=*), intent(in) :: string


        if(this%rank .EQ. 0) write(*,'(A)') string
    end subroutine print_string


    subroutine sync_particles(this)
        class(replicated_distribution), intent(inout) :: this

        integer :: rank
        integer :: chunk_size, chunk_start, chunk_end

        integer :: MPI_particle

        integer :: ierror


        call generate_MPI_particle(MPI_particle)

        do rank=0, this%nprocs-1
            call this%get_chunk_data(rank, chunk_size, chunk_start, chunk_end)

            ! Sync positions
            call MPI_Bcast( &
                this%particles(chunk_start:chunk_end), &
                chunk_size, &
                MPI_particle, &
                rank, &
                this%comm, &
                ierror &
            )
        end do

    end subroutine sync_particles

    ! Get chunk size and boundary data for process i
    subroutine get_chunk_data(this, i, chunk_size, chunk_start, chunk_end)
        class(replicated_distribution), intent(inout) :: this
        integer, intent(in) :: i
        integer, intent(out) :: chunk_size
        integer, intent(out) :: chunk_start
        integer, intent(out) :: chunk_end

        
        ! Get basic sizes
        chunk_size = ceiling(real(this%num_particles)/this%nprocs)
        chunk_start = i*chunk_size + 1
        chunk_end = (i+1)*chunk_size

        ! Fix sizes
        if(chunk_end .GT. this%num_particles) chunk_end = this%num_particles
        chunk_size = chunk_end - chunk_start + 1
    end subroutine get_chunk_data

end module replicated_distribution_type
