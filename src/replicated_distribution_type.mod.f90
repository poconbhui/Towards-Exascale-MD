module replicated_distribution_type
    use abstract_distribution_type
    use particle_type
    use global_variables
    use mpi
    use bench_flags
    implicit none

    private

    public :: replicated_distribution
    public :: new_replicated_distribution

    type, EXTENDS(abstract_distribution) :: replicated_distribution
        integer, private :: num_particles

        type(particle), private, allocatable :: particles(:)
        type(particle), private, allocatable :: local_particles(:)

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

        integer :: local_chunk_size, chunk_start, chunk_end

        integer :: comm_dup
        integer :: ierror


        !
        ! Set up MPI variables
        !
        call MPI_Comm_dup(comm, comm_dup, ierror)
        call MPI_Comm_rank(comm_dup, this%rank, ierror)
        call MPI_Comm_size(comm_dup, this%nprocs, ierror)
        this%comm = comm_dup

        ! Set up reduce ops
        this%sum = MPI_SUM


        !
        ! Ensure num_particles >= nprocs. If not, die.
        !
        if(num_particles .LT. this%nprocs) then
            ! Write to stderr
            call this%print_string("ERROR: num_particles > num_procs. Exiting")
            call MPI_Abort(MPI_COMM_WORLD, 1, ierror)
        end if

        
        !
        ! Set up particle list variables
        !
        this%num_particles = num_particles

        ! Find size of local list
        call this%get_chunk_data( &
            this%rank, local_chunk_size, chunk_start, chunk_end &
        )

        ! Allocate lists
        allocate(this%particles(num_particles))
        allocate(this%local_particles(local_chunk_size))
    end subroutine init

    subroutine pair_operation( &
        this, pair_to_val, val_to_particle, reduce_op, reduction_identity &
    )
        class(replicated_distribution), intent(inout) :: this

        procedure(two_particle_to_array_subroutine) :: pair_to_val
        procedure(particle_and_array_to_particle_function) :: val_to_particle
        integer :: reduce_op
        real(p) :: reduction_identity(:)

        integer :: N

        integer :: i_size, i_start, i_end
        real(p) :: reduce_val(size(reduction_identity))
        real(p) :: tmp_val(size(reduction_identity))
        integer :: i, j


        interface reduce_types
            PURE function reduce_type(arr1, arr2)
                use global_variables
                implicit none

                real(p), intent(in) :: arr1(:)
                real(p), intent(in) :: arr2(size(arr1))

                real(p) :: reduce_type(size(arr1))
            end function reduce_type
        end interface

        procedure(reduce_type), pointer :: reduce_func


        !
        ! Set reduction function
        !
        if(reduce_op .EQ. MPI_SUM) then
            reduce_func => reduce_sum
        else
            call this%print_string("Error: provided reduce_op not supported!")
            call exit(1)
        end if


        N = size(reduction_identity)


        if(.NOT. disable_calculation) then

            call this%get_chunk_data(this%rank, i_size, i_start, i_end)

            do i=i_start, i_end
                reduce_val = reduction_identity

                do j=1, this%num_particles
                    if(i .EQ. j) cycle

                    call pair_to_val( &
                        this%particles(i), this%particles(j), &
                        tmp_val &
                    )
                    
                    reduce_val = reduce_sum(reduce_val, tmp_val)

                end do

                this%particles(i) = val_to_particle( &
                    this%particles(i), tmp_val &
                )
            end do

        end if

        call this%sync_particles

    contains
        PURE function reduce_sum(arr1, arr2)
            real(p), intent(in) :: arr1(:)
            real(p), intent(in) :: arr2(size(arr1))

            real(p) :: reduce_sum(size(arr1))


            reduce_sum = arr1 + arr2
        end function reduce_sum
    end subroutine pair_operation

    subroutine individual_operation(this, update_func)
        class(replicated_distribution), intent(inout) :: this
        procedure(one_particle_function) :: update_func

        integer :: i


        if(disable_calculation) return

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


        call this%sync_particles

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


        if(this%rank .EQ. 0) write(*,*) string
    end subroutine print_string


    subroutine sync_particles(this)
        class(replicated_distribution), intent(inout) :: this

        integer :: rank
        integer :: chunk_size, chunk_start, chunk_end

        integer :: chunk_sizes(this%nprocs)
        integer :: chunk_starts(this%nprocs)

        integer :: MPI_particle

        integer :: ierror


        if(disable_mpi) return


        call generate_MPI_particle(MPI_particle)

        do rank=0, this%nprocs-1
            call this%get_chunk_data( &
                rank, chunk_sizes(rank+1), chunk_starts(rank+1), chunk_end &
            )
        end do
        ! Convert Fortran offsets to C offsets
        chunk_starts = chunk_starts -1

        call this%get_chunk_data( &
            this%rank, chunk_size, chunk_start, chunk_end &
        )

        ! Sync positions
        call MPI_Allgatherv( &
            this%particles(chunk_start), chunk_size, MPI_particle, &
            this%particles, chunk_sizes, chunk_starts, MPI_particle, &
            this%comm, ierror &
        )

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
