module systolic_distribution_type
    use abstract_distribution_type
    use particle_type
    use global_variables
    use mpi
    implicit none

    private

    public :: systolic_distribution
    public :: new_systolic_distribution

    type, EXTENDS(abstract_distribution) :: systolic_distribution
        integer, private :: num_particles
        integer, private :: num_local_particles

        type(particle), private, allocatable :: particles(:)
        type(particle), private, allocatable :: foreign_particles(:)
        type(particle), private, allocatable :: swap_particles(:)
        integer, private :: MPI_particle

        integer, private :: current_foreign_rank

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

        procedure, private :: do_systolic_pulse
        procedure, private :: get_chunk_data
    end type systolic_distribution

contains
    function new_systolic_distribution(particle_count, comm)
        type(systolic_distribution) :: new_systolic_distribution

        integer, intent(in) :: particle_count
        integer, intent(in) :: comm

        call new_systolic_distribution%init(particle_count, comm)
    end function new_systolic_distribution

    subroutine init(this, num_particles, comm)
        class(systolic_distribution), intent(inout) :: this
        integer, intent(in) :: num_particles
        integer, intent(in) :: comm

        integer :: chunk_size
        integer :: chunk_start
        integer :: chunk_end

        integer :: ierror


        call MPI_Comm_dup(comm, this%comm, ierror)
        call MPI_Comm_rank(this%comm, this%rank, ierror)
        call MPI_Comm_size(this%comm, this%nprocs, ierror)

        this%num_particles = num_particles
        call this%get_chunk_data(this%rank, chunk_size, chunk_start, chunk_end)
        this%num_local_particles = chunk_size

        call generate_MPI_particle(this%MPI_particle)

        ! Allocate as many particles as the largest chunk
        call this%get_chunk_data(0, chunk_size, chunk_start, chunk_end)
        allocate(this%particles(chunk_size))
        allocate(this%foreign_particles(chunk_size))
        allocate(this%swap_particles(chunk_size))
    end subroutine init

    subroutine pair_operation(this, compare_func, merge_func)
        class(systolic_distribution), intent(inout) :: this
        procedure(two_particle_function) :: compare_func
        procedure(two_particle_function) :: merge_func

        integer :: chunk_size, chunk_start, chunk_end

        type(particle) :: tmp_particle
        integer :: i, j

        integer :: pulse


        this%foreign_particles = this%particles

        this%current_foreign_rank = this%rank

        do pulse=0, this%nprocs-1
            if(pulse .NE. 0) call this%do_systolic_pulse

            call this%get_chunk_data( &
                this%current_foreign_rank, &
                chunk_size, chunk_start, chunk_end &
            )

            do i=1, this%num_local_particles
                do j=1, chunk_size
                    if(pulse .EQ. 0 .AND. i .EQ. j) cycle

                    tmp_particle = compare_func( &
                        this%particles(i), this%foreign_particles(j) &
                    )

                    this%particles(i) = merge_func( &
                        this%particles(i), tmp_particle &
                    )
                end do
            end do
        end do
    end subroutine pair_operation

    subroutine individual_operation(this, update_func)
        class(systolic_distribution), intent(inout) :: this
        procedure(one_particle_function) :: update_func

        integer :: chunk_size
        integer :: chunk_start
        integer :: chunk_end

        integer :: i


        call this%get_chunk_data(this%rank, chunk_size, chunk_start, chunk_end)

        do i=1, this%num_local_particles
            this%particles(i) = update_func(this%particles(i), chunk_start-1+i)
        end do
    end subroutine individual_operation

    subroutine global_map_reduce(this, map, reduce, reduce_value)
        class(systolic_distribution), intent(inout) :: this
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
            tmp_reduce_value, reduce_value, 1, MPI_REAL_P, &
            MPI_SUM, &
            this%comm, &
            ierror &
        )
    end subroutine

    subroutine print_particles(this, print_func)
        class(systolic_distribution), intent(inout) :: this
        procedure(print_particle_function) :: print_func

        integer :: chunk_size
        integer :: chunk_start
        integer :: chunk_end

        character(len=80) :: string
        integer :: i
        integer :: rank

        integer :: ierror


        call this%get_chunk_data(this%rank, chunk_size, chunk_start, chunk_end)

        call MPI_Barrier(this%comm, ierror)
        do rank=0, this%nprocs
            call MPI_Barrier(this%comm, ierror)
            if(this%rank .EQ. rank) then
                do i=1, this%num_local_particles
                    call print_func(this%particles(i), chunk_start-1+i, string)
                    write(*,'(A)') string
                end do
            end if
        end do
        call MPI_Barrier(this%comm, ierror)
    end subroutine print_particles

    subroutine print_string(this, string)
        class(systolic_distribution), intent(inout) :: this
        character(len=*), intent(in) :: string


        if(this%rank .EQ. 0) write(*,'(A)') string
    end subroutine print_string


    subroutine do_systolic_pulse(this)
        class(systolic_distribution), intent(inout) :: this

        integer :: chunk_size, chunk_start, chunk_end

        integer :: send_request
        integer :: recv_request

        integer :: ierror


        call this%get_chunk_data(0, chunk_size, chunk_start, chunk_end)

        this%swap_particles = this%foreign_particles

        ! Swap particles
        call MPI_Isend( &
            this%swap_particles, &
            chunk_size, &
            this%MPI_particle, &
            mod(this%rank+1, this%nprocs), 0, &
            this%comm, &
            send_request, &
            ierror &
        )
        call MPI_Irecv( &
            this%foreign_particles, &
            chunk_size, &
            this%MPI_particle, &
            mod(this%rank-1 + this%nprocs, this%nprocs), 0, &
            this%comm, &
            recv_request, &
            ierror &
        )

        call MPI_Waitall( &
            2, (/ send_request, recv_request /), &
            MPI_STATUSES_IGNORE, &
            ierror &
        )

        this%current_foreign_rank = mod( &
            this%current_foreign_rank-1 + this%nprocs, this%nprocs &
        )
    end subroutine do_systolic_pulse

    ! Get chunk size and boundary data for process i
    subroutine get_chunk_data(this, i, chunk_size, chunk_start, chunk_end)
        class(systolic_distribution), intent(inout) :: this
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

end module systolic_distribution_type
