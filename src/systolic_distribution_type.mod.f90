! MODULE systolic_distribution_type
!
! This module provides the systolic_distribution type, which
! should implement lists in a distributed fashion and perform
! pair_operations using the systolic loop approach
module systolic_distribution_type
    use abstract_distribution_type
    use particle_type
    use global_variables
    use mpi
    implicit none

    private

    ! Exported types
    public :: systolic_distribution

    ! Exported functions
    public :: new_systolic_distribution

    
    procedure(global_reduce_function), pointer, private :: local_reduce_op


    ! TYPE systolic_distribution
    !
    ! The systolic_distribution type splits the particle list up
    ! among processors.
    !
    ! Individual updates can be performed locally on each process.
    ! List comparisons are performed using systolic pulses.
    !
    type, EXTENDS(abstract_distribution) :: systolic_distribution
        ! Particle list size data
        integer, private :: num_particles
        integer, private :: num_local_particles

        ! Particle lists
        type(particle), private, allocatable :: particles(:)
        type(particle), private, allocatable :: foreign_particles(:)
        type(particle), private, allocatable :: swap_particles(:)

        ! MPI derived datatypes
        integer, private :: MPI_particle

        ! Systolic pulse tracking
        integer, private :: current_foreign_rank

        ! MPI data
        integer, private :: rank
        integer, private :: nprocs
        integer, private :: comm

    contains

        ! Members inherited from abstract_distribution
        procedure :: init
        procedure :: pair_operation
        procedure :: individual_operation
        procedure :: global_map_reduce
        procedure :: print_particles
        procedure :: print_string

        ! Members for performing pulses and getting pulse sizes
        procedure, private :: do_systolic_pulse
        procedure, private :: get_chunk_data
    end type systolic_distribution

contains


    ! FUNCTION new_systolic_distribution
    !
    ! Return a new initialized systolic_distribution
    !
    function new_systolic_distribution(particle_count, comm)
        type(systolic_distribution) :: new_systolic_distribution

        integer, intent(in) :: particle_count
        integer, intent(in) :: comm

        call new_systolic_distribution%init(particle_count, comm)
    end function new_systolic_distribution


    ! SUBROUTINE init
    !
    ! Initialize the systolic_distribution
    !
    subroutine init(this, num_particles, comm)
        class(systolic_distribution), intent(inout) :: this
        integer, intent(in) :: num_particles
        integer, intent(in) :: comm

        integer :: chunk_size
        integer :: chunk_start
        integer :: chunk_end

        integer :: ierror


        ! Set up MPI variables
        call MPI_Comm_dup(comm, this%comm, ierror)
        call MPI_Comm_rank(this%comm, this%rank, ierror)
        call MPI_Comm_size(this%comm, this%nprocs, ierror)

        ! Set particle list sizes
        this%num_particles = num_particles
        call this%get_chunk_data(this%rank, chunk_size, chunk_start, chunk_end)
        this%num_local_particles = chunk_size

        ! Set MPI derived type for particle
        call generate_MPI_particle(this%MPI_particle)

        ! We allocate as many particles as the largest possible chunk
        ! (process 0 should have the largest possible chunk)
        call this%get_chunk_data(0, chunk_size, chunk_start, chunk_end)

        ! Allocate particle arrays
        allocate(this%particles(chunk_size))
        allocate(this%foreign_particles(chunk_size))
        allocate(this%swap_particles(chunk_size))
    end subroutine init


    ! SUBROUTINE pair_operation
    ! Extended from abstract_distribution
    !
    ! pair_operation implements the list comparison algorithm using
    ! a systolic pulse approach.
    !
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


    ! SUBROUTINE individual_operation
    ! Extended from abstract_distribution
    !
    ! individual_operation updates the local list of particles.
    !
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


    ! SUBROUTINE global_map_reduce
    ! Extended from abstract_distribution
    !
    ! global_map_reduce performs map/reduce over all local particles
    ! and then reduces values over all processes.
    !
    subroutine global_map_reduce(this, map, reduce, reduce_value)
        class(systolic_distribution), intent(inout) :: this
        procedure(global_map_function) :: map
        procedure(global_reduce_function) :: reduce
        real(p), intent(inout) :: reduce_value

        real(p) :: tmp_reduce_value
        integer :: i
        integer :: ierror

        integer :: myop


        do i=1, this%num_local_particles
            reduce_value = reduce( reduce_value, map(this%particles(i)) )
        end do

        tmp_reduce_value = reduce_value


        myop = MPI_reduce_binding(reduce)
        call MPI_Allreduce( &
            tmp_reduce_value, reduce_value, 1, MPI_REAL_P, &
            myop, &
            this%comm, &
            ierror &
        )
    end subroutine


    ! FUNCTION MPI_reduce_binding
    !
    ! This functions returns an MPI_Op that can be used for
    ! MPI_Allreduce.
    !
    ! This is a slightly disgusting hack. We define a function with
    ! the signature necessary for an MPI_User_function which then
    ! calls a previously set function pointer with the binding we want
    ! out users to use.
    !
    ! This function sets that local function pointer, and returns the
    ! MPI_Op. If necessary, this function will register the binding
    ! with MPI.
    !
    ! It should be noted that this will only work for one binding at a time,
    ! if even. I'm not quite brave enough to come up with a fix.
    !
    function MPI_reduce_binding(reduce_op)
        integer :: MPI_reduce_binding

        procedure(global_reduce_function) :: reduce_op

        logical, save :: first_entry = .TRUE.
        integer, save :: saved_binding

        integer :: ierror


        local_reduce_op => reduce_op

        if(first_entry) then
            call MPI_Op_create( &
                mpi_exec_local_reduce_op, .TRUE., saved_binding, ierror &
            )

            first_entry = .FALSE.
        end if

        MPI_reduce_binding = saved_binding
    end function MPI_reduce_binding


    ! FUNCTION mpi_exec_local_reduce_op
    !
    ! This is the function registered with MPI to call the reduce operation
    ! with the binding we want our users to use.
    !
    ! This function should only be called by the MPI library.
    !
    function mpi_exec_local_reduce_op(in, inout, len, type)
        integer mpi_exec_local_reduce_op

        real(p) :: in(len), inout(len)
        integer :: len
        integer :: type

        integer :: i


        do i=1, len
            inout(i) = local_reduce_op(inout(i), in(i));
        end do

        mpi_exec_local_reduce_op = MPI_SUCCESS
    end function mpi_exec_local_reduce_op


    ! SUBROUTINE print_particles
    ! Extended form abstract_distribution
    !
    ! This routine gets all processes to output their list of
    ! particles in turn.
    !
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


    ! SUBROUTINE print_string
    ! Extended from abstract_distribution
    !
    ! This simply has process 0 output the string.
    !
    subroutine print_string(this, string)
        class(systolic_distribution), intent(inout) :: this
        character(len=*), intent(in) :: string


        if(this%rank .EQ. 0) write(*,'(A)') string
    end subroutine print_string


    ! SUBROUTINE do_systolic_pulse
    !
    ! This subroutine sends the current contents of foreign_particles
    ! to the foreign_particles list on the process to the "left" and
    ! then sets the contents of the foreign_particles list to the
    ! foreign_particles list on the process to the "right".
    !
    ! Effectively, this looks like the contents of the foreign_particles
    ! buffer on each process moves to the foreign_particles buffer on
    ! the next processor in the ring.
    !
    subroutine do_systolic_pulse(this)
        class(systolic_distribution), intent(inout) :: this

        integer :: chunk_size, chunk_start, chunk_end

        integer :: send_request
        integer :: recv_request

        integer :: ierror


        ! All processes send lists of the same size as process 0.
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


    ! SUBROUTINE get_chunk_data
    !
    ! Gets chunk size and boundary data for process i.
    !
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
