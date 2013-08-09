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
    use bench_flags
    implicit none

    private

    ! Exported types
    public :: systolic_distribution

    ! Exported functions
    public :: new_systolic_distribution


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


        !
        ! Set up MPI variables
        !
        call MPI_Comm_dup(comm, this%comm, ierror)
        call MPI_Comm_rank(this%comm, this%rank, ierror)
        call MPI_Comm_size(this%comm, this%nprocs, ierror)

        ! Generate MPI derived type for particle
        call generate_MPI_particle(this%MPI_particle)

        ! Define reduction ops
        this%sum = MPI_SUM


        !
        ! Require that num_particles >= nprocs
        !
        if(num_particles .LT. this%nprocs) then
            call this%print_string("ERROR: num_particles < num_procs. Exiting.")
            call MPI_Abort(MPI_COMM_WORLD, 1, ierror)
        end if


        !
        ! Generate particle array data
        !

        ! Set particle list sizes
        this%num_particles = num_particles
        call this%get_chunk_data(this%rank, chunk_size, chunk_start, chunk_end)
        this%num_local_particles = chunk_size

        ! We allocate as many particles as the largest possible chunk
        ! (process 0 should have the largest possible chunk)
        call this%get_chunk_data(0, chunk_size, chunk_start, chunk_end)

        ! Do the allocations
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
    subroutine pair_operation( &
        this, pair_to_val, val_to_particle, reduce_op, reduction_identity &
    )
        class(systolic_distribution), intent(inout) :: this

        procedure(two_particle_to_array_function) :: pair_to_val
        procedure(particle_and_array_to_particle_function) :: val_to_particle
        integer :: reduce_op
        real(p) :: reduction_identity(:)

        integer :: N

        integer :: chunk_size, chunk_start, chunk_end

        logical, save :: tmp_vals_allocated = .FALSE.
        real(p), allocatable, save :: tmp_vals(:,:)
        integer :: i, j

        integer :: pulse


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


        ! allocate array for reduction values
        if(.NOT. tmp_vals_allocated) then
            allocate(tmp_vals(this%num_local_particles, N))
            tmp_vals_allocated = .TRUE.
        end if
        if(N .NE. size(tmp_vals, 2)) then
            deallocate(tmp_vals)
            allocate(tmp_vals(this%num_local_particles, N))
        end if


        this%foreign_particles = this%particles

        this%current_foreign_rank = this%rank

        ! Initialise reduction value list
        if(.NOT. disable_calculation) then
            do i=1, this%num_local_particles
                tmp_vals(i,:) = reduction_identity
            end do
        end if

        ! Do systolic loop
        do pulse=0, this%nprocs-1
            if(.NOT. disable_mpi) then
                if(pulse .NE. 0) call this%do_systolic_pulse
            end if

            if(.NOT. disable_calculation) then

                call this%get_chunk_data( &
                    this%current_foreign_rank, &
                    chunk_size, chunk_start, chunk_end &
                )

                do i=1, this%num_local_particles
                    do j=1, chunk_size
                        if(pulse .EQ. 0 .AND. i .EQ. j) cycle

                        tmp_vals(i,:) = reduce_sum( &
                            tmp_vals(i,:), pair_to_val( &
                                this%particles(i), &
                                this%foreign_particles(j), &
                                N &
                            ) &
                        )

                    end do
                end do
            end if
        end do

        ! Set particle values with reduction values
        if(.NOT. disable_calculation) then
            do i=1, this%num_local_particles
                this%particles(i) = val_to_particle( &
                    this%particles(i), tmp_vals(i,:), N &
                )
            end do
        end if
    contains
        PURE function reduce_sum(arr1, arr2)
            real(p), intent(in) :: arr1(:)
            real(p), intent(in) :: arr2(size(arr1))

            real(p) :: reduce_sum(size(arr1))


            reduce_sum = arr1 + arr2
        end function reduce_sum
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


        if(disable_calculation) return


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

        integer :: send_rank
        integer :: recv_rank

        integer :: ierror

        
        if(disable_mpi) return


        ! All processes send lists of the same size as process 0.
        call this%get_chunk_data(0, chunk_size, chunk_start, chunk_end)

        this%swap_particles = this%foreign_particles


        send_rank = mod(this%rank+1, this%nprocs)
        recv_rank = mod(this%rank-1 + this%nprocs, this%nprocs)


        ! Swap particles
        call MPI_Sendrecv( &
            this%swap_particles, chunk_size, this%MPI_particle, &
            send_rank, 0, &
            this%foreign_particles, chunk_size, this%MPI_particle, &
            recv_rank, 0, &
            this%comm, MPI_STATUS_IGNORE, ierror &
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
