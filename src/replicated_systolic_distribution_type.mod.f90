! MODULE replicated_systolic_distribution_type
!
! This module provides the replicated_systolic_distribution type, which
! should implement lists in a distributed fashion and perform
! pair_operations using a combination of the replicated data
! and systolic loop approaches
!
module replicated_systolic_distribution_type
    use abstract_distribution_type
    use particle_type
    use global_variables
    use mpi
    use bench_flags
    implicit none

    private

    ! Exported types
    public :: replicated_systolic_distribution

    ! Exported functions
    public :: new_replicated_systolic_distribution


    ! TYPE replicated_systolic_distribution
    !
    ! The replicated_systolic_distribution type splits the particle list up
    ! among processors and across several replica systolic loops.
    !
    ! Individual updates can be performed locally on each process.
    !
    ! List comparisons are performed by using several systolic rings
    ! and having each do a subset of the systolic list comparisons
    ! and then reduce these partial comparisons together.
    !
    type, EXTENDS(abstract_distribution) :: replicated_systolic_distribution
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

        ! MPI data about position on the local systolic ring
        integer, private :: local_ring_rank
        integer, private :: local_ring_nprocs
        integer, private :: local_ring_comm

        ! MPI data about equivalent ring elements on different rings
        integer, private :: equiv_elem_rank
        integer, private :: equiv_elem_nprocs
        integer, private :: equiv_elem_comm


    contains

        ! Members inherited from abstract_distribution
        procedure :: init
        procedure :: pair_operation
        procedure :: individual_operation
        procedure :: global_map_reduce
        procedure :: print_particles
        procedure :: print_string

        ! Members for performing pulses
        procedure, private :: do_initial_swap
        procedure, private :: do_systolic_pulse

        ! Members for getting array subset sizes and offsets
        procedure, private :: get_pulse_offsets
        procedure, private :: get_chunk_data
    end type replicated_systolic_distribution

contains


    ! FUNCTION new_replicated_systolic_distribution
    !
    ! Return a new initialized replicated_systolic_distribution
    !
    function new_replicated_systolic_distribution(particle_count, comm)
        type(replicated_systolic_distribution) &
            :: new_replicated_systolic_distribution

        integer, intent(in) :: particle_count
        integer, intent(in) :: comm

        call new_replicated_systolic_distribution%init(particle_count, comm)
    end function new_replicated_systolic_distribution


    ! SUBROUTINE init
    !
    ! Initialize the replicated_systolic_distribution
    !
    subroutine init(this, num_particles, comm)
        class(replicated_systolic_distribution), intent(inout) :: this
        integer, intent(in) :: num_particles
        integer, intent(in) :: comm

        integer :: chunk_size
        integer :: chunk_start
        integer :: chunk_end

        integer :: global_nprocs

        integer :: dims(2)
        integer :: coords(2)
        logical :: periods(2)

        integer :: cart_comm
        integer :: cart_rank

        integer :: ierror


        !
        ! Set up MPI variables
        !

        ! Generate MPI derived type for particle
        call generate_MPI_particle(this%MPI_particle)

        ! Define reduction ops
        this%sum = MPI_SUM


        !
        ! Generate replicated systolic loop comms
        !

        call MPI_Comm_size(comm, global_nprocs, ierror)

        ! Create a balanced 2d topology
        dims = 0
        call MPI_Dims_create(global_nprocs, 2, dims, ierror)

        ! Want dims(1) >= dims(2) because
        ! there should be more loop elements than replica loops
        if(dims(1) .LT. dims(2)) then
            ierror  = dims(1)
            dims(1) = dims(2)
            dims(2) = ierror
        end if

        ! Systolic loop is periodic, replicas elements aren't
        periods = (/ .TRUE., .FALSE. /)

        call MPI_Cart_create( &
            comm, 2, dims, periods, &
            .TRUE., cart_comm, ierror &
        )

        ! Get position in 2d topology
        call MPI_Comm_rank(cart_comm, cart_rank, ierror)

        call MPI_Cart_get(cart_comm, 2, dims, periods, coords, ierror)


        ! Generate local loop info
        ! Choose largest dim from cart_comm to put the ring across.
        call MPI_Comm_split( &
            cart_comm, coords(2), 0, this%local_ring_comm, ierror &
        )
        call MPI_Comm_rank(this%local_ring_comm, this%local_ring_rank, ierror)
        call MPI_Comm_size(this%local_ring_comm, this%local_ring_nprocs, ierror)


        ! Generate equivalent element info
        ! Processes with the same local_ring_rank are considered equivalent
        call MPI_Comm_split( &
            cart_comm, this%local_ring_rank, 0, this%equiv_elem_comm, ierror &
        )
        call MPI_Comm_rank(this%equiv_elem_comm, this%equiv_elem_rank, ierror)
        call MPI_Comm_size(this%equiv_elem_comm, this%equiv_elem_nprocs, ierror)


        ! Free Cartesian comm
        call MPI_Comm_free(cart_comm, ierror)


        !
        ! Require that num_particles >= local_ring_nprocs
        !
        if(num_particles .LT. this%local_ring_nprocs) then
            call this%print_string( &
                "ERROR: num_particles < local_ring_num_procs. Exiting." &
            )
            call MPI_Abort(MPI_COMM_WORLD, 1, ierror)
        end if


        !
        ! Generate particle array data
        !

        ! Set particle list sizes
        this%num_particles = num_particles
        call this%get_chunk_data( &
            this%local_ring_rank, chunk_size, chunk_start, chunk_end &
        )
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
    ! a replicated data and systolic pulse approach.
    !
    subroutine pair_operation( &
        this, pair_to_val, val_to_particle, reduce_op, reduction_identity &
    )
        class(replicated_systolic_distribution), intent(inout) :: this

        procedure(two_particle_to_array_subroutine) :: pair_to_val
        procedure(particle_and_array_to_particle_function) :: val_to_particle
        integer :: reduce_op
        real(p) :: reduction_identity(:)

        integer :: chunk_size, chunk_start, chunk_end

        ! Arrays of data to be reduced
        !
        ! local_reduce_vals and global_reduce_vals are allocated
        ! the first time this routine is called and kept allocated.
        ! They are only reallocated if this is called with a larger
        ! reduction_identity array.
        !
        logical, save :: reduce_vals_allocated = .FALSE.
        real(p), allocatable, save :: local_reduce_vals(:, :)
        real(p), allocatable, save :: global_reduce_vals(:, :)
        real(p) :: tmp_val(size(reduction_identity))

        ! Pulse offset variables
        integer :: pulse_offset_start
        integer :: pulse_offset_end

        ! Iterators
        integer :: pulse
        integer :: i, j

        integer :: ierror


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
            stop 1
        end if


        !
        ! allocate arrays for reduction operations
        !
        if(.NOT. reduce_vals_allocated) then
            allocate( &
                local_reduce_vals( &
                    size(reduction_identity), this%num_local_particles &
                ) &
            )
            allocate( &
                global_reduce_vals( &
                    size(reduction_identity), this%num_local_particles &
                ) &
            )
            reduce_vals_allocated = .TRUE.
        end if
        if(size(reduction_identity) .GT. size(local_reduce_vals, 1)) then
            deallocate(local_reduce_vals)
            deallocate(global_reduce_vals)
            allocate( &
                local_reduce_vals( &
                    size(reduction_identity), this%num_local_particles &
                ) &
            )
            allocate( &
                global_reduce_vals( &
                    size(reduction_identity), this%num_local_particles &
                ) &
            )
        end if


        !
        ! Find the offsets for the pulses this ring will be
        ! dealing with.
        !
        call this%get_pulse_offsets(pulse_offset_start, pulse_offset_end)


        !
        ! Do initial swap
        !
        if(.NOT. disable_mpi) then
            call this%do_initial_swap(pulse_offset_start)
        end if


        !
        ! Do the systolic pulses and comparisons
        !

        ! Initialise local reduction value list
        if(.NOT. disable_calculation) then
            do i=1, this%num_local_particles
                local_reduce_vals(:, i) = reduction_identity
            end do
        end if

        ! Loop over our subset of the systolic loop
        do pulse=pulse_offset_start, pulse_offset_end-1

            if(.NOT. disable_mpi) then


                ! Do the systolic pulse
                !
                ! Don't do pulse on the first iteration
                ! because the foreign list has already been set to
                ! a good value
                !
                if(pulse .NE. pulse_offset_start) call this%do_systolic_pulse

            end if

            if(.NOT. disable_calculation) then

                ! Get array bounds of current foreign list
                call this%get_chunk_data( &
                    this%current_foreign_rank, &
                    chunk_size, chunk_start, chunk_end &
                )

                ! Do the list comparison
                do i=1, this%num_local_particles
                    do j=1, chunk_size
                        if( &
                            this%current_foreign_rank &
                            .EQ. this%local_ring_rank &
                            .AND. &
                            i .EQ. j &
                        ) cycle

                        call pair_to_val( &
                            this%particles(i), this%foreign_particles(j), &
                            tmp_val &
                        )

                        local_reduce_vals(:, i) = reduce_func( &
                            tmp_val, local_reduce_vals(:, i) &
                        )

                    end do
                end do

            end if  !(.NOT. disable_calculation)

        end do !pulse=pulse_offset_start, pulse_offset_end-1


        !
        ! Globally reduce values and set values for local particles.
        !

        if(.NOT. disable_mpi) then

            ! Reduce partial solutions across equivalent elements
            call MPI_Allreduce( &
                local_reduce_vals, global_reduce_vals, &
                size(reduction_identity)*this%num_local_particles, MPI_REAL_P, &
                reduce_op, this%equiv_elem_comm, ierror &
            )

        end if

        ! Set particle values with global reduction values
        if(.NOT. disable_calculation) then
            do i=1, this%num_local_particles

                this%particles(i) = val_to_particle( &
                    this%particles(i), global_reduce_vals(:, i) &
                )

            end do
        end if
    end subroutine pair_operation


    ! FUNCTION reduce_sum
    !
    ! This function accepts two arrays and sums them elementwise.
    !
    ! This is used by pair_operation so the reduce_op used can
    ! be changed dynamically.
    !
    PURE function reduce_sum(arr1, arr2)
        real(p), intent(in) :: arr1(:)
        real(p), intent(in) :: arr2(size(arr1))

        real(p) :: reduce_sum(size(arr1))


        reduce_sum = arr1 + arr2
    end function reduce_sum


    ! SUBROUTINE individual_operation
    ! Extended from abstract_distribution
    !
    ! individual_operation updates the local list of particles.
    !
    subroutine individual_operation(this, update_func)
        class(replicated_systolic_distribution), intent(inout) :: this
        procedure(one_particle_function) :: update_func

        integer :: chunk_size
        integer :: chunk_start
        integer :: chunk_end

        integer :: i


        if(disable_calculation) return


        call this%get_chunk_data( &
            this%local_ring_rank, chunk_size, chunk_start, chunk_end &
        )

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
        class(replicated_systolic_distribution), intent(inout) :: this
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
            this%local_ring_comm, &
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
        class(replicated_systolic_distribution), intent(inout) :: this
        procedure(print_particle_function) :: print_func

        integer :: chunk_size
        integer :: chunk_start
        integer :: chunk_end

        character(len=80) :: string
        integer :: i
        integer :: rank

        integer :: ierror


        call this%get_chunk_data( &
            this%local_ring_rank, chunk_size, chunk_start, chunk_end &
        )

        ! Print only for loop 0
        if(this%equiv_elem_rank .NE. 0) return

        ! Loop over local_ring_ranks
        call MPI_Barrier(this%local_ring_comm, ierror)
        do rank=0, this%local_ring_nprocs
            call MPI_Barrier(this%local_ring_comm, ierror)
            if(this%local_ring_rank .EQ. rank) then
                do i=1, this%num_local_particles
                    call print_func(this%particles(i), chunk_start-1+i, string)
                    write(*,'(A)') string
                end do
            end if
        end do
        call MPI_Barrier(this%local_ring_comm, ierror)
    end subroutine print_particles


    ! SUBROUTINE print_string
    ! Extended from abstract_distribution
    !
    ! This simply has process 0 output the string.
    !
    subroutine print_string(this, string)
        class(replicated_systolic_distribution), intent(inout) :: this
        character(len=*), intent(in) :: string


        if( &
            this%local_ring_rank .EQ. 0 &
            .AND. &
            this%equiv_elem_rank .EQ. 0 &
        ) then
            write(*,'(A)') string
        end if
    end subroutine print_string


    ! SUBROUTINE do_initial_swap
    !
    ! This subroutine is used for initialising a replicated systolic
    ! loop.
    !
    ! Each element of the local_ring will send it's local list of particles
    ! to the element "offset" to the right and have its foreign_particles
    ! set to the local list from "offset" to the left.
    !
    ! This should look similar to performing "offset" systolic pulses
    ! in one go with swap_particles = particles
    !
    subroutine do_initial_swap(this, offset)
        class(replicated_systolic_distribution), intent(inout) :: this
        integer, intent(in) :: offset
        
        integer :: initial_send
        integer :: initial_recv

        integer :: chunk_size
        integer :: chunk_start
        integer :: chunk_end

        integer :: ierror


        ! Find initial swap ranks
        initial_send = mod( &
            (this%local_ring_rank + offset), &
            this%local_ring_nprocs &
        )
        initial_recv = mod( &
            (this%local_ring_rank - offset + this%local_ring_nprocs), &
            this%local_ring_nprocs &
        )

        ! Get swap_particles and foreign_particles bounds
        call this%get_chunk_data( &
            0, chunk_size, chunk_start, chunk_end &
        )

        ! Swap particles
        call MPI_Sendrecv( &
            this%particles, chunk_size, this%MPI_particle, &
            initial_send, 0, &
            this%foreign_particles, chunk_size, this%MPI_particle, &
            initial_recv, 0, &
            this%local_ring_comm, MPI_STATUS_IGNORE, ierror &
        )

        ! Initialise the foreign_particles rank to the just received rank
        this%current_foreign_rank = initial_recv
    end subroutine do_initial_swap


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
        class(replicated_systolic_distribution), intent(inout) :: this

        integer :: chunk_size, chunk_start, chunk_end

        integer :: send_rank
        integer :: recv_rank

        integer :: ierror

        
        if(disable_mpi) return


        ! All processes send lists of the same size as process 0.
        call this%get_chunk_data(0, chunk_size, chunk_start, chunk_end)

        this%swap_particles = this%foreign_particles


        send_rank = mod(this%local_ring_rank+1, this%local_ring_nprocs)
        recv_rank = mod( &
            this%local_ring_rank-1 + this%local_ring_nprocs, &
            this%local_ring_nprocs &
        )


        ! Swap particles
        call MPI_Sendrecv( &
            this%swap_particles, chunk_size, this%MPI_particle, &
            send_rank, 0, &
            this%foreign_particles, chunk_size, this%MPI_particle, &
            recv_rank, 0, &
            this%local_ring_comm, MPI_STATUS_IGNORE, ierror &
        )


        this%current_foreign_rank = mod( &
            this%current_foreign_rank-1 + this%local_ring_nprocs, &
            this%local_ring_nprocs &
        )
    end subroutine do_systolic_pulse


    ! SUBROUTINE get_pulse_offsets
    !
    ! Given the size of the local systolic loop and the number of
    ! replica loops, this function determines how far away the initial
    ! systolic pulse should send the local particle list and how
    ! far away the last systolic pulse received should be coming from.
    !
    ! pulse_offset_end - pulse_offset_start should be the number of
    ! systolic pulses performed by the local systolic loop, and
    ! pulse_offset_start is how far away the initial data should be
    ! coming from.
    !
    subroutine get_pulse_offsets(this, pulse_offset_start, pulse_offset_end)
        class(replicated_systolic_distribution), intent(inout) :: this
        integer, intent(out) :: pulse_offset_start
        integer, intent(out) :: pulse_offset_end

        integer :: num_pulse_chunk


        ! Find maximum chunk size
        num_pulse_chunk = &
            (this%local_ring_nprocs + this%equiv_elem_nprocs -1) &
            / this%equiv_elem_nprocs

        ! Find pulse offsets
        pulse_offset_start = this%equiv_elem_rank*num_pulse_chunk
        pulse_offset_end   = (this%equiv_elem_rank+1)*num_pulse_chunk
        if(this%equiv_elem_rank .EQ. this%equiv_elem_nprocs-1) then
            pulse_offset_end = this%local_ring_nprocs
        end if
    end subroutine get_pulse_offsets



    ! SUBROUTINE get_chunk_data
    !
    ! Gets chunk size and boundary data for process i.
    !
    subroutine get_chunk_data(this, i, chunk_size, chunk_start, chunk_end)
        class(replicated_systolic_distribution), intent(inout) :: this
        integer, intent(in) :: i
        integer, intent(out) :: chunk_size
        integer, intent(out) :: chunk_start
        integer, intent(out) :: chunk_end

        
        ! Get basic sizes
        chunk_size = ceiling(real(this%num_particles)/this%local_ring_nprocs)
        chunk_start = i*chunk_size + 1
        chunk_end = (i+1)*chunk_size

        ! Fix sizes
        if(chunk_end .GT. this%num_particles) chunk_end = this%num_particles
        chunk_size = chunk_end - chunk_start + 1
    end subroutine get_chunk_data

end module replicated_systolic_distribution_type
