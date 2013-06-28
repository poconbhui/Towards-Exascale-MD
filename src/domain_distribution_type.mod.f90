! MODULE domain_distribution_type
!
! Module provides the domain_distribution type.
! This is a domain decomposed implementation of an
! abstract_distribution.
!
module domain_distribution_type
    use abstract_distribution_type
    use particle_type
    use global_variables
    use mpi
    implicit none

    private

    ! Exported types
    public :: domain_distribution

    ! Exported functions
    public :: new_domain_distribution


    ! TYPE domain_distribution
    !
    ! The domain_distribution type splits the particle list up
    ! across processors based on the current position of the particle.
    ! Particles that are spatially close should be on a close processor.
    !
    type, EXTENDS(abstract_distribution) :: domain_distribution
        ! Particle list sizes
        integer, private :: num_particles
        integer, private :: num_local_particles

        ! Domain size
        real(p), private :: domain_size(Ndim)

        ! Particle lists
        integer, private, allocatable :: particle_ids(:)
        type(particle), private, allocatable :: particles(:)

        ! MPI data
        integer, private :: rank
        integer, private :: nprocs
        integer, private :: comm

    contains

        ! Methods inherited from abstract_distribution
        procedure :: init
        procedure :: pair_operation
        procedure :: individual_operation
        procedure :: global_map_reduce
        procedure :: print_particles
        procedure :: print_string

        ! Functions for getting particle lists from other processors
        ! and migrating particles between processors.
        procedure, private :: get_foreign_list
        procedure :: balance_lists
        procedure, private :: get_rank_by_particle
    end type domain_distribution

contains


    ! FUNCTION new_domain_distribution
    !
    ! Return a new initialized domain_distribution
    !
    function new_domain_distribution( &
      num_particles, domain_size, initial_distribution, comm &
    )
        type(domain_distribution) :: new_domain_distribution

        integer, intent(in) :: num_particles
        real(p), intent(in) :: domain_size(Ndim)
        procedure(one_particle_function) :: initial_distribution
        integer, intent(in) :: comm


        call new_domain_distribution%init( &
            num_particles, domain_size, initial_distribution, comm &
        )
    end function new_domain_distribution


    ! SUBROUTINE init
    !
    ! Initialize the domain_distribution.
    !
    ! initial_distribution is somewhat important so the lists are
    ! already relatively well balanced before any particles need to
    ! be migrated. If, for example, the position of particles were
    ! initially set to 0, process 0 may end up with the entire list.
    ! This may not be desirable.
    !
    subroutine init( &
        this, num_particles, domain_size, initial_distribution, comm &
    )
        class(domain_distribution), intent(out) :: this
        integer, intent(in) :: num_particles
        real(p), intent(in) :: domain_size(Ndim)
        procedure(one_particle_function) :: initial_distribution
        integer, intent(in) :: comm

        type(particle) :: tmp_particle
        integer :: chunk_size
        real(p) :: domain_start
        real(p) :: domain_end

        integer :: comm_dup

        integer :: i

        integer :: ierror


        !
        ! Set up MPI stuff
        !
        call MPI_Comm_dup(comm, comm_dup, ierror)
        call MPI_Comm_rank(comm_dup, this%rank, ierror)
        call MPI_Comm_size(comm_dup, this%nprocs, ierror)

        ! Set some distribution data
        this%num_particles = num_particles
        this%domain_size   = domain_size
        this%comm          = comm_dup

        !
        ! Find initial particle distribution size
        ! and initialize particles
        !
!        chunk_size = ceiling(real(this%num_particles)/this%nprocs)
!        this%num_local_particles = chunk_size
!        if((this%rank+1)*chunk_size .GT. num_particles) then
!            this%num_local_particles = this%num_local_particles &
!                - ((this%rank+1)*chunk_size - num_particles)
!        end if
        
        ! Domain decomposed over x direction
        chunk_size = 0
        domain_start = this%rank*domain_size(1)/this%nprocs
        domain_end   = (this%rank+1)*domain_size(1)/this%nprocs
        if(this%rank .EQ. this%nprocs-1) domain_end = domain_end + 1

        do i=1, num_particles
            tmp_particle = particle(pos=0, vel=0, force=0, mass=1)
            tmp_particle = initial_distribution(tmp_particle, i)

            if( this%get_rank_by_particle(tmp_particle) .EQ. this%rank) then
                chunk_size = chunk_size + 1

                call resize_particles(this, chunk_size)

                this%particle_ids(chunk_size) = i
                this%particles(chunk_size) = tmp_particle

                this%num_local_particles = chunk_size
            end if
        end do

    end subroutine init


    ! FUNCTION get_rank_by_particle
    !
    ! Accepts a particle and returns a processor number.
    !
    ! This function defines the decomposition pattern.
    ! By passing a particle to this function, the domain_distribution
    ! can tell which processor a given particle should be resident on.
    !
    function get_rank_by_particle(this, pi)
        integer :: get_rank_by_particle

        class(domain_distribution), intent(inout) :: this
        type(particle), intent(in) :: pi


        ! Distribute particles by x coordinate
        get_rank_by_particle = int( &
            abs(pi%pos(1))/(this%domain_size(1)/this%nprocs) &
        )

        if(get_rank_by_particle .GT. this%nprocs-1) then
            get_rank_by_particle = this%nprocs-1
        end if
    end function get_rank_by_particle


    ! SUBROUTINE resize_particles
    !
    ! This subroutine resizes the list of local particles.
    !
    ! Attempts have been made to reduce the frequency with thich
    ! the list must be reallocated. Generally, a reallocation will
    ! occur if the requested size is a factor of 2 out from the
    ! currently allocated size.
    !
    subroutine resize_particles(this, new_size_in)
        class(domain_distribution), intent(inout) :: this
        integer, intent(in) :: new_size_in

        integer :: new_size

        integer, allocatable :: old_particle_ids(:)
        type(particle), allocatable :: old_particles(:)


        new_size = new_size_in


        ! If new size is only slightly bigger, double current allocation
        ! to avoid potentially more allocations.
        !
        ! If new size is only slightly smaller, do nothing.

        if(.NOT. allocated(this%particles)) then
            allocate(this%particle_ids(new_size))
            allocate(this%particles(new_size))
        else if(new_size .GT. size(this%particles)) then
            allocate(old_particle_ids(size(this%particle_ids)))
            allocate(old_particles(size(this%particles)))

            old_particle_ids = this%particle_ids
            old_particles = this%particles

            deallocate(this%particle_ids)
            deallocate(this%particles)

            if(new_size .LT. 2*size(this%particles)) then
                new_size = 2*size(this%particles)
            end if

            allocate(this%particle_ids(new_size))
            allocate(this%particles(new_size))

            this%particle_ids(1:size(old_particles)) = old_particle_ids
            this%particles(1:size(old_particles)) = old_particles

            deallocate(old_particle_ids)
            deallocate(old_particles)
        else if(new_size .LT. 0.5*size(this%particles)) then
            allocate(old_particle_ids(new_size))
            allocate(old_particles(new_size))

            old_particle_ids = this%particle_ids(1:new_size)
            old_particles = this%particles(1:new_size)

            deallocate(this%particle_ids)
            deallocate(this%particles)
            allocate(this%particle_ids(new_size))
            allocate(this%particles(new_size))

            this%particle_ids(1:new_size) = old_particle_ids
            this%particles(1:new_size) = old_particles

            deallocate(old_particle_ids)
            deallocate(old_particles)
        end if
    end subroutine resize_particles


    ! SUBROUTINE pair_operation
    ! Extended from abstract_distribution
    !
    ! This subroutine initially performs a list balancing and
    ! then goes about comparing particles.
    !
    subroutine pair_operation(this, compare_func, merge_func)
        class(domain_distribution), intent(inout) :: this
        procedure(two_particle_function) :: compare_func
        procedure(two_particle_function) :: merge_func

        integer :: num_foreign_particles=0
        type(particle), allocatable :: foreign_list(:)

        type(particle) :: tmp_particle
        integer :: rank
        integer :: i, j


        call this%balance_lists

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


    ! SUBROUTINE individual_operation
    ! Extended from abstract_distribution
    !
    ! This subroutine performs the update_func on the local list
    ! of particles.
    !
    subroutine individual_operation(this, update_func)
        class(domain_distribution), intent(inout) :: this
        procedure(one_particle_function) :: update_func

        integer :: chunk_size
        integer :: chunk_start

        integer :: i


        chunk_size = ceiling(real(this%num_particles)/this%nprocs)
        chunk_start = this%rank*chunk_size+1

        do i=1, this%num_local_particles
            this%particles(i) = update_func( &
                this%particles(i), this%particle_ids(i) &
            )
        end do
    end subroutine individual_operation


    ! SUBROUTINE global_map_reduce
    !
    ! This performs the map/reduce over the local list of particles
    ! and then reduces values across all processors.
    !
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


    ! SUBROUTINE print_particles
    ! Extended from abstract_distribution
    !
    ! This routine has all processes print their list of particles.
    ! The order of particle output is not guaranteed.
    !
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
                        this%particle_ids(i), &
                        string &
                    )
                    write(*,'(A)') string
                end do
            end if
        end do
        call MPI_Barrier(this%comm, ierror)
    end subroutine print_particles


    ! SUBROUTINE print_string
    ! Extended from abstract_distribution
    !
    ! Process 0 outputs the requested string.
    !
    subroutine print_string(this, string)
        class(domain_distribution), intent(inout) :: this
        character(len=*), intent(in) :: string


        if(this%rank .EQ. 0) write(*,'(A)') string
    end subroutine print_string


    ! SUBROUTINE get_foreign_list
    !
    ! This subroutine gets the list of particles resident on process rank
    ! and copies it into foreign_list.
    !
    ! Currently, it is expected that all processes call this
    ! simultaneously with the same value for rank.
    !
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


    ! SUBROUTINE balance_lists
    !
    ! This subroutine updates the particle lists on all processors
    ! according to where get_rank_from_particle says they should be.
    !
    subroutine balance_lists(this)
        class(domain_distribution), intent(inout) :: this

        integer :: old_particle_ids(size(this%particles))
        type(particle) :: old_particles(size(this%particles))

        integer :: send_sizes(this%nprocs)
        integer :: recv_sizes(this%nprocs)
        integer :: send_index_list(this%nprocs, this%num_local_particles)
        integer, allocatable :: send_particle_ids(:)
        type(particle), allocatable :: send_particles(:)

        integer :: MPI_particle

        integer :: rank
        integer :: i

        integer :: ierror


        old_particle_ids = this%particle_ids
        old_particles = this%particles


        call generate_MPI_particle(MPI_particle)

        
        ! Find out what needs to be sent
        send_sizes = 0
        send_index_list = -1
        do i=1, this%num_local_particles
            rank = this%get_rank_by_particle(this%particles(i))
            send_sizes(rank+1) = send_sizes(rank+1) + 1
            send_index_list(rank+1, send_sizes(rank+1)) = i
        end do


        do rank=0, this%nprocs-1
            ! Find sizes of incoming list
            call MPI_Gather( &
                send_sizes(rank+1), 1, MPI_INTEGER, &
                recv_sizes, 1, MPI_INTEGER, &
                rank, this%comm, &
                ierror &
            )


            if(rank .EQ. this%rank) then
                ! Resize local arrays to receiving sizes
                call resize_particles(this, sum(send_sizes))
                this%num_local_particles = sum(send_sizes)
            end if


            ! Allocate send buffers.
            if(allocated(send_particle_ids)) then
                deallocate(send_particle_ids, send_particles)
            end if
            allocate(send_particle_ids(send_sizes(rank+1)))
            allocate(send_particles(send_sizes(rank+1)))


            ! Copy particles to send buffers
            send_particle_ids(1:send_sizes(rank+1)) = old_particle_ids( &
                send_index_list(rank+1,1:send_sizes(rank+1)) &
            )
            send_particles(1:send_sizes(rank+1)) = old_particles( &
                send_index_list(rank+1,1:send_sizes(rank+1)) &
            )


            ! Send/receive particle lists
            call MPI_Gatherv( &
                send_particle_ids, send_sizes(rank+1), MPI_INTEGER, &
                this%particle_ids, recv_sizes, num_to_off(recv_sizes), &
                MPI_INTEGER, &
                rank, this%comm, &
                ierror &
            )
            call MPI_Gatherv( &
                send_particles, send_sizes(rank+1), MPI_particle, &
                this%particles, recv_sizes, num_to_off(recv_sizes), &
                MPI_particle, &
                rank, this%comm, &
                ierror &
            )

        end do

    contains
        ! Return a sum scan across sizes, minus the first element.
        PURE function num_to_off(sizes)
            integer, intent(in) :: sizes(:)
            integer :: num_to_off(size(sizes))

            integer :: i
            integer :: offset

            offset=0
            num_to_off = 0
            do i=1, size(sizes)
                num_to_off(i) = offset
                offset = offset+sizes(i)
            end do
        end function num_to_off
    end subroutine balance_lists

end module domain_distribution_type
