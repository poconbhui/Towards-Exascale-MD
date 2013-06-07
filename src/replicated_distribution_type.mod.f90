module replicated_distribution_type
    use abstract_distribution_type
    use particle_type
    use global_variables
    use mpi
    implicit none

    private

    public :: replicated_distribution

    type, EXTENDS(abstract_distribution) :: replicated_distribution
        integer, private :: num_particles

        real(p), private, allocatable :: particle_pos(:,:)
        real(p), private, allocatable :: particle_vel(:,:)
        real(p), private, allocatable :: particle_force(:,:)
        real(p), private, allocatable :: particle_mass(:)

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

        procedure :: sync_particles

        procedure, private :: get_particle
        procedure, private :: set_particle
        procedure, private :: get_chunk_data
    end type replicated_distribution
    interface replicated_distribution
        module procedure constructor
    end interface replicated_distribution

contains
    function constructor(particle_count, comm)
        type(replicated_distribution) :: constructor
        integer, intent(in) :: particle_count
        integer, intent(in) :: comm

        call constructor%init(particle_count, comm)
    end function constructor

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

        allocate(this%particle_pos(Ndim,num_particles))
        allocate(this%particle_vel(Ndim,num_particles))
        allocate(this%particle_force(Ndim,num_particles))
        allocate(this%particle_mass(num_particles))
    end subroutine init

    subroutine pair_operation(this, compare_func, merge_func)
        class(replicated_distribution), intent(inout) :: this
        procedure(two_particle_function) :: compare_func
        procedure(two_particle_function) :: merge_func

        integer :: i_size, i_start, i_end
        type(particle) :: tmp_particle
        type(particle) :: particle_i, particle_j
        integer :: i, j


        call this%get_chunk_data(this%rank, i_size, i_start, i_end)

        do i=i_start, i_end
            do j=1, this%num_particles
                if(i .EQ. j) cycle

                call this%get_particle(i, particle_i)
                call this%get_particle(j, particle_j)

                tmp_particle = compare_func( &
                    particle_i, particle_j &
                )
                call this%set_particle( &
                    i, merge_func(particle_i, tmp_particle) &
                )
            end do
        end do

        call this%sync_particles
    end subroutine pair_operation

    subroutine individual_operation(this, update_func)
        class(replicated_distribution), intent(inout) :: this
        procedure(one_particle_function) :: update_func

        type(particle) :: particle_i
        integer :: i


        do i=1, this%num_particles
            call this%get_particle(i, particle_i)
            call this%set_particle(i, update_func(particle_i, i))
        end do
    end subroutine individual_operation

    subroutine global_map_reduce(this, map, reduce, reduce_value)
        class(replicated_distribution), intent(inout) :: this
        procedure(global_map_function) :: map
        procedure(global_reduce_function) :: reduce
        real(p), intent(inout) :: reduce_value

        type(particle) :: particle_i
        integer :: i


        do i=1, this%num_particles
            call this%get_particle(i, particle_i)
            reduce_value = reduce( reduce_value, map(particle_i) )
        end do
    end subroutine

    subroutine print_particles(this, print_func)
        class(replicated_distribution), intent(inout) :: this
        procedure(print_particle_function) :: print_func

        character(len=80) :: string
        type(particle) :: particle_i
        integer :: i


        if(this%rank .EQ. 0) then
            do i=1, this%num_particles
                call this%get_particle(i, particle_i)
                call print_func(particle_i, i, string)
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

        integer :: ierror
        integer :: rank
        integer :: chunk_size, chunk_start, chunk_end


        do rank=0, this%nprocs-1
            call this%get_chunk_data(rank, chunk_size, chunk_start, chunk_end)

            ! Sync positions
            call MPI_Bcast( &
                this%particle_pos(:,chunk_start:chunk_end), &
                size(this%particle_pos,1)*chunk_size, &
                MPI_REAL_P, &
                rank, &
                this%comm, &
                ierror &
            )

            ! Sync velocities
            call MPI_Bcast( &
                this%particle_vel(:,chunk_start:chunk_end), &
                size(this%particle_vel,1)*chunk_size, &
                MPI_REAL_P, &
                rank, &
                this%comm, &
                ierror &
            )

            ! Sync forces
            call MPI_Bcast( &
                this%particle_force(:,chunk_start:chunk_end), &
                size(this%particle_force,1)*chunk_size, &
                MPI_REAL_P, &
                rank, &
                this%comm, &
                ierror &
            )

            ! Sync masses
            call MPI_Bcast( &
                this%particle_mass(chunk_start:chunk_end), &
                chunk_size, &
                MPI_REAL_P, &
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


    subroutine get_particle(this, i, p)
        class(replicated_distribution), intent(inout) :: this
        integer, intent(in) :: i
        type(particle), intent(out) :: p


        p = particle( &
            pos=this%particle_pos(:,i), &
            vel=this%particle_vel(:,i), &
            force=this%particle_force(:,i), &
            mass=this%particle_mass(i) &
        )
    end subroutine get_particle

    subroutine set_particle(this, i, p)
        class(replicated_distribution), intent(inout) :: this
        integer, intent(in) :: i
        type(particle), intent(in) :: p


        this%particle_pos(:,i) = p%pos
        this%particle_vel(:,i) = p%vel
        this%particle_force(:,i) = p%force
        this%particle_mass(i) = p%mass
    end subroutine set_particle

end module replicated_distribution_type
