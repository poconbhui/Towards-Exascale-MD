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

        procedure :: sync_particles
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

    subroutine init(this, particle_count, comm)
        class(replicated_distribution), intent(out) :: this
        integer, intent(in) :: particle_count
        integer, intent(in) :: comm

        integer :: ierror


        this%num_particles = particle_count
        this%comm = comm

        call MPI_Comm_rank(comm, this%rank, ierror)
        call MPI_Comm_size(comm, this%nprocs, ierror)

        allocate(this%particles(this%num_particles))
    end subroutine init

    subroutine pair_operation(this, compare_func, merge_func)
        class(replicated_distribution), intent(inout) :: this
        procedure(two_particle_function) :: compare_func
        procedure(two_particle_function) :: merge_func

        integer :: i_size, i_start, i_end
        type(particle) :: tmp_particle
        integer :: i, j


        ! Cheating for the moment
        ! i_size = ceiling(real(this%num_particles)/this%nprocs)
        ! i_start = this%rank*i_size + 1
        ! i_end   = (this%rank+1)*i_size
        !
        ! if(i_end .GT. this%num_particles) i_end = this%num_particles

        i_start=1
        i_end = this%num_particles


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


        if(this%rank .EQ. 0) then
            write(*,'(A)') string
        end if
    end subroutine print_string


    subroutine sync_particles(this)
        class(replicated_distribution), intent(inout) :: this

    end subroutine sync_particles

end module replicated_distribution_type
