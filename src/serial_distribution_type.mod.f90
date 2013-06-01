module serial_distribution_type
    use abstract_distribution_type
    use particle_type
    use global_variables
    implicit none

    private

    public :: serial_distribution

    type, EXTENDS(abstract_distribution) :: serial_distribution
        integer, private :: num_particles
        type(particle), private, allocatable :: particles(:)

    contains
        procedure :: init
        procedure :: pair_operation
        procedure :: individual_operation
        procedure :: global_map_reduce
        procedure :: print_particles
        procedure :: print_string
    end type serial_distribution
    interface serial_distribution
        procedure constructor
    end interface serial_distribution

    contains
    function constructor(particle_count)
        type(serial_distribution) :: constructor
        integer :: particle_count

        call constructor%init(particle_count)
    end function constructor

    subroutine init(this, particle_count)
        class(serial_distribution), intent(out) :: this
        integer, intent(in) :: particle_count


        this%num_particles = particle_count

        allocate(this%particles(this%num_particles))
    end subroutine init

    subroutine pair_operation(this, compare_func, merge_func)
        class(serial_distribution), intent(inout) :: this
        procedure(two_particle_function) :: compare_func
        procedure(two_particle_function) :: merge_func

        type(particle) :: tmp_particle
        integer :: i, j


        do i=1, this%num_particles
            do j=1, this%num_particles
                if(i .EQ. j) cycle

                tmp_particle = compare_func( &
                    this%particles(i), this%particles(j) &
                )
                this%particles(i) = merge_func(this%particles(i), tmp_particle)
            end do
        end do
    end subroutine pair_operation

    subroutine individual_operation(this, update_func)
        class(serial_distribution), intent(inout) :: this
        procedure(one_particle_function) :: update_func

        integer :: i

        do i=1, this%num_particles
            this%particles(i) = update_func(this%particles(i))
        end do
    end subroutine individual_operation

    function global_map_reduce(this, map, reduce, reduce_init)
        class(serial_distribution), intent(inout) :: this
        procedure(global_map_function) :: map
        procedure(global_reduce_function) :: reduce
        real(p) :: reduce_init

        real(p) :: global_map_reduce

        integer :: i

        global_map_reduce = 0
        do i=1, this%num_particles
            global_map_reduce = reduce( &
                global_map_reduce, &
                map(this%particles(i)) &
            )
        end do
    end function

    subroutine print_particles(this, print_func)
        class(serial_distribution), intent(inout) :: this
        procedure(print_particle_function) :: print_func

        integer :: i

        do i=1, this%num_particles
            write(*,'(A)') print_func(this%particles(i))
        end do

    end subroutine print_particles

    subroutine print_string(this, string)
        class(serial_distribution), intent(inout) :: this
        character(len=*), intent(in) :: string

        write(*,'(A)') string
    end subroutine print_string

end module serial_distribution_type
