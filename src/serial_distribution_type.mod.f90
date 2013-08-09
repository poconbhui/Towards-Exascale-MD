module serial_distribution_type
    use abstract_distribution_type
    use particle_type
    use global_variables
    implicit none

    private

    public :: serial_distribution
    public :: new_serial_distribution

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

contains
    function new_serial_distribution(particle_count)
        type(serial_distribution) :: new_serial_distribution
        integer :: particle_count

        call new_serial_distribution%init(particle_count)
    end function new_serial_distribution

    subroutine init(this, particle_count)
        class(serial_distribution), intent(out) :: this
        integer, intent(in) :: particle_count


        ! Initialise reduction values
        this%sum = 1

        ! Initialise number of particles
        this%num_particles = particle_count

        ! Allocate particle array
        allocate(this%particles(this%num_particles))
    end subroutine init

    subroutine pair_operation( &
        this, pair_to_val, val_to_particle, reduce_op, reduction_identity &
    )
        class(serial_distribution), intent(inout) :: this

        procedure(two_particle_to_array_function) :: pair_to_val
        procedure(particle_and_array_to_particle_function) :: val_to_particle
        integer :: reduce_op
        real(p) :: reduction_identity(:)

        integer :: N
        real(p) :: tmp_val(size(reduction_identity))
        integer :: i, j


        interface reduce_types
            PURE function reduce_type(arr1, arr2, N)
                use global_variables
                implicit none

                integer, intent(in) :: N
                real(p) :: reduce_type(N)

                real(p), intent(in) :: arr1(N)
                real(p), intent(in) :: arr2(N)
            end function reduce_type
        end interface

        procedure(reduce_type), pointer :: reduce_func


        !
        ! Set reduction function
        !
        if(reduce_op .EQ. 1) then
            reduce_func => reduce_sum
        else
            call this%print_string("Error: provided reduce_op not supported!")
            call exit(1)
        end if


        N = size(reduction_identity)

        do i=1, this%num_particles
            tmp_val = reduction_identity

            do j=1, this%num_particles
                if(i .EQ. j) cycle

                tmp_val = reduce_func( &
                    tmp_val, pair_to_val( &
                        this%particles(i), this%particles(j), N &
                    ), N &
                )
            end do
            this%particles(i) = val_to_particle(this%particles(i), tmp_val, N)
        end do

    contains

        PURE function reduce_sum(arr1, arr2, N)
            integer, intent(in) :: N
            real(p) :: reduce_sum(N)

            real(p), intent(in) :: arr1(N)
            real(p), intent(in) :: arr2(N)

            reduce_sum = arr1 + arr2
        end function reduce_sum

    end subroutine pair_operation

    subroutine individual_operation(this, update_func)
        class(serial_distribution), intent(inout) :: this
        procedure(one_particle_function) :: update_func

        integer :: i

        do i=1, this%num_particles
            this%particles(i) = update_func(this%particles(i), i)
        end do
    end subroutine individual_operation

    subroutine global_map_reduce(this, map, reduce, reduce_value)
        class(serial_distribution), intent(inout) :: this
        procedure(global_map_function) :: map
        procedure(global_reduce_function) :: reduce
        real(p), intent(inout) :: reduce_value

        integer :: i

        do i=1, this%num_particles
            reduce_value = reduce( reduce_value, map(this%particles(i)) )
        end do
    end subroutine

    subroutine print_particles(this, print_func)
        class(serial_distribution), intent(inout) :: this
        procedure(print_particle_function) :: print_func

        character(len=80) :: string
        integer :: i

        do i=1, this%num_particles
            call print_func(this%particles(i), i, string)
            write(*,'(A)') string
        end do
    end subroutine print_particles

    subroutine print_string(this, string)
        class(serial_distribution), intent(inout) :: this
        character(len=*), intent(in) :: string

        write(*,'(A)') string
    end subroutine print_string

end module serial_distribution_type
