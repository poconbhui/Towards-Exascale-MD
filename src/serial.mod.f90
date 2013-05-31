module serial
    use distribution
    use particle_types
    implicit none

    private

    public :: serial_distribution

    type, EXTENDS(distribution_type) :: serial_distribution
            INTEGER, private :: num_particles
            type(particle_type), private, allocatable :: particles(:)

        contains
            procedure :: init
            procedure :: pair_operation
            procedure :: individual_operation
    end type serial_distribution
    interface serial_distribution
        procedure constructor
    end interface serial_distribution

    contains
    function constructor(particle_count)
        type(serial_distribution) :: constructor
        INTEGER :: particle_count

        call constructor%init(particle_count)
    end function constructor

    subroutine init(this, particle_count)
        class(serial_distribution), intent(out) :: this
        INTEGER, intent(in) :: particle_count


        this%num_particles = particle_count

        allocate(this%particles(this%num_particles))
    end subroutine init

    subroutine pair_operation(this, compare_func, merge_func)
        class(serial_distribution), intent(inout) :: this
        procedure(two_particle_function) :: compare_func
        procedure(two_particle_function) :: merge_func

        type(particle_type) :: tmp_particle
        INTEGER :: i, j


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

        INTEGER :: i

        do i=1, this%num_particles
            this%particles(i) = update_func(this%particles(i))
        end do
    end subroutine individual_operation
        
end module serial
