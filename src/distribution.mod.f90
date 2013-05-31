module distribution
    implicit none

    private

    public :: distribution_type, &
              one_particle_function, &
              two_particle_function

    type distribution_type
        contains
            procedure :: init
            procedure :: pair_operation
            procedure :: individual_operation
    end type distribution_type

    interface
        !
        ! Interfaces of particle level subroutines
        !
        function one_particle_function(p1)
            use particle_types
            implicit none

            type(particle_type), intent(in) :: p1
            type(particle_type) :: one_particle_function
        end function one_particle_function

        function two_particle_function(p1, p2)
            use particle_types
            implicit none

            type(particle_type), intent(in) :: p1
            type(particle_type), intent(in) :: p2
            type(particle_type) :: two_particle_function
        end function two_particle_function
    end interface

    contains
    subroutine init(this, particle_count)
        class(distribution_type), intent(out) :: this
        INTEGER, intent(in) :: particle_count
    end subroutine init

    subroutine pair_operation(this, compare_func, merge_func)
        class(distribution_type), intent(inout) :: this
        procedure(two_particle_function) :: compare_func
        procedure(two_particle_function) :: merge_func
    end subroutine pair_operation

    subroutine individual_operation(this, update_func)
        class(distribution_type), intent(inout) :: this
        procedure(one_particle_function) :: update_func
    end subroutine individual_operation
end module distribution
