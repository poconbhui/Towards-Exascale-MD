module distribution
    implicit none

    private

    public :: distribution_type, &
              one_particle_interface, &
              two_particle_interface

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
        subroutine one_particle_interface(p1)
            use particle_types
            implicit none

            type(particle_type), intent(inout) :: p1
        end subroutine one_particle_interface

        subroutine two_particle_interface(p1, p2)
            use particle_types
            implicit none

            type(particle_type), intent(inout) :: p1
            type(particle_type), intent(in)    :: p2
        end subroutine two_particle_interface
    end interface

    contains
    subroutine init(this, particle_count)
        class(distribution_type), intent(out) :: this
        INTEGER, intent(in) :: particle_count
    end subroutine init

    subroutine pair_operation(this, compare_func, merge_func)
        class(distribution_type), intent(inout) :: this
        procedure(two_particle_interface) :: compare_func
        procedure(two_particle_interface) :: merge_func
    end subroutine pair_operation

    subroutine individual_operation(this, update_func)
        class(distribution_type), intent(inout) :: this
        procedure(one_particle_interface) :: update_func
    end subroutine individual_operation
end module distribution
