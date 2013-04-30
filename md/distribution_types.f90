module distribution
    implicit none

    type distribution_type
        procedure(pair_operations_interface), pointer, nopass &
            :: pair_operation
        procedure(individual_operations_interface), pointer, nopass &
            :: individual_operation
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

        !
        ! Interfaces of list level subroutines
        !
        subroutine pair_operations_interface(compare_func, merge_func)
            use particle_types
            implicit none

            procedure(two_particle_interface) :: compare_func
            procedure(two_particle_interface) :: merge_func
        end subroutine pair_operations_interface

        subroutine individual_operations_interface(update_func)
            use particle_types
            implicit none

            procedure(one_particle_interface) :: update_func
        end subroutine individual_operations_interface
    end interface
end module distribution
