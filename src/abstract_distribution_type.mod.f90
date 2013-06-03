!
! MODULE abstract_distribution_type
!
! Module provides the abstract_distribution type, an abstract class
! to be extended by all distributions.
!
! Module provides interfaces for functions accepted by abstract_distribution
! methods.
!

module abstract_distribution_type
    implicit none

    private

    public :: abstract_distribution
    public :: one_particle_function
    public :: two_particle_function
    public :: global_map_function
    public :: global_reduce_function
    public :: print_particle_function

    !
    ! TYPE abstract_distribution
    !
    ! abstract_distribution type is an abstract type specifying a minimum
    ! functionality that must be provided by distribution types.
    !
    ! A distribution type is expected to extend this type and be run as
    ! an instance. Methods described here accept an instance of
    ! abstract_distribution passed to them implicitly.
    !
    type, abstract :: abstract_distribution
    contains
        procedure(pair_operation_subroutine), pass, deferred &
            :: pair_operation
        procedure(individual_operation_subroutine), pass, deferred &
            :: individual_operation
        procedure(global_map_reduce_subroutine), pass, deferred &
            :: global_map_reduce
        procedure(print_particles_subroutine), pass, deferred &
            :: print_particles
        procedure(print_string_subroutine), pass, deferred &
            :: print_string
    end type abstract_distribution


    abstract interface
        !
        ! Interfaces for functions accepted by abstract_distribution methods.
        !
        PURE function one_particle_function(p1, i)
            use particle_type
            implicit none

            type(particle) :: one_particle_function

            type(particle), intent(in) :: p1
            integer, intent(in) :: i
        end function one_particle_function

        PURE function two_particle_function(p1, p2)
            use particle_type
            implicit none

            type(particle) :: two_particle_function

            type(particle), intent(in) :: p1
            type(particle), intent(in) :: p2
        end function two_particle_function

        PURE function global_map_function(p1)
            use particle_type
            use global_variables
            implicit none

            real(p) :: global_map_function

            type(particle), intent(in) :: p1
        end function global_map_function

        PURE function global_reduce_function(d1, d2)
            use global_variables
            implicit none

            real(p) :: global_reduce_function

            real(p), intent(in) :: d1
            real(p), intent(in) :: d2
        end function global_reduce_function

        PURE subroutine print_particle_function(p, i, string)
            use particle_type
            implicit none

            type(particle), intent(in) :: p
            integer, intent(in) :: i
            character(len=*), intent(out) :: string
        end subroutine print_particle_function


        !
        ! Interfaces for methods of abstract_distribution type
        !

        subroutine pair_operation_subroutine(this, compare_func, merge_func)
            import abstract_distribution
            implicit none

            class(abstract_distribution), intent(inout) :: this

            procedure(two_particle_function) :: compare_func
            procedure(two_particle_function) :: merge_func
        end subroutine pair_operation_subroutine

        subroutine individual_operation_subroutine(this, update_func)
            import abstract_distribution
            implicit none

            class(abstract_distribution), intent(inout) :: this

            procedure(one_particle_function) :: update_func
        end subroutine individual_operation_subroutine

        subroutine global_map_reduce_subroutine(this, map, reduce, reduce_value)
            use global_variables
            import abstract_distribution
            implicit none

            class(abstract_distribution), intent(inout) :: this

            procedure(global_map_function) :: map
            procedure(global_reduce_function) :: reduce
            real(p), intent(inout) :: reduce_value
            
        end subroutine global_map_reduce_subroutine

        subroutine print_particles_subroutine(this, print_func)
            import abstract_distribution
            implicit none

            class(abstract_distribution), intent(inout) :: this
            procedure(print_particle_function) :: print_func
        end subroutine print_particles_subroutine

        subroutine print_string_subroutine(this, string)
            import abstract_distribution
            implicit none

            class(abstract_distribution), intent(inout) :: this
            character(len=*), intent(in) :: string
        end subroutine print_string_subroutine

    end interface

end module abstract_distribution_type
