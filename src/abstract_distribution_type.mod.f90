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

    ! Exported types
    public :: abstract_distribution

    ! Exported interfaces
    public :: one_particle_function
    public :: two_particle_function
    public :: two_particle_to_array_subroutine
    public :: particle_and_array_to_particle_function
    public :: global_map_function
    public :: global_reduce_function
    public :: print_particle_function


    ! TYPE abstract_distribution
    !
    ! A distribution type is an abstraction for defining a list of
    ! particles and a set of operators that accept functions and
    ! apply them to the list of particles.
    !
    ! The list of particles is manipulated and data is gathered from it
    ! exclusively through the use of these operators.
    !
    ! abstract_distribution type is an abstract type specifying a minimum
    ! functionality that must be provided by distribution types. It is
    ! generally used as an interface for distribution types, and as
    ! a class for pointers and function arguments when the exact
    ! distribution type to be used is not known in advance.
    !
    ! Distribution types are expected to extend this type and be run as
    ! instances. Methods described here accept an instance of
    ! abstract_distribution passed to them implicitly.
    !
    type, abstract :: abstract_distribution


        !
        ! Reduction operations that are expected to be supported
        ! These should be set to appropriate values in the
        ! distribution's initialiser
        !

        ! INTEGER sum
        !
        ! This should be used for an elementwise summation over a set of arrays
        !
        ! Eg, s = [ [1,2,3], [1,2,3], [1,2,3] ]
        ! The sum should yield an array [3, 6, 9]
        !
        integer :: sum

    contains


        ! SUBROUTINE pair_operation(this, compare_func, merge_func)
        !
        ! This function should effectively implement a list comparison
        ! algorithm for resident particles.
        !
        ! The algorithm should be equivalent to:
        !
        ! do i=1, particle_list_size
        !   do j=1, particle_list_size
        !     if(i .EQ. j) cycle
        !     
        !     particle(i) = merge_func( &
        !       particle(i), compare_func(particle(i), particle(j)) &
        !     )
        !   end do
        ! end do
        !
        ! compare_func and merge_func should be idempotent.
        ! Further, the values of the particles that compare_func changes
        ! should be expected to be junk values upon entry.
        ! merge_func should only alter values altered by compare_func.
        ! It should not be expected that compare_func is run for a given
        ! pair only once, and no particular ordering should be assumed.
        ! It should not be assumed that the algorithm outlined above will
        ! be implemented as outlined, or even run only once per invocation.
        !
        procedure(pair_operation_subroutine), pass, deferred &
            :: pair_operation


        ! SUBROUTINE individual_operation(this, update_func)
        !
        ! This subroutine will effectively apply update_func to
        ! every particle in the particle list.
        !
        ! It should effectively implement the following:
        !
        ! do i=1, particle_list_size
        !   particle(i) = update_func(particle(i), i)
        ! end do
        !
        procedure(individual_operation_subroutine), pass, deferred &
            :: individual_operation


        ! SUBROUTINE global_map_reduce(map, reduce, value)
        !
        ! This subroutine will perform a map/reduce operation over
        ! the list of particles.
        !
        ! This subroutine is expected to be used to extract data from
        ! the list of particles out to the main program.
        !
        ! The value argument when passed in will be used as the initial
        ! value for the reduce operation. When the map/reduce operation
        ! is complete, value will contain the resulting value.
        !
        ! global_map_reduce should effectively implement the following:
        !
        ! do i=1, particle_list_size
        !   value = reduce( value, map(particle(i)) )
        ! end do
        !
        procedure(global_map_reduce_subroutine), pass, deferred &
            :: global_map_reduce


        ! SUBROUTINE print_particles(print_sub)
        !
        ! This function prints out a string for every particle in the
        ! list, as returned by print_sub.
        !
        ! The subroutine print_sub should accept a particle and return
        ! a string. The subroutine will then print this to screen.
        !
        ! It should effectively implement the following:
        !
        ! do i=1, particle_list_size
        !   call print_sub(particle(i), i, particle_string)
        !   write(*,*) particle_string
        ! end do
        !
        procedure(print_particles_subroutine), pass, deferred &
            :: print_particles


        ! SUBROUTINE print_string(string)
        !
        ! This subroutine outputs a given string.
        !
        ! It should effectively implement the following:
        !
        ! write(*,*) string
        !
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

        PURE subroutine two_particle_to_array_subroutine(p1, p2, array_out)
            use particle_type
            use global_variables
            implicit none

            real(p), intent(out) :: array_out(:)

            type(particle), intent(in) :: p1
            type(particle), intent(in) :: p2
        end subroutine two_particle_to_array_subroutine

        PURE function particle_and_array_to_particle_function( &
            p1, array &
        )
            use particle_type
            use global_variables
            implicit none

            type(particle) :: particle_and_array_to_particle_function

            type(particle), intent(in) :: p1
            real(p), intent(in) :: array(:)
        end function particle_and_array_to_particle_function


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

        subroutine pair_operation_subroutine( &
            this, pair_to_val, val_to_particle, reduce_op, reduction_identity &
        )
            use global_variables
            import abstract_distribution
            import two_particle_to_array_subroutine
            import particle_and_array_to_particle_function
            implicit none

            class(abstract_distribution), intent(inout) :: this

            procedure(two_particle_to_array_subroutine) :: pair_to_val
            procedure(particle_and_array_to_particle_function) &
                :: val_to_particle
            integer :: reduce_op
            real(p) :: reduction_identity(:)
        end subroutine pair_operation_subroutine

        subroutine individual_operation_subroutine(this, update_func)
            import abstract_distribution
            import one_particle_function
            implicit none

            class(abstract_distribution), intent(inout) :: this

            procedure(one_particle_function) :: update_func
        end subroutine individual_operation_subroutine

        subroutine global_map_reduce_subroutine(this, map, reduce, reduce_value)
            use global_variables
            import abstract_distribution
            import global_map_function
            import global_reduce_function
            implicit none

            class(abstract_distribution), intent(inout) :: this

            procedure(global_map_function) :: map
            procedure(global_reduce_function) :: reduce
            real(p), intent(inout) :: reduce_value
            
        end subroutine global_map_reduce_subroutine

        subroutine print_particles_subroutine(this, print_func)
            import abstract_distribution
            import print_particle_function
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
