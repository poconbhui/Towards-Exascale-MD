module abstract_distribution_type
    implicit none

    private

    public :: abstract_distribution
    public :: one_particle_function
    public :: two_particle_function
    public :: global_map_function
    public :: global_reduce_function
    public :: print_particle_function


    type, abstract :: abstract_distribution
    contains
        procedure(init_subroutine), deferred &
            :: init
        procedure(pair_operation_subroutine), deferred &
            :: pair_operation
        procedure(individual_operation_subroutine), deferred &
            :: individual_operation
        procedure(global_map_reduce_function), deferred &
            :: global_map_reduce
        procedure(print_particles_subroutine), deferred &
            :: print_particles
        procedure(print_string_subroutine), deferred &
            :: print_string
    end type abstract_distribution


    abstract interface
        !
        ! Interfaces of particle level subroutines
        !
        function one_particle_function(p1)
            use particle_type
            implicit none

            type(particle), intent(in) :: p1
            type(particle) :: one_particle_function
        end function one_particle_function

        function two_particle_function(p1, p2)
            use particle_type
            implicit none

            type(particle), intent(in) :: p1
            type(particle), intent(in) :: p2
            type(particle) :: two_particle_function
        end function two_particle_function


        subroutine init_subroutine(this, particle_count)
            import abstract_distribution
            implicit none

            class(abstract_distribution), intent(out) :: this
            integer, intent(in) :: particle_count
        end subroutine init_subroutine

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

        function global_map_function(p1)
            use particle_type
            use global_variables
            implicit none

            type(particle), intent(in) :: p1

            real(p) :: global_map_function
        end function global_map_function

        function global_reduce_function(d1, d2)
            use global_variables
            implicit none

            real(p), intent(in) :: d1
            real(p), intent(in) :: d2

            real(p) :: global_reduce_function
        end function global_reduce_function

        function global_map_reduce_function(this, map, reduce, reduce_init)
            use global_variables
            import abstract_distribution
            implicit none

            class(abstract_distribution), intent(inout) :: this
            procedure(global_map_function) :: map
            procedure(global_reduce_function) :: reduce
            real(p) :: reduce_init

            real(p) :: global_map_reduce_function
            
        end function global_map_reduce_function

        function print_particle_function(p)
            use particle_type
            implicit none

            type(particle), intent(in) :: p
            character(len=*) :: print_particle_function
        end function print_particle_function
            
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
