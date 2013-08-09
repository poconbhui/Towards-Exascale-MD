module force_test
    use global_variables
    use particle_type
    use abstract_distribution_type
    use test_suite
    implicit none

contains
    subroutine force_module_test( &
        name, pair_to_val, set_val, &
        gen_reduce_op, reduction_init &
    )
        character(len=*) :: name
        procedure(two_particle_to_array_function) :: pair_to_val
        procedure(particle_and_array_to_particle_function) :: set_val

        interface
            PURE function gen_reduce_op_function(dist)
                use abstract_distribution_type
                implicit none

                integer :: gen_reduce_op_function

                class(abstract_distribution), intent(in) :: dist
            end function gen_reduce_op_function
        end interface

        procedure(gen_reduce_op_function) :: gen_reduce_op

        real(p) :: reduction_init(:)

        type(particle) :: test_particle
        type(particle) :: p1, p2

        integer :: N
        real(p) :: tmp_val(size(reduction_init))

        N = size(reduction_init)


        !
        ! Expect val_to_particle to only affect force
        !
        call describe(name//" #pair_to_val, #set_val")

        p1 = particle(pos=1, vel=2, force=3, mass=4)
        p2 = particle(pos=5, vel=6, force=7, mass=8)

        tmp_val = pair_to_val(p1, p2, N)
        test_particle = set_val(p1, tmp_val, N)

        call expect( &
            "pos should be unchanged", &
            ALL(p1%pos .EQ. test_particle%pos) &
        )
        
        call expect( &
            "vel should be unchanged", &
            ALL(p1%vel .EQ. test_particle%vel) &
        )
        
        call expect( &
            "mass should be unchanged", &
            p1%mass .EQ. test_particle%mass &
        )
        
    end subroutine force_module_test

end module force_test
