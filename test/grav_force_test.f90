program grav_force_test
    use particle_types
    use distribution
    use grav_force
    use force_test
    implicit none

    ! Check function signatures are correct
    procedure(one_particle_function), pointer :: grav_init_test => grav_init
    procedure(two_particle_function), pointer :: &
        grav_compare_test => grav_compare
    procedure(two_particle_function), pointer :: &
        grav_merge_test => grav_merge

    call force_module_test("grav_force", grav_init, grav_compare, grav_merge)

    call exit(end_test())

end program grav_force_test
