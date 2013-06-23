program grav_force_test
    use test_suite
    use particle_type
    use abstract_distribution_type
    use grav_force
    use force_test
    implicit none

    procedure(one_particle_function), pointer :: grav_init_test
    procedure(two_particle_function), pointer :: grav_compare_test
    procedure(two_particle_function), pointer :: grav_merge_test

    ! Check function signatures are correct
    grav_init_test => grav_init
    grav_compare_test => grav_compare
    grav_merge_test => grav_merge


    call force_module_test("grav_force", grav_init, grav_compare, grav_merge)

    call exit(end_test())

end program grav_force_test
