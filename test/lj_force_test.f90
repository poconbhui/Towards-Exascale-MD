program lj_force_test
    use particle_type
    use abstract_distribution_type
    use lj_force
    use force_test
    implicit none

    ! Check function signatures are correct
    procedure(one_particle_function), pointer :: lj_init_test
    procedure(two_particle_function), pointer :: lj_compare_test
    procedure(two_particle_function), pointer :: lj_merge_test

    lj_init_test => lj_init
    lj_compare_test => lj_compare
    lj_merge_test => lj_merge

    call force_module_test("lj_force", lj_init, lj_compare, lj_merge)

    call exit(end_test())

end program lj_force_test
