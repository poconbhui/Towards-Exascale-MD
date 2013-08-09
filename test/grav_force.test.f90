program grav_force_test
    use test_suite
    use particle_type
    use abstract_distribution_type
    use grav_force
    use force_test
    implicit none

    call force_module_test( &
        "grav_force", &
        grav_pair_to_val, grav_set_val, &
        grav_gen_reduce_op, grav_reduction_init &
    )

    call exit(end_test())

end program grav_force_test
