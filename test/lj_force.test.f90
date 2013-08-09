program lj_force_test
    use test_suite
    use particle_type
    use abstract_distribution_type
    use lj_force
    use force_test
    implicit none

    call force_module_test( &
        "lj_force", &
        lj_pair_to_val, lj_set_val, &
        lj_gen_reduce_op, lj_reduction_init &
    )

    call exit(end_test())

end program lj_force_test
