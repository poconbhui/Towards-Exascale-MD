! PROGRAM grav_force_test
!
! Test that the grav_force module behaves as expected.
!
program grav_force_test
    use grav_force

    use test_suite

    use force_test
    implicit none


    call force_module_test( &
        "grav_force", &
        grav_pair_to_val, grav_set_val, &
        grav_gen_reduce_op, grav_reduction_init &
    )


    if(end_test() .NE. 0) stop 1

end program grav_force_test
