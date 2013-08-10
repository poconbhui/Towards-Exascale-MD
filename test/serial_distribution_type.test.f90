! PROGRAM serial_distribution_test
!
! This program checks that the serial_distribution_type works as expected.
!
program serial_distribution_test
    use serial_distribution_type

    use test_suite

    use distribution_test
    implicit none


    integer :: num_particles
    type(serial_distribution):: dist


    num_particles = 10
    dist = new_serial_distribution(num_particles)

    call distribution_module_test(dist, num_particles)


    call exit(end_test())

end program serial_distribution_test
