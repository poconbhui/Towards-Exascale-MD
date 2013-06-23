program serial_bench
    use mpi
    use bench_suite
    use particle_type
    use abstract_distribution_type
    use serial_distribution_type
    use grav_force
    use integration
    implicit none

    integer :: num_particles
    integer :: num_reps

    type(serial_distribution) :: dist

    double precision :: time
    character(len=80) :: output_string
    integer :: i

    integer :: argc
    character(len=20), dimension(:), allocatable :: argv

    integer :: ierror


    call MPI_Init(ierror)


    argc = command_argument_count()
    allocate(argv(argc))
    do i=1, argc
        call get_command_argument(i, argv(i))
    end do

    if(argc .LT. 2) then
        call MPI_Comm_rank(MPI_COMM_WORLD, i, ierror)
        if(i .EQ. 0) write(*, *) "Usage: ./program num_particles num_reps"

        call MPI_Finalize(ierror)

        call exit(1)
    end if

    read(argv(1), *) num_particles
    read(argv(2), *) num_reps


    dist = new_serial_distribution(num_particles)

    call dist%individual_operation(dist_init)

    !call dist%print_particles(print_particle)

    time = get_time()
    do i=1, num_reps
        call dist%individual_operation(verlet_integrate_pt1)

        call dist%individual_operation(grav_init)
        call dist%pair_operation(grav_compare, grav_merge)

        call dist%individual_operation(verlet_integrate_pt2)
    end do


    write(output_string, *) "Particles: ", num_particles
    call dist%print_string(output_string)
    write(output_string, *) "Reps: ", num_reps
    call dist%print_string(output_string)
    write(output_string, *) "Time:", get_time() - time
    call dist%print_string(output_string)


    call MPI_Finalize(ierror)

end program serial_bench
