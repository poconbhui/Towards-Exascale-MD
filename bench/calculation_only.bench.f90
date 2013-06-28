program calculation_only_bench
    use mpi
    use bench_suite

    use global_variables
    use particle_type
    use abstract_distribution_type

    use serial_distribution_type
    use replicated_distribution_type
    use domain_distribution_type
    use systolic_distribution_type

    use grav_force
    use integration

    implicit none


    integer :: num_particles
    integer :: num_reps
    character(len=20) :: distribution_name
    integer :: num_procs

    class(abstract_distribution), pointer :: dist

    ! Distribution types
    type(serial_distribution), target :: serial
    type(replicated_distribution), target :: replicated
    type(domain_distribution), target :: domain
    type(systolic_distribution), target :: systolic

    double precision :: time
    character(len=80) :: output_string
    integer :: i

    integer :: argc
    character(len=20), dimension(:), allocatable :: argv

    integer :: ierror


    call MPI_Init(ierror)


    call MPI_Comm_size(MPI_COMM_WORLD, num_procs, ierror)

    call parse_arguments( &
      argc, argv, &
      distribution_name, num_particles, num_reps &
    )


    select case(distribution_name)
        case ("serial")
            serial = new_serial_distribution(num_particles)
            call set_distribution_pointer(serial, dist)

        case ("replicated")
            replicated = new_replicated_distribution( &
                num_particles, MPI_COMM_WORLD &
            )
            call set_distribution_pointer(replicated, dist)

        case ("domain")
            domain = new_domain_distribution( &
                num_particles, (/ 1.0_p, 1.0_p, 1.0_p /), dist_init, &
                MPI_COMM_WORLD &
            )
            call set_distribution_pointer(domain, dist)

        case ("systolic")
            systolic = new_systolic_distribution( &
                num_particles, MPI_COMM_WORLD &
            )
            call set_distribution_pointer(systolic, dist)

        case default
            write(*,*) "Error: Invalid distribution type"

    end select

    call dist%individual_operation(dist_init)

    !call dist%print_particles(print_particle)

    time = get_time()
    do i=1, num_reps
        call dist%individual_operation(verlet_integrate_pt1)

        call dist%individual_operation(grav_init)
        call dist%pair_operation(grav_compare, grav_merge)

        call dist%individual_operation(verlet_integrate_pt2)
    end do


    write(output_string, *) &
        "# distrubution_name num_procs num_particles num_reps time"
    call dist%print_string(output_string)
    write(output_string, *) &
        trim(distribution_name)//",", &
        num_procs, num_particles, num_reps, get_time()-time
    call dist%print_string(output_string)


    call MPI_Finalize(ierror)


contains
    subroutine parse_arguments( &
        argc, argv, distribution_name, num_particles, num_reps &
    )
        integer, intent(out) :: argc
        character(len=*), dimension(:), allocatable, intent(out) :: argv
        character(len=*), intent(out) :: distribution_name
        integer, intent(out) :: num_particles
        integer, intent(out) :: num_reps

        integer :: i


        argc = command_argument_count()
        allocate(argv(argc))
        do i=1, argc
            call get_command_argument(i, argv(i))
        end do

        if(argc .NE. 3) then
            write(*,*) "Usage: ./bench dist num_particles num_reps"
            call exit(1)
        end if

        read(argv(1), *) distribution_name
        read(argv(2), *) num_particles
        read(argv(3), *) num_reps
    end subroutine parse_arguments

    subroutine set_distribution_pointer(dist_in, dist_out)
        class(abstract_distribution), target  :: dist_in
        class(abstract_distribution), pointer :: dist_out

        dist_out => dist_in
    end subroutine set_distribution_pointer

end program calculation_only_bench
