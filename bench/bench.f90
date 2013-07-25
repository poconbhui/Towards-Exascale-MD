program bench
    use mpi
    use bench_suite

    use global_variables
    use particle_type
    use abstract_distribution_type

    use serial_distribution_type
    use replicated_distribution_type
    use domain_distribution_type
    use systolic_distribution_type

    use integration

    implicit none


    integer :: num_particles
    integer :: num_reps
    character(len=20) :: distribution_name
    character(len=20) :: benchmark_name
    integer :: num_procs


    class(abstract_distribution), pointer :: dist

    ! Distribution types
    type(serial_distribution), target :: serial
    type(replicated_distribution), target :: replicated
    type(domain_distribution), target :: domain
    type(systolic_distribution), target :: systolic


    procedure(bench_type_subroutine), pointer :: bench_ptr => null()
    interface bench_type_interface
        subroutine bench_type_subroutine
        end subroutine bench_type_subroutine
    end interface


    double precision :: start_time, end_time
    real(p) :: dt
    character(len=100) :: output_string

    integer :: argc
    character(len=20), dimension(:), allocatable :: argv

    integer :: rep

    integer :: ierror

    dt = 0.001


    call MPI_Init(ierror)


    call MPI_Comm_size(MPI_COMM_WORLD, num_procs, ierror)

    call parse_arguments( &
      argc, argv, &
      distribution_name, benchmark_name, num_particles, num_reps &
    )


    call bench_suite_init(num_particles, (/ 1.0_p, 1.0_p, 1.0_p /), dt)


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
                num_particles, (/ 1.0_p, 1.0_p, 1.0_p /), bench_dist_init, &
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
            call exit(1)

    end select


    select case(benchmark_name)
        case ("full_calculation")
            bench_ptr => full_calculation

        case ("individual_operation")
            bench_ptr => individual_operation

        case ("pair_operation")
            bench_ptr => pair_operation

        case default
            write(*,*) "Error: Invalid bench type"
            call exit(1)

    end select


    call dist%individual_operation(bench_dist_init)
    call dist%individual_operation(bench_force_init)

    !call dist%print_particles(print_particle)


    start_time = get_time()
    end_time = start_time
    rep=0

    ! Do while we have done fewer than the min reps or we haven't run
    ! for at least a second
    do while (rep .LT. num_reps .OR. end_time - start_time .LT. 1)
        call bench_ptr
        end_time = get_time()

        rep = rep+1
    end do


    write(output_string, *) "# distrubution_name benchmark_name num_particles num_procs num_reps time"
    output_string = adjustl(output_string)
    call dist%print_string(output_string)

    write(output_string, *) &
        trim(distribution_name)//" ", trim(benchmark_name)//" ", &
        num_particles, num_procs, rep, end_time-start_time
    call dist%print_string(output_string)


    call MPI_Finalize(ierror)


contains

    !
    ! Our benchmarks!
    !
    subroutine full_calculation
        call dist%individual_operation(bench_integrate_1)

        call dist%individual_operation(bench_force_init)
        call dist%pair_operation(bench_force_compare, bench_force_merge)

        call dist%individual_operation(bench_integrate_2)
    end subroutine full_calculation

    subroutine individual_operation
        call dist%individual_operation(bench_integrate_1)
    end subroutine individual_operation

    subroutine pair_operation
        call dist%pair_operation(bench_force_compare, bench_force_merge)
    end subroutine pair_operation


    !
    ! Parse input arguments into variables
    !   distribution_name, benchmark_name, num_particles, num_reps.
    !
    ! Expect program to be called as
    !   ./bench distribution_name benchmark_name num_particles num_reps
    !
    subroutine parse_arguments( &
        argc, argv, &
        distribution_name, benchmark_name, &
        num_particles, num_reps &
    )
        integer, intent(out) :: argc
        character(len=*), dimension(:), allocatable, intent(out) :: argv
        character(len=*), intent(out) :: distribution_name
        character(len=*), intent(out) :: benchmark_name
        integer, intent(out) :: num_particles
        integer, intent(out) :: num_reps

        integer :: i


        argc = command_argument_count()
        allocate(argv(argc))
        do i=1, argc
            call get_command_argument(i, argv(i))
        end do

        if(argc .NE. 4) then
            write(*,*) &
                "Usage: ./bench dist_name benchmark_name num_particles num_reps"
            call exit(1)
        end if

        read(argv(1), *) distribution_name
        read(argv(2), *) benchmark_name
        read(argv(3), *) num_particles
        read(argv(4), *) num_reps
    end subroutine parse_arguments


    !
    ! Sneaky cheaky hack to set abstract_distribution pointer to
    ! an implementation of abstract_distribution on crayftn
    !
    subroutine set_distribution_pointer(dist_in, dist_out)
        class(abstract_distribution), target  :: dist_in
        class(abstract_distribution), pointer :: dist_out

        dist_out => dist_in
    end subroutine set_distribution_pointer


end program bench
