module bench_types
    use abstract_distribution_type
    use bench_suite
    implicit none

contains


    !
    ! Our benchmarks!
    !
    subroutine full_calculation(dist)
        class(abstract_distribution), intent(inout) :: dist


        call dist%individual_operation(bench_integrate_1)

        call dist%individual_operation(bench_force_init)
        call dist%pair_operation(bench_force_compare, bench_force_merge)

        call dist%individual_operation(bench_integrate_2)
    end subroutine full_calculation

    subroutine individual_operation(dist)
        class(abstract_distribution), intent(inout) :: dist


        call dist%individual_operation(bench_integrate_1)
    end subroutine individual_operation

    subroutine pair_operation(dist)
        class(abstract_distribution), intent(inout) :: dist


        call dist%pair_operation(bench_force_compare, bench_force_merge)
    end subroutine pair_operation

end module bench_types

program bench
    use mpi
    use bench_suite
    use bench_types
    use bench_flags

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
        subroutine bench_type_subroutine(dist)
            use abstract_distribution_type
            implicit none

            class(abstract_distribution), intent(inout) :: dist
        end subroutine bench_type_subroutine
    end interface


    double precision :: start_time, end_time
    real(p) :: dt
    character(len=200) :: output_string

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


    !
    ! Do one warm up rep, time it, set future rep count based on that.
    !

    ! Get rid of synchronization fuzz before timing
    call MPI_Barrier(MPI_COMM_WORLD, ierror)
    start_time = get_time()
    call bench_ptr(dist)
    end_time = get_time()

    ! Get suggested reps
    num_reps = max(num_reps, int(1.0/(end_time - start_time)))

    ! Use num_reps from process 0
    call MPI_Bcast(num_reps, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)


    !
    ! Do actual timing
    !

    start_time = get_time()
    do rep=1, num_reps
        call bench_ptr(dist)
    end do
    end_time = get_time()


    !
    ! Output data
    !

    write(output_string, *) "# distrubution_name benchmark_name num_particles mpi_disabled calculation_disabled num_procs num_reps time"
    output_string = adjustl(output_string)
    call dist%print_string(output_string)

    write(output_string, *) &
        trim(distribution_name)//" ", trim(benchmark_name)//" ", &
        num_particles, num_procs, &
        disable_mpi, disable_calculation, &
        num_reps, end_time-start_time
    call dist%print_string(output_string)


    call MPI_Finalize(ierror)


contains


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

        if(argc .NE. 6) then
            write(*,*) &
                "Usage: ./bench dist_name benchmark_name num_particles num_reps disable_mpi disable_calculation"
            call exit(1)
        end if

        read(argv(1), *) distribution_name
        read(argv(2), *) benchmark_name
        read(argv(3), *) num_particles
        read(argv(4), *) num_reps

        read(argv(5), *) disable_mpi
        read(argv(6), *) disable_calculation
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
