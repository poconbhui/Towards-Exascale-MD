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

        call dist%pair_operation( &
            bench_force_pair_to_val, bench_force_set_val, &
            bench_force_gen_reduce_op(dist), bench_force_reduction_init &
        )

        call dist%individual_operation(bench_integrate_2)
    end subroutine full_calculation

    subroutine individual_operation(dist)
        class(abstract_distribution), intent(inout) :: dist


        call dist%individual_operation(bench_integrate_1)
    end subroutine individual_operation

    subroutine pair_operation(dist)
        class(abstract_distribution), intent(inout) :: dist


        call dist%pair_operation( &
            bench_force_pair_to_val, bench_force_set_val, &
            bench_force_gen_reduce_op(dist), bench_force_reduction_init &
        )
    end subroutine pair_operation

end module bench_types

program bench
    use mpi
    use omp_lib
    use bench_suite
    use bench_types
    use bench_flags

    use global_variables
    use particle_type
    use abstract_distribution_type

    use serial_distribution_type
    use replicated_distribution_type
    use systolic_distribution_type
    use shared_and_replicated_distribution_type

    use integration

    implicit none


    integer :: num_particles
    integer :: num_reps
    character(len=50)  :: distribution_name
    character(len=50)  :: benchmark_name
    character(len=255) :: output_filename
    integer :: num_procs
    integer :: total_cores


    class(abstract_distribution), pointer :: dist

    ! Distribution types
    type(serial_distribution), target :: serial
    type(replicated_distribution), target :: replicated
    type(systolic_distribution), target :: systolic
    type(shared_and_replicated_distribution), target :: shared_and_replicated


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

    integer :: argc
    character(len=255), dimension(:), allocatable :: argv

    integer :: rep

    integer :: rank

    integer :: ierror


    dt = 0.001


    call MPI_Init(ierror)


    !
    ! Find total number of cores between openmp and mpi
    !

    total_cores = 0
    !$OMP PARALLEL REDUCTION(+:total_cores)
    total_cores = total_cores + 1
    !$OMP END PARALLEL

    call MPI_Comm_size(MPI_COMM_WORLD, num_procs, ierror)

    total_cores = total_cores*num_procs


    call parse_arguments( &
      argc, argv, &
      distribution_name, num_particles, num_reps &
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

        case ("systolic")
            systolic = new_systolic_distribution( &
                num_particles, MPI_COMM_WORLD &
            )
            call set_distribution_pointer(systolic, dist)

        case ("shared_and_replicated")
            shared_and_replicated = new_shared_and_replicated_distribution( &
                num_particles, MPI_COMM_WORLD &
            )
            call set_distribution_pointer(shared_and_replicated, dist)


        case default
            write(0,*) "Error: Invalid distribution type "//distribution_name
            stop 1

    end select



    !
    ! Generate total time
    !

    disable_mpi = .FALSE.
    disable_calculation = .FALSE.

    ! Do full calculation
    bench_ptr => full_calculation
    benchmark_name = "full_calculation"
    call run_bench(bench_ptr, dist, num_reps, output_filename)

    ! Do individual operation
    bench_ptr => individual_operation
    benchmark_name = "individual_operation"
    call run_bench(bench_ptr, dist, num_reps, output_filename)

    ! Do pair operation
    bench_ptr => pair_operation
    benchmark_name = "pair_operation"
    call run_bench(bench_ptr, dist, num_reps, output_filename)


    !
    ! Generate Calculation only
    !

    disable_mpi = .TRUE.
    disable_calculation = .FALSE.

    ! Do full calculation
    bench_ptr => full_calculation
    benchmark_name = "full_calculation"
    call run_bench(bench_ptr, dist, num_reps, output_filename)

    ! Do individual operation
    bench_ptr => individual_operation
    benchmark_name = "individual_operation"
    call run_bench(bench_ptr, dist, num_reps, output_filename)

    ! Do pair operation
    bench_ptr => pair_operation
    benchmark_name = "pair_operation"
    call run_bench(bench_ptr, dist, num_reps, output_filename)


    !
    ! Generate MPI only
    !

    disable_mpi = .FALSE.
    disable_calculation = .TRUE.

    ! Do full calculation
    bench_ptr => full_calculation
    benchmark_name = "full_calculation"
    call run_bench(bench_ptr, dist, num_reps, output_filename)

    ! Do individual operation
    bench_ptr => individual_operation
    benchmark_name = "individual_operation"
    call run_bench(bench_ptr, dist, num_reps, output_filename)

    ! Do pair operation
    bench_ptr => pair_operation
    benchmark_name = "pair_operation"
    call run_bench(bench_ptr, dist, num_reps, output_filename)


    call MPI_Finalize(ierror)


contains


    !
    ! SUBROUTINE gen_output_filename
    !
    ! This subroutine accepts a set of variables related to the
    ! benchmark being run and generates a standardized filename
    ! for the results to be written to.
    !
    subroutine gen_output_filename( &
        distribution_name, benchmark_name, num_particles, nprocs, &
        disable_mpi, disable_calculation, output_filename &
    )
        character(len=*), intent(in) :: distribution_name
        character(len=*), intent(in) :: benchmark_name
        integer, intent(in) :: num_particles
        integer, intent(in) :: nprocs
        logical, intent(in) :: disable_mpi
        logical, intent(in) :: disable_calculation
        character(len=*), intent(out) :: output_filename


        output_filename = &
            trim(distribution_name)//"."// &
            trim(benchmark_name)//"."// &
            trim(integer_to_string(num_particles))//"."// &
            trim(integer_to_string(nprocs))//"."// &
            trim(logical_to_string(disable_mpi))//"."// &
            trim(logical_to_string(disable_calculation))//".bench.dat"

    end subroutine gen_output_filename


    !
    ! FUNCTION logical_to_string
    !
    ! This function accepts a logical type and returns the string
    ! "true" for .TRUE. and "false" for .FALSE.
    !
    ! It may be necessary to trim the returned result.
    !
    function logical_to_string(l)
        character(len=5) :: logical_to_string

        logical, intent(in) :: l


        if(l) then
            logical_to_string = "true"
        else
            logical_to_string = "false"
        end if
    end function logical_to_string


    !
    ! FUNCTION integer_to_string
    !
    ! This function accepts an integer and returns the string
    ! representation of that integer.
    !
    ! It may be necessary to trim the returned result.
    !
    ! This is only expected to work with positive integers no
    ! greater than 100 digits.
    !
    function integer_to_string(i)
        ! Expect no larger than a 100 digit integer.
        character(len=100) :: integer_to_string

        integer, intent(in) :: i


        write(integer_to_string, *) i
        integer_to_string = adjustl(integer_to_string)
    end function integer_to_string


    !
    ! SUBROUTINE run_bench
    !
    ! Run the benchmark pointed to by bench_ptr on
    ! the distribution pointed to by dist and
    ! output the results to the file output_filename
    !
    subroutine run_bench(bench_ptr, dist, num_reps_in, output_filename)
        procedure(bench_type_subroutine), pointer, intent(inout) :: bench_ptr
        class(abstract_distribution), intent(inout) :: dist
        integer, intent(in) :: num_reps_in
        character(len=*), intent(inout) :: output_filename

        integer :: num_reps

        logical :: do_more_reps


        num_reps = num_reps_in
        do_more_reps = .FALSE.


        call gen_output_filename( &
            distribution_name, benchmark_name, num_particles, total_cores, &
            disable_mpi, disable_calculation, output_filename &
        )
        call dist%print_string("Running "//output_filename)


        call dist%individual_operation(bench_dist_init)

        !call dist%print_particles(print_particle)

        ! Do a warm up rep
        call bench_ptr(dist)


        !
        ! Try timing num_reps
        !
        call MPI_Barrier(MPI_COMM_WORLD, ierror)

        start_time = get_time()
        do rep=1, num_reps
            call bench_ptr(dist)
        end do
        end_time = get_time()

        call MPI_Barrier(MPI_COMM_WORLD, ierror)


        !
        ! If our measured time is too low, try more reps
        !
        if(end_time - start_time .LT. 1E-3) then
            do_more_reps = .TRUE.
        end if

        ! Make sure everyone is doing the same thing
        call MPI_Bcast( &
            do_more_reps, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, ierror &
        )

        ! Do the repeat, if necessary
        if(do_more_reps) then
            ! Get suggested rep size
            num_reps = int(1.0/(end_time - start_time))

            ! Use num_reps from process 0
            call MPI_Bcast(num_reps, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)


            !
            ! Do longer timing
            !

            call MPI_Barrier(MPI_COMM_WORLD, ierror)

            start_time = get_time()
            do rep=1, num_reps
                call bench_ptr(dist)
            end do
            end_time = get_time()

            call MPI_Barrier(MPI_COMM_WORLD, ierror)

        end if


        !
        ! Output data
        !

        call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)

        if(rank .EQ. 0) then
            open(88, FILE=output_filename)
            
            write(88, *) &
                "# distrubution_name benchmark_name &
                &num_particles mpi_disabled calculation_disabled &
                &total_cores num_reps time"

            write(88, *) &
                trim(distribution_name)//" ", trim(benchmark_name)//" ", &
                num_particles, total_cores, &
                disable_mpi, disable_calculation, &
                num_reps, end_time-start_time

            close(88)
        end if

    end subroutine run_bench



    !
    ! Parse input arguments into variables
    !   distribution_name, benchmark_name, num_particles, num_reps.
    !
    ! Expect program to be called as
    !   ./bench distribution_name benchmark_name num_particles num_reps
    !
    subroutine parse_arguments( &
        argc, argv, &
        distribution_name, &
        num_particles, num_reps &
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
            write(*,*) &
                "Usage: ./bench dist_name num_particles num_reps"
            stop 1
        end if

        read(argv(1), *) distribution_name
        read(argv(2), *) num_particles
        read(argv(3), *) num_reps
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
