! MODULE test_suite
!
! This module provides a set of routines for outlining tests
! and for outputting test results.
!
! This is useful for running tests in an MPI environment where
! the result from every process is important, but it
! is desirable for only one process to output the overall result.
!
! For a test to pass, every process should return true. If they don't,
! the test will simply fail. This test suite will give no information
! on why or on which process. It will only state which tests have failed.
!
! This suite should work regardless of whether MPI_Init has been called,
! so it is useful for both MPI and serial environments.
!
module test_suite
    use mpi
    implicit none


    private


    ! Export test_suite functions
    public :: describe
    public :: expect
    public :: end_test


    ! Variable tracking the number of failed tests so far.
    integer, private :: error_count = 0

contains


    ! SUBROUTINE get_rank
    !
    ! Get the rank of the current process.
    ! If MPI is not initialised, return 0.
    !
    subroutine get_rank(comm, rank)
        integer, intent(in)  :: comm
        integer, intent(out) :: rank

        logical :: initialized

        integer :: ierror


        ! Check if MPI_Init has been called.
        call MPI_Initialized(initialized, ierror)

        if(initialized) then
            call MPI_Comm_rank(comm, rank, ierror)
        else
            rank = 0
        end if
    end subroutine get_rank


    ! SUBROUTINE print_string
    !
    ! Print the passed string to screen from rank 0 of the given comm.
    !
    subroutine print_string(string, comm)
        character(len=*), intent(in) :: string
        integer, intent(in) :: comm

        integer :: rank


        call get_rank(comm, rank)

        if(rank .EQ. 0) write(*,*) string
    end subroutine print_string


        


    ! SUBROUTINE reduce_expr
    !
    ! Check if expr evaluates to true on all processes.
    !
    subroutine reduce_expr(comm, expr)
        integer, intent(in) :: comm
        logical, intent(inout) :: expr

        logical :: initialized

        logical :: global_expr

        integer :: ierror


        call MPI_Initialized(initialized, ierror)

        if(initialized) then
            call MPI_Allreduce( &
                expr, global_expr, 1, MPI_LOGICAL, &
                MPI_LAND, comm, &
                ierror &
            )

            expr = global_expr
        end if
    end subroutine reduce_expr


    ! SUBROUTINE describe
    !
    ! Output a string to the screen describing what is currently being
    ! tested.
    !
    subroutine describe(string, comm_in)
        character(len=*) :: string
        integer, optional :: comm_in

        integer :: comm


        if(present(comm_in)) then
            comm = comm_in
        else
            comm = MPI_COMM_WORLD
        end if

        call print_string("- "//string, comm)
    end subroutine describe


    ! SUBROUTINE expect
    !
    ! Test that expr evaluates to true in all the processes in comm_in.
    !
    ! expect will output string to the screen and PASS is expr evaluates
    ! to true on all processes in comm_in, or FALSE otherwise.
    !
    ! The comm_in variable is optional. The default is MPI_COMM_WORLD
    !
    subroutine expect(string, expr, comm_in)
        character(len=*) :: string
        logical :: expr
        integer, optional :: comm_in

        integer :: comm


        ! Deal with optional comm_in parameter
        if(present(comm_in)) then
            comm = comm_in
        else
            comm = MPI_COMM_WORLD
        end if


        ! Find global trueness of expr
        call reduce_expr(comm, expr)


        ! Output expected message
        call print_string("-- "//string, comm)

        ! Output pass status
        if(expr) then
            call print_string("--- Pass", comm)
        else
            call print_string("--- Failed", comm)

            error_count = error_count + 1
        end if
    end subroutine expect


    ! FUNCTION end_test
    !
    ! Finish the tests, output the overall results of the tests to
    ! the screen, and return an appropriate exit value.
    !
    function end_test()
        integer :: end_test

        character(len=20) error_count_str


        if(error_count .EQ. 0) then
            call print_string("All tests passed!", MPI_COMM_WORLD)

            end_test = 0
        else
            write(error_count_str,*) error_count
            error_count_str = adjustl(error_count_str)

            call print_string( &
                "Encountered "//trim(error_count_str)//" errors", &
                MPI_COMM_WORLD &
            )

            end_test = 1
        end if
    end function end_test

end module test_suite
