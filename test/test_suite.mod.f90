module test_suite
    use mpi
    implicit none

    INTEGER, PRIVATE :: error_count = 0

contains
    subroutine get_rank(comm, rank)
        integer, intent(in)  :: comm
        integer, intent(out) :: rank

        logical :: initialized

        integer :: ierror


        call MPI_Initialized(initialized, ierror)

        if(initialized) then
            call MPI_Comm_rank(comm, rank, ierror)
        else
            rank = 0
        end if
    end subroutine get_rank

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

    subroutine describe(string, comm_in)
        character(len=*) :: string
        integer, optional :: comm_in

        integer :: comm
        integer :: rank


        if(present(comm_in)) then
            comm = comm_in
        else
            comm = MPI_COMM_WORLD
        end if

        call get_rank(comm, rank)

        if(rank .EQ. 0) write(*,*) "-", string
    end subroutine describe

    subroutine expect(string, expr, comm_in)
        character(len=*) :: string
        logical :: expr
        integer, optional :: comm_in

        integer :: comm
        integer :: rank

        logical :: global_expr


        if(present(comm_in)) then
            comm = comm_in
        else
            comm = MPI_COMM_WORLD
        end if

        call get_rank(comm, rank)

        global_expr = expr
        call reduce_expr(comm, global_expr)

        if(rank .EQ. 0) then
            write(*,*) "--", string

            if(global_expr) then
                write(*,*) "--- Pass"
            else
                write(*,*) "--- Failed"

                error_count = error_count + 1
            end if
        end if
    end subroutine expect

    function end_test()
        integer :: end_test

        integer :: rank


        call get_rank(MPI_COMM_WORLD, rank)

        if(error_count .EQ. 0) then
            if(rank .EQ. 0) write(*,*) "All tests passed!"
            end_test = 0
        else
            if(rank .EQ. 0) write(*,*) "Encountered ",error_count,"errors"
            end_test = 1
        end if
    end function end_test
end module test_suite
