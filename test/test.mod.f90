module test
    implicit none

    CHARACTER(len=80), PRIVATE :: description = ""
    INTEGER, PRIVATE :: error_count = 0

    contains
    subroutine describe(string)
        character(len=*) :: string

        description = string
    end subroutine describe

    subroutine expect(string, expr)
        CHARACTER(len=*) :: string
        LOGICAL :: expr

        ! If expr is true, do nothing
        if(expr) return

        write(*,*)
        write(*,*) description, string

        error_count = error_count + 1
    end subroutine expect

    function end_test()
        INTEGER :: end_test
        end_test = 0

        if(error_count .GT. 0) then
            write(*,*)
            write(*,*) "Encountered ",error_count,"errors"
            end_test = 1
        end if
    end function end_test
end module test
