! MODULE global_variables
!
! Define global_variables to be used throughout the program.
!
module global_variables
    use mpi, only : MPI_DOUBLE_PRECISION
    implicit none

    ! Precision data
    integer, parameter :: dp = kind(0.0d0)
    integer, parameter :: p = dp
    integer, parameter :: MPI_REAL_P = MPI_DOUBLE_PRECISION

    ! Dimensions
    integer, parameter :: Ndim = 3
end module global_variables
