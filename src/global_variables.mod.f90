module global_variables
    use mpi
    implicit none

    integer, parameter :: dp = kind(0.0d0)
    integer, parameter :: p = dp
    integer, parameter :: MPI_REAL_P = MPI_DOUBLE


    integer, parameter :: Ndim = 3
end module global_variables
