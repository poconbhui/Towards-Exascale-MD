module bench_suite
    use mpi
    use global_variables
    use particle_type
    implicit none

    integer, parameter :: badRNG_p = selected_int_kind(18)
    integer(badRNG_p), parameter :: badRNG_max = (2_badRNG_p)**32

contains
    function get_time()
        double precision :: get_time

        get_time = MPI_Wtime()
    end function get_time

    PURE function dist_init(pi, i)
        type(particle) :: dist_init

        type(particle), intent(in) :: pi
        integer, intent(in) :: i

        integer(badRNG_p) :: state
        real(p) :: pos(Ndim)

        integer :: d

        state = i
        state = badRNG(state)

        do d=1, Ndim
            state  = badRNG(state)
            pos(d) = state
        end do

        pos = pos/badRNG_max

        dist_init = particle(pos=pos, vel=0, force=0, mass=1)
    end function dist_init

    PURE subroutine print_particle(pi, i, string)
        type(particle), intent(in) :: pi
        integer, intent(in) :: i
        character(len=*), intent(out) :: string

        character(len=100) :: tmp

        write(tmp, *) i, ":", real(pi%pos(:))

        string = adjustl(tmp)
    end subroutine print_particle

    ! We don't need a good RNG.
    ! We just need something to spread particles out a bit which is
    ! PURE and reproducable.
    !
    ! badRNG produces outputs which are close for inputs that are close.
    PURE function badRNG(x)
        integer(badRNG_p) :: badRNG

        integer(badRNG_p), intent(in) :: x

        ! LCG values from Numerical Recipes.
        integer(badRNG_p), parameter :: m = (2_badRNG_p)**32
        integer(badRNG_p), parameter :: a = 1664525
        integer(badRNG_p), parameter :: c = 1013904223

        badRNG = mod(a*x + c, m)
    end function badRNG
end module bench_suite
