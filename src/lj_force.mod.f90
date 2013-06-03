module LJ_force
    use global_variables
    use particle_type
    implicit none

    !
    ! Delta and Epsilon parameters for the Lennard-Jones potential
    !
    real(p) :: del=1, eps=1

    contains
    PURE function LJ_init(p, i)
        type(particle) :: LJ_init

        type(particle), intent(in) :: p
        integer, intent(in) :: i


        LJ_init = p
        LJ_init%force = 0
    end function LJ_init

    PURE function LJ_compare(p1, p2)
        type(particle) :: LJ_compare

        type(particle), intent(in) :: p1
        type(particle), intent(in)    :: p2


        real(p) :: d(Ndim)
        real(p) :: r
        real(p) :: d_unit(Ndim)
        real(p) :: force(Ndim)


        d = p2%pos - p1%pos
        r = sqrt(sum(d*d))
        d_unit = d/r

        if(r.LT.0.05) r=0.05

        force = -24*eps * ( 2*(del**12/r**13) - (del**6/r**7) ) * d_unit

        LJ_compare = p1
        LJ_compare%force = force
    end function LJ_compare

    PURE function LJ_merge(p1, p2)
        type(particle) :: LJ_merge

        type(particle), intent(in) :: p1
        type(particle), intent(in)    :: p2


        LJ_merge = p1
        LJ_merge%force = p1%force + p2%force
    end function LJ_merge
end module LJ_force
