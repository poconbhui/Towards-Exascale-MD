module particle_types
    use global_variables

    type particle_type
        REAL(p) :: pos(Ndim)
        REAL(p) :: vel(Ndim)
        REAL(p) :: force(Ndim)
        REAL(p) :: mass
    end type particle_type

end module particle_types
