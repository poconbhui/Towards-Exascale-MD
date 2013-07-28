module bench_suite
    use mpi

    use global_variables
    use particle_type

    use abstract_distribution_type
    use integration
    use lj_force

    implicit none

    real(p), private :: system_width(Ndim)
    integer, private :: num_particles
    real(p), private :: time_step

    integer, private :: particle_grid_dim



contains
    function get_time()
        double precision :: get_time

        get_time = MPI_Wtime()
    end function get_time



    !
    ! Initialize number of particles and size of box in bench and integrator
    !
    subroutine bench_suite_init(num_particles_in, system_width_in, time_step_in)
        integer, intent(in) :: num_particles_in
        real(p), intent(in) :: system_width_in(Ndim)
        real(p), intent(in) :: time_step_in


        num_particles = num_particles_in
        system_width  = system_width_in
        time_step     = time_step_in

        call integration_init(time_step)


        ! Expect the number of particles to be in the form n**(3*n)

        particle_grid_dim = int(num_particles_in**(1.0/3))

        if(particle_grid_dim**3 .NE. num_particles_in) then
            write(*,*) "num_particles must be a cube."
            call exit(1)
        end if


    end subroutine bench_suite_init


    !
    ! Distribute particles in a regular grid
    !
    PURE function bench_dist_init(pi, i)
        type(particle) :: bench_dist_init

        type(particle), intent(in) :: pi
        integer, intent(in) :: i


        ! This particles grid position
        integer :: particle_grid_pos(Ndim)
        integer :: d
        integer :: skip_count

        
        skip_count = 1
        do d=Ndim, 1, -1
            particle_grid_pos(d) = mod(i/skip_count, particle_grid_dim)
            skip_count = skip_count*particle_grid_dim
        end do


        bench_dist_init = particle( &
            pos=(particle_grid_pos*system_width), vel=0, force=0, mass=1 &
        )
    end function bench_dist_init


    !
    ! Set procedures
    !

    PURE function bench_integrate_1(p_i, i)
        type(particle) :: bench_integrate_1

        type(particle), intent(in) :: p_i
        integer, intent(in) :: i


        bench_integrate_1 = verlet_integrate_pt1(p_i, i)
    end function bench_integrate_1

    PURE function bench_integrate_2(p_i, i)
        type(particle) :: bench_integrate_2

        type(particle), intent(in) :: p_i
        integer, intent(in) :: i


        bench_integrate_2 = verlet_integrate_pt2(p_i, i)
    end function bench_integrate_2

    PURE function bench_force_init(p_i, i)
        type(particle) :: bench_force_init

        type(particle), intent(in) :: p_i
        integer, intent(in) :: i


        bench_force_init = lj_init(p_i, i)
    end function bench_force_init


    PURE function bench_force_compare(p_i, p_j)
        type(particle) :: bench_force_compare

        type(particle), intent(in) :: p_i
        type(particle), intent(in) :: p_j


        bench_force_compare = lj_compare(p_i, p_j)
    end function bench_force_compare

    PURE function bench_force_merge(p_i, p_j)
        type(particle) :: bench_force_merge

        type(particle), intent(in) :: p_i
        type(particle), intent(in) :: p_j


        bench_force_merge = lj_merge(p_i, p_j)
    end function bench_force_merge


    PURE subroutine print_particle(pi, i, string)
        type(particle), intent(in) :: pi
        integer, intent(in) :: i
        character(len=*), intent(out) :: string

        character(len=100) :: tmp

        write(tmp, *) i, ":", real(pi%pos(:))

        string = adjustl(tmp)
    end subroutine print_particle

end module bench_suite
