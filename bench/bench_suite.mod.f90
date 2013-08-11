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


    real(p), allocatable :: bench_force_reduction_init(:)



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

        ! Initialise integrator
        call integration_init(time_step)


        ! Set reduction init
        allocate(bench_force_reduction_init(size(lj_reduction_init)))
        bench_force_reduction_init = lj_reduction_init


        ! Expect the number of particles to be in the form n**(3*n)

        particle_grid_dim = int(num_particles_in**(1.0/3))

        if(particle_grid_dim**3 .NE. num_particles_in) then
            write(*,*) "num_particles must be a cube."
            stop 1
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


    PURE subroutine bench_force_pair_to_val(p_i, p_j, val)
        type(particle), intent(in) :: p_i
        type(particle), intent(in) :: p_j

        real(p), intent(out) :: val(:)


        call lj_pair_to_val(p_i, p_j, val)
    end subroutine bench_force_pair_to_val

    PURE function bench_force_set_val(p_i, force)
        type(particle) :: bench_force_set_val

        type(particle), intent(in) :: p_i
        real(p), intent(in) :: force(:)


        bench_force_set_val = lj_set_val(p_i, force)
    end function bench_force_set_val

    PURE function bench_force_gen_reduce_op(dist)
        integer :: bench_force_gen_reduce_op

        class(abstract_distribution), intent(in) :: dist


        bench_force_gen_reduce_op = lj_gen_reduce_op(dist)
    end function bench_force_gen_reduce_op


    PURE subroutine print_particle(pi, i, string)
        type(particle), intent(in) :: pi
        integer, intent(in) :: i
        character(len=*), intent(out) :: string

        character(len=100) :: tmp

        write(tmp, *) i, ":", real(pi%pos(:))

        string = adjustl(tmp)
    end subroutine print_particle

end module bench_suite
