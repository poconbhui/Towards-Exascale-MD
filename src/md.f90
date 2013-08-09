! MODULE md_functions
!
! The functions contained in this module should really be
! contained in the md program unit, but compiler complaints
! have forced them to be placed in a separate module.
!
module md_functions
  use global_variables
  use particle_type
  implicit none


contains


    ! FUNCTION initialize_particles
    !
    ! Set initial velocities and forces to 0. Set mass to 1.
    ! Set position such that particles are somewhat spread across
    ! space.
    !
    PURE function initialize_particles(p, i)
        type(particle) :: initialize_particles

        type(particle), intent(in) :: p
        integer, intent(in) :: i


        initialize_particles = particle(pos=i, vel=0, force=0, mass=1)
    end function initialize_particles


    ! FUNCTION print_particle
    !
    ! Set output string to the particle number and the current position.
    !
    PURE subroutine print_particle(p, i, string)
        type(particle), intent(in) :: p
        integer, intent(in) :: i
        character(len=*), intent(out) :: string


        write(string,*) i, ": ", p%pos(1), " ", p%pos(2), " ", p%pos(3)
    end subroutine print_particle

end module md_functions


! PROGRAM MD
!
! Perform an MD simulation.
!
program MD
    use md_functions

    use global_variables
    use particle_type

    use abstract_distribution_type
    use serial_distribution_type

    use grav_force
    use integration
    implicit none


    ! Simulation parameters
    real(p) :: total_time = 0.01
    real(p) :: time_step = 0.001
    integer :: particle_count = 10

    ! Command line parsing
    integer :: argc
    character(len=20), dimension(:), allocatable :: argv

    ! Distribution parameters
    character(len=30) :: distribution_name = "serial"

    ! Distribution variables
    class(abstract_distribution), pointer :: dist
    type(serial_distribution), target :: serial_dist

    ! Temporary variables
    real(p) :: current_time
    integer :: i


    ! Parse command line arguments
    argc = command_argument_count()
    allocate(argv(argc))
    do i=1, argc
        call get_command_argument(i, argv(i))
    end do

    if(argc .GT. 0) distribution_name = argv(1)


    ! Initialize random numbers
    call random_seed


    ! Initialize Simulation Parameters
    current_time = 0


    !
    ! Initialize modules
    !

    ! Select distribution module
    select case(distribution_name)
        case("serial")
            serial_dist = new_serial_distribution(particle_count)
            call set_dist(dist, serial_dist)

        case default
            write(*,*) "No such distribution type"
    end select

    ! Initialize integration
    call integration_init(time_step)

    ! Initialize particles
    call dist%individual_operation(initialize_particles)


    ! Perform MD Simulation
    do while(current_time .LT. total_time)

        ! Update forces, velocities and positions
        call dist%individual_operation(verlet_integrate_pt1)

        call dist%pair_operation( &
            grav_pair_to_val, grav_set_val, &
            grav_gen_reduce_op(dist), grav_reduction_init &
        )

        call dist%individual_operation(verlet_integrate_pt2)


        ! Output particle data on every tick
        ! Data output in the form of an xyz movie file as understood
        ! by VMD.
        call dist%print_string(adjustl(particle_count_string()))
        call dist%print_string(adjustl(current_time_string()))
        call dist%print_particles(print_particle)


        ! Update time
        current_time = current_time + time_step

    end do


contains


    !
    ! Printing functions
    !


    ! FUNCTION particle_count_string
    !
    ! Return a string containing the current particle count.
    !
    function particle_count_string()
        character(len=80) :: particle_count_string


        write(particle_count_string, *) particle_count
    end function particle_count_string


    ! FUNCTION current_time_string
    !
    ! Return a string containing the current time.
    !
    function current_time_string()
        character(len=80) :: current_time_string


        write(current_time_string, *) "Time: ", current_time
    end function current_time_string


    ! SUBROUTINE set_dist
    !
    ! This subroutine is required because setting an abstract_distribution
    ! class pointer to point to some initialized distribution type
    ! causes a compiler error in crayftn.
    !
    subroutine set_dist(dist_out, dist_in)
      class(abstract_distribution), pointer :: dist_out
      class(abstract_distribution), target  :: dist_in

      dist_out => dist_in
    end subroutine set_dist

end program MD
