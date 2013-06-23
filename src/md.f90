module ops
  use global_variables
  use particle_type
  implicit none

contains
    PURE subroutine print_particle(p, i, string)
        type(particle), intent(in) :: p
        integer, intent(in) :: i
        character(len=*), intent(out) :: string


        write(string,*) p%pos
        string = adjustl(string)
    end subroutine print_particle

end module ops

program MD
    use global_variables
    use particle_type
    use integration
    use abstract_distribution_type
    use serial_distribution_type
    use grav_force
    use ops
    implicit none

    !
    ! Simulation parameters
    !
    real(p) :: total_time = 10
    real(p) :: time_step = 0.001
    integer :: particle_count = 10

    !
    ! Command line parsing
    !
    integer :: argc
    character(len=20), dimension(:), allocatable :: argv

    !
    ! Distribution parameters
    !
    character(len=30) :: distribution_name = "serial"

    !
    ! Distribution variables
    !
    class(abstract_distribution), pointer :: dist
    type(serial_distribution), target :: serial_dist

    !
    ! Temporary variables
    !
    real(p) :: current_time
    integer :: i


    !
    ! Parse command line arguments
    !
    argc = command_argument_count()
    allocate(argv(argc))
    do i=1, argc
        call get_command_argument(i, argv(i))
    end do

    if(argc .GT. 0) distribution_name = argv(1)


    !
    ! Initialize random numbers
    !
    call random_seed


    !
    ! Initialize Simulation Parameters
    !
    current_time = 0


    !
    ! Initialize modules
    !

    ! Select distribution module
    select case(distribution_name)
        case("serial")
            serial_dist = new_serial_distribution(particle_count)
            dist => serial_dist

        case default
            write(*,*) "No such distribution type"
    end select

    ! Initialize integration
    call integration_init(time_step)



    !
    ! Initialize particles
    !
    !call dist%individual_operation(initialize_particles)


    !
    ! Perform Simulation
    !
    do while(current_time .LT. total_time)
        call dist%individual_operation(verlet_integrate_pt1)

        call dist%individual_operation(grav_init)
        call dist%pair_operation(grav_compare, grav_merge)

        call dist%individual_operation(verlet_integrate_pt2)

        ! Output particle data
        call dist%print_string(adjustl(particle_count_string()))
        call dist%print_string(adjustl(current_time_string()))
        call dist%print_particles(print_particle)

        current_time = current_time + time_step
    end do



contains

    !
    ! Initialization functions
    !

    PURE function initialize_particles(p, i)
        type(particle) :: initialize_particles

        type(particle), intent(in) :: p
        integer, intent(in) :: i


        initialize_particles = particle(pos=i, vel=0, force=0, mass=1)
    end function initialize_particles


    !
    ! Printing functions
    !

    function particle_count_string()
        character(len=80) :: particle_count_string


        write(particle_count_string, *) particle_count
    end function particle_count_string

    function current_time_string()
        character(len=80) :: current_time_string


        write(current_time_string, *) "Time: ", current_time
    end function current_time_string

end program MD
