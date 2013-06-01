program MD
    use global_variables
    use particle_type
    use integration
    use abstract_distribution_type
    use serial_distribution_type
    use grav_force
    implicit none

    !
    ! Simulation parameters
    !
    real(p) :: total_time = 10
    real(p) :: time_step = 0.001
    integer :: particle_count = 10

    !
    ! Distribution parameters
    !
    character(len=30) :: distribution_name = "SERIAL"

    !
    ! Distribution variables
    !
    class(abstract_distribution), pointer :: dist
    type(serial_distribution), target :: serial_dist

    !
    ! Temporary variables
    !
    real(p) :: current_time
    character(len=80) :: string


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
        case("SERIAL")
            serial_dist = serial_distribution(particle_count)
            dist => serial_dist
    end select

    ! Initialize integration
    call integration_init(time_step)



    !
    ! Initialize particles
    !
    call dist%individual_operation(initialize_particles)


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

    function initialize_particles(p)
        type(particle), intent(in) :: p
        type(particle) :: initialize_particles

        ! initialize to zero
        initialize_particles = particle(pos=0, vel=0, force=0, mass=1)

        ! randomize position
        call random_number(initialize_particles%pos)
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

    function print_particle(p)
        type(particle), intent(in) :: p
        character(len=80) :: print_particle

        character(len=100) :: tmp_string

        write(tmp_string,*) p%pos
        tmp_string = adjustl(tmp_string)

        print_particle = tmp_string
    end function print_particle


    !
    ! Consistency test functions
    !

    function map(p1)
        type(particle), intent(in) :: p1

        real(p) :: map

        map = 2
    end function map

    function reduce(d1, d2)
        real(p), intent(in) :: d1
        real(p), intent(in) :: d2

        real(p) :: reduce

        reduce = d1 + d2
    end function reduce
end program MD
