program serial_distribution_test
    use distribution_test
    use particle_type
    use abstract_distribution_type
    use serial_distribution_type
    use global_variables
    implicit none


    integer :: num_particles
    type(serial_distribution):: dist


    num_particles = 10
    dist = new_serial_distribution(num_particles)

    call distribution_module_test(dist, num_particles)

contains
    PURE subroutine print_particle(pi, i, string)
        type(particle), intent(in) :: pi
        integer, intent(in) :: i
        character(len=*), intent(out) :: string


        write(string,*) i, pi%pos(i), pi%vel(i)
    end subroutine print_particle



end program serial_distribution_test
