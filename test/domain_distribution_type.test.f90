module domain_distribution_test_funcs
    use particle_type
    use global_variables
    use test_suite
    use mpi
    implicit none

    integer :: num_particles
    real(p) :: domain_size(Ndim)

    integer :: rank

contains
    PURE function initial_distribution(pi, i)
        type(particle) :: initial_distribution
        
        type(particle), intent(in) :: pi
        integer, intent(in) :: i


        initial_distribution = particle( &
            pos   = i*domain_size/num_particles, &
            vel   = 0, &
            force = 0, &
            mass  = 1 &
        )
    end function initial_distribution

    PURE function second_distribution(pi, i)
        type(particle) :: second_distribution

        type(particle), intent(in) :: pi
        integer, intent(in) :: i


        second_distribution = particle( &
            pos   = (mod(i, num_particles)+1)*domain_size/num_particles, &
            vel   = 0, &
            force = 0, &
            mass  = 1 &
        )
    end function second_distribution

    PURE function balance_list_test_map(pi)
        real(p) :: balance_list_test_map

        type(particle), intent(in) :: pi


        balance_list_test_map = pi%pos(1)*rank
    end function balance_list_test_map

    PURE function balance_list_test_reduce(d1, d2)
        real(p) :: balance_list_test_reduce

        real(p), intent(in) :: d1, d2


        balance_list_test_reduce = d1 + d2
    end function balance_list_test_reduce

    PURE subroutine print_particle(pi, i, string)
        type(particle), intent(in) :: pi
        integer, intent(in) :: i
        character(len=*), intent(out) :: string


        write(string,*) rank, i, pi%pos(1)
    end subroutine print_particle

end module domain_distribution_test_funcs

program domain_distribution_test
    use particle_type
    use distribution_test
    use domain_distribution_type
    use global_variables
    use test_suite
    use mpi
    use domain_distribution_test_funcs
    implicit none


    type(domain_distribution) :: dist

    real(p) :: test_value(2)

    integer :: exit_value
    integer :: ierr


    call MPI_Init(ierr)


    num_particles = 10
    domain_size = 10
    dist = new_domain_distribution( &
        num_particles, domain_size, initial_distribution, &
        MPI_COMM_WORLD &
    )


    !
    ! Check usual distribution requirements
    !
    call distribution_module_test(dist, num_particles)


    !
    ! Check list rebalancing
    !
    call test_list_balancing


    exit_value = end_test()
    call MPI_Finalize(ierr)
    call exit(exit_value)

contains
    subroutine test_list_balancing
        !
        ! Expect balancing to happen over x dim
        !
        call describe("List Balancing")

        call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

        call dist%individual_operation(initial_distribution)
        call dist%balance_lists

        test_value(1) = 0
        call dist%global_map_reduce( &
            balance_list_test_map, balance_list_test_reduce, test_value(1) &
        )

        call dist%individual_operation(second_distribution)
        call dist%balance_lists

        test_value(2) = 0
        call dist%global_map_reduce( &
            balance_list_test_map, balance_list_test_reduce, test_value(2) &
        )

        call expect( &
            "After positions are updated, particles should move so processors&
            & contain particles in the same positions as before.", &
            int(test_value(1)) .EQ. int(test_value(2)) &
        )
    end subroutine test_list_balancing
end program domain_distribution_test
