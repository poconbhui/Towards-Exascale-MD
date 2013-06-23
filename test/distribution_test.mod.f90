module distribution_test
    use test_suite
    use abstract_distribution_type
    use particle_type
    use global_variables
    implicit none

    private

    public :: distribution_module_test

    real(p) :: init_multiplier = 1

contains
    subroutine distribution_module_test(dist, num_particles)
        class(abstract_distribution), intent(inout) :: dist
        integer, intent(in) :: num_particles

        real(p) :: reduction_value

        real(p) :: x
        integer :: i, j


        call describe("global_map_reduce")

        ! We've assumed individual_operation works here to check global_map_reduce.
        ! We'll later assume, based on this test that global_map_reduce works
        ! to test individual_operation. This is probably a slly thing to do.
        init_multiplier = 1
        call dist%individual_operation(init)

        reduction_value = 0
        call dist%global_map_reduce( &
            pos_sum_map, pos_sum_reduce, reduction_value &
        )

        x=0
        do i=1, num_particles
            x = x + i
        end do

        call expect( &
            "Reduction should return the sum of the numbers from 1 to 10.", &
            reduction_value .EQ. x &
        )


        call describe("individual_operation")

        do i=0, 100, 10
            init_multiplier = i
            call dist%individual_operation(init)

            reduction_value = 0
            call dist%global_map_reduce( &
                pos_sum_map, pos_sum_reduce, reduction_value &
            )

            x=0
            do j=1, num_particles
                x = x + i*j
            end do


            call expect( &
                "Reduction should return a sum from 1 to 10 times i.", &
                reduction_value .EQ. x &
            )
        end do


        call describe("pair_operation")
        x = 0
        do i=1, num_particles
            x = x + i
        end do

        init_multiplier = 1
        call dist%individual_operation(init)

        ! Expect pair_operation to set each particle to the sum of the number
        ! of every particle minus their own.
        ! When all of these values are summed together, it should look like
        ! a multiple of the value of all the numbers summed together minus
        ! the value of all the numbers summed together.
        call dist%pair_operation(pair_operation, pair_merge)

        reduction_value = 0
        call dist%global_map_reduce( &
            vel_sum_map, vel_sum_reduce, reduction_value &
        )

        call expect( &
            "pair_operation should compare all particles &
            &with all other particles",&
            reduction_value .EQ. x*(num_particles-1) &
        )
    end subroutine distribution_module_test

    PURE function init(pi, i)
        type(particle) :: init

        type(particle), intent(in) :: pi
        integer, intent(in) :: i


        init = particle( &
            pos=init_multiplier*i, vel=0, force=0, mass=1 &
        )
    end function init

    PURE subroutine print_particle(pi, i, string)
        type(particle), intent(in) :: pi
        integer, intent(in) :: i
        character(len=*), intent(out) :: string


        write(string, *) i, pi%pos(1), pi%vel(1)
        string = adjustl(string)
    end subroutine print_particle

    PURE function pos_sum_map(pi)
        real(p) :: pos_sum_map

        type(particle), intent(in) :: pi


        pos_sum_map = pi%pos(1)
    end function pos_sum_map

    PURE function pos_sum_reduce(d1, d2)
        real(p) :: pos_sum_reduce

        real(p), intent(in) :: d1
        real(p), intent(in) :: d2


        pos_sum_reduce = d1 + d2
    end function pos_sum_reduce

    PURE function pair_operation(p1, p2)
        type(particle) :: pair_operation

        type(particle), intent(in) :: p1
        type(particle), intent(in) :: p2


        pair_operation = p1
        pair_operation%vel = p2%pos
    end function pair_operation

    PURE function pair_merge(p1, p2)
        type(particle) :: pair_merge

        type(particle), intent(in) :: p1
        type(particle), intent(in) :: p2


        pair_merge = p1
        pair_merge%vel = p1%vel + p2%vel
    end function pair_merge

    PURE function vel_sum_map(p1)
        real(p) :: vel_sum_map

        type(particle), intent(in) :: p1


        vel_sum_map = p1%vel(1)
    end function vel_sum_map

    PURE function vel_sum_reduce(d1, d2)
        real(p) :: vel_sum_reduce

        real(p), intent(in) :: d1
        real(p), intent(in) :: d2


        vel_sum_reduce = d1 + d2
    end function vel_sum_reduce

end module distribution_test
