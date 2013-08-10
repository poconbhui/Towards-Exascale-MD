! MODULE distribution_test
!
! This module exports a function distribution_module_test which is
! used to check if a given distribution has the minimum facilities
! that we expect and that they work.
!
! A distribution that passes distribution_module_test is considered
! to be working.
!
module distribution_test
    use test_suite

    use abstract_distribution_type

    use particle_type
    use global_variables
    implicit none


    private


    ! Export the distribution_module_test
    public :: distribution_module_test


    ! REAL init_multiplier
    !
    ! This is used in conjunction with the function init
    ! to set the multiplier used when setting a partile pos.
    !
    real(p) :: init_multiplier = 1


contains


    ! SUBROUTINE distribution_module_test
    !
    ! This should accept a distribution and a number of particles
    ! to run the tests for.
    !
    ! This should be able to tell if the individual_operation,
    ! global_map_reduce and pair_operation routines are working
    ! as expected.
    !
    ! If a distribution passes these tests, it is considered to be working.
    !
    subroutine distribution_module_test(dist, num_particles)
        class(abstract_distribution), intent(inout) :: dist
        integer, intent(in) :: num_particles

        real(p) :: tmp_reduction_value
        real(p) :: reduction_value
        real(p) :: pair_op_identity(Ndim)

        real(p) :: x
        integer :: i, j


        !
        ! TEST global_map_reduce
        !

        call describe("global_map_reduce")

        !
        ! We've assumed individual_operation works here
        ! to check global_map_reduce.
        !
        ! We'll later assume, based on this test that global_map_reduce works
        ! to test individual_operation.
        !
        ! This is probably a silly thing to do, but we should at least
        ! know that they're self consistent.
        !

        ! Initialise particles with pos=i
        init_multiplier = 1
        call dist%individual_operation(init)

        ! Sum particle positions
        reduction_value = 0
        call dist%global_map_reduce( &
            pos_sum_map, pos_sum_reduce, reduction_value &
        )

        ! Calculate expected sum of particle positions
        x=0
        do i=1, num_particles
            x = x + i
        end do

        ! Check actual and expected values are the same
        call expect( &
            "Reduction should return the sum of the numbers from &
            &1 to num_particles", &
            reduction_value .EQ. x &
        )


        !
        ! TEST individual_operation
        !

        call describe("individual_operation")

        !
        ! Test that individual_operation works over multiple calls
        ! with different initialisation conditions on each call.
        !
        ! Should also test that global_map_reduce works over
        ! multiple calls.
        !

        x=0
        reduction_value = 0
        do i=0, 100, 10

            ! Set pos=particle_i*i
            init_multiplier = i
            call dist%individual_operation(init)

            ! Find sum of particle pos
            tmp_reduction_value = 0
            call dist%global_map_reduce( &
                pos_sum_map, pos_sum_reduce, tmp_reduction_value &
            )

            ! Add it to the total reduction value for this loop
            reduction_value = reduction_value + tmp_reduction_value

            ! Calculate expected sum and add it to the total expected sum
            do j=1, num_particles
                x = x + i*j
            end do
        end do

        ! Check that actual reduction and expected reduction match
        call expect( &
            "Reduction should return &
            &sum(i=0,100,10, i*sum(n=1,num_particles,1, n))", &
            reduction_value .EQ. x &
        )



        !
        ! TEST pair_operation
        !

        call describe("pair_operation")

        ! Initialise particle pos=i
        init_multiplier = 1
        call dist%individual_operation(init)

        !
        ! Expect pair_operation to for a particle to sum the pos of
        ! each particle it is compared with into its velocity.
        ! So, the velocity should be a sum of the pos of every particle
        ! in the system, minus the particle's own pos.
        !
        ! When all of these values are summed together, it should look like
        ! a multiple of the value of all the numbers summed together minus
        ! the value of all the numbers summed together.
        !
        pair_op_identity = 0
        call dist%pair_operation( &
            pair_to_p2_pos, pos_to_p1_vel, &
            dist%sum, pair_op_identity &
        )

        ! Find sum of all vels
        reduction_value = 0
        call dist%global_map_reduce( &
            vel_sum_map, vel_sum_reduce, reduction_value &
        )

        ! Calculate expected reduction
        x = 0
        do i=1, num_particles
            x = x + i
        end do
        x = x*(num_particles-1)

        ! Check actual and expected values match
        call expect( &
            "pair_operation should compare all particles &
            &with all other particles",&
            reduction_value .EQ. x &
        )
    end subroutine distribution_module_test


    ! FUNCTION init
    !
    ! This is for use in the individual_operation.
    !
    ! This sets a particle to some default values.
    ! In particular, it sets the pos to the particle number
    ! multiplied by init_multiplier.
    !
    ! init_multiplier is a module variable that should be set
    ! to an appropriate value before this function is used.
    !
    PURE function init(pi, i)
        type(particle) :: init

        type(particle), intent(in) :: pi
        integer, intent(in) :: i


        init = particle( &
            pos=init_multiplier*i, vel=0, force=0, mass=1 &
        )
    end function init


    ! FUNCTION pos_sum_map
    !
    ! This is used as a map in global_map_reduce.
    ! This maps a particle to the value of particle%pos(1)
    !
    PURE function pos_sum_map(pi)
        real(p) :: pos_sum_map

        type(particle), intent(in) :: pi


        pos_sum_map = pi%pos(1)
    end function pos_sum_map


    ! FUNCTION pos_sum_reduce
    !
    ! This function is used as a reducer in global_map_reduce
    ! This just sums the values passed into it.
    !
    ! It is intended to be used in conjunction with pos_sum_map
    !
    PURE function pos_sum_reduce(d1, d2)
        real(p) :: pos_sum_reduce

        real(p), intent(in) :: d1
        real(p), intent(in) :: d2


        pos_sum_reduce = d1 + d2
    end function pos_sum_reduce


    ! SUBROUTINE pair_to_p2_pos
    !
    ! This is for use in pair_operation
    ! It is expected to be used in conjunction with pos_to_p1_vel.
    !
    ! It maps two particles to the pos of the 2nd particle.
    !
    PURE subroutine pair_to_p2_pos(p1, p2, pos)
        type(particle), intent(in) :: p1
        type(particle), intent(in) :: p2

        real(p), intent(out) :: pos(:)


        pos = p2%pos
    end subroutine pair_to_p2_pos


    ! FUNCTION pos_to_p1_vel
    !
    ! This is for use in pair_operation
    ! It is expected to be used in conjunction with pair_to_p2_pos
    !
    ! It maps a particle and a value to a particle with
    ! its velocity set to the given value.
    !
    PURE function pos_to_p1_vel(p1, pos)
        type(particle) :: pos_to_p1_vel

        type(particle), intent(in) :: p1
        real(p), intent(in) :: pos(:)


        pos_to_p1_vel = p1
        pos_to_p1_vel%vel = pos
    end function pos_to_p1_vel


    ! FUNCTION vel_sum_map
    !
    ! This is for use as a mapper with global_map_reduce
    ! It is expected to be used in conjunction with vel_sum_reduce
    !
    ! This maps a particle to the value of particle%vel(1)
    !
    PURE function vel_sum_map(p1)
        real(p) :: vel_sum_map

        type(particle), intent(in) :: p1


        vel_sum_map = p1%vel(1)
    end function vel_sum_map


    ! FUNCTION vel_sum_reduce
    !
    ! This is for use as a reducer with global_map_reduce
    ! It is expected to be used in conjunction with vel_sum_map
    !
    ! This adds the values passed into it.
    !
    PURE function vel_sum_reduce(d1, d2)
        real(p) :: vel_sum_reduce

        real(p), intent(in) :: d1
        real(p), intent(in) :: d2


        vel_sum_reduce = d1 + d2
    end function vel_sum_reduce

end module distribution_test
