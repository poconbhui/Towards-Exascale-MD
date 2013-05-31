module force_test
    use particle_types
    use distribution
    use test
    implicit none

    contains
    subroutine force_module_test(name, init, compare, merge)
        character(len=*) :: name
        procedure(one_particle_function) :: init
        procedure(two_particle_function) :: compare
        procedure(two_particle_function) :: merge

        type(particle_type) :: test_particle
        type(particle_type) :: p1, p2

        !
        ! Describe init
        !
        call describe(name // "#init")

        test_particle = particle_type(pos=1, vel=1, force=1, mass=1)

        test_particle = init(test_particle)

        call expect("Position should be unchanged", &
            ALL(test_particle%pos .EQ. 1) &
        )
        call expect("Velocity should be unchanged", &
            ALL(test_particle%vel .EQ. 1) &
        )
        call expect("Force should be initialized to 0", &
            ALL(test_particle%force .EQ. 0) &
        )
        call expect("Mass should be unchanged", &
            test_particle%mass .EQ. 1 &
        )


        !
        ! Describe compare
        !

        ! Expect only force to change
        call describe(name // "#compare force changing")

        p1 = init(particle_type(pos=1, vel=1, force=1, mass=1))
        p2 = init(particle_type(pos=0, vel=1, force=1, mass=1))

        test_particle = compare(p1, p2)

        call expect("Position should be unchanged", &
            ALL(test_particle%pos .EQ. 1) &
        )
        call expect("Velocity should be unchanged", &
            ALL(test_particle%vel .EQ. 1) &
        )
        call expect("Force should change", &
            ALL(test_particle%force .NE. 1) &
        )
        call expect("Mass should be unchanged", &
            test_particle%mass .EQ. 1 &
        )


        ! Expect idempotency
        call describe(name // "#compare idempotency")

        p1 = init(particle_type(pos=1, vel=1, force=1, mass=1))
        p2 = init(particle_type(pos=0, vel=1, force=1, mass=1))

        test_particle = compare(p1, p2)
        p1            = compare(p1, p2)

        call expect("Position should be unchanged", &
            ALL(test_particle%pos .EQ. p1%pos) &
        )
        call expect("Velocity should be unchanged", &
            ALL(test_particle%vel .EQ. p1%vel) &
        )
        call expect("Force should be unchanged", &
            ALL(test_particle%force .EQ. p1%force) &
        )
        call expect("Mass should be unchanged", &
            test_particle%mass .EQ. p1%mass &
        )

        ! Expect idempotency upon repeated application to output variable
        call describe(name // "#compare recursive idempotency")

        p1 = init(particle_type(pos=1, vel=1, force=1, mass=1))
        p2 = init(particle_type(pos=0, vel=1, force=1, mass=1))

        p1 = compare(p1, p2)
        test_particle = p1
        test_particle = compare(test_particle, p2)
        test_particle = compare(test_particle, p2)
        test_particle = compare(test_particle, p2)

        call expect("Position should be unchanged", &
            ALL(test_particle%pos .EQ. p1%pos) &
        )
        call expect("Velocity should be unchanged", &
            ALL(test_particle%vel .EQ. p1%vel) &
        )
        call expect("Force should be unchanged", &
            ALL(test_particle%force .EQ. p1%force) &
        )
        call expect("Mass should be unchanged", &
            test_particle%mass .EQ. p1%mass &
        )
    end subroutine force_module_test

end module force_test
