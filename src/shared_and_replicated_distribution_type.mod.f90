module shared_and_replicated_distribution_type
    use abstract_distribution_type
    use replicated_distribution_type
    use particle_type
    use global_variables
    use mpi
    use bench_flags
    implicit none


    private


    public :: shared_and_replicated_distribution
    public :: new_shared_and_replicated_distribution


    type, EXTENDS(replicated_distribution) :: shared_and_replicated_distribution
        integer :: num_openmp
    contains
        procedure :: init
        procedure :: pair_operation
    end type shared_and_replicated_distribution

contains
    function new_shared_and_replicated_distribution(particle_count, comm)
        type(shared_and_replicated_distribution) &
            :: new_shared_and_replicated_distribution
        integer, intent(in) :: particle_count
        integer, intent(in) :: comm

        call new_shared_and_replicated_distribution%init(particle_count, comm)
    end function new_shared_and_replicated_distribution


    subroutine init(this, num_particles, comm)
        class(shared_and_replicated_distribution), intent(out) :: this
        integer, intent(in) :: num_particles
        integer, intent(in) :: comm

        integer :: num_openmp
        integer :: num_cores


        !
        ! Initialise as a replicated distribution
        !
        call this%replicated_distribution%init(num_particles, comm)


        !
        ! Find the number of OpenMP thread per MPI process
        !

        ! Find the number of OpenMP cores per MPI process
        num_openmp = 0
        !$OMP PARALLEL REDUCTION(+:num_openmp)
            num_openmp = num_openmp + 1
        !$OMP END PARALLEL

        this%num_openmp = num_openmp


        !
        ! Require num_particles <= total number of cores
        !

        ! Find the total number of cores used
        num_cores = this%num_openmp*this%nprocs

        if(this%num_particles .LT. num_cores) then
            call this%print_string( &
                "Error: The total number of cores requested is less than &
                &the number of particles in the system" &
            )
            stop 1
        end if


    end subroutine init


    subroutine pair_operation( &
        this, pair_to_val, val_to_particle, reduce_op, reduction_identity &
    )
        class(shared_and_replicated_distribution), intent(inout) :: this

        procedure(two_particle_to_array_subroutine) :: pair_to_val
        procedure(particle_and_array_to_particle_function) :: val_to_particle
        integer :: reduce_op
        real(p) :: reduction_identity(:)

        integer :: N

        integer :: i_size, i_start, i_end
        real(p) :: reduce_val(size(reduction_identity))
        real(p) :: tmp_val(size(reduction_identity))
        integer :: i, j


        interface reduce_types
            PURE function reduce_type(arr1, arr2)
                use global_variables
                implicit none

                real(p), intent(in) :: arr1(:)
                real(p), intent(in) :: arr2(size(arr1))

                real(p) :: reduce_type(size(arr1))
            end function reduce_type
        end interface

        procedure(reduce_type), pointer :: reduce_func


        !
        ! Set reduction function
        !
        if(reduce_op .EQ. MPI_SUM) then
            reduce_func => reduce_sum
        else
            call this%print_string("Error: provided reduce_op not supported!")
            stop 1
        end if


        N = size(reduction_identity)


        if(.NOT. disable_calculation) then

            call this%get_chunk_data(this%rank, i_size, i_start, i_end)


            !$OMP PARALLEL DO &
            !$OMP&  DEFAULT(none) &
            !$OMP&  PRIVATE(reduce_val, tmp_val, i, j) &
            !$OMP&  SHARED( &
            !$OMP&      reduction_identity, this, reduce_func, &
            !$OMP&      i_start, i_end &
            !$OMP&  )
            !
            do i=i_start, i_end
                reduce_val = reduction_identity

                do j=1, this%num_particles
                    if(i .EQ. j) cycle

                    call pair_to_val( &
                        this%particles(i), this%particles(j), &
                        tmp_val &
                    )
                    
                    reduce_val = reduce_func(reduce_val, tmp_val)

                end do

                this%particles(i) = val_to_particle( &
                    this%particles(i), reduce_val &
                )
            end do
            !
            !$OMP END PARALLEL DO

        end if

        call this%sync_particles

    end subroutine pair_operation


    ! FUNCTION reduce_sum
    !
    ! This function accepts two arrays and sums them elementwise.
    !
    ! This is used by pair_operation so the reduce_op used can
    ! be changed dynamically.
    !
    PURE function reduce_sum(arr1, arr2)
        real(p), intent(in) :: arr1(:)
        real(p), intent(in) :: arr2(size(arr1))

        real(p) :: reduce_sum(size(arr1))


        reduce_sum = arr1 + arr2
    end function reduce_sum


end module shared_and_replicated_distribution_type
