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
    contains
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
            call exit(1)
        end if


        N = size(reduction_identity)


        if(.NOT. disable_calculation) then

            call this%get_chunk_data(this%rank, i_size, i_start, i_end)

            !$OMP PARALLEL DO &
            !$OMP&  DEFAULT(none) &
            !$OMP&  PRIVATE(reduce_val, tmp_val, i, j) &
            !$OMP&  SHARED(reduction_identity, this, reduce_func)
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

    PURE function reduce_sum(arr1, arr2)
        real(p), intent(in) :: arr1(:)
        real(p), intent(in) :: arr2(size(arr1))

        real(p) :: reduce_sum(size(arr1))


        reduce_sum = arr1 + arr2
    end function reduce_sum


end module shared_and_replicated_distribution_type
