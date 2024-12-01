module m_problem_setup

   ! Dependencies =======================
    use m_global_parameters
    use m_derived_types
    use m_problems
    ! ====================================

    implicit none

    private; public :: s_get_problem, &
        s_setup_problem, &
        s_finalize_problem

contains

    ! This subroutine gets the static parameters that describe the boundary
    ! conditions, computational domain, and discretization for a problem. It
    ! gets this information by call the subroutine s_get_<case> for a specific
    ! <case>
    subroutine s_get_problem()

        ! Get case specific details
        select case(problemID)
            case(0)
                call s_get_2D_lid_driven_cavity()
            case(1)
                call s_get_3D_lid_driven_cavity()
        end select

    end subroutine s_get_problem

    ! This subroutine allocates memory time variables given the information
    ! provided by s_get_problem. This subroutine allocates variables that all
    ! cases will use and then calls the respective s_get_<case> subroutine to
    ! assign the initial condition. Its inputs are:
    !  Q: The scalar field that will be used to store the initial condition
    !  f: The distribution function
    !  fEq: The equilibrium distribution function
    subroutine s_setup_problem(Q, f, fEq)

        real(kind(0d0)), allocatable, dimension(:,:,:,:) :: Q
        real(kind(0d0)), allocatable, dimension(:,:,:,:) :: f, fEq
        integer :: i

        ! Allocating cell boundary locations
        allocate(coord_info%x_cb(0:decomp_info%m + 1))
        allocate(coord_info%y_cb(0:decomp_info%n + 1))
        if (num_dims == 3) then
            allocate(coord_info%z_cb(0:decomp_info%p + 1))
        end if
        !$acc enter data create(coord_info)

        allocate(Q(0:decomp_info%m, 0:decomp_info%n, 0:decomp_info%p, 0:num_dims))
        !$acc enter data create(Q)

        allocate(f(0:decomp_info%m, 0:decomp_info%n, 0:decomp_info%p, 0:coll_op%Q))
        allocate(fEq(0:decomp_info%m, 0:decomp_info%n, 0:decomp_info%p, 0:coll_op%Q))
        !$acc enter data create(f, fEq)

        do i = 0, decomp_info%m + 1
            coord_info%x_cb(i) = i
        end do

        do i = 0, decomp_info%m + 1
            coord_info%y_cb(i) = i
        end do

        if (num_dims == 3) then
            do i = 0, decomp_info%p + 1
                coord_info%z_cb(i) = i
            end do
        end if
        !$acc update device(coord_info)

        ! Setup case specific details
        select case(problemID)
            case(0)
                call s_setup_2D_lid_driven_cavity(Q)
            case(1)
                call s_setup_3D_lid_driven_cavity(Q)
        end select

    end subroutine s_setup_problem

    ! This subroutine deallocates memory for the variables that were allocated.
    ! Its inputs are:
    !  Q: The scalar field that will be used to store the initial condition
    !  f: The distribution function
    !  fEq: The equilibrium distribution function
    subroutine s_finalize_problem(Q, f, fEq)

        real(kind(0d0)), allocatable, dimension(:,:,:,:) :: Q
        real(kind(0d0)),allocatable, dimension(:,:,:,:) :: f, fStar, fEq
        integer :: i

        deallocate(Q)

        deallocate(f)

        deallocate(fEq)

        !$acc exit data delete(Q, f, fEq)

    end subroutine s_finalize_problem

end module m_problem_setup
