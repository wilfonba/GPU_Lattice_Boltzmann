module m_problem_setup

   ! Dependencies =======================
    use m_global_parameters
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
        allocate(x_cb(0:m + 1))
        allocate(y_cb(0:n + 1))
        !$acc enter data create(x_cb, y_cb)
        if (num_dims == 3) then
            allocate(z_cb(0:p + 1))
            !$acc enter data create(z_cb)
        end if

        allocate(Q(0:m, 0:n, 0:p, 0:num_dims))
        !$acc enter data create(Q)

        allocate(f(0:m, 0:n, 0:p, 0:nQ))
        allocate(fEq(0:m, 0:n, 0:p, 0:nQ))
        !$acc enter data create(f, fEq)

        do i = 0, m + 1
            x_cb(i) = i
        end do
        !$acc update device(x_cb)

        do i = 0, m + 1
            y_cb(i) = i
        end do
        !$acc update device(y_cb)

        if (num_dims == 3) then
            do i = 0, p + 1
                z_cb(i) = i
            end do
            !$acc update device(z_cb)
        end if

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
