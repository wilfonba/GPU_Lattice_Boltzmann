module m_problem_setup

   ! Dependencies =======================
    use m_global_parameters
    use m_mpi
    use m_derived_types
    ! ====================================

    implicit none

    integer :: i, j, k ! Standard loop indices

    public; private :: s_get_problem

contains

    subroutine s_get_problem()

        select case(problemID)
            case(0)
                call s_get_2D_lid_driven_cavity()
            case(1)
                call s_get_2D_laminar_boundary_layer()
            case(2)
                call s_get_2D_laminar_channel_flow()
            case(3)
                call s_get_3D_flow_over_sphere()
            case(4)
                call s_get_3D_rayleigh_taylor_instability()
            case default
                call s_mpi_abort("Invalid problem ID")
        end select

    end subroutine s_get_problem

    subroutine s_get_2D_lid_driven_cavity()

        ! Global number of grid points in each direction
        decomp_info%m_global = 128
        decomp_info%n_global = 128

        ! Number of processors in each direction
        decomp_info%p_x = 1
        decomp_info%p_y = 1

        ! Global domain extents
        coord_info%x_min = 0.0d0
        coord_info%x_max = 1.0d0
        coord_info%y_min = 0.0d0
        coord_info%y_max = 1.0d0

        ! Set global boundary conditions
        bc_info%bc_x%beg = 1
        bc_info%bc_x%end = 1
        bc_info%bc_y%beg = 1
        bc_info%bc_y%end = 1

        call s_get_local_problem_setup()

        ! Assign initial condition
        do k = 0, decomp_info%p
            do j = 0, decom_info%n
                do i = 0, decomp_info%m
                    ! Assign pressure
                    Q(1)%sf(i, j, k) = 1d0

                    ! Assign x-momentum
                    Q(2)%sf(i, j, k) = 0.0d0

                    ! Assign y-momentum
                    Q(3)%sf(i, j, k) = 0.0d0
                end do
            end do
        end if

    end subroutine s_get_2D_lid_driven_cavity

    subroutine s_get_2D_laminar_boundary_layer()

    end subroutine s_get_2D_laminar_boundary_layer

    subroutine s_get_2D_laminar_channel_flow()

    end subroutine s_get_2D_laminar_channel_flow

    subroutine s_get_3D_flow_over_sphere()

    end subroutine s_get_3D_flow_over_sphere

    subroutine s_get_3D_rayleigh_taylor_instability()

    end subroutine s_get_3D_rayleigh_taylor_instability

end module m_problem_setup
