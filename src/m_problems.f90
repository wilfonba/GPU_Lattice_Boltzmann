module m_problems

    ! Dependencies ==========================================
    use m_derived_types
    use m_global_parameters
    ! =======================================================

    implicit none

    private; public :: s_Get_2D_lid_driven_cavity, &
        s_setup_2D_lid_driven_cavity, &
        s_get_3D_lid_driven_cavity, &
        s_setup_3D_lid_driven_cavity

contains

    ! =========================================================================
    ! 2D Lid Driven Cavity  ===================================================
    ! =========================================================================
    ! This subroutine contains the problem specific information for the 2D lid
    ! driven cavity problem. It takes no inputs.
    subroutine s_get_2D_lid_driven_cavity()

        ! Global number of grid points in each direction
        decomp_info%m = 1000
        decomp_info%n = 1000

        time_info%dt = 1d0
        time_info%t_step_stop = 10
        time_info%t_step_save = 10

        !$acc update device(decomp_info, time_info)

    end subroutine s_get_2D_lid_driven_cavity

    ! This subroutine initializes the primitive variables field for the 2D lid
    ! driven cavity. It take one input:
    !  Q : A scalar_field that holds the primitive variables
    subroutine s_setup_2D_lid_driven_cavity(Q)

        real(kind(0d0)), dimension(0:, 0:, 0:, 0:) :: Q

        integer :: i, j

        !$acc parallel loop vector gang default(present) collapse(2)
        do j = 0, decomp_info%n
            do i = 0, decomp_info%m
                Q(i, j, 0, 0) = 5d0
                Q(i, j, 0, 1) = 0d0
                Q(i, j, 0, 2) = 0d0
            end do
        end do

    end subroutine s_setup_2D_lid_driven_cavity

    ! =========================================================================
    ! 3D Lid Driven Cavity ====================================================
    ! =========================================================================
    subroutine s_get_3D_lid_driven_cavity()

        ! Global number of grid points in each direction
        decomp_info%m = 100
        decomp_info%n = 100
        decomp_info%p = 100

        time_info%dt = 1d0
        time_info%t_step_stop = 100000
        time_info%t_step_save = 100000

        !$acc update device(decomp_info, time_info)

    end subroutine s_get_3D_lid_driven_cavity

    subroutine s_setup_3D_lid_driven_cavity(Q)

        real(kind(0d0)), dimension(0:, 0:, 0:, 0:) :: Q

        integer :: i, j, k

        !$acc parallel loop vector gang default(present) collapse(3)
        do k = 0, decomp_info%p
            do j = 0, decomp_info%n
                do i = 0, decomp_info%m
                    Q(i, j, k, 0) = 5d0
                    Q(i, j, k, 1) = 0d0
                    Q(i, j, k, 2) = 0d0
                    Q(i, j, k, 3) = 0d0
                end do
            end do
        end do

    end subroutine s_setup_3D_lid_driven_cavity

end module m_problems
