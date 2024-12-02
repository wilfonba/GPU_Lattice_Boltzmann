module m_boundary_conditions

    ! Dependencies ==========================================
    use m_global_parameters
    use m_derived_types
    ! =======================================================

    implicit none

    private; public :: s_apply_boundary_conditions

contains

    ! This subroutine selects the correct boundary conditions subroutine to call
    ! It has one input:
    ! f: the distribution function
    subroutine s_apply_boundary_conditions(f)

        real(kind(0d0)), dimension(0:, 0:, 0:, 0:) :: f

        if (num_dims == 2) then
            call s_apply_boundary_conditions_2d(f)
        else
            call s_apply_boundary_conditions_3d(f)
        end if

    end subroutine s_apply_boundary_conditions

    ! This subroutine applies the boundary conditions to the distribution
    ! function for the lid driven cavity problem in 2D. It has one input:
    ! f: the distribution function
    subroutine s_apply_boundary_conditions_2d(f)

        real(kind(0d0)), dimension(0:, 0:, 0:, 0:) :: f

        integer :: i
        real(kind(0d0)) :: rhoW

        ! Left bounce back
        !$acc parallel loop gang vector default(present)
        do i = 0, decomp_info%n
            f(0, i, 0, 1) = f(0, i, 0, 3)
            f(0, i, 0, 5) = f(0, i, 0, 7)
            f(0, i, 0, 8) = f(0, i, 0, 6)
        end do

        ! Right bounce back
        !$acc parallel loop gang vector default(present)
        do i = 0, decomp_info%n
            f(decomp_info%m, i, 0, 3) = f(decomp_info%m, i, 0, 1)
            f(decomp_info%m, i, 0, 7) = f(decomp_info%m, i, 0, 5)
            f(decomp_info%m, i, 0, 6) = f(decomp_info%m, i, 0, 8)
        end do

        ! Bottom bounce back
        !$acc parallel loop gang vector default(present)
        do i = 0, decomp_info%m
            f(i, 0, 0, 2) = f(i, 0, 0, 4)
            f(i, 0, 0, 5) = f(i, 0, 0, 7)
            f(i, 0, 0, 6) = f(i, 0, 0, 8)
        end do

        ! Top moving lid
        !$acc parallel loop gang vector default(present) private(rhoW)
        do i = 1, decomp_info%m - 1
            rhoW = f(i, decomp_info%n, 0, 0) + f(i, decomp_info%n, 0, 1) + &
                f(i, decomp_info%n, 0, 3) + &
                2.0d0 * (f(i, decomp_info%n, 0, 2) + &
                f(i, decomp_info%n, 0, 5) + f(i, decomp_info%n, 0, 6))
            f(i, decomp_info%n, 0, 4) = f(i, decomp_info%n, 0, 2)
            f(i, decomp_info%n, 0, 7) = f(i, decomp_info%n, 0, 5) - rhoW*lidVel/6.0
            f(i, decomp_info%n, 0, 8) = f(i, decomp_info%n, 0, 6) + rhoW*lidVel/6.0
            !f(i, decomp_info%n, 0, 4) = f(i, decomp_info%n, 0, 2)
            !f(i, decomp_info%n, 0, 7) = f(i, decomp_info%n, 0, 5)
            !f(i, decomp_info%n, 0, 8) = f(i, decomp_info%n, 0, 6)
        end do

    end subroutine s_apply_boundary_conditions_2d

    ! This subroutine applies the boundary conditions to the distribution
    ! function for the lid driven cavity problem in 3D. It has one input:
    ! f: the distribution function
    subroutine s_apply_boundary_conditions_3d(f)

        real(kind(0d0)), dimension(0:, 0:, 0:, 0:) :: f

    end subroutine s_apply_boundary_conditions_3d

end module m_boundary_conditions
