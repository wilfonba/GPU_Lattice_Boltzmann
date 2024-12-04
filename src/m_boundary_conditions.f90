module m_boundary_conditions

    ! Dependencies ==========================================
    use m_global_parameters
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
        do i = 0, n
            f(0, i, 0, 1) = f(0, i, 0, 3)
            f(0, i, 0, 5) = f(0, i, 0, 7)
            f(0, i, 0, 8) = f(0, i, 0, 6)
        end do

        ! Right bounce back
        !$acc parallel loop gang vector default(present)
        do i = 0, n
            f(m, i, 0, 3) = f(m, i, 0, 1)
            f(m, i, 0, 7) = f(m, i, 0, 5)
            f(m, i, 0, 6) = f(m, i, 0, 8)
        end do

        ! Bottom bounce back
        !$acc parallel loop gang vector default(present)
        do i = 0, m
            f(i, 0, 0, 2) = f(i, 0, 0, 4)
            f(i, 0, 0, 5) = f(i, 0, 0, 7)
            f(i, 0, 0, 6) = f(i, 0, 0, 8)
        end do

        ! Top moving lid
        !$acc parallel loop gang vector default(present) private(rhoW)
        do i = 1, m - 1
            rhoW = f(i, n, 0, 0) + f(i, n, 0, 1) + f(i, n, 0, 3) + &
                2.0d0 * (f(i, n, 0, 2) + f(i, n, 0, 5) + f(i, n, 0, 6))
            f(i, n, 0, 4) = f(i, n, 0, 2)
            f(i, n, 0, 7) = f(i, n, 0, 5) - rhoW*lidVel/6d0
            f(i, n, 0, 8) = f(i, n, 0, 6) + rhoW*lidVel/6d0
        end do

    end subroutine s_apply_boundary_conditions_2d

    ! This subroutine applies the boundary conditions to the distribution
    ! function for the lid driven cavity problem in 3D. It has one input:
    ! f: the distribution function
    subroutine s_apply_boundary_conditions_3d(f)

        real(kind(0d0)), dimension(0:, 0:, 0:, 0:) :: f

        integer :: i, j
        real(kind(0d0)) :: rhoW

        ! Left bounce back
        !$acc parallel loop gang vector collapse(2) default(present)
        do j = 0, p
            do i = 0, n
                f(0, i, j, 1) = f(0, i, j, 2)
                f(0, i, j, 7) = f(0, i, j, 8)
                f(0, i, j, 9) = f(0, i, j, 10)
                f(0, i, j, 13) = f(0, i, j, 14)
                f(0, i, j, 15) = f(0, i, j, 16)
            end do
        end do

        ! right bounce back
        !$acc parallel loop gang vector collapse(2) default(present)
        do j = 0, p
            do i = 0, n
                f(m, i, j, 2) = f(m, i, j, 1)
                f(m, i, j, 8) = f(m, i, j, 7)
                f(m, i, j, 10) = f(m, i, j, 9)
                f(m, i, j, 14) = f(m, i, j, 13)
                f(m, i, j, 16) = f(m, i, j, 15)
            end do
        end do

        ! front bounce back
        !$acc parallel loop gang vector collapse(2) default(present)
        do j = 0, n
            do i = 0, m
                f(i, j, 0, 5) = f(i, j, 0, 6)
                f(i, j, 0, 9) = f(i, j, 0, 10)
                f(i, j, 0, 11) = f(i, j, 0, 12)
                f(i, j, 0, 16) = f(i, j, 0, 15)
                f(i, j, 0, 18) = f(i, j, 0, 17)
            end do
        end do

        ! back bounce back
        !$acc parallel loop gang vector collapse(2) default(present)
        do j = 0, n
            do i = 0, m
                f(i, j, p, 6) = f(i, j, p, 5)
                f(i, j, p, 10) = f(i, j, p, 9)
                f(i, j, p, 12) = f(i, j, p, 11)
                f(i, j, p, 15) = f(i, j, p, 16)
                f(i, j, p, 17) = f(i, j, p, 18)
            end do
        end do

        ! bottom bounce back
        !$acc parallel loop gang vector collapse(2) default(present)
        do j = 0, p
            do i = 0, m
                f(i, 0, j, 3) = f(i, 0, j, 4)
                f(i, 0, j, 7) = f(i, 0, j, 8)
                f(i, 0, j, 11) = f(i, 0, j, 12)
                f(i, 0, j, 14) = f(i, 0, j, 13)
                f(i, 0, j, 17) = f(i, 0, j, 18)
            end do
        end do

        ! Top moving lid
        !$acc parallel loop gang vector collapse(2) default(present) private(rhoW)
        do j = 0, p
            do i = 1, m - 1
                rhoW = f(i, n, j, 0) + f(i, n, j, 1) + f(i, n, j, 2) + &
                    f(i, n, j, 5) + f(i, n, j, 6) + f(i, n, j, 9) + &
                    f(i, n, j, 10) + f(i, n, j, 15) + f(i, n, j, 16) + &
                    2.0d0 * (f(i, n, j, 3) + f(i, n, j, 7) + f(i, n, j, 11) + &
                    f(i, n, j, 14) + f(i, n, j, 17))
                f(i, n, j, 4) = f(i, n, j, 3)
                f(i, n, j, 8) = f(i, n, j, 7) - rhoW*lidVel/6d0
                f(i, n, j, 12) = f(i, n, j, 11)
                f(i, n, j, 13) = f(i, n, j, 14) + rhoW*lidVel/6d0
                f(i, n, j, 18) = f(i, n, j, 17)
            end do
        end do

    end subroutine s_apply_boundary_conditions_3d

end module m_boundary_conditions
