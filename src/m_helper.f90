module m_helper

    ! Dependencies ==========================================
    use ieee_arithmetic
    use m_global_parameters
    ! =======================================================

    implicit none

    private; public :: s_print_2d_array

contains

   subroutine s_print_2D_array(A, div)

        real(kind(0d0)), dimension(:, :), intent(in) :: A
        real, optional, intent(in) :: div

        integer :: i, j
        integer :: m, n
        real :: c

        m = size(A, 1)
        n = size(A, 2)

        if (present(div)) then
            c = div
        else
            c = 1
        end if

        print *, m, n

        do i = 1, m
            do j = 1, n
                write (*, fmt="(F32.16)", advance="no") A(i, j)/c
            end do
            write (*, fmt="(A1)") " "
        end do
        write (*, fmt="(A1)") " "

    end subroutine

end module m_helper
