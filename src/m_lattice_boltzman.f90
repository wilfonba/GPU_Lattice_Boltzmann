module m_lattice_boltzmann

    ! Dependencies ==========================================
    use m_global_parameters
    use m_boundary_conditions
    ! =======================================================

    implicit none

    private; public :: s_assign_D2Q9_collision_operator, &
        s_assign_D3Q19_collision_operator, &
        s_collision, &
        s_streaming, &
        s_compute_prim_vars

contains

    ! This subroutine assigns the collision operator for the D2Q9 lattice. It
    ! takes 1 input:
    ! coll_op: a collision_operator type variable describing the operator
    subroutine s_assign_D2Q9_collision_operator()

        ws(0) = 4d0/9d0
        ws(1) = 1d0/9d0
        ws(2) = 1d0/9d0
        ws(3) = 1d0/9d0
        ws(4) = 1d0/9d0
        ws(5) = 1d0/36d0
        ws(6) = 1d0/36d0
        ws(7) = 1d0/36d0
        ws(8) = 1d0/36d0

        cx(0) = 0
        cx(1) = 1
        cx(2) = 0
        cx(3) = -1
        cx(4) = 0
        cx(5) = 1
        cx(6) = -1
        cx(7) = -1
        cx(8) = 1

        cy(0) = 0
        cy(1) = 0
        cy(2) = 1
        cy(3) = 0
        cy(4) = -1
        cy(5) = 1
        cy(6) = 1
        cy(7) = -1
        cy(8) = -1

        D = 2
        nQ = 9

        !$acc update device(ws, cx, cy, D, nQ)

    end subroutine s_assign_D2Q9_collision_operator

    ! This subroutine assigns the collision operator for the D3Q19 lattice. It
    ! takes 1 input:
    ! coll_op: a collision_operator type variable describing the operator
    subroutine s_assign_D3Q19_collision_operator()

        ws(0) = 1d0/3d0
        ws(1) = 1d0/18d0
        ws(2) = 1d0/18d0
        ws(3) = 1d0/18d0
        ws(4) = 1d0/18d0
        ws(5) = 1d0/18d0
        ws(6) = 1d0/18d0
        ws(7) = 1d0/36d0
        ws(8) = 1d0/36d0
        ws(9) = 1d0/36d0
        ws(10) = 1d0/36d0
        ws(11) = 1d0/36d0
        ws(12) = 1d0/36d0
        ws(13) = 1d0/36d0
        ws(14) = 1d0/36d0
        ws(15) = 1d0/36d0
        ws(16) = 1d0/36d0
        ws(17) = 1d0/36d0
        ws(18) = 1d0/36d0

        cx(0) = 0
        cx(1) = 1
        cx(2) = -1
        cx(3) = 0
        cx(4) = 0
        cx(5) = 0
        cx(6) = 0
        cx(7) = 1
        cx(8) = -1
        cx(9) = 1
        cx(10) = -1
        cx(11) = 0
        cx(12) = 0
        cx(13) = 1
        cx(14) = -1
        cx(15) = 1
        cx(16) = -1
        cx(17) = 0
        cx(18) = 0

        cy(0) = 0
        cy(1) = 0
        cy(2) = 0
        cy(3) = 1
        cy(4) = -1
        cy(5) = 0
        cy(6) = 0
        cy(7) = 1
        cy(8) = -1
        cy(9) = 0
        cy(10) = 0
        cy(11) = 1
        cy(12) = -1
        cy(13) = -1
        cy(14) = 1
        cy(15) = 0
        cy(16) = 0
        cy(17) = 1
        cy(18) = -1

        cz(0) = 0
        cz(1) = 0
        cz(2) = 0
        cz(3) = 0
        cz(4) = 0
        cz(5) = 1
        cz(6) = -1
        cz(7) = 0
        cz(8) = 0
        cz(9) = 1
        cz(10) = -1
        cz(11) = 1
        cz(12) = -1
        cz(13) = 0
        cz(14) = 0
        cz(15) = -1
        cz(16) = 1
        cz(17) = -1
        cz(18) = 1

        D = 3
        nQ = 19

        !$acc update device(ws, cx, cy, cz, D, nQ)

    end subroutine s_assign_D3Q19_collision_operator

    ! This subroutine performs the collision step of the LBM. Its inputs are
    !   Q: a scalar field holding the primitive variables
    !   f: a 4D array holding the distribution functions
    !   fEq: a 4D array holding the equilibrium distribution functions
    subroutine s_collision(Q, f, fEq)

        real(kind(0d0)), dimension(0:, 0:, 0:, 0:) :: f, fEq
        real(kind(0d0)), dimension(0:, 0:, 0:, 0:) :: Q
        integer :: i, j, k, l
        real(kind(0d0)) :: cidotu

        real(kind(0d0)) :: rho, u, v, w

        C = dt/tau
        !$acc update device(C)

        if (num_dims == 2) then
            !$acc parallel loop collapse(2) gang vector default(present) private(rho, u, v, cidotu)
            do j = 0, n
                do i = 0, m
                    rho = Q(i,j,0,0)
                    u = Q(i,j,0,1)
                    v = Q(i,j,0,2)
                    !$acc loop seq
                    do l = 0, nQ - 1
                        cidotu = cx(l)*u + cy(l)*v
                        fEq(i,j,0,l) = rho*ws(l)*(1d0 + 3d0*cidotu + &
                            4.5d0*cidotu*cidotu - 1.5d0*(u*u + v*v))
                        f(i,j,0,l) = f(i,j,0,l)*(1d0-C) + C*feq(i, j, 0, l)
                    end do
                end do
            end do
        else
            !$acc parallel loop  gang vector collapse(3) default(present) &
            !$acc private(rho, u, v, w, cidotu)
            do k = 0, p
                do j = 0, n
                    do i = 0, m
                        rho = Q(i,j,k,0)
                        u = Q(i,j,k,1)
                        v = Q(i,j,k,2)
                        w = Q(i,j,k,3)
                        !$acc loop seq
                        do l = 0, nQ - 1
                            cidotu = cx(l)*u + cy(l)*v + cz(l)*w
                            fEq(i,j,k,l) = rho*ws(l)*(1d0 + 3d0*cidotu + &
                                4.5d0*cidotu*cidotu - 1.5d0*(u*u + v*v + w*w))
                            f(i,j,k,l) = f(i,j,k,l)*(1d0-C) + C*feq(i, j, k, l)
                        end do
                    end do
                end do
            end do
        end if

    end subroutine s_collision

    ! This subroutine performs the streaming step of the LBM. Its input is
    !  f: a 4D array holding the distribution functions
    subroutine s_streaming(f, fOld)

        real(kind(0d0)), dimension(0:, 0:, 0:, 0:) :: f
        real(kind(0d0)), dimension(0:, 0:, 0:, 0:) :: fOld

        if (num_dims == 2) then
            call s_streaming_2d(f, fOld)
        else
            call s_streaming_3d(f, fOld)
        end if

    end subroutine s_streaming

    subroutine s_streaming_2d(f, fOld)

        real(kind(0d0)), dimension(0:, 0:, 0:, 0:) :: f
        real(kind(0d0)), dimension(0:, 0:, 0:, 0:) :: fOld

        integer :: i, j, k

        !$acc parallel loop collapse(2) gang vector default(present)
        do k = 0, nQ - 1
            do j = 0, n
                do i = 0, m
                    fOld(i,j,0,k) = f(i,j,0,k)
                end do
            end do
        end do

        !$acc parallel loop collapse(2) gang vector default(present)
        do j = 0, n
            do i = 1, m
                f(i,j,0,1) = fOld(i-1,j,0,1)
            end do
        end do

        !$acc parallel loop collapse(2) gang vector default(present)
        do j = 1, n
            do i = 0, m
                f(i,j,0,2) = fOld(i,j-1,0,2)
            end do
        end do

        !$acc parallel loop collapse(2) gang vector default(present)
        do j = 0, n
            do i = 0, m - 1
                f(i,j,0,3) = fOld(i+1,j,0,3)
            end do
        end do

        !$acc parallel loop collapse(2) gang vector default(present)
        do j = 0, n - 1
            do i = 0, m
                f(i,j,0,4) = fOld(i,j+1,0,4)
            end do
        end do

        !$acc parallel loop collapse(2) gang vector default(present)
        do j = 1, n
            do i = 1, n
                f(i,j,0,5) = fOld(i-1,j-1,0,5)
            end do
        end do

        !$acc parallel loop collapse(2) gang vector default(present)
        do j = 1, n
            do i = 0, m - 1
                f(i,j,0,6) = fOld(i+1,j-1,0,6)
            end do
        end do

        !$acc parallel loop collapse(2) gang vector default(present)
        do j = 0, n - 1
            do i = 0, m - 1
                f(i,j,0,7) = fOld(i+1,j+1,0,7)
            end do
        end do

        !$acc parallel loop collapse(2) gang vector default(present)
        do j = 0, n - 1
            do i = 1, m
                f(i,j,0,8) = fOld(i-1,j+1,0,8)
            end do
        end do

    end subroutine s_streaming_2d

    subroutine s_streaming_3d(f, fOld)

        real(kind(0d0)), dimension(0:, 0:, 0:, 0:) :: f
        real(kind(0d0)), dimension(0:, 0:, 0:, 0:) :: fOld

        integer :: i, j, k, l

        !$acc parallel loop collapse(4) gang vector default(present)
        do l = 0, nQ - 1
            do k = 0, p
                do j = 0, n
                    do i = 0, m
                        fOld(i,j,k,l) = f(i,j,k,l)
                    end do
                end do
            end do
        end do

        !$acc parallel loop collapse(3) gang vector default(present)
        do k = 0, p
            do j = 0, n
                do i = 1, m
                    f(i,j,k,1) = fOld(i-1,j,k,1)
                end do
            end do
        end do

        !$acc parallel loop collapse(3) gang vector default(present)
        do k = 0, p
            do j = 0, n
                do i = 0, m-1
                    f(i,j,k,2) = fOld(i+1,j,k,2)
                end do
            end do
        end do

        !$acc parallel loop collapse(3) gang vector default(present)
        do k = 0, p
            do j = 1, n
                do i = 0,m
                    f(i,j,k,3) = fOld(i,j-1,k,3)
                end do
            end do
        end do

        !$acc parallel loop collapse(3) gang vector default(present)
        do k = 0, p
            do j = 0, n-1
                do i = 0, m
                    f(i,j,k,4) = fOld(i,j+1,k,4)
                end do
            end do
        end do

        !$acc parallel loop collapse(3) gang vector default(present)
        do k = 1, p
            do j = 0, n
                do i = 0, m
                    f(i,j,k,5) = fOld(i,j,k-1,5)
                end do
            end do
        end do

        !$acc parallel loop collapse(3) gang vector default(present)
        do k = 0, p - 1
            do j = 0, n
                do i = 0, m
                    f(i,j,k,6) = fOld(i,j,k+1,6)
                end do
            end do
        end do

        !$acc parallel loop collapse(3) gang vector default(present)
        do k = 0, p
            do j = 1, n
                do i = 1, m
                    f(i,j,k,7) = fOld(i-1,j-1,k,7)
                end do
            end do
        end do

        !$acc parallel loop collapse(3) gang vector default(present)
        do k = 0, p
            do j = 0, n - 1
                do i = 0, m - 1
                    f(i,j,k,8) = fOld(i+1,j+1,k,8)
                end do
            end do
        end do

        !$acc parallel loop collapse(3) gang vector default(present)
        do k = 1, p
            do j = 0, n
                do i = 1, m
                    f(i,j,k,9) = fOld(i-1,j,k-1,9)
                end do
            end do
        end do

        !$acc parallel loop collapse(3) gang vector default(present)
        do k = 0, p - 1
            do j = 0, n
                do i = 1, m
                    f(i,j,k,10) = fOld(i-1,j,k+1,10)
                end do
            end do
        end do

        !$acc parallel loop collapse(3) gang vector default(present)
        do k = 1, p
            do j = 1, n
                do i = 0, m
                    f(i,j,k,11) = fOld(i,j-1,k-1,11)
                end do
            end do
        end do

        !$acc parallel loop collapse(3) gang vector default(present)
        do k = 0, p - 1
            do j = 0, n - 1
                do i = 0, m
                    f(i,j,k,12) = fOld(i,j+1,k+1,12)
                end do
            end do
        end do

        !$acc parallel loop collapse(3) gang vector default(present)
        do k = 0, p
            do j = 0, n - 1
                do i = 1, m
                    f(i,j,k,13) = fOld(i-1,j+1,k,13)
                end do
            end do
        end do

        !$acc parallel loop collapse(3) gang vector default(present)
        do k = 0, p
            do j = 1, n
                do i = 0, m - 1
                    f(i,j,k,14) = fOld(i+1,j-1,k,14)
                end do
            end do
        end do

        !$acc parallel loop collapse(3) gang vector default(present)
        do k = 0, p - 1
            do j = 0, n
                do i = 1, m
                    f(i,j,k,15) = fOld(i-1,j,k+1,15)
                end do
            end do
        end do

        !$acc parallel loop collapse(3) gang vector default(present)
        do k = 1, p
            do j = 0, n
                do i = 0, m - 1
                    f(i,j,k,16) = fOld(i+1,j,k-1,16)
                end do
            end do
        end do

        !$acc parallel loop collapse(3) gang vector default(present)
        do k = 0, p - 1
            do j = 1, n
                do i = 0, m
                    f(i,j,k,17) = fOld(i,j-1,k+1,17)
                end do
            end do
        end do

        !$acc parallel loop collapse(3) gang vector default(present)
        do k = 1, p
            do j = 0, n - 1
                do i = 0, m
                    f(i,j,k,18) = fOld(i,j+1,k-1,18)
                end do
            end do
        end do

    end subroutine s_streaming_3d

    ! This subroutine computes the primitive variables from the distribution
    ! function. Its inputs are:
    !  Q: a scalar field holding the primitive variables
    !  f: a 4D array holding the distribution functions
    subroutine s_compute_prim_vars(Q, f)

        real(kind(0d0)), dimension(0:, 0:, 0:, 0:) :: Q
        real(kind(0d0)), dimension(0:, 0:, 0:, 0:) :: f

        integer :: i, j, k, l

        if (num_dims == 2) then
            !$acc parallel loop collapse(2) gang vector default(present)
            do j = 0, n
                do i = 0, m
                    Q(i,j,0,0) = f(i,j,0,0) + f(i,j,0,1) + f(i,j,0,2) + &
                        f(i,j,0,3) + f(i,j,0,4) + f(i,j,0,5) + f(i,j,0,6) + &
                        f(i,j,0,7) + f(i,j,0,8)

                    Q(i,j,0,1) = (f(i,j,0,1) + f(i,j,0,5) + f(i,j,0,8) - &
                        f(i,j,0,3) - f(i,j,0,6) - f(i,j,0,7))/Q(i,j,0,0)

                    Q(i,j,0,2) = (f(i,j,0,2) + f(i,j,0,5) + f(i,j,0,6) - &
                        f(i,j,0,4) - f(i,j,0,7) - f(i,j,0,8))/Q(i,j,0,0)
                end do
            end do
        elseif (num_dims == 3) then
            !$acc parallel loop collapse(3) gang vector default(present)
            do k = 0, p
                do j = 0, n
                    do i = 0, m
                        Q(i,j,k,0) = f(i,j,k,0) + f(i,j,k,1) + f(i,j,k,2) + &
                            f(i,j,k,3) + f(i,j,k,4) + f(i,j,k,5) + f(i,j,k,6) + &
                            f(i,j,k,7) + f(i,j,k,8) + f(i,j,k,9) + f(i,j,k,10) + &
                            f(i,j,k,11) + f(i,j,k,12) + f(i,j,k,13) + f(i,j,k,14) + &
                            f(i,j,k,15) + f(i,j,k,16) + f(i,j,k,17) + f(i,j,k,18)

                        Q(i,j,k,1) = (f(i,j,k,1) - f(i,j,k,2) + f(i,j,k,7) - &
                            f(i,j,k,8) + f(i,j,k,9) - f(i,j,k,10) + f(i,j,k,13) - &
                            f(i,j,k,14) + f(i,j,k,15) - f(i,j,k,16))/Q(i,j,k,0)

                        Q(i,j,k,2) = (f(i,j,k,3) - f(i,j,k,4) + f(i,j,k,7) - &
                            f(i,j,k,8) + f(i,j,k,11) - f(i,j,k,12) - f(i,j,k,13) + &
                            f(i,j,k,14) + f(i,j,k,17) - f(i,j,k,18))/Q(i,j,k,0)

                        Q(i,j,k,3) = (f(i,j,k,5) - f(i,j,k,6) + f(i,j,k,9) - &
                            f(i,j,k,10) + f(i,j,k,11) - f(i,j,k,12) - f(i,j,k,15) + &
                            f(i,j,k,16) - f(i,j,k,17) + f(i,j,k,18))/Q(i,j,k,0)
                    end do
                end do
            end do
        end if

    end subroutine s_compute_prim_vars

end module m_lattice_boltzmann
