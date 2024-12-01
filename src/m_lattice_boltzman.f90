module m_lattice_boltzmann

    ! Dependencies ==========================================
    use m_global_parameters
    use m_derived_types
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
    subroutine s_assign_D2Q9_collision_operator(coll_op)

        type(collision_operator) :: coll_op

        coll_op%w(0) = 4d0/9d0
        coll_op%w(1) = 1d0/9d0
        coll_op%w(2) = 1d0/9d0
        coll_op%w(3) = 1d0/9d0
        coll_op%w(4) = 1d0/9d0
        coll_op%w(5) = 1d0/36d0
        coll_op%w(6) = 1d0/36d0
        coll_op%w(7) = 1d0/36d0
        coll_op%w(8) = 1d0/36d0

        coll_op%cx(0) = 0
        coll_op%cx(1) = 1
        coll_op%cx(2) = 0
        coll_op%cx(3) = -1
        coll_op%cx(4) = 0
        coll_op%cx(5) = 1
        coll_op%cx(6) = -1
        coll_op%cx(7) = -1
        coll_op%cx(8) = 1

        coll_op%cy(0) = 0
        coll_op%cy(1) = 0
        coll_op%cy(2) = 1
        coll_op%cy(3) = 0
        coll_op%cy(4) = -1
        coll_op%cy(5) = 1
        coll_op%cy(6) = 1
        coll_op%cy(7) = -1
        coll_op%cy(8) = -1

        coll_op%D = 2
        coll_op%Q = 9

        !$acc update device(coll_op)

    end subroutine s_assign_D2Q9_collision_operator

    ! This subroutine assigns the collision operator for the D3Q19 lattice. It
    ! takes 1 input:
    ! coll_op: a collision_operator type variable describing the operator
    subroutine s_assign_D3Q19_collision_operator(coll_op)

        type(collision_operator) :: coll_op

        coll_op%w(0) = 1d0/3d0
        coll_op%w(1) = 1d0/18d0
        coll_op%w(2) = 1d0/18d0
        coll_op%w(3) = 1d0/18d0
        coll_op%w(4) = 1d0/18d0
        coll_op%w(5) = 1d0/18d0
        coll_op%w(6) = 1d0/18d0
        coll_op%w(7) = 1d0/36d0
        coll_op%w(8) = 1d0/36d0
        coll_op%w(9) = 1d0/36d0
        coll_op%w(10) = 1d0/36d0
        coll_op%w(11) = 1d0/36d0
        coll_op%w(12) = 1d0/36d0
        coll_op%w(13) = 1d0/36d0
        coll_op%w(14) = 1d0/36d0
        coll_op%w(15) = 1d0/36d0
        coll_op%w(16) = 1d0/36d0
        coll_op%w(17) = 1d0/36d0
        coll_op%w(18) = 1d0/36d0

        coll_op%cx(0) = 0
        coll_op%cx(1) = 1
        coll_op%cx(2) = -1
        coll_op%cx(3) = 0
        coll_op%cx(4) = 0
        coll_op%cx(5) = 0
        coll_op%cx(6) = 0
        coll_op%cx(7) = 1
        coll_op%cx(8) = -1
        coll_op%cx(9) = 1
        coll_op%cx(10) = -1
        coll_op%cx(11) = 0
        coll_op%cx(12) = 0
        coll_op%cx(13) = 1
        coll_op%cx(14) = -1
        coll_op%cx(15) = 1
        coll_op%cx(16) = -1
        coll_op%cx(17) = 0
        coll_op%cx(18) = 0

        coll_op%cy(0) = 0
        coll_op%cy(1) = 0
        coll_op%cy(2) = 0
        coll_op%cy(3) = 1
        coll_op%cy(4) = -1
        coll_op%cy(5) = 0
        coll_op%cy(6) = 0
        coll_op%cy(7) = 1
        coll_op%cy(8) = -1
        coll_op%cy(9) = 0
        coll_op%cy(10) = 0
        coll_op%cy(11) = 1
        coll_op%cy(12) = -1
        coll_op%cy(13) = -1
        coll_op%cy(14) = 1
        coll_op%cy(15) = 0
        coll_op%cy(16) = 0
        coll_op%cy(17) = 1
        coll_op%cy(18) = -1

        coll_op%cz(0) = 0
        coll_op%cz(1) = 0
        coll_op%cz(2) = 0
        coll_op%cz(3) = 0
        coll_op%cz(4) = 0
        coll_op%cz(5) = 1
        coll_op%cz(6) = -1
        coll_op%cz(7) = 0
        coll_op%cz(8) = 0
        coll_op%cz(9) = 1
        coll_op%cz(10) = -1
        coll_op%cz(11) = 1
        coll_op%cz(12) = -1
        coll_op%cz(13) = 0
        coll_op%cz(14) = 0
        coll_op%cz(15) = -1
        coll_op%cz(16) = 1
        coll_op%cz(17) = -1
        coll_op%cz(18) = 1

        coll_op%D = 3
        coll_op%Q = 19

        !$acc update device(coll_op)

    end subroutine s_assign_D3Q19_collision_operator

    ! This subroutine performs the collision step of the LBM. Its inputs are
    !   Q: a scalar field holding the primitive variables
    !   f: a 4D array holding the distribution functions
    !   fEq: a 4D array holding the equilibrium distribution functions
    subroutine s_collision(Q, f, fEq)

        real(kind(0d0)), dimension(0:, 0:, 0:, 0:) :: f, fEq
        type(scalar_field), dimension(0:num_dims) :: Q
        integer :: i, j, k, l
        real(kind(0d0)) :: cidotu

        real(kind(0d0)) :: C, rho, u, v, w

        C = time_info%dt/time_info%tau

        if (num_dims == 2) then
            !$acc parallel loop vector gane collapse(2) default(present) private(rho, u, v, cidotu)
            do j = 0, decomp_info%n
                do i = 0, decomp_info%m
                    rho = Q(0)%sf(i,j,0)
                    u = Q(1)%sf(i,j,0)
                    v = Q(2)%sf(i,j,0)
                    !$acc loop seq
                    do l = 0, coll_op%Q - 1
                        cidotu = coll_op%cx(l)*u + coll_op%cy(l)*v
                        fEq(i,j,0,l) = rho*coll_op%w(l)*(1d0 + 3d0*cidotu + &
                            4.5d0*cidotu**2d0 - 1.5d0*(u**2d0 + v**2d0))
                        f(i,j,0,l) = f(i,j,0,l)*(1d0-C) + C*feq(i, j, k, l)
                    end do
                end do
            end do
        else
            !$acc parallel loop vector gane collapse(3) default(present) private(rho, u, v, cidotu)
            do k = 0, decomp_info%p
                do j = 0, decomp_info%n
                    do i = 0, decomp_info%m
                        rho = Q(0)%sf(i,j,k)
                        u = Q(1)%sf(i,j,k)
                        v = Q(2)%sf(i,j,k)
                        w = Q(3)%sf(i,j,k)
                        !$acc loop seq
                        do l = 0, coll_op%Q - 1
                            cidotu = coll_op%cx(l)*u + coll_op%cy(l)*v
                            fEq(i,j,k,l) = rho*coll_op%w(l)*(1d0 + 3d0*cidotu + &
                                4.5d0*cidotu**2d0 - 1.5d0*(u**2d0 + v**2d0))
                            f(i,j,k,l) = f(i,j,k,l)*(1d0-C) + C*feq(i, j, k, l)
                        end do
                    end do
                end do
            end do
        end if

    end subroutine s_collision

    ! This subroutine performs the streaming step of the LBM. Its input is
    !  f: a 4D array holding the distribution functions
    subroutine s_streaming(f)

        real(kind(0d0)), dimension(0:, 0:, 0:, 0:) :: f

        if (num_dims == 2) then
            call s_streaming_2d(f)
        else
            call s_streaming_3d(f)
        end if

    end subroutine s_streaming

    subroutine s_streaming_2d(f)

        real(kind(0d0)), dimension(0:, 0:, 0:, 0:) :: f

        integer :: i, j, k, l

        ! Side to side
        !$acc parallel loop vector gang default(present) collapse(2)
        do j = 0, decomp_info%n
            do i = decomp_info%m, 1, -1
                f(i,j,0,1) = f(i-1,j,0,1)
            end do
        end do

        !$acc parallel loop vector gang default(present) collapse(2)
        do j = 0, decomp_info%n
            do i = 0, decomp_info%m - 1
                f(i,j,0,3) = f(i+1,j,0,3)
            end do
        end do

        ! Top to bottom
        !$acc parallel loop vector gang default(present) collapse(2)
        do j = decomp_info%n, 1, -1
            do i = 0, decomp_info%m
                f(i,j,0,2) = f(i,j-1,0,2)
            end do
        end do

        !$acc parallel loop vector gang default(present) collapse(2)
        do j = 0, decomp_info%n - 1
            do i = decomp_info%m, 1, -1
                f(i,j,0,5) = f(i-1,j-1,0,5)
            end do
        end do

        !$acc parallel loop vector gang default(present) collapse(2)
        do j = 0, decomp_info%n - 1
            do i = 0, decomp_info%m - 1
                f(i,j,0,6) = f(i+1,j-1,0,6)
            end do
        end do

        ! Bottom to top
        !$acc parallel loop vector gang default(present) collapse(2)
        do j = 0, decomp_info%n - 1
            do i = 0, decomp_info%m
                f(i,j,0,4) = f(i,j+1,0,4)
            end do
        end do

        !$acc parallel loop vector gang default(present) collapse(2)
        do j = 0, decomp_info%n - 1
            do i = 0, decomp_info%m - 1
                f(i,j,0,7) = f(i+1,j+1,0,7)
            end do
        end do

        !$acc parallel loop vector gang default(present) collapse(2)
        do j = 0, decomp_info%n - 1
            do i = decomp_info%m, 1, -1
                f(i,j,0,8) = f(i-1,j+1,0,8)
            end do
        end do

    end subroutine s_streaming_2d

    ! This subroutine computes the primitive variables from the distribution
    ! function. Its inputs are:
    !  Q: a scalar field holding the primitive variables
    !  f: a 4D array holding the distribution functions
    subroutine s_compute_prim_vars(Q, f)

        type(scalar_field), dimension(0:num_dims), intent(in) :: Q
        real(kind(0d0)), dimension(0:, 0:, 0:, 0:) :: f

        integer :: i, j, k, l

        if (num_dims == 2) then
            !$acc parallel loop vector gang default(present) collapse(2)
            do j = 0, decomp_info%n
                do i = 0, decomp_info%m
                    Q(0)%sf(i,j,0) = f(i,j,0,0) + f(i,j,0,1) + f(i,j,0,2) + &
                        f(i,j,0,3) + f(i,j,0,4) + f(i,j,0,5) + f(i,j,0,6) + &
                        f(i,j,0,7) + f(i,j,0,8)
                    Q(1)%sf(i,j,0) = (f(i,j,0,1) + f(i,j,0,5) + f(i,j,0,8) - &
                        f(i,j,0,3) - f(i,j,0,6) - f(i,j,0,7))/Q(0)%sf(i,j,0)
                    Q(2)%sf(i,j,0) = (f(i,j,0,2) + f(i,j,0,5) + f(i,j,0,6) - &
                        f(i,j,0,4) - f(i,j,0,7) - f(i,j,0,8))/Q(0)%sf(i,j,0)
                end do
            end do
        elseif (num_dims == 3) then
            !$acc parallel loop vector gang default(present) collapse(3)
            do k = 0, decomp_info%p
                do j = 0, decomp_info%n
                    do i = 0, decomp_info%m
                        Q(0)%sf(i,j,k) = 0d0
                        Q(1)%sf(i,j,k) = 0d0
                        Q(2)%sf(i,j,k) = 0d0
                        !$acc loop seq
                        do l = 0, coll_op%Q - 1
                            Q(0)%sf(i,j,k) = Q(0)%sf(i,j,k) + f(i,j,k,l)
                            Q(1)%sf(i,j,k) = Q(1)%sf(i,j,k) + f(i,j,k,l)*coll_op%cx(l)
                            Q(2)%sf(i,j,k) = Q(2)%sf(i,j,k) + f(i,j,k,l)*coll_op%cy(l)
                        end do
                        Q(1)%sf(i,j,k) = Q(1)%sf(i,j,k)/Q(0)%sf(i,j,k)
                        Q(2)%sf(i,j,k) = Q(2)%sf(i,j,k)/Q(0)%sf(i,j,k)
                        Q(3)%sf(i,j,k) = Q(3)%sf(i,j,k)/Q(0)%sf(i,j,k)
                    end do
                end do
            end do
        end if

    end subroutine s_compute_prim_vars

end module m_lattice_boltzmann
