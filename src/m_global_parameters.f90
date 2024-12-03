module m_global_parameters

    ! Dependencies ===================================
    ! =================================================

    implicit none

    !==========================================================================
    ! Select the problem ID and number of dimensions by changing the parameters
    ! here so that compiler optimizations can be made
    ! =========================================================================
    integer, parameter :: problemID = 0
    integer, parameter :: num_dims = 2
    real(kind(0d0)) :: Re = 1000
    ! =========================================================================

    real(kind(0d0)), parameter :: pi = 3.141592653589793
    integer, parameter :: default_int = -100
    real(kind(0d0)), parameter :: default_real = -1d6
    real(kind(0d0)), parameter :: cs = 1d0/sqrt(3d0) ! Lattice speed of sound
    real(kind(0d0)) :: lidVel
    real(kind(0d0)), parameter :: alpha = 0.01d0 ! Lattice viscosity

    !$acc declare create(pi, default_int, default_real, cs, lidVel, alpha)

    integer :: t_step_stop, t_step_save
    real(kind(0d0)) :: dt, tau

    !$acc declare create(t_step_stop, t_step_save, dt, tau)

    integer :: m, n, p

    !$acc declare create(m, n, p)

    real(kind(0d0)), dimension(:), allocatable :: x_cb    ! x cell boundaries
    real(kind(0d0)), dimension(:), allocatable :: y_cb    ! y cell boundaries
    real(kind(0d0)), dimension(:), allocatable :: z_cb    ! z cell boundaries

    !$acc declare create(x_cb, y_cb, z_cb)

    real(kind(0d0)), dimension(0:19) :: ws ! weights
    integer, dimension(0:19) :: cx ! x component of c_i
    integer, dimension(0:19) :: cy ! y component of c_i
    integer, dimension(0:19) :: cz ! z component of c_i
    integer :: D ! Dimension of lattice
    integer :: nQ ! Number of velocities

    !$acc declare create(ws, cx, cy, cz, D, nQ)

    real(kind(0d0)) :: C

    !$acc declare create(C)

    public

contains

end module m_global_parameters
