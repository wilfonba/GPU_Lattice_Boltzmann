module m_global_parameters

    ! Dependencies ===================================
    use m_derived_types
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
    real(kind(0d0)), parameter :: cs = 1d0/sqrt(3d0)
    real(kind(0d0)) :: lidVel
    real(kind(0d0)), parameter :: alpha = 0.01d0 ! Lattice viscosity

    !$acc declare create(pi, default_int, default_real, cs, lidVel, alpha)

    type(timestepping_info) :: time_info
    type(decomposition_info) :: decomp_info
    type(collision_operator) :: coll_op
    type(coordinate_info) :: coord_info

    !$acc declare create(time_info, decomp_info, coll_op, coord_info)

    real(kind(0d0)) :: C

    !$acc declare create(C)

    public

contains

end module m_global_parameters
