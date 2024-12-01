module m_derived_types

    implicit none

    type scalar_field
        real(kind(0d0)), pointer, dimension(:, :, :) :: sf => null()
    end type scalar_field

    type int_bounds_info
        integer :: beg ! Left bounds value
        integer :: end ! Right bounds value
    end type int_bounds_info

    type real_bounds_info
        real(kind(0d0)) :: beg ! Left bounds value
        real(kind(0d0)) :: end ! Right bounds value
    end type real_bounds_info

    type decomposition_info
        integer :: m ! cells in x direction
        integer :: n ! cells in y direction
        integer :: p ! cells in z direction
    end type decomposition_info

    type coordinate_info
        real(kind(0d0)), dimension(:), allocatable :: x_cb    ! x cell boundaries
        real(kind(0d0)), dimension(:), allocatable :: y_cb    ! y cell boundaries
        real(kind(0d0)), dimension(:), allocatable :: z_cb    ! z cell boundaries
    end type coordinate_info

    type timestepping_info
        integer :: t_step_stop  ! Ending time step
        integer :: t_step_save  ! Time step save frequency
        real(kind(0d0)) :: dt   ! Time step size
        real(kind(0d0)) :: tau  ! Relaxation time
    end type timestepping_info

    type collision_operator
        real(kind(0d0)), dimension(0:19) :: w ! weights
        integer, dimension(0:19) :: cx ! x component of c_i
        integer, dimension(0:19) :: cy ! y component of c_i
        integer, dimension(0:19) :: cz ! z component of c_i
        integer :: D ! Dimension of lattice
        integer :: Q ! Number of velocities
    end type collision_operator

end module m_derived_types
