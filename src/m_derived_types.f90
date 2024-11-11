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

    type vel_bounds_info
        type(real_bounds_info) :: u
        type(real_bounds_info) :: v
        type(real_bounds_info) :: w
    end type vel_bounds_info

    type decomposition_info
        integer :: m_global  ! Global number of grid points in x-direction
        integer :: n_global  ! Global number of grid points in y-direction
        integer :: p_global  ! Global number of grid points in z-direction
        integer :: m         ! Local number of grid points in x-direction
        integer :: n         ! Local number of grid points in y-direction
        integer :: p         ! Local number of grid points in z-direction
        integer :: p_x       ! Number of processors in x-direction
        integer :: p_y       ! Number of processors in y-direction
        integer :: p_z       ! Number of processors in z-direction
    end type decomposition_info

    type coordinate_info
        real(kind(0d0)) :: x_min ! Minimum x-coordinate
        real(kind(0d0)) :: x_max ! Maximum x-coordinate
        real(kind(0d0)) :: y_min ! Minimum y-coordinate
        real(kind(0d0)) :: y_max ! Maximum y-coordinate
        real(kind(0d0)) :: z_min ! Minimum z-coordinate
        real(kind(0d0)) :: z_max ! Maximum z-coordinate
        real(kind(0d0)) :: x_min_loc ! Minimum x-coordinate for local domain
        real(kind(0d0)) :: x_max_loc ! Maximum x-coordinate for local domain
        real(kind(0d0)) :: y_min_loc ! Minimum y-coordinate for local domain
        real(kind(0d0)) :: y_max_loc ! Maximum y-coordinate for local domain
        real(kind(0d0)) :: z_min_loc ! Minimum z-coordinate for local domain
        real(kind(0d0)) :: z_max_loc ! Maximum z-coordinate for local domain
        real(kind(0d0)), dimension(:), allocatable :: x_cc    ! x-coordinate of cell center
        real(kind(0d0)), dimension(:), allocatable :: y_cc    ! y-coordinate of cell center
        real(kind(0d0)), dimension(:), allocatable :: z_cc    ! z-coordinate of cell center
        real(kind(0d0)) :: dx    ! Grid spacing in x-direction
        real(kind(0d0)) :: dy    ! Grid spacing in y-direction
        real(kind(0d0)) :: dz    ! Grid spacing in z-direction
    end type coordinate_info

    type boundary_info
        type(int_bounds_info) :: bc_x ! Integer identifier for x-boundary conditions
        type(int_bounds_info) :: bc_y ! Integer identifier for y-boundary conditions
        type(int_bounds_info) :: bc_z ! Integer identifier for z-boundary conditions
        type(vel_bounds_info) :: vel_x_face ! Velocity boundary conditions on x-faces
        type(vel_bounds_info) :: vel_y_face ! Velocity boundary conditions on y-faces
        type(vel_bounds_info) :: vel_z_face ! Velocity boundary conditions on z-faces
    end type boundary_info

    type timestepping_info
        integer :: t_step_start ! Starting time step
        integer :: t_step_stop  ! Ending time step
        integer :: t_step_save  ! Time step save frequency
        real(kind(0d0)) :: dt   ! Time step size
    end type timestepping_info

end module m_derived_types
