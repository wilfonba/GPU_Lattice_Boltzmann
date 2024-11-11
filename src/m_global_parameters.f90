module m_global_parameters

    ! Dependencies ===================================
    use m_derived_types
    ! =================================================

    implicit none

    real(kind(0d0)), parameter :: pi = 3.141592653589793
    integer, parameter :: default_int = -100
    real(kind(0d0)), parameter :: default_real = -1d6

    integer :: problemID ! problem to select from m_setup_problem

    character(len=400) :: case_dir ! case directory

    type(timestepping_info) :: time_info
    type(decomposition_info) :: decomp_info
    type(coordinate_info) :: coord_info
    type(boundary_info) :: bc_info

    public

contains

    subroutine s_assign_default_values()

        ! Set defuault values for the decomposition info
        decomp_info%m_global = default_int
        decomp_info%n_global = default_int
        decomp_info%p_global = default_int

        decomp_info%m = default_int
        decomp_info%n = default_int
        decomp_info%p = default_int

        decomp_info%p_x = default_int
        decomp_info%p_y = default_int
        decomp_info%p_z = default_int

        ! Set default parameters for the coordinate info
        coord_info%x_min = default_real
        coord_info%x_max = default_real
        coord_info%y_min = default_real
        coord_info%y_max = default_real
        coord_info%z_min = default_real
        coord_info%z_max = default_real

        coord_info%x_min_loc = default_real
        coord_info%x_max_loc = default_real
        coord_info%y_min_loc = default_real
        coord_info%y_max_loc = default_real
        coord_info%z_min_loc = default_real
        coord_info%z_max_loc = default_real

        coord_info%dx = default_real
        coord_info%dy = default_real
        coord_info%dz = default_real

        ! Set default parameters for the boundary info
        bc_info%bc_x%beg = default_int
        bc_info%bc_x%end = default_int
        bc_info%bc_y%beg = default_int
        bc_info%bc_y%end = default_int
        bc_info%bc_z%beg = default_int
        bc_info%bc_z%end = default_int
        bc_info%vel_x_face%u%beg = default_real
        bc_info%vel_x_face%u%end = default_real
        bc_info%vel_y_face%v%beg = default_real
        bc_info%vel_y_face%v%end = default_real
        bc_info%vel_z_face%w%beg = default_real
        bc_info%vel_z_face%w%end = default_real

        ! Set default parameters for time_info
        time_info%t_step_start = default_int
        time_info%t_step_stop = default_int
        time_info%t_step_save = default_int
        time_info%dt = default_real

    end subroutine s_assign_default_values

end module m_global_parameters
