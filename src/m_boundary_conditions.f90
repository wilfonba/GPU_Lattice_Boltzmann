module m_boundary_conditions

    ! Dependencies ==========================================
    use m_global_parameters
    use m_derived_types
    use m_mpi_proxy
    ! =======================================================

    implicit none

    private; public :: s_apply_boundary_conditions

contains

    subroutine s_apply_boundary_conditions()

        ! x-direction =========================================================
        select case(bc_x%beg) ! left x-direction BCs
            case(0) ! no slip
                call s_no_slip_bc(1,1)
            case(1) ! prescribed velocity
                call s_prescribed_velocity_bc(1,1)
            case default ! processor domain
                call s_populate_ghost_cells(1,1)
        end select

        select case(bc_x%end) ! right x-direction BCs
            case(0) ! no slip
                call s_no_slip_bc(1,-1)
            case(1) ! prescribed velocity
                call s_prescribed_velocity_bc(1,-1)
            case default ! processor domain
                call s_populate_ghost_cells(1,-1)
        end select
        ! =====================================================================

        ! y-direction =========================================================
        select case(bc_y%beg) ! left y-direction BCs
            case(0) ! no slip
                call s_no_slip_bc(2,-1)
            case(1) ! prescribed velocity
                call s_prescribed_velocity_bc(2,-1)
            case default ! processor domain
                call s_populate_ghost_cells(2,-1)
        end select

        select case(bc_y%end) ! right y-direction BCs
            case(0) ! no slip
                call s_no_slip_bc(2,1)
            case(1) ! prescribed velocity
                call s_prescribed_velocity_bc(2,1)
            case default ! processor domain
                call s_populate_ghost_cells(2,1)
        end select
        ! =====================================================================

        ! z-direction =========================================================
        select case(bc_z%beg) ! left z-direction BCs
            case(0) ! no slip
                call s_no_slip_bc(3,-1)
            case(1) ! prescribed velocity
                call s_prescribed_velocity_bc(3,-1)
            case default ! processor domain
                call s_populate_ghost_cells(3,-1)
        end select

        select case(bc_z%end) ! right z-direction BCs
            case(0) ! no slip
                call s_no_slip_bc(3,1)
            case(1) ! prescribed velocity
                call s_prescribed_velocity_bc(3,1)
            case default ! processor domain
                call s_populate_ghost_cells(3,1)
        end select
        ! =====================================================================

    end subroutine s_apply_boundary_conditions

    subroutine s_no_slip_bc(bc_dir, bc_loc)

        integer :: bc_dir ! Boundary condition direction, X = 1, Y = 2, Z = 3
        integer :: bc_loc ! Boundary location, -1 = left, 1 = right

        ! x-direction =========================================================
        if (bc_dir == 1) then ! x-direction
            if (bc_loc == -1) then ! left

            elseif (bc_loc == 1) then ! right

            end if
        ! y-direction =========================================================
        elseif (bc_dir == 2) then ! y-direction
            if (bc_loc == -1) then ! left

            elseif (bc_loc == 1) then ! right

            end if
        ! z-direction =========================================================
        elseif (bc_dir == 3) then ! z-direction
            if (bc_loc == -1) then ! left

            elseif (bc_loc == 1) then ! right

            end if
        end if
        ! =====================================================================

    end subroutine s_no_slip_bc

    subroutine s_no_slip_bc(bc_dir, bc_loc)

        integer :: bc_dir ! Boundary condition direction, X = 1, Y = 2, Z = 3
        integer :: bc_loc ! Boundary location, -1 = left, 1 = right

        ! x-direction =========================================================
        if (bc_dir == 1) then ! x-direction
            if (bc_loc == -1) then ! left

            elseif (bc_loc == 1) then ! right

            end if
        ! y-direction =========================================================
        elseif (bc_dir == 2) then ! y-direction
            if (bc_loc == -1) then ! left

            elseif (bc_loc == 1) then ! right

            end if
        ! z-direction =========================================================
        elseif (bc_dir == 3) then ! z-direction
            if (bc_loc == -1) then ! left

            elseif (bc_loc == 1) then ! right

            end if
        end if
        ! =====================================================================

    end subroutine s_no_slip_bc

end module m_boundary_conditions
