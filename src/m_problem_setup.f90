module m_problem_setup

   ! Dependencies =======================
    use m_global_parameters
    use m_mpi
    ! ====================================

    implicit none

    public; private :: s_get_problem

contains


    subroutine s_get_problem()

        select case(problemID)
            case(1)
                call s_get_2D_laminar_boundary_layer()
            case(2)
                call s_get_2D_laminar_channel_flow()
            case(3)
                call s_get_3D_flow_over_sphere()
            case(4)
                call s_get_3D_rayleigh_taylor_instability()
            case default
                call s_mpi_abort("Invalid problem ID")
        end select

    end subroutine s_get_problem

end module m_problem_setup
