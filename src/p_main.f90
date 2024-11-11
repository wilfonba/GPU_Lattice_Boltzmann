program p_main

    ! Dependencies ==========================================
    use m_global_parameters
    use m_helpers
    use m_mpi_proxy
    ! =======================================================

    integer :: i

    implicit none

    call s_mpi_initialize()

    if (proc_rank == 0) call s_read_user_input()

    call s_broadcast_user_input()

    call s_get_problem()

    call s_initialize_modules()

    do i = 1, n_steps

        call s_advance_solution()

    end do

    call s_finalize_modules()

    call s_mpi_finalize()

end program p_main
