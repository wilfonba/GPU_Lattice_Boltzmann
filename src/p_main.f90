program p_main

    ! Dependencies ==========================================
    use m_global_parameters
    use m_problem_setup
    use m_lattice_boltzmann
    use m_boundary_conditions
    use m_vtk
    use m_helper
    ! =======================================================

    implicit none

    integer :: i

    real(kind(0d0)) :: t_start, t_stop, t_comp, t_io

    real(kind(0d0)), dimension(:,:,:,:), allocatable :: Q
    real(kind(0d0)), dimension(:,:,:,:), allocatable :: f,fEq, fOld
    !$acc declare create(Q, f, fEq, fOld)

    if (num_dims == 2) call s_assign_D2Q9_collision_operator()
    if (num_dims == 3) call s_assign_D3Q19_collision_operator()

    call s_get_problem()

    call s_setup_problem(Q, f, fEq, fOld)

    tau = (3*alpha + 0.5)
    nu = (1d0/3d0)*(tau - 0.5)
    lidVel = nu*Re/m

    !$acc update device(lidVel, tau)

    print*, "Re = ", Re, "lidVel = ", lidVel, "tau = ", tau

    !call s_save_data(Q,0)

    t_comp = 0d0; t_io = 0d0

    do i = 1, t_step_stop
        call cpu_time(t_start)
        print*, "Time step: ", i
        call s_collision(Q,f,fEq)
        call s_streaming(f, fOld)
        call s_apply_boundary_conditions(f)
        call s_compute_prim_vars(Q, f)
        call cpu_time(t_stop)
        t_comp = t_comp + (t_stop - t_start)
        if (mod(i, t_step_save) == 0) then
            call cpu_time(t_start)
            !call s_save_data(Q,i)
            call cpu_time(t_stop)
            t_io = t_io + (t_stop - t_start)
        end if
    end do

    print*, "Compute time: ", t_comp
    print*, "Time per iteration: ", t_comp/t_step_stop
    print*, "IO time: ", t_io
    print*, "Time per io: ", t_io/(t_step_stop/t_step_save)
    call s_finalize_problem(Q, f, fEq)

end program p_main
