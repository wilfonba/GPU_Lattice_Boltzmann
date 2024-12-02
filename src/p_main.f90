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
    real(kind(0d0)), dimension(:,:,:,:), allocatable :: f,fEq

    if (num_dims == 2) call s_assign_D2Q9_collision_operator(coll_op)
    if (num_dims == 3) call s_assign_D3Q19_collision_operator(coll_op)

    call s_get_problem()

    call s_setup_problem(Q, f, fEq)

    lidVel = alpha*Re/decomp_info%m
    time_info%tau = (3d0*alpha + 0.5d0)

    !$acc update device(lidVel, time_info%tau)

    print*, "Re = ", Re, "lidVel = ", lidVel, "tau = ", time_info%tau

    call s_save_data(Q,0)

    t_comp = 0d0; t_io = 0d0

    do i = 1, time_info%t_step_stop
        call cpu_time(t_start)
        call s_collision(Q,f,fEq)
!$acc update host(f)
call s_print_2d_array(f(:,:,0,0))
call s_print_2d_array(f(:,:,0,1))
call s_print_2d_array(f(:,:,0,2))
call s_print_2d_array(f(:,:,0,3))
call s_print_2d_array(f(:,:,0,4))
call s_print_2d_array(f(:,:,0,5))
call s_print_2d_array(f(:,:,0,6))
call s_print_2d_array(f(:,:,0,7))
call s_print_2d_array(f(:,:,0,8))
        call s_streaming(f)
        call s_apply_boundary_conditions(f)
        call s_compute_prim_vars(Q, f)
        call cpu_time(t_stop)
        t_comp = t_comp + (t_stop - t_start)
        if (mod(i, time_info%t_step_save) == 0) then
            call cpu_time(t_start)
            call s_save_data(Q,i)
            print*, "Time step: ", i
            call cpu_time(t_stop)
            t_io = t_io + (t_stop - t_start)
        end if
    end do

    print*, "Compute time: ", t_comp
    print*, "Time per iteration: ", t_comp/time_info%t_step_stop
    print*, "IO time: ", t_io
    print*, "Time per io: ", t_io/(time_info%t_step_stop/time_info%t_step_save)
    call s_finalize_problem(Q, f, fEq)

end program p_main
