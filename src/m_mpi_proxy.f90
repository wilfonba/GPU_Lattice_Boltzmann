module m_mpi_proxy

    ! Dependencies ==========================================
    use m_global_parameters

    use m_helpers
    ! =======================================================

    implicit none

    real(kind(0d0)), dimension(:), allocatable :: q_buff_send, q_buff_recv

    private; public :: s_initialize_mpi

contains

    subroutine s_mpi_initialize

        call MPI_INIT(ierr)

        if (ierr /= MPI_SUCCESS) then
            print '(A)', 'Unable to initialize MPI environment. Exiting ...'
            call MPI_ABORT(MPI_COMM_WORLD, 1, ierr)
        end if

        call MPI_COMM_SIZE(MPI_COMM_WORLD, num_procs, ierr)
        call MPI_COMM_RANK(MPI_COMM_WORLD, proc_rank, ierr)

    end subroutine s_mpi_initialize

    subroutine s_mpi_abort(prnt)

        character(len=*), intent(in), optional :: prnt

        if (present(prnt)) then
            print *, prnt
            call flush (6)
        end if

        call MPI_ABORT(MPI_COMM_WORLD, 1, ierr)

    end subroutine s_mpi_abort

    subroutine s_initialize_mpi_module()

        call s_mpi_initialize()

        if (p > 0) then
            allocate(q_buff_send(0:(m+2)**2 + 1)
            allocate(q_buff_recv(0:(m+2)**2 + 1)
        else
            allocate(q_buff_send(0:(m+1) + 1))
            allocate(q_buff_send(0:(m+1) + 1))
        end if

    end subroutine s_initialize_mpi_module

    subroutine s_finalize_mpi_module()

        if (p > 0) then
            allocate(q_buff_send(0:(m+2)**2 + 1)
            allocate(q_buff_recv(0:(m+2)**2 + 1)
        else
            allocate(q_buff_send(0:(m+1) + 1))
            allocate(q_buff_send(0:(m+1) + 1))
        end if

        call s_mpi_initialize()

    end subroutine s_finalize_mpi_module

    subroutine s_decompose_computational_domain

        if (p > 0) then

        else

        end if

    end subroutine s_decompose_computational_domain

    subroutine s_mpi_exchange_ghost_cells(dir)

        integer, intent(in) :: dir



    end subroutine s_mpi_exchange_ghost_cells

    subroutine s_finalize_mpi_module

        deallocate(q_buff_send, q_buff_recv)

        call s_mpi_finalize(ierr)

    end subroutine s_finalize_mpi_module

end module m_mpi_proxy
