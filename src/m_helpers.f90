module m_helpers

    ! Dependencies ==========================================
    use m_global_parameters
    ! =======================================================

    implicit none

    private; public :: s_perform_time_step

contains

    subroutine s_perform_time_step()

    end subroutine s_perform_time_step

    subroutine s_read_user_input()

        character(LEN=100) :: line
        character(LEN=400) :: file_path
        integer :: iostatus
        logical :: file_exist

        namelist /user_inputs/ problemID

        inquire (FILE=trim(file_path), EXIST=file_exist)

        if (file_exist) then
            open (1, FILE=trim(file_path), &
                  FORM='formatted', &
                  ACTION='read', &
                  STATUS='old')
            read (1, NML=user_inputs, iostat=iostatus)

            if (iostatus /= 0) then
                backspace (1)
                read (1, fmt='(A)') line
                print *, 'Invalid line in namelist: '//trim(line)
                call abort()
            end if

            close (1)
        else
            print*, "Missing input file"
            call abort()
        end if

    end subroutine s_read_user_input

    subroutine s_print_user_input()

        print*, "Solving problemID ", problemID, "in case directory ", case_dir

    end subroutine s_print_user_input

    subroutine s_get_local_problem_setup()

    end subroutine s_get_local_problem_setup

end module m_helpers
