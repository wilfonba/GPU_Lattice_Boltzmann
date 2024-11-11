module m_vtk

    ! Dependencies ==========================================
    use m_global_parameters

    use m_helpers
    ! =======================================================

    implicit none

    private; public :: s_save_data, &
        s_open_vtk_data_file, &
        s_write_variable_to_vtk_file, &
        s_close_vtk_data_file, &
        s_write_parallel_vtk_data_file

contains

    ! this subrouting saves all of the simulation data. Its inputs are
    ! Q: A 1D array of sclalar fields with dimension (N+1) x (N+1)
    ! n: The number of grid points in each direction
    ! save_count: The number of the save
    subroutine s_save_data(Q, n, save_count)

        type(scalar_field), dimension(1:) :: Q
        real(kind(0d0)), dimension(0:N,0:N) :: data_wrt
        integer :: n, save_count, i, j

        call s_open_vtk_data_file(n, save_count)

        call s_write_variable_to_vtk_file(Q(1)%sf(0:,0:), n, 'pressure')

        call s_write_variable_to_vtk_file(Q(2)%sf(0:,0:), n, 'x-velocity')

        call s_write_variable_to_vtk_file(Q(3)%sf(0:,0:), n, 'y-velocity')

        call s_close_vtk_data_file()

    end subroutine s_save_data

    ! This subroutine opens a .vtr file and writes the header and recttilinear
    ! grid to it. It requires the following inputs:
    ! N: The number of grid points in each direction
    ! n_save: The number of the save
    subroutine s_open_vtk_data_file(N, n_save)

        integer :: N, i, n_save

        character(len=100) :: file_name
        character(len=20) :: dir_name = 'data'
        character(len=100) :: line

        file_name = trim(dir_name)//'/output_'//trim(f_int_to_str(n_save))//'.vtr'

        open (3, FILE=trim(file_name), &
              FORM='formatted', &
              STATUS='replace')

        ! header
        write(3,"(A)") "<?xml version='1.0'?>"
        write(3,"(A)") "<VTKFile type='RectilinearGrid' version='0.1' byte_order='LittleEndian'>"
        line = trim(f_int_to_str(0))//" "//trim(f_int_to_str(N+1))//" "// &
                trim(f_int_to_str(0))//" "//trim(f_int_to_str(N+1))//" "// &
                trim(f_int_to_str(0))//" "//trim(f_int_to_str(0))
        write(3,"(A)") "    <RectilinearGrid WholeExtent='"//trim(line)//"'>"
        write(3,"(A)") "        <Piece Extent='"//trim(line)//"'>"

        ! x-coordinates
        write(3,"(A)") "            <Coordinates>"
        write(3,"(A)") "                <DataArray type='Float64' format='ascii'>"
        write(3,"(A)",advance='no') "               "
        do i = 0, N + 1
            write(3,"(A)",advance='no') trim(f_dbl_to_str(xs_cb(i)))//" "
        end do
        write(3,*)
        write(3,"(A)") "                </DataArray>"

        ! y-coordinates
        write(3,"(A)") "                <DataArray type='Float64' format='ascii'>"
        write(3,"(A)",advance='no') "               "
        do i = 0, N + 1
            write(3,"(A)",advance='no') trim(f_dbl_to_str(ys_cb(i)))//" "
        end do
        write(3,*)
        write(3,"(A)") "                </DataArray>"

        ! z-coordinates
        write(3,"(A)") "                <DataArray type='Float64' format='ascii'>"
        write(3,"(A)",advance='no') "               "
        write(3,"(A)",advance='no') trim(f_dbl_to_str(0d0))//" "
        write(3,"(A)") trim(f_dbl_to_str(0d0))
        write(3,"(A)") "                </DataArray>"
        write(3,"(A)") "            </Coordinates>"

        ! point data
        write(3,"(A)") "            <CellData>"

    end subroutine s_open_vtk_data_file

    ! This subroutine writes a variable to the .vtr file. It requires the
    ! following inputs
    !  Q: An (N + 1) x (N + 1) array to be stored
    !  N: The number of grid points in each direction
    !  name: The name to store the variable as
    subroutine s_write_variable_to_vtk_file(Q, N, name)

        real(kind(0d0)), dimension(0:, 0:) :: Q
        integer :: i, j, N, idx
        character(len=*) :: name

        write(3,"(A)") "                <DataArray type='Float64' Name='"//trim(name)//"' format='ascii'>"
        do i = 0, N
            do j = 0, N
                write(3,"(A)",advance='no') trim(f_dbl_to_str(Q(j,i)))//" "
            end do
        end do
        write(3,*)
        write(3,"(A)") "                </DataArray>"

    end subroutine s_write_variable_to_vtk_file

    ! This subroutine adds the foot to the .vtr file and closes it. It requires
    ! no inputs.
    subroutine s_close_vtk_data_file()

        write(3,"(A)") "            </CellData>"
        write(3,"(A)") "        </Piece>"
        write(3,"(A)") "    </RectilinearGrid>"
        write(3,"(A)") "</VTKFile>"

        close(3)

    end subroutine s_close_vtk_data_file

    function f_int_to_str(N) result(res)

        integer, intent(in) :: N
        character(len=10) :: res

        write (res, '(I0)') N

    end function

    function f_dbl_to_str(d) result(str)

        real(8), intent(in) :: d
        character(LEN=64) :: str
        write(str, '(E16.8)') d

   end function

    subroutine s_write_parallel_vtk_data_file()

    end subroutine s_write_parallel_vtk_data_file

end module m_vtk