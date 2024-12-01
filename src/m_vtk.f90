module m_vtk

    ! Dependencies ==========================================
    use m_global_parameters
    ! =======================================================

    implicit none

    private; public :: s_save_data, &
        s_open_vtk_data_file, &
        s_write_variable_to_vtk_file, &
        s_close_vtk_data_file

contains

    ! this subrouting saves all of the simulation data. Its inputs are
    ! Q: A 1D array of sclalar fields with dimension (N+1) x (N+1)
    ! save_count: The number of the save
    subroutine s_save_data(Q, save_count)

        type(scalar_field), dimension(0:) :: Q
        integer :: save_count

        call s_open_vtk_data_file(save_count)

        !$acc update host(Q(0)%sf)
        call s_write_variable_to_vtk_file(Q(0)%sf, 'density')
        !$acc update host(Q(1)%sf)
        call s_write_variable_to_vtk_file(Q(1)%sf, 'x-velocity')
        !$acc update host(Q(2)%sf)
        call s_write_variable_to_vtk_file(Q(2)%sf, 'y-velocity')

        if (num_dims == 3) then
            !$acc update host(Q(3)%sf)
            call s_write_variable_to_vtk_file(Q(3)%sf, 'z-velocity')
        end if

        call s_close_vtk_data_file()

    end subroutine s_save_data

    ! This subroutine opens a .vtr file and writes the header and recttilinear
    ! grid to it. It requires the following inputs:
    ! N: The number of grid points in each direction
    ! n_save: The number of the save
    subroutine s_open_vtk_data_file(n_save)

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
        if (num_dims == 2) then
            line = trim(f_int_to_str(0))//" "//trim(f_int_to_str(decomp_info%m + 1))//" "// &
                    trim(f_int_to_str(0))//" "//trim(f_int_to_str(decomp_info%n + 1))//" "// &
                    trim(f_int_to_str(0))//" "//trim(f_int_to_str(0))
        else
            line = trim(f_int_to_str(0))//" "//trim(f_int_to_str(decomp_info%m + 1))//" "// &
                    trim(f_int_to_str(0))//" "//trim(f_int_to_str(decomp_info%n + 1))//" "// &
                    trim(f_int_to_str(0))//" "//trim(f_int_to_str(decomp_info%p + 1))
        end if
        write(3,"(A)") "    <RectilinearGrid WholeExtent='"//trim(line)//"'>"
        write(3,"(A)") "        <Piece Extent='"//trim(line)//"'>"

        ! x-coordinates
        write(3,"(A)") "            <Coordinates>"
        write(3,"(A)") "                <DataArray type='Float64' format='ascii'>"
        write(3,"(A)",advance='no') "               "
        do i = 0, decomp_info%m + 1
            write(3,"(A)",advance='no') trim(f_dbl_to_str(coord_info%x_cb(i)))//" "
        end do
        write(3,*)
        write(3,"(A)") "                </DataArray>"

        ! y-coordinates
        write(3,"(A)") "                <DataArray type='Float64' format='ascii'>"
        write(3,"(A)",advance='no') "               "
        do i = 0, decomp_info%n + 1
            write(3,"(A)",advance='no') trim(f_dbl_to_str(coord_info%y_cb(i)))//" "
        end do
        write(3,*)
        write(3,"(A)") "                </DataArray>"

        ! z-coordinates
        if (num_dims == 2) then
            write(3,"(A)") "                <DataArray type='Float64' format='ascii'>"
            write(3,"(A)",advance='no') "               "
            write(3,"(A)",advance='no') trim(f_dbl_to_str(0d0))//" "
            write(3,"(A)") trim(f_dbl_to_str(0d0))
            write(3,"(A)") "                </DataArray>"
        else
            write(3,"(A)") "                <DataArray type='Float64' format='ascii'>"
            write(3,"(A)",advance='no') "               "
            do i = 0, decomp_info%p + 1
                write(3,"(A)",advance='no') trim(f_dbl_to_str(coord_info%z_cb(i)))//" "
            end do
            write(3,*)
            write(3,"(A)") "                </DataArray>"
        end if

        write(3,"(A)") "            </Coordinates>"

        ! point data
        write(3,"(A)") "            <CellData>"

    end subroutine s_open_vtk_data_file

    ! This subroutine writes a variable to the .vtr file. It requires the
    ! following inputs
    !  Q: An (N + 1) x (N + 1) array to be stored
    !  N: The number of grid points in each direction
    !  name: The name to store the variable as
    subroutine s_write_variable_to_vtk_file(Q,  name)

        real(kind(0d0)), dimension(0:, 0:, 0:) :: Q
        integer :: i, j, k
        character(len=*) :: name

        write(3,"(A)") "                <DataArray type='Float64' Name='"//trim(name)//"' format='ascii'>"
        if (num_dims == 2) then
            do j = 0, decomp_info%n
                do i = 0, decomp_info%m
                    write(3,"(A)",advance='no') trim(f_dbl_to_str(Q(i,j,0)))//" "
                end do
            end do
        else
            do k = 0, decomp_info%p
                do j = 0, decomp_info%n
                    do i = 0, decomp_info%m
                        write(3,"(A)",advance='no') trim(f_dbl_to_str(Q(i,j,k)))//" "
                    end do
                end do
            end do
        end if
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

end module m_vtk
