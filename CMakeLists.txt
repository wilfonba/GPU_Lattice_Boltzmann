cmake_minimum_required(VERSION 3.10)
project(MyFortranMPIProject Fortran)

# Specify the Fortran standard (optional)
set(CMAKE_Fortran_STANDARD 90)
set(CMAKE_Fortran_STANDARD_REQUIRED ON)

# Find MPI
find_package(MPI REQUIRED)

# Include the MPI Fortran compiler flags
include_directories(${MPI_Fortran_INCLUDE_PATH})
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${MPI_Fortran_COMPILE_FLAGS}")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${MPI_Fortran_LINK_FLAGS}")

# Gather all Fortran source files in src/ directory
file(GLOB_RECURSE SOURCES "src/*.f90" "src/*.f")

# Add the executable and link with MPI
add_executable(my_mpi_program ${SOURCES})
target_link_libraries(my_mpi_program ${MPI_Fortran_LIBRARIES})

