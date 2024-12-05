# About
This is a simple 2D and 3D Lattice Boltzmann solver for the lid-driven cavity problem.

# Building
As of now, support for AMD GPUs is experimental.
Intel GPUs are not supported.
Running 3D simulations larger than 200 X 200 X 200 is not advised due to a poor selection in data I/O format.
In 2D, this limitation corresponds to approximately 2800 X 2800.

- To build for CPUs with serial execution, run `./make.sh serial` on a system with any compiler and CPUs.
- To build for CPUs with multithreading, run `./make.sh multicore` on a system with the NVHPC compiler and multicore CPUs.
By default, OpenACC will use all available cores.
The number of cores can be set by running `export ACC_NUM_CORES=N` to run on `N` cores.
- To build with GPU offloading, run `./make.sh gpu` on a system with NVHPC compilers and GPUs.

# Problem Definition
The resolution of the problem can be modified by changing `m`, `n`, and `p` for the respective problem in the file `src/m_problems`.
You can select the 2D simulation by assigning `problemID = 0` and `num_dims = 2` in `src/m_global_parameters`.
You can select the 3D simulation by assigning `problemID = 1` and `num_dims = 3` in `src/m_global_parameters`.
The Reynolds number is selected by assigning `Re = N` in `src/m_global_parameters`.
