module use /opt/intel/oneapi/mpi/latest/modulefiles/
module load mpi

export I_MPI_TUNING_BIN=mpi_tunnings/tuning_results.dat

mpirun -hosts head,n165 -ppn 32 -np 64 ./ddm