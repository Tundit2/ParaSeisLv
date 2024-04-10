module use /opt/intel/oneapi/mpi/latest/modulefiles/
module load mpi

export I_MPI_TUNING_MODE=auto:cluster
export I_MPI_TUNING_AUTO_SYNC=1
export I_MPI_TUNING_AUTO_ITER_NUM=5
export I_MPI_TUNING_BIN_DUMP=./mpi_tunnings/tuning_results.dat

mpirun -hosts head,n165 -ppn 32 -np 64 ./ddm