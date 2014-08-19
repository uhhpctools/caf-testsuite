## These variables are needed for the output files and for the naming of graphs.
## Please set these variables to their appropiate values.

#OpenUH CAF Compiler
COMPILER="gfortran"
FC="mpif90"
FFLAGS="-fcoarray=lib -cpp -DCO_REDUCTIONS_SUPPORT"
LIB_CAF="-lcaf_mpi"
LAUNCHER="mpirun -n ${NPROCS}"
EXEC_OPTIONS=""

COMPILE_CMD="${FC} ${FFLAGS} ${FFLAGS_BENCH_DEFS}"
