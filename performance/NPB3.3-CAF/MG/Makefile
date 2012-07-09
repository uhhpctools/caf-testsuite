SHELL=/bin/sh
BENCHMARK=mg
BENCHMARKU=MG

include ../config/make.def

OBJS = coarray_globals.o coreduce.o mg.o ${COMMON}/print_results.o  \
       ${COMMON}/${RAND}.o ${COMMON}/timers.o ${COMMON}/wtime.o

include ../sys/make.common

${PROGRAM}: config ${OBJS}
	${FLINK} ${FLINKFLAGS} -o ${PROGRAM} ${OBJS} ${FMPI_LIB}

mg.o:		mg.f  globals.h cafnpb.h npbparams.h
	${FCOMPILE} mg.f

coarray_globals.o:		coarray_globals.f90
	${FCOMPILE} coarray_globals.f90

coreduce.o:		coreduce.F90
	${FCOMPILE} coreduce.F90

clean:
	- rm -f *.o *~ *.mod
	- rm -f npbparams.h core



