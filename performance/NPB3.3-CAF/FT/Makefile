SHELL=/bin/sh
BENCHMARK=ft
BENCHMARKU=FT

include ../config/make.def

include ../sys/make.common

OBJS = coreduce.o ft.o ${COMMON}/${RAND}.o ${COMMON}/print_results.o ${COMMON}/timers.o ${COMMON}/wtime.o

${PROGRAM}: config ${OBJS}
	${FLINK} ${FLINKFLAGS} -o ${PROGRAM} ${OBJS} ${FMPI_LIB}

ft.o:             ft.f global.h npbparams.h
	${FCOMPILE} ft.f

coreduce.o:		coreduce.F90
	${FCOMPILE} coreduce.F90

clean:
	- rm -f *.o *.mod *~ mputil*
	- rm -f ft npbparams.h core
