SHELL=/bin/sh
BENCHMARK=lu
BENCHMARKU=LU
VEC=

include ../config/make.def

OBJS = lu.o init_comm.o read_input.o bcast_inputs.o proc_grid.o neighbors.o \
       nodedim.o subdomain.o setcoeff.o sethyper.o setbv.o exact.o setiv.o \
       erhs.o ssor.o exchange_1.o exchange_3.o exchange_4.o exchange_5.o \
       exchange_6.o rhs.o l2norm.o jacld.o blts$(VEC).o jacu.o buts$(VEC).o \
       error.o pintgr.o verify.o coarray_globals.o ${COMMON}/print_results.o \
	   coreduce.o ${COMMON}/timers.o ${COMMON}/wtime.o

include ../sys/make.common


# npbparams.h is included by applu.incl
# The following rule should do the trick but many make programs (not gmake)
# will do the wrong thing and rebuild the world every time (because the
# mod time on header.h is not changed. One solution would be to 
# touch header.h but this might cause confusion if someone has
# accidentally deleted it. Instead, make the dependency on npbparams.h
# explicit in all the lines below (even though dependence is indirect). 

# applu.incl: npbparams.h
.SUFFIXES: .f .o .h .mod .F .f90 .F90

${PROGRAM}: config
	@if [ x$(VERSION) = xvec ] ; then	\
		${MAKE} VEC=_vec exec;		\
	elif [ x$(VERSION) = xVEC ] ; then	\
		${MAKE} VEC=_vec exec;		\
	else					\
		${MAKE} exec;			\
	fi

exec: $(OBJS)
	${FLINK} ${FLINKFLAGS} -o ${PROGRAM} ${OBJS} ${FMPI_LIB}

.f.o :
	${FCOMPILE} $<

.f90.o :
	${FCOMPILE} $<

.F90.o :
	${FCOMPILE} $<

lu.o:		lu.f applu.incl npbparams.h
bcast_inputs.o:	bcast_inputs.f applu.incl npbparams.h cafnpb.h coarray_globals.mod
blts$(VEC).o:	blts$(VEC).f
buts$(VEC).o:	buts$(VEC).f
erhs.o:		erhs.f applu.incl npbparams.h
error.o:	error.f applu.incl npbparams.h cafnpb.h custom_coreduce.mod
exact.o:	exact.f applu.incl npbparams.h
exchange_1.o:	exchange_1.f applu.incl npbparams.h cafnpb.h coarray_globals.mod
exchange_3.o:	exchange_3.f applu.incl npbparams.h cafnpb.h coarray_globals.mod
exchange_4.o:	exchange_4.f applu.incl npbparams.h cafnpb.h coarray_globals.mod
exchange_5.o:	exchange_5.f applu.incl npbparams.h cafnpb.h coarray_globals.mod
exchange_6.o:	exchange_6.f applu.incl npbparams.h cafnpb.h coarray_globals.mod
init_comm.o:	init_comm.f applu.incl npbparams.h cafnpb.h 
jacld.o:	jacld.f applu.incl npbparams.h
jacu.o:		jacu.f applu.incl npbparams.h
l2norm.o:	l2norm.f cafnpb.h custom_coreduce.mod
neighbors.o:	neighbors.f applu.incl npbparams.h
nodedim.o:	nodedim.f
pintgr.o:	pintgr.f applu.incl npbparams.h cafnpb.h custom_coreduce.mod
proc_grid.o:	proc_grid.f applu.incl npbparams.h
read_input.o:	read_input.f applu.incl npbparams.h cafnpb.h
rhs.o:		rhs.f applu.incl npbparams.h
setbv.o:	setbv.f applu.incl npbparams.h
setiv.o:	setiv.f applu.incl npbparams.h
setcoeff.o:	setcoeff.f applu.incl npbparams.h
sethyper.o:	sethyper.f applu.incl npbparams.h
ssor.o:		ssor.f applu.incl npbparams.h cafnpb.h custom_coreduce.mod
subdomain.o:	subdomain.f applu.incl npbparams.h cafnpb.h
verify.o:	verify.f applu.incl npbparams.h
coarray_globals.mod : coarray_globals.o npbparams.h
coarray_globals.o : coarray_globals.f
custom_coreduce.mod : coreduce.o
coreduce.o : coreduce.F90


clean:
	- /bin/rm -f npbparams.h
	- /bin/rm -f *.o *~ *.mod
