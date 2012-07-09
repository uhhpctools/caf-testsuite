SHELL=/bin/bash
BENCHMARK=bt
BENCHMARKU=BT
VEC=

include ../config/make.def


OBJS = bt.o make_set.o initialize.o exact_solution.o exact_rhs.o \
       set_constants.o adi.o define.o copy_faces.o rhs.o solve_subs.o \
       x_solve$(VEC).o y_solve$(VEC).o z_solve$(VEC).o add.o error.o \
       verify.o setup_caf.o coreduce.o npbcaf.o \
       ${COMMON}/print_results.o ${COMMON}/timers.o ${COMMON}/wtime.o

include ../sys/make.common

# npbparams.h is included by header.h
# The following rule should do the trick but many make programs (not gmake)
# will do the wrong thing and rebuild the world every time (because the
# mod time on header.h is not changed. One solution would be to
# touch header.h but this might cause confusion if someone has
# accidentally deleted it. Instead, make the dependency on npbparams.h
# explicit in all the lines below (even though dependence is indirect).

# header.h: npbparams.h
.SUFFIXES: .f .F .o .h .mod .f90 .F90

${PROGRAM}: config
	@if [ x$(VERSION) = xvec ] ; then	\
		${MAKE} VEC=_vec exec;		\
	elif [ x$(VERSION) = xVEC ] ; then	\
		${MAKE} VEC=_vec exec;		\
	else					\
		${MAKE} exec;			\
	fi

# N1830 does not specifically permit collectively OPENing the same unit across 
#       multiple images.
# Default: We set I/O phases to no-op (btio.f) in bt-bt
# N12-136 proposes relaxing the above constraint.
exec: $(OBJS)  
	@if [ x$(SUBTYPE) = xfortran ] ; then	\
		${MAKE} bt-fortran;		\
	elif [ x$(SUBTYPE) = xFORTRAN ] ; then	\
		${MAKE} bt-fortran;		\
	elif [ x$(SUBTYPE) = xepio ] ; then	\
		${MAKE} bt-epio;		\
	elif [ x$(SUBTYPE) = xEPIO ] ; then	\
		${MAKE} bt-epio;		\
	else					\
		${MAKE} bt-bt;			\
	fi

bt-bt: ${OBJS} btio.o
	${FLINK} ${FLINKFLAGS} -o ${PROGRAM} ${OBJS} btio.o

bt-fortran: ${OBJS} fortran_io.o btio_common.o
	${FLINK} ${FLINKFLAGS} -o ${PROGRAM}.fortran_io ${OBJS} btio_common.o fortran_io.o

bt-epio: ${OBJS} epio.o btio_common.o
	${FLINK} ${FLINKFLAGS} -o ${PROGRAM}.ep_io ${OBJS} btio_common.o epio.o

.f.o:
	${FCOMPILE} $<

.F.o:
	${FCOMPILE} $< # -DUSE_BARRIER

.f90.o:
	${FCOMPILE} $<

.F90.o:
	${FCOMPILE} $<


bt.o:             bt.f  header.h npbparams.h custom_coreduce.mod
make_set.o:       make_set.f  header.h npbparams.h
initialize.o:     initialize.f  header.h npbparams.h
exact_solution.o: exact_solution.f  header.h npbparams.h
exact_rhs.o:      exact_rhs.f  header.h npbparams.h
set_constants.o:  set_constants.f  header.h npbparams.h
adi.o:            adi.f  header.h npbparams.h
define.o:         define.f  header.h npbparams.h
copy_faces.o:     copy_faces.f  header.h npbparams.h
rhs.o:            rhs.f  header.h npbparams.h
x_solve.o:        x_solve.F  header.h work_lhs.h npbparams.h
y_solve.o:        y_solve.F  header.h work_lhs.h npbparams.h
z_solve.o:        z_solve.F  header.h work_lhs.h npbparams.h
solve_subs.o:     solve_subs.f  npbparams.h
add.o:            add.f  header.h npbparams.h
error.o:          error.f  header.h npbparams.h custom_coreduce.mod
verify.o:         verify.f  header.h npbparams.h
setup_caf.o:      setup_caf.f npbparams.h npbcaf.mod
btio.o:           btio.f
btio_common.o:    btio_common.f header.h npbparams.h
fortran_io.o:     fortran_io.f header.h npbparams.h
epio.o:           epio.f header.h npbparams.h
custom_coreduce.mod:   coreduce.o
coreduce.o:     coreduce.F90
header.h:         npbcaf.mod
npbcaf.mod:	  npbcaf.o
npbcaf.o:	  npbcaf.f npbparams.h

clean:
	- rm -f *.o *~ mputil* *.mod
	- rm -f  npbparams.h core
