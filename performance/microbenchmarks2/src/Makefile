compiler ?= uhcaf
NPROCS ?= 4

TEMP_1:=$(shell cd ../../../config; ./config2makedef.sh; cd -)  # generate make.def from CONFIG
include ../../../config/make-compiler.$(compiler).def  # auto-generated

BINDIR=../bin

LATENCY_TESTS=-DLATENCY_TESTS
BW_TESTS=-DBW_TESTS
STRIDED_BW_TESTS=-DSTRIDED_BW_TESTS
REDUCE_TESTS=-DREDUCE_TESTS
ALL_TESTS=$(LATENCY_TESTS) $(BW_TESTS) $(STRIDED_BW_TESTS) $(REDUCE_TESTS)

.PHONY:all_tests latency_tests bw_tests strided_bw_tests reduce_tests

run_all_tests: all_tests
	$(LAUNCHER) $(BINDIR)/$^ $(EXEC_OPTIONS)

all_tests: all_tests.o wtime.o
	$(COMPILE_CMD) $(LDFLAGS) -o $(BINDIR)/$@ $^

all_tests.o: caf_tests.F90
	$(COMPILE_CMD) -c $(FFLAGS) $(ALL_TESTS) $< -o $@

run_latency_tests: latency_tests
	$(LAUNCHER) $(BINDIR)/$^ $(EXEC_OPTIONS)

latency_tests: latency_tests.o wtime.o
	$(COMPILE_CMD) $(LDFLAGS) -o $(BINDIR)/$@ $^

latency_tests.o: caf_tests.F90
	$(COMPILE_CMD) -c $(FFLAGS) $(LATENCY_TESTS) $< -o $@

run_bw_tests: bw_tests
	$(LAUNCHER) $(BINDIR)/$^ $(EXEC_OPTIONS)

bw_tests: bw_tests.o wtime.o
	$(COMPILE_CMD) $(LDFLAGS) -o $(BINDIR)/$@ $^

bw_tests.o: caf_tests.F90
	$(COMPILE_CMD) -c $(FFLAGS) $(BW_TESTS) $< -o $@

run_strided_bw_tests: strided_bw_tests
	$(LAUNCHER) $(BINDIR)/$^ $(EXEC_OPTIONS)

strided_bw_tests: strided_bw_tests.o wtime.o
	$(COMPILE_CMD) $(LDFLAGS) -o $(BINDIR)/$@ $^

strided_bw_tests.o: caf_tests.F90
	$(COMPILE_CMD) -c $(FFLAGS) $(STRIDED_BW_TESTS) $< -o $@

run_reduce_tests: reduce_tests
	$(LAUNCHER) $(BINDIR)/$^ $(EXEC_OPTIONS)

reduce_tests: reduce_tests.o wtime.o
	$(COMPILE_CMD) $(LDFLAGS) -o $(BINDIR)/$@ $^

reduce_tests.o: caf_tests.F90
	$(COMPILE_CMD) -c $(FFLAGS) $(REDUCE_TESTS) $< -o $@


.PHONY:clean
clean:
	rm -rf *.o $(BINDIR)/*_tests *.mod
