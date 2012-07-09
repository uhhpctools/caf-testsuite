SHELL=bash
SRCDIR=./src
BINDIR=./bin

compiler ?= uhcaf
NPROCS ?= 4

RUN_TESTS = run_all_tests run_latency_tests run_bw_tests run_strided_bw_tests run_reduce_tests

TESTS = all_tests latency_tests bw_tests strided_bw_tests reduce_tests

$(RUN_TESTS) $(TESTS):
	if [ ! -d "$(BINDIR)" ]; then \
		mkdir $(BINDIR); \
	fi
	@make -C $(SRCDIR) $@ compiler=$(compiler) NPROCS=$(NPROCS)

help:
	@echo -e "Usage: make [ run_<tests> | <tests>  [ compiler=<name>] [NPROCS=<num-imgs>] ]"
	@echo -e "     : make clean"
	@echo -e "Where: tests =  all_tests        | "
	@echo -e "                latency_tests    | "
	@echo -e "                bw_tests         | "
	@echo -e "                strided_bw_tests | "
	@echo -e "                reduce_tests"


.PHONY:clean
clean:
	@make -C $(SRCDIR) clean
