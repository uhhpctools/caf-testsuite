SHELL = bash
COMPILER ?= uhcaf
TYPE ?= ALL

help default: ./test_microbenchmarks.sh
	@echo -e "\nUsage: make [ compile|execute|complete|clean|cleanall [COMPILER=uhcaf(default)|ifort|g95] [TYPE=<test-name>] ]\n"
	@echo -e "Where, <test-name> is  bidirectional|broadcastget_bandwidth|get_latency||noncontiguous|partial_data|ping_pong|put_bandwidth|put_latency|reduce"
	@echo " Note: make cleanall : cleans all compilation, execution, and testsuite logs "
	@echo -e "Please ensure:\n The test_suite specific parameters are set in ../../config/CONFIG \n The compiler specific parameters in ../../config/CONFIG-compiler.<compiler> \n"
	@echo -e "The results of all the microbenchmarks are stored in plottable format in the configured output directory \n"

compile: ./test_microbenchmarks.sh
	@./test_microbenchmarks.sh compile $(COMPILER) $(TYPE)

execute: ./test_microbenchmarks.sh
	@./test_microbenchmarks.sh execute $(COMPILER) $(TYPE)

complete all: ./test_microbenchmarks.sh
	@./test_microbenchmarks.sh  complete $(COMPILER) $(TYPE)

.PHONY: clean cleanall

cleanall:  ./test_microbenchmarks.sh
	@./test_microbenchmarks.sh  cleanall

clean:  ./test_microbenchmarks.sh
	@./test_microbenchmarks.sh  clean
