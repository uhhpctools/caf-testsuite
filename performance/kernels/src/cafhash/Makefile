SHELL := /bin/bash

CC=cc

TEST = cafhash

COMPILER ?= uhcaf

TEMP_1:=$(shell cd ../../../../config; ./config2makedef.sh; cd -)  # generate make.def from CONFIG

include ./$(TEST).def # user-defined
include ../../../../config/make.def   # auto-generated
include ../../../../config/make-compiler.$(COMPILER).def   # auto-generated

default: clean print_test compile execute

compile $(TEST).x: $(TEST).F90 hashobj.o
	@echo -e "\nCompiling ... $(COMPILE_CMD) -DNHASH=${NHASH} $(TEST).f90 hashobj.o -o $(TEST).x"
	@-$(COMPILE_CMD) -DNHASH=${NHASH} $(TEST).F90 hashobj.o -o $(TEST).x | tee $(TEST).compile.out

hashobj.o: hashobj.c
	$(CC) $(CFLAGS) -DNHASH=${NHASH} -c hashobj.c

execute: $(TEST).x
	@if [ -f $(TEST).x ]; then \
		echo -e "\nExecuting ... $(TEST).x ... with $(NPROCS) images "; \
		if [ "$(COMPILER)" == "ifort" ]; then export FOR_COARRAY_NUM_IMAGES=$(NPROCS); fi; \
		perl ../../../../support/timedexec.pl $(TIMEOUT) "$(LAUNCHER) ./$(TEST).x $(EXEC_OPTIONS)"  | tee $(TEST).exec.out; \
		../../../../support/kill_orphan_procs.sh  $(TEST).x; \
	 fi

print_test:
	@echo -e "\n--------------------------"
	@echo -e "TESTING $(TEST)"
	@echo -e "---------------------------\n"

clean:
	rm -rf *.x *.out *.mod *.compile *.o
