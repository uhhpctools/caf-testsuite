SHELL := /bin/bash


COMPILER ?= uhcaf
TEMP_1:=$(shell cd ../../config; ./config2makedef.sh; cd -)  # generate make.def from CONFIG
include ../../config/make-compiler.$(COMPILER).def   # auto-generated

TESTS = buffon_laplace cafhash cg_2d image_smoothing insertion_sort insertion_sort_dtype jacobi mm syncbench

RUN_TESTS := $(addprefix run_, $(TESTS))

default: $(TESTS)

help:
	@echo "Usage: make [KERNEL|run_KERNEL|runtests|clean] [COMPILER=uhcaf(default)|ifort|g95] "
	@echo -e "Where KERNEL is one of:\n\t $(TESTS)"
	@echo -e "Please ensure:\n The test_suite specific parameters are set in ../../config/CONFIG \n The compiler specific parameters in ../../config/CONFIG-compiler.<compiler> \n"
	@ echo -e "To set the number of images used while launching a kernel, modify the make.def within the corresponding directory  at kernels/src/KERNEL "

runtests: $(RUN_TESTS)

##################### COMPILATION ###################
$(TESTS):
	@cd src/$@; make compile TEST=$@ COMPILER=$(COMPILER)

##################### EXECUTION ###################
$(RUN_TESTS): run_%:
	@cd src/$*; make -s TEST=$* COMPILER=$(COMPILER)
	@echo -e "\n--------------------------------------\n"

clean:
	@for d in $(TESTS); do if [ -d src/$$d ]; then cd src/$$d ; make TEST=$$d clean ; cd ../.. ; fi;  done 
