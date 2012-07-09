SHELL := /bin/bash

COMPILER ?= uhcaf
TEMP_1:=$(shell cd ../../config; ./config2makedef.sh; cd -)  # generate make.def from CONFIG
include ../../config/make-validation.def #auto-generated
include ../../config/make-compiler.$(COMPILER).def #auto-generated


DATE:=$(shell echo "`date +"%m-%d-%y-%T"`")

LOG_NAME:=$(DATE).log
LOGFILE:=$(LOG_NAME)
SCRIPTS = crosschecked_feature_test.sh feature_test.sh status_test.sh
CROSS_TEST_HELPER_SRC = ../cross_test_helper.f90


all: clean_output clean_exec test_file  cross_test_helper.o description $(SCRIPTS) header
	   @source few_test.sh $(LOGFILE) $(COMPILER)
	   @cp $(LOGFILE) ./HISTORY/
	   @mv $(LOGFILE) ./latest_results.log
	   @rm $(SCRIPTS) description


#copying scripts from other folders to this one (after modifying the var names).
$(SCRIPTS):
	   @ cp ../crosschecked_feature_tests/crosschecked_feature_test.sh .
	   @ cp ../status_tests/status_test.sh .
	   @ cp ../feature_tests/feature_test.sh .



header:
	   @printf '\n%s\n\n%s\n\n' "--------------- CAF SPECIFIC testsuite ---------------" \
			            "Evaluating $(COMPILER)" | tee -a $(LOGFILE)
	   @printf '%-20s\t%-70s\t%s\t%s\n\n' "SPEC_IDX" "DESCRIPTION" "COMPILATION" "EXECUTION"

cross_test_helper.o: $(CROSS_TEST_HELPER_SRC)
	   @$(FC) $(FFLAGS) -c $(CROSS_TEST_HELPER_SRC)


description:
	   @cat ../crosschecked_feature_tests/description >>./description
	   @cat ../status_tests/description >>./description
	   @cat ../feature_tests/description >>./description


clean_output:
	   @rm -rf compile_output/*.out exec_output/*.out


clean_exec:
	   @rm  -rf  ../bin/*.x   ./*.x  ./*.mod ./*.o


clean_history:
	   @rm -rf ./HISTORY/*.log


cleanall:  clean_output clean_exec clean_history
	   @rm -rf uhcaf.keep ./*.log ./*.o ./*.f90  $(SCRIPTS) description
