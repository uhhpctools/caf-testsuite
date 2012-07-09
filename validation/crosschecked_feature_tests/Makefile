SHELL := /bin/bash

COMPILER ?= uhcaf
TEMP_1:=$(shell cd ../../config; ./config2makedef.sh; cd -)  # generate make.def from CONFIG
include ../../config/make-validation.def #auto-generated
include ../../config/make-compiler.$(COMPILER).def #auto-generated

DATE:=$(shell echo "`date +"%m-%d-%y-%T"`")

LOG_NAME:=$(DATE).log
LOGFILE:=$(LOG_NAME)


F_TESTS = $(wildcard *.f90)
F_EXES  = $(F_TESTS:.f90=.x)
EXES    = $(F_EXES)
total_cnt:=0

.PHONY: all header

all:  clean_output clean_exec header cross_test_helper.o  $(EXES)
	   @cp $(LOGFILE) ./HISTORY/
	   @mv $(LOGFILE) ./latest_results.log


.SUFFIXES: .x


header:
	   @printf '\n%s\n%s\n' "----------- CAF CROSS-CHECKED FEATURE TESTS --------------"  \
                                 "Evaluating $(COMPILER)"
	   @ printf '%-18s\t%-60s\t%s\t%s\t%s\n\n' "SPEC_IDX" "DESCRIPTION" "COMPILATION" "EXECUTION"  "CROSS CHECK" | tee -a $(LOGFILE)


#$(BIN_PATH)/%.x: %.f90
%.x: %.f90
	   $(eval SPEC_IDX:=$(shell echo "`echo "$^"|sed 's/.f90//'| sed 's/*_//'`"))
	   $(eval DESCRIPTION:=$(shell echo "`cat description | grep "$^" | sed s/"$^"//`"))
	   @printf '%-18s\t%-60s\t' "$(SPEC_IDX)" "$(DESCRIPTION)" | tee -a $(LOGFILE)
	   @./crosschecked_feature_test.sh "$@"  "$^" $(LOGFILE) $(COMPILER)
	   $(eval total_cnt=$(shell echo $(total_cnt)\+1 | bc))


cross_test_helper.o: ../cross_test_helper.f90
	@ $(FC) $(FFLAGS) $(FFLAGS_VALIDATION_DEFS) -c  ../cross_test_helper.f90  2>/dev/null;\
	  if [ "$$?" -ne "0" ]; then \
	        echo "$(FC) $(FFLAGS) $(FFLAGS_VALIDATION_DEFS) -c  ../cross_test_helper.f90  2>/dev/null;" \
	  	echo "NOTE***Error while compiling cross_test_helper.f90" ;\
	  fi


clean_output:
	    @rm -rf compile_output/*.out exec_output/*.out


clean_exec:
	    @rm  -rf  ../bin/*.x*  ./*.mod ./*.o


clean_history:
	    @rm -rf ./HISTORY/*.log


cleanall:  clean_output clean_exec clean_history
	    @rm -rf uhcaf.keep ./*.log  cross_test_helper.o ./*.mod ./tmp


clean:
	@ printf '%s\n%s\n' "Usage:" "$ $(MAKE) clean<_output | _exec | _history | all>"

