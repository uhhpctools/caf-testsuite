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


all:   clean_output clean_exec header $(EXES) tail
	   @cp $(LOGFILE) ./HISTORY/
	   @mv $(LOGFILE) latest_results.log


.SUFFIXES: .x


header:
	 @printf '\n%s\n%s\n' "-------------------- CAF FEATURE TESTS ----------------------"  \
		 "Evaluating $(COMPILER)"
	 @printf '%-20s\t%-70s\t%s\t%s\n\n' "SPEC_IDX" "DESCRIPTION" "COMPILATION" "EXECUTION" | tee -a $(LOGFILE)


tail:
	@ printf '\n%s%d\n\n' "TOTAL TESTS=" "`echo $(total_cnt)`" | tee -a $(LOGFILE)
# Experimental : email notifications
#	@ if [ "$(EMAIL)" == "yes" ]; then \
#		 mutt -s "$(SUBJ)" -a $(LOGFILE) $(EMAIL_LIST) < $(EMAIL_BODY);\
#	  fi



%.x: %.f90
	   $(eval SPEC_IDX:=$(shell echo "`echo "$^"|sed 's/.f90//'| sed 's/*_//'`"))
	   $(eval DESCRIPTION:=$(shell echo "`cat description | grep "$^" | sed s/"$^"//`"))
	   @printf '%-20s\t%-70s\t' "$(SPEC_IDX)" "$(DESCRIPTION)" | tee -a $(LOGFILE)
	   @source ./feature_test.sh "$@" "$^" $(LOGFILE) $(COMPILER)
	   $(eval total_cnt=$(shell echo $(total_cnt)\+1 | bc))


clean_output:
	    @rm -rf compile_output/*.out exec_output/*.out


clean_exec:
	    @rm  -rf  ../bin/*.x   ./*.mod


clean_history:
	    @rm -rf ./HISTORY/*.log


cleanall:  clean_output clean_exec clean_history
	    @rm -rf uhcaf.keep ./*.log ./*.mod


clean:
	@printf '%s\n%s\n' "Usage:" "$ $(MAKE) -s clean<_output | _exec | _history | all>"

