SHELL := /bin/bash

COMPILER ?= uhcaf
TEMP_1:=$(shell cd ../../config; ./config2makedef.sh; cd -)  # generate make.def from CONFIG
include ../../config/make-validation.def #auto-generated
include ../../config/make-compiler.$(COMPILER).def #auto-generated

BIN_PATH:=../bin/
EXEC_OUTPUT:=./
COMPILE_OUTPUT:=./

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
	   @printf '\n%s\n%s\n' "-------------------- CAF NONCONFORMANCE TESTS ----------------------"  \
	   				"Evaluating $(COMPILER)"
	   @printf '%-20s\t%-80s\t%s\t%s\n\n' "SPEC_IDX" "DESCRIPTION" "COMPILATION" "EXECUTION" | tee -a $(LOGFILE)


tail:
	@ printf '\n%s%d\n\n' "TOTAL TESTS=" "`echo $(total_cnt)`" | tee -a $(LOGFILE)
#	@ if [ "$(EMAIL)" == "yes" ]; then \
#		 mutt -s "$(SUBJ)" -a $(LOGFILE) $(EMAIL_LIST) < $(EMAIL_BODY);\
#	  fi



%.x: %.f90
	   $(eval SPEC_IDX:=$(shell echo "`echo "$^"|sed 's/.f90//'| sed 's/*_//'`"))
	   $(eval DESCRIPTION:=$(shell echo "`cat description | grep "$^" | sed s/"$^"//`"))
	   @ printf '%-20s\t%-80s\t' "$(SPEC_IDX)" "$(DESCRIPTION)" | tee -a $(LOGFILE)
	   @ $(FC) $(FFLAGS) $(FFLAGS_VALIDATION_DEFS)  -o ../$@ $^ 2>$(COMPILE_OUTPUT)/$^.out ;\
	     if [ "$$?" == "1" ]; then \
		     printf '%s\t\t' "PASS" | tee -a $(LOGFILE)  ; \
			 perl ../../support/timedexec.pl $(TIMEOUT) $(LAUNCHER)  $(BIN_PATH)/$@  2>$(EXEC_OUTPUT)/$@.out ; \
			 ../../support/kill_orphan_procs.sh $@
			 if [ "$$?" == "1" ]; then \
			 	printf '%s\n' "PASS" | tee -a $(LOGFILE) ;\
			 else \
			 	printf '%s\n' "FAIL" | tee -a $(LOGFILE) ;\
			 fi \
		 else \
		 	 	printf '%s\t\t%s\n' "FAIL" "N/A" | tee -a $(LOGFILE) ;\
		 fi
	    $(eval total_cnt=$(shell echo $(total_cnt)\+1 | bc))


clean_output:
	    @rm -rf compile_output/*.out exec_output/*.out


clean_exec:
	    @rm  -rf  $(BIN_PATH)/*.x   ./*.mod


clean_history:
	    @rm -rf ./HISTORY/*.log


cleanall:  clean_output clean_exec clean_history
	    @rm -rf uhcaf.keep ./*.log


clean:
	@ printf '%s\n%s\n' "Usage:" "$ $(MAKE) clean-<output | exec | history | all>"
