SHELL := /bin/bash

.PHONY: all feature non-conformance crosschecked_feature fewtests help

COMPILER ?= uhcaf
TEMP_1:=$(shell cd ../config; ./config2makedef.sh; cd -)  # generate make.def from CONFIG
include ../config/make-compiler.$(COMPILER).def  # auto-generated

help :
	@echo -e "(If test suite is run from within validation directory, then run: $(MAKE) <OPTIONS> [COMPILER=uhcaf(default)|ifort|g95|crayftn])\n\n" "OPTIONS include:"
	@printf '%-30s\n'      " |  "
	@printf '%-30s%-90s\n' " |_ all:" "all validation tests"
	@printf '%-30s%-90s\n' " |_ fewtests:" "tests only those listed in the file "test_file" under ./few directory"
	@printf '%-30s%-90s\n' " |_ feature:" "tests the compiler's feature support (excludes tests that include verification)"
	@printf '%-30s%-90s\n' " |_ crosschecked_feature:" "performs cross-testing to test support for non-determinism handling constructs"
	@printf '%-30s%-90s\n' " |_ status:" "tests for verifying handling of status returned by certain statements"
	@printf '%-30s%-90s\n' " |_ clean<suite>:" "Cleans executables (where <suite> = fewtests|feature|crosschecked_feature|status)"
	@printf '%-30s%-90s\n' " |_ cleanall:" "Cleans everything - results, tests-history, compiler-logs, runtime-logs"
	@echo -e "\nBefore running any <suite>, please ensure that:\n The 'user-defined' parameters are set in config/CONFIG-validation \n The compiler specific parameters in config/CONFIG-compiler.<compiler> \n"


all:  feature crosschecked_feature status

feature:
	@cd feature_tests; $(MAKE) -s  all;

crosschecked_feature:
	@cd crosschecked_feature_tests; $(MAKE) -s all;

nonconformance:
	@cd nonconformance_tests; $(MAKE) -s all

status:
	@cd status_tests; $(MAKE) -s all

fewtests:
	@cd few_tests; $(MAKE) -s  all

clean:
	@ printf '%s\n' " $(MAKE) cleanall|clean_output|clean_exec|clean_history"

clean_fewtests:
	@cd few_tests; $(MAKE) -s clean_exec;

clean_feature:
	@cd feature_tests; $(MAKE) -s clean_exec;

clean_crosschecked_feature:
	@cd crosschecked_feature_tests; $(MAKE) -s clean_exec;

clean_status:
	@cd status_tests; $(MAKE) -s clean_exec;

cleanall clean_output clean_exec clean_history:
	@cd feature_tests; $(MAKE) -s $@; cd .. ;
	@cd few_tests; $(MAKE) -s $@; cd .. ;
	@cd crosschecked_feature_tests; $(MAKE) -s $@; cd .. ;
	@cd status_tests; $(MAKE) -s $@; cd .. ;
	@if [ "$@" == "cleanall" ];  then \
		rm -rf ./bin/*.* , *.mod; rm -rf ./config/make.def ; \
		rm -rf ./config/make*.def; \
		rm -rf *.log; \
	 fi;

