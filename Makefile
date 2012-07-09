SHELL := /bin/bash
PERFORMANCE_PATH=./performance
VALID_PATH=./validation

TEMP=$(shell cd config; ./config2makedef.sh)

.PHONY: help performance validation


all help default:
	@printf '\n\n%10s' "==========================================="
	@printf '\n%10s\n' "CAF Validation and Performance Test Suites"
	@printf '%10s\n\n' "==========================================="
	@printf '%s\n\n%s\n\n' "If the tests are executed from this root directory of the test suite, then:" "USAGE: $(MAKE) performance|validation|clean PARAMS=<OPTIONS> [COMPILER=uhcaf(default)|ifort|g95|crayftn]"
	@printf '%s\n' "Examples:"
	@printf '%s\n' "  make validation                                       # to see options for running validation tests"
	@printf '%s\n' "  make validation PARAMS=all COMPILER=uhcaf             # run all the validation tests"
	@printf '%s\n' "  make performance                                      # to see options for running performance tests"
	@printf '%s\n' "  make performance PARAMS=complete_all COMPILER=uhcaf   # run all the performance tests"
	@printf '\n%s\n' "Alternatively, you may run the validation and performances tests from the validation and performance directories, respectively."


performance:
	@cd $(PERFORMANCE_PATH); $(MAKE) -s $(PARAMS)


validation:
	@cd $(VALID_PATH); $(MAKE) -s $(PARAMS)


clean cleanall: performance-clean validation-clean
	@rm -rf config/make-compiler.* config/make.def config/make-validation.def config/make-npb.def


performance-clean:
	@cd $(PERFORMANCE_PATH); $(MAKE) -s cleanall


validation-clean:
	@cd $(VALID_PATH); $(MAKE) -s cleanall

