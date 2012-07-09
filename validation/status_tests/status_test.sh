#!/bin/bash

EXEC_OUTPUT=exec_output
COMPILE_OUTPUT=compile_output

TEST=$1
SOURCE=$2
LOGFILE=$3
COMPILER=$4

config_file="../../config/CONFIG-validation"
source $config_file
config_file="../../config/CONFIG-compiler.${COMPILER}"
source $config_file

exec_out="0"

$FC $FFLAGS $FFLAGS_VALIDATION_DEFS -o ../bin/$TEST  $SOURCE &>$COMPILE_OUTPUT/$SOURCE.out
if [ "$?" == "0" ]; then
		printf '%-15s\t' "PASS"  | tee -a $LOGFILE
		for i in {1..$NITER}
		do
			perl ../../support/timedexec.pl $TIMEOUT $LAUNCHER  ../bin/$TEST $EXEC_OPTIONS &> $EXEC_OUTPUT/$SOURCE.out
			../../support/kill_orphan_procs.sh $TEST
			if [ "$?" != "0" ]; then
				exec_out="1"
				break
	   		fi
  		done
		if [ "$exec_out" != "0" ]; then
			printf '%-15s\n' "FAIL"  | tee -a $LOGFILE
		else
		    printf '%-15s\n' "PASS"  | tee -a $LOGFILE
		fi
else
		printf '%-15s\t%-15s\n' "FAIL" "N/A"  | tee -a $LOGFILE
fi


