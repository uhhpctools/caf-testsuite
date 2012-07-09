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

$FC $FFLAGS $FFLAGS_VALIDATION_DEFS  -o  ../bin/$TEST $SOURCE &> $COMPILE_OUTPUT/$SOURCE.out
if [ "$?" == "0" ]; then
     printf '%s\t\t' "PASS" | tee -a $LOGFILE
     perl ../../support/timedexec.pl $TIMEOUT $LAUNCHER  ../bin/$TEST $EXEC_OPTIONS &>$EXEC_OUTPUT/$TEST.out
     ../../support/kill_orphan_procs.sh $TEST
     ANS="$?"
     if [ "$ANS" == "0" ]; then
	 	printf '%s\n' "PASS" | tee -a $LOGFILE
     elif [ "$ANS" == "4"  ]; then
    	        printf '%s\n' "TIMEOUT" | tee -a $LOGFILE
       		#killprocs $1
     else
	 	printf '%s\n' "FAIL" | tee -a $LOGFILE
     fi
else
     printf '%s\t\t%s\n' "FAIL" "N/A" | tee -a $LOGFILE
fi

