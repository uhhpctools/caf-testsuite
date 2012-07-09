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

$FC $FFLAGS $FFLAGS_VALIDATION_DEFS -o  ../bin/$TEST cross_test_helper.o $SOURCE &> $COMPILE_OUTPUT/$SOURCE.out
ANS="$?"
if [ "$ANS" == "0" ]; then
     printf '%s\t' "PASS" | tee -a $LOGFILE
   # run the feature test
   perl ../../support/timedexec.pl $TIMEOUT $LAUNCHER ../bin/$TEST $EXEC_OPTIONS &>$EXEC_OUTPUT/$TEST.out
   RETURN="$?"
   ../../support/kill_orphan_procs.sh $TEST
   # check if the feature test passed execution
   if [ "$?" == "0" ]; then
       # feature test passed execution
       printf '%15s\t' "PASS"  | tee -a $LOGFILE

       # compile the cross test
       $FC $FFLAGS $FFLAGS_VALIDATION_DEFS -DCROSS_  -o  ../bin/$TEST.cross cross_test_helper.o $SOURCE &>/dev/null

       # run the cross test
       perl ../../support/timedexec.pl $TIMEOUT $LAUNCHER ../bin/$TEST.cross $EXEC_OPTIONS &>./tmp
       RETURN_CROSS="$?"
       ../../support/kill_orphan_procs.sh $TEST.cross
       if [ "$RETURN_CROSS" == "4" ]; then
	   CROSS_CHK_RESULT="TIMEOUT"
       else
           touch ./tmp ./conf.temp
           if [ "$FC" == "g95" ]; then
            	CROSS_CHK_RESULT="`sed -n 's/.*(//;s/).*//p' ./tmp`%"
           else
       		CROSS_CHK_RESULT="`cat conf.temp`"
           fi
       fi
       printf '%20s\n' "${CROSS_CHK_RESULT}" | tee -a $LOGFILE

   elif [ "$RESULT" == "4" ]; then
       # feature test timed out
       printf '%15s\t%s\n' "TIMEOUT" "--"  | tee -a $LOGFILE
   else
       # feature test failed execution
       printf '%15s\t%s\n' "FAIL" "--"  | tee -a $LOGFILE
   fi

else
   # feature test failed compilation
   printf '%s\t%15s\t%s\n' "FAIL" "N/A" "--"  | tee -a $LOGFILE

fi

rm -rf ./tmp ./conf.temp
