#!/bin/bash

if [ -f ../../config/CONFIG ]; then
  source ../../config/CONFIG
else
  echo "CONFIG file missing. Please ensure that CONFIG file is present under ${ROOT}/../../config/"
fi

#specify NITER parameter for microbenchmarks
FFLAGS_BENCH_DEFS="-DNITER=${NITER}"

if [ "$1" == "cleanall" ]; then
    rm -rf $LOG_DIR
    rm -rf $TESTS_DIR/*.mod $BIN_DIR
    exit 0
elif [ "$1" == "clean" ]; then
    rm -rf $TESTS_DIR/*.mod $BIN_DIR
    exit 0
fi
cd $TESTS_DIR ;

if [ $# == 3 ]; then
  if [ "$1" == "compile" ]; then
    COMPILE_TESTS="1"
    EXECUTE_TESTS="0"
    compiler=$2
  elif [ "$1" == "execute" ]; then
    COMPILE_TESTS="0"
    EXECUTE_TESTS="1"
    compiler=$2
  elif [ "$1" == "complete" ]; then
    BOTH="1"
    compiler=$2
  else
    echo "USAGE: ./test_microbenchmarks.sh <mode> <compiler> <file> "
    echo "           mode     = compile|execute|complete"
    echo "           compiler = uhcaf|ifort|g95"
    echo "           file = <file-name>|ALL"
    echo -e "Please ensure:\n The test_suite specific parameters are set in ${BENCH_PATH}/../../config/CONFIG \n The compiler specific parameters in ${BENCH_PATH}/../../config/CONFIG-compiler.<compiler> \n"
    echo "The results of all the microbenchmarks are stored in a plottable format in $EXEC_OUT_DIR"
    exit 1
  fi
  if [ "$3" != "ALL" ]; then
  	if [ -f $3.f90 ]; then
		FILE_LIST="$3.f90"
		echo "file found"
	else
	        echo "File  $3.f90 not found !"
		exit 1
	fi
  else
  	FILE_LIST="`ls *.f90`"
  fi
else
    echo "USAGE: ./test_microbenchmarks.sh <mode> <compiler> <file> "
    echo "           mode     = compile|execute|complete"
    echo "           compiler = uhcaf|ifort|g95"
    echo "           file = <file-name>|ALL"
    echo -e "Please ensure:\n The test_suite specific parameters are set in ${BENCH_PATH}/../../config/CONFIG \n The compiler specific parameters in ${BENCH_PATH}/../../config/CONFIG-compiler.<compiler> \n"
    echo "The results of all the microbenchmarks are stored in a plottable format in $EXEC_OUT_DIR"
    exit 1
fi

if [ ! -f ${BENCH_PATH}/../../config/CONFIG-compiler.${compiler} ]; then
  echo "../../config/CONFIG-compiler.${compiler} file missing. Please ensure that the compiler specific file is present under ${ROOT}/../../config/"
  exit 1
fi

# delete past results and make folders if needed
rm -rf $COMP_OUT_DIR $EXEC_OUT_DIR 
mkdir -p $COMP_OUT_DIR $EXEC_OUT_DIR  $HISTORY_OUT_DIR $BIN_DIR $LOG_DIR

$CC -c rtc.c -o $BIN_DIR/rtc.o -D$TIMER_ARCH

if [ "$COMPILE_TESTS" -eq "1" -o "$BOTH" -eq "1" ]; then
 for file in $FILE_LIST ; do
       NP=2
       NPROCS=$NP
       source ${BENCH_PATH}/../../config/CONFIG-compiler.${compiler}
       type=`echo $file | awk -F"/" '{print $NF}'`
       opfile=$type.x
       logfile=$DATE.log
        echo "$COMPILE_CMD  $type $BIN_DIR/rtc.o -o $BIN_DIR/$opfile >>$COMP_OUT_DIR/$opfile.compile"
        COMPILE_OUT=`$COMPILE_CMD  $type $BIN_DIR/rtc.o -o $BIN_DIR/$opfile >>$COMP_OUT_DIR/$opfile.compile 2>&1 && echo 1 || echo -1`
        if [ "$COMPILE_OUT" -eq "1" ]; then
          COMPILE_STATUS="PASS"
        else
          COMPILE_STATUS="FAIL"
        fi
       printf 'Test: %s\tCompilation: %s\n' "$type" "$COMPILE_STATUS" | tee -a $LOG_DIR/$logfile
 done
fi

if [ "$EXECUTE_TESTS" -eq "1" -o "$BOTH" -eq "1" ]; then           #execution enabled
 for file in $FILE_LIST ; do
       NP=2
       NPROCS=$NP
       source ${BENCH_PATH}/../../config/CONFIG-compiler.${compiler}
       type=`echo $file | awk -F"/" '{print $NF}'`
       opfile=$type.x
       logfile=$DATE.log

       if [ -f  $BIN_DIR/$opfile ]; then  #compilation passed
	        # While using the Intel compiler, the number of images 
	        # to be launched for an executable is determined by:
	        # (a) passing -coarray-num-images=<img-cnt> to ifort
	        # (b) setting FOR_COARRAY_NUM_IMAGES=<img-cnt> in the env. [overrides flag above]
	        # The following construct dynamically sets the number of images to override 
	        # any settings at CONFIG-ifort
	        if [ "$compiler" == "ifort" ]; then
	               export FOR_COARRAY_NUM_IMAGES=$NP
	        fi
                EXEC_OUT=` perl $ROOT/../../support/timedexec.pl $TIMEOUT "$LAUNCHER $BIN_DIR/$opfile $EXEC_OPTIONS  "  &> $EXEC_OUT_DIR/$opfile.exec  && echo 1||echo -1`
                $ROOT/../../support/kill_orphan_procs.sh $opfile

                if [ "$EXEC_OUT" == "-1" ]; then                         #runtime error
                    EXEC_STATUS="RUNTIME ERROR"
          	    FAILED_COUNT=$(($FAILED_COUNT+1))
                else                                                      #execution completed cleanly
                    EXEC_STATUS="PASS"
          	    PASSED_COUNT=$(($PASSED_COUNT+1))
                fi
            else
                EXEC_STATUS="NO BINARY"                                   #compilation passed
       fi
       echo "--------------------------------------"
       printf 'Test: %s\tNumber of Images: %s\tExecution Status: %s\n' "$type" "$NP" "$EXEC_STATUS" | tee -a $LOG_DIR/$logfile
       cat $EXEC_OUT_DIR/$opfile.exec  2>/dev/null
 done
echo "============================= EXECUTION STATISTICS =========================" | tee -a $LOG_DIR/$logfile
echo "TOTAL PASSED = $PASSED_COUNT TOTAL FAILED = $FAILED_COUNT"  | tee -a $LOG_DIR/$logfile
echo "Results of this performance run can be found in: $LOG_DIR/$logfile"
echo "The results of all the microbenchmarks are stored in plottable format in $EXEC_OUT_DIR"
fi

# backing up results to HISTORY folder
cp -r  $COMP_OUT_DIR  $HISTORY_OUT_DIR/compile_$DATE
cp -r  $EXEC_OUT_DIR  $HISTORY_OUT_DIR/execute_$DATE

cd ..;

