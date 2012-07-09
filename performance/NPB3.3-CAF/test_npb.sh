#!/bin/bash

if [ -f config/CONFIG-npb ]; then
  source config/CONFIG-npb
else
  echo "CONFIG-npb file missing. Please ensure that CONFIG file is present under config/CONFIG-npb"
fi

COMPILE_TESTS="0"
EXECUTE_TESTS="0"
BOTH="0"
PASSED_COUNT=0
FAILED_COUNT=0
COMPILE_STATUS="UNKNOWN"
EXEC_STATUS="UNKNOWN"
EXEC_OUT="0"


if [ "$1" == "cleanall" ]; then
    rm -rf $LOG_DIR $BIN_DIR
    exit 0
fi

if [ $# == 2 ]; then
  if [ "$1" == "compile" ]; then
    COMPILE_TESTS="1"
    compiler=$2
  elif [ "$1" == "execute" ]; then
    EXECUTE_TESTS="1"
    compiler=$2
  elif [ "$1" == "complete" ]; then
    BOTH="1"
    compiler=$2
  else
  echo -e "USAGE: test_npb.sh [mode [compiler] ]\n where "
  echo "           mode     = compile|execute|complete"
  echo "           compiler = uhcaf|ifort|g95|crayftn"
    exit 1
  fi
else
  echo "USAGE: test_npb.sh [mode] [compiler] where "
  echo "           mode     = compile|execute|complete"
  echo "           compiler = uhcaf|ifort|g95|crayftn"
  echo "Please ensure: "
  echo "The test_suite specific parameters are set in ./config/CONFIG-npb. The "
  echo "compiler specific parameters are set in ./config/make.def.\$compiler."
  exit 1
fi

if [ -f ./config/make.def.${compiler} ]; then
  cp ./config/make.def.$compiler ./config/make.def
else
  echo "./config/make.def.${compiler} file missing. Please ensure that this file is present inside config"
  exit 1
fi

# delete past regression results and make folders if needed
rm -rf $COMP_OUT_DIR $EXEC_OUT_DIR $BIN_DIR
mkdir -p $COMP_OUT_DIR $EXEC_OUT_DIR  $HISTORY_OUT_DIR $BIN_DIR $LOG_DIR

printf '\n%8s %8s %8s %15s %15s %15s %25s \n' "<NAME>" "<CLASS>" "<NPROCS>" "<COMPILATION>" "<EXECUTION>" "<RESULT>" "<TIME(secs)>" | tee -a $LOG_DIR/$logfile

for BM in $BENCHMARKS
do
	for CLASS in $CLASSES
	do
  		  if [ "$BM" == "ep" -o "$BM" == "cg" -o "$BM" == "ft" -o "$BM" == "mg" -o "$BM" == "lu" ]; then
  		       NPROCS_LST=$NPROCS_LST1
  		  else
  		       NPROCS_LST=$NPROCS_LST2
  		  fi

  		  for NP in  $NPROCS_LST
  		  do
		  	NPROCS=$NP
			COMPILE_STATUS="N/A"
			EXEC_STATUS="N/A"
			VERIFICATION="N/A"
			TIME="N/A"

            # for launcher/execution settings
            if [ -f ./../../config/CONFIG-compiler.$compiler ]; then
                source ./../../config/CONFIG-compiler.$compiler
            else
                echo "CONFIG-compiler.$compiler is missing. Choose a different compiler"
            fi


  		    opfile=$BM.$CLASS.$NP
            printf '%8s %8s %8s ' "$BM" "$CLASS" "$NP"  | tee -a $LOG_DIR/$logfile

            if [ "$COMPILE_TESTS" -eq "1" -o "$BOTH" -eq "1" ]; then

                make clean &>/dev/null
                COMPILE_OUT=`make $BM NPROCS=$NP CLASS=$CLASS COMPILER=$compiler >>$COMP_OUT_DIR/$opfile.compile 2>&1 && echo 1 || echo -1`
                if [ "$COMPILE_OUT" == "1" ]; then
                    COMPILE_STATUS="PASS"
                else
                    COMPILE_STATUS="FAIL"
                fi
            fi

            printf '%15s ' "$COMPILE_STATUS"  | tee -a $LOG_DIR/$logfile

            if [ "$EXECUTE_TESTS" -eq "1" -o "$BOTH" -eq "1" ]; then           #execution enabled
                VERIFICATION="N/A"
                TIME="--"
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

                    EXEC_OUT=` perl ./../../support/timedexec.pl $TIMEOUT "$LAUNCHER $BIN_DIR/$opfile -v $EXEC_OPTIONS " &> $EXEC_OUT_DIR/$opfile.exec && echo 1||echo -1`
                    ./../../support/kill_orphan_procs.sh $opfile

                    if [ "$EXEC_OUT" == "-1" ]; then                         #runtime error
                        EXEC_STATUS="RUNTIME ERROR"
                        FAILED_COUNT=$(($FAILED_COUNT+1))
                    else                                                      #execution completed cleanly
                        EXEC_STATUS="PASS"
                        PASSED_COUNT=$(($PASSED_COUNT+1))
                        VERIFICATION=`grep "Verification"  $EXEC_OUT_DIR/$opfile.exec | grep -oh "\w*SUCCESSFUL"  `
			if [ "$VERIFICATION" == "" ]; then
                        	EXEC_STATUS="FAIL"
				VERIFICATION="CHECK-LOG"
				TIME="--"
			else
                                TIME=`grep "Time in seconds ="  $EXEC_OUT_DIR/$opfile.exec | sed 's/.*=//g' |sed 's/\s\+//g' `
			fi
                    fi
                else
                    EXEC_STATUS="NO BINARY"                                   #compilation failed
                fi
            fi
            printf '%15s %18s %15s\n' "${EXEC_STATUS}" "${VERIFICATION}" "${TIME}" | tee -a $LOG_DIR/$logfile
  		  done
	done
done

echo "\n============================= EXECUTION STATISTICS =========================\n" | tee -a $LOG_DIR/$logfile
echo "TOTAL PASSED = $PASSED_COUNT TOTAL FAILED = $FAILED_COUNT"  | tee -a $LOG_DIR/$logfile
echo "Results of this performance run can be found in: $LOG_DIR/$logfile\n"

# backing up results to HISTORY folder
cp -r  $COMP_OUT_DIR  $HISTORY_OUT_DIR/compile_$DATE
cp -r  $EXEC_OUT_DIR  $HISTORY_OUT_DIR/execute_$DATE
rm -rf ./config/make.def
