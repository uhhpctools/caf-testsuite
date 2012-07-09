#!/bin/bash

print_file_descriptor()
{
    file=$1
    file_exec="`echo $file | sed "s/.f90/.x/g" `"
    printf '%-20s\t' "`echo "$file"|sed 's/.f90//'| \
    sed 's/*_//'`" | tee -a $2
    printf '%-70s\t' "`cat description | grep "$file" | \
    sed "s/$file//" | sed 's/^[ ]*//g'`"| tee -a $2
}

LOGFILE=$1
COMPILER=$2

config_file="../../config/CONFIG-validation"
source $config_file
CURRENT=./
MAKE_CMD="make -s "

for file in `cat test_file`
do

    if [ -z "$file" ]; then # empty line!
      continue
    fi

    if [ "$file" == "FEATURE-TESTS" -o \
         "$file" == "CROSSCHECKED-FEATURE-TESTS"  -o \
         "$file" == "STATUS-TESTS"       -o \
         "$file" == "NON-CONFORMANCE-TESTS"   -o \
         "$file" == "END-TESTS" ]; then

	  if [ "$file" == "FEATURE-TESTS" ]; then
	        type="feature"
    	  elif [ "$file" == "CROSSCHECKED-FEATURE-TESTS" ]; then
               	type="crosschecked_feature"
          elif [ "$file" == "STATUS-TESTS"  ]; then
                type="status"
    	  elif [ "$file" == "NON-CONFORMANCE" ]; then
	        	type="non-conformance"
    	  elif [ "$file" == "END-TESTS" ]; then
		        printf '%s\n' "-----END OF TESTS-----" | tee -a $LOGFILE
                break
		  fi

         # cd $CURRENT
          continue

    else

#         file_exec="`echo $file | sed "s/.f90/.x/g" `"
#         printf '%-20s\t' "`echo "$file"|sed 's/.f90//'| \
#         sed 's/*_//'`" | tee -a $1
#         printf '%-70s\t' "`cat description | grep "$file" | \
#         sed "s/$file//" | sed 's/^[ ]*//g'`"| tee -a $1

        if [ "$type" == "crosschecked_feature" ]; then

            #cd ../; $MAKE_CMD crosschecked_feature-header; cd few_tests;
            print_file_descriptor $file $LOGFILE
            if [ -f ../crosschecked_feature_tests/$file ]; then
                cp crosschecked_feature_tests/$file .
                bash crosschecked_feature_test.sh "$file_exec" "$file" $LOGFILE $COMPILER
                rm ./$file
            else
                echo "ABSENT"
            fi

        elif [ "$type" == "feature" ]; then

            #cd ../; $MAKE_CMD feature-header; cd few_tests;
            print_file_descriptor $file $LOGFILE
            if [ -f ../feature_tests/$file ]; then
                cp ../feature_tests/$file .
                bash feature_test.sh "$file_exec" "$file" $LOGFILE $COMPILER
                rm ./$file
            else
                echo "ABSENT"
            fi

        elif [ "$type" == "status" ]; then

           #$MAKE_CMD header; cd few_tests;
           print_file_descriptor $file $1
           if [ -f ../status_tests/$file ]; then
               cp ../status_tests/$file .
               bash status_test.sh "$file_exec" "$file" $LOGFILE $COMPILER
               rm ./$file
           else
               echo "ABSENT"
           fi

        elif [ "$type" == "non-conformance" ]; then

            #cd ../; $MAKE_CMD non-conformance-header
            print_file_descriptor $file $LOGFILE
            echo "Not supported"

        fi
    fi
    rm -rf conf.temp tmp 
done
