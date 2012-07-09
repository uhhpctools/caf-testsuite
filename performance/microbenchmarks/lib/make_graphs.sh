#!/bin/bash

function ratio()
{

    cat $1 | cut -d";" -f1 > MSG_SIZE
    sed 1d MSG_SIZE -i # Deletes the comment in the first line of the file

    cat $1 | cut -d";" -f$4 > VALUES_I  # MPI
    cat $2 | cut -d";" -f$4 > VALUES_II # CAF

    paste VALUES_I VALUES_II > VALUES 
    sed 1d VALUES -i

    awk '{print $2/$1}' VALUES > RATIO

    paste MSG_SIZE RATIO > $3

    rm -f VALUES
    rm -f VALUES_I
    rm -f VALUES_II
    rm -f MSG_SIZE
    rm -f RATIO
}


MPI_LIST="$(ls -1 $MPI_PATH --hide=save)"
CAF_LIST="$(ls -1 $CAF_PATH --hide=save)"

NB=$(ls -1 $MPI_PATH --hide=save | wc -l)

i=1

while [ $i -le $NB ]; do 

    MPI_FILE=$(echo $MPI_LIST | cut -d " " -f$i)
    CAF_FILE=$(echo $CAF_LIST | cut -d " " -f$i)

    BENCH=${MPI_FILE%%_*}
    NCNP=${MPI_FILE#*${CLUSTER}_}; NCNP=${NCNP%.dat*}
    TITLE=${NCNP%_*}"/"${NCNP#*_}

    echo "generating graphs for $BENCH ..."

    if [ $BENCH != "latency" ] && [ $BENCH != "bandwidth" ]; then
    ## Curves
        rm -f curve_templates/${BENCH}_${NCNP}.gp
        
        sed -e "s:TITLE:(${TITLE}):g" -e "s:OUTPUT:${DIAGS_PATH}/${BENCH}_Curve_Time_${NCNP}.eps:g" -e "s:YLABEL:\"Time [Microseconds]\":g" < curve_templates/${BENCH}_template.gp > curve_templates/${BENCH}_${NCNP}.gp
        echo "plot \"${CAF_PATH}/${CAF_FILE}\" using 1:2 lc rgb \"\#0000CD\" pt 2 title 'CAF-${COMM_LAYER}' with linespoints ,\"${MPI_PATH}/${MPI_FILE}\" using 1:2 lc rgb \"\#FF0000\" pt 4 title 'MPI' with linespoints" >> curve_templates/${BENCH}_${NCNP}.gp 
        
        gnuplot < curve_templates/${BENCH}_${NCNP}.gp 


        rm -f curve_templates/${BENCH}_${NCNP}.gp
        
        sed -e "s:TITLE:(${TITLE}):g" -e "s:OUTPUT:${DIAGS_PATH}/${BENCH}_Curve_Bandwidth_${NCNP}.eps:g" -e "s:YLABEL:\"Bandwidth [KB/sec]\":g" < curve_templates/${BENCH}_template.gp > curve_templates/${BENCH}_${NCNP}.gp
        echo "plot \"${CAF_PATH}/${CAF_FILE}\" using 1:3 lc rgb \"\#FF00FF\" pt 2 title 'CAF-${COMM_LAYER}' with linespoints ,\"${MPI_PATH}/${MPI_FILE}\" using 1:3 lc rgb \"\#32CD32\" pt 4 title 'MPI' with linespoints" >> curve_templates/${BENCH}_${NCNP}.gp 
        
        gnuplot < curve_templates/${BENCH}_${NCNP}.gp 
    
    ## Charts
        rm -f chart_templates/${BENCH}_${NCNP}.gp
        
        ratio $MPI_PATH/$MPI_FILE $CAF_PATH/$CAF_FILE $DATA_PATH/ratio/${BENCH}_TIMERATIO_${NCNP}.dat 2 # EXECUTION TIME RATIO
        
        sed  -e "s:RATIO:Execution time:g" -e "s:TITLE:${TITLE}:g" -e "s:OUTPUT:${DIAGS_PATH}/${BENCH}_Chart_Time_${NCNP}.eps:g" -e "s:YLABEL:\"Time Ratio\":g" < chart_templates/${BENCH}_template.gp > chart_templates/${BENCH}_${NCNP}.gp
        echo "plot \"${DATA_PATH}/ratio/${BENCH}_TIMERATIO_${NCNP}.dat\" using 2 fs solid 1.0 lc rgb \"\#20B2AA\" " >> chart_templates/${BENCH}_${NCNP}.gp
        
        gnuplot < chart_templates/${BENCH}_${NCNP}.gp

        
        rm -f chart_templates/${BENCH}_${NCNP}.gp
        
        ratio $MPI_PATH/$MPI_FILE $CAF_PATH/$CAF_FILE $DATA_PATH/ratio/${BENCH}_BDWTHRATIO_${NCNP}.dat 3 # BANDWIDTH RATIO
        
        sed -e "s:RATIO:Bandwidth:g" -e "s:TITLE:${TITLE}:g" -e "s:OUTPUT:${DIAGS_PATH}/${BENCH}_Chart_Bandwidth_${NCNP}.eps:g" -e "s:YLABEL:\"Bandwidth Ratio\":g" < chart_templates/${BENCH}_template.gp > chart_templates/${BENCH}_${NCNP}.gp
        echo "plot \"${DATA_PATH}/ratio/${BENCH}_BDWTHRATIO_${NCNP}.dat\" using 2 fs solid 1.0 lc rgb \"\#FF6347\" " >> chart_templates/${BENCH}_${NCNP}.gp
        
        gnuplot < chart_templates/${BENCH}_${NCNP}.gp

    else

        # Curves
        rm -f curve_templates/${BENCH}_${NCNP}.gp
        
        sed -e "s:TITLE:(${TITLE}):g" -e "s:OUTPUT:${DIAGS_PATH}/${BENCH}_Curve_${NCNP}.eps:g"< curve_templates/${BENCH}_template.gp > curve_templates/${BENCH}_${NCNP}.gp
        echo "plot \"${CAF_PATH}/${CAF_FILE}\" using 1:2 lc rgb \"\#0000CD\" pt 2 title 'CAF-${COMM_LAYER}' with linespoints ,\"${MPI_PATH}/${MPI_FILE}\" using 1:2 lc rgb \"\#FF0000\" pt 4 title 'MPI' with linespoints" >> curve_templates/${BENCH}_${NCNP}.gp 
        
        gnuplot < curve_templates/${BENCH}_${NCNP}.gp 

        # Charts
        rm -f chart_templates/${BENCH}_${NCNP}.gp
        
        ratio $MPI_PATH/$MPI_FILE $CAF_PATH/$CAF_FILE $DATA_PATH/ratio/${BENCH}_${NCNP}.dat 2 # EXECUTION TIME RATIO
        
        sed  -e "s:TITLE:${TITLE}:g" -e "s:OUTPUT:${DIAGS_PATH}/${BENCH}_Chart_${NCNP}.eps:g" -e "s:YLABEL:\"Ratio\":g" < chart_templates/${BENCH}_template.gp > chart_templates/${BENCH}_${NCNP}.gp
        echo "plot \"${DATA_PATH}/ratio/${BENCH}_${NCNP}.dat\" using 2 fs solid 1.0 lc rgb \"\#20B2AA\" " >> chart_templates/${BENCH}_${NCNP}.gp
        
        gnuplot < chart_templates/${BENCH}_${NCNP}.gp   

    fi

    i=$((i+1))

done # each benchmark

exit 0
