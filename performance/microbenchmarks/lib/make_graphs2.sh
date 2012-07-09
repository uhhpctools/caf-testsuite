#!/bin/bash

# plot styles
style[0]=" lc rgb \"\#0000CD\" pt 1 "
style[1]=" lc rgb \"\#A52A2A\" pt 2 "
style[2]=" lc rgb \"\#5F9EA0\" pt 3 "
style[3]=" lc rgb \"\#00FFFF\" pt 4 "
style[4]=" lc rgb \"\#00008B\" pt 5 "
style[5]=" lc rgb \"\#B8860B\" pt 6 "
style[6]=" lc rgb \"\#006400\" pt 7 "
style[7]=" lc rgb \"\#8B008B\" pt 8 "
style[8]=" lc rgb \"\#9400D3\" pt 9 "
style[9]=" lc rgb \"\#008000\" pt 10 "
style[10]=" lc rgb \"\#7CFC00\" pt 11 "
style[11]=" lc rgb \"\#FF1493\" pt 12 "
style[12]=" lc rgb \"\#BA55D3\" pt 13 "

j=0
i=0
NB=100000
NB_tmp=0

if [ "$#" = 0 ]; then
  echo "specify the outputs you want to plot"
  exit
fi

for output in "$@"; do
    outputs[i]=$output
    results_list[i]="$(ls -1 $DATA_PATH/${outputs[i]} --hide=save)"
    NB_tmp=$(ls -1 $DATA_PATH/${outputs[i]} --hide=save | wc -l)
    if [ "$NB_tmp" -lt "$NB" ]; then
      NB=$NB_tmp
      j=$i
    fi
    i=$((i+1))
done 

k=1
while [ $k -le $NB ]; do
  i=0
  while [ $i -lt $# ]; do
    file[i]=$(echo ${results_list[i]} | cut -d " " -f$k)
    i=$((i+1))
  done 
  BENCH=${file[j]%%_*}
  NCNP=${file[j]#*${CLUSTER}_}; NCNP=${NCNP%.dat*}
  TITLE=${NCNP%_*}"/"${NCNP#*_}
  OUTPUT=${file[j]#${BENCH}_}; OUTPUT=${OUTPUT%%_${CLUSTER}_*}

  if test "$BENCH" = "bidirectional" -o "$BENCH" = "get-bandwidth" -o \
          "$BENCH" = "put-bandwidth"; then
       # Curves (bandwidth)
       rm -f curve_templates/${BENCH}_${NCNP}.gp
       echo "create plot for $BENCH ..."
       sed -e "s:TITLE:(${TITLE}):g" \
         -e "s:OUTPUT:${DIAGS_PATH}/${BENCH}_Curve_Bandwidth_${NCNP}.eps:g" \
         -e "s:YLABEL:\"Bandwidth [KB/sec]\":g" \
           < curve_templates/${BENCH}_template.gp \
         > curve_templates/${BENCH}_${NCNP}.gp

       i=0
       plotinfo="plot "
       while [ $i -lt $# ]; do
         out=${outputs[i]}
         data_file[i]="$DATA_PATH/${out}/${file[i]}"
         title[i]="title '${out}'"
         if [ $((i+1)) -eq $# ]; then
           plotinfo+="\"${data_file[i]}\" using 1:3 ${style[i]} ${title[i]} with linespoints"
         else
           plotinfo+="\"${data_file[i]}\" using 1:3 ${style[i]} ${title[i]} with linespoints,"
         fi
         i=$((i+1))
       done 

       echo $plotinfo >> curve_templates/${BENCH}_${NCNP}.gp
       gnuplot < curve_templates/${BENCH}_${NCNP}.gp

       # Curves (time)
  fi

  k=$((k+1))
done
