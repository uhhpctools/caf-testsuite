set terminal postscript eps enhanced color solid
set output "OUTPUT"

set border 3 front linetype -1 linewidth 1.000
set boxwidth 0.95 absolute
set style fill   solid 1.00 noborder
set grid nopolar
set grid noxtics nomxtics ytics nomytics noztics nomztics nox2tics nomx2tics noy2tics nomy2tics nocbtics nomcbtics
set grid layerdefault  linetype 0 linewidth 1.000,  linetype 0 linewidth 1.000

unset key
set style histogram clustered gap 3 title offset character 0, 0, 0
set datafile missing '-'
set style data histograms
set xtics border in scale 1,0.5 nomirror rotate by -45  offset character 0, 0, 0 
set xtics   ("8B" 0.00000, "16B" 1.00000, "32B" 2.00000, "64B" 3.00000, "128B" 4.00000, "256B" 5.00000, "512B" 6.00000, "1KB" 7.00000, "2KB" 8.0000, "4KB" 9.0000, "8KB" 10.0000, "16KB" 11.0000, "32KB" 12.0000)
set ytics border in scale 0,0 mirror norotate  offset character 0, 0, 0 autofreq 

set title "RATIO ratio for partial data using TITLE"
set xlabel "Data Size"
set ylabel YLABEL

#set logscale y 2
set yrange [ * : * ] noreverse nowriteback
