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
set xtics   ("4B" 0.00000, "8B" 1.00000, "16B" 2.00000, "32B" 3.00000, "64B" 4.00000, "128B" 5.00000, "256B" 6.00000, "512B" 7.00000, "1KB" 8.00000, "2KB" 9.0000, "4KB" 10.0000, "8KB" 11.0000, "16KB" 12.0000, "32KB" 13.0000, "64KB" 14.0000, "128KB" 15.0000, "256KB" 16.0000, "512KB" 17.0000, "1MB" 18.0000, "2MB" 19.0000, "4MB" 20.0000, "8MB" 21.0000, "16MB" 22.0000, "32MB" 23.0000, "64MB" 24,0000, "128MB" 25,0000, "256MB" 26,0000)
set ytics border in scale 0,0 mirror norotate  offset character 0, 0, 0 autofreq 

set title "Latency ratio relatively to MPI using TITLE"
set xlabel "Data Size"
set ylabel YLABEL

#set logscale y 2
set yrange [ * : * ] noreverse nowriteback
