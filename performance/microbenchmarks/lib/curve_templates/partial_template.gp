set terminal postscript eps enhanced color solid
set output "OUTPUT"

set border 3 front linetype -1 linewidth 1.000
set boxwidth 0.95 absolute
set style fill   solid 1.00 noborder
set grid nopolar
set grid noxtics nomxtics ytics nomytics noztics nomztics nox2tics nomx2tics noy2tics nomy2tics nocbtics nomcbtics
set grid layerdefault  linetype 0 linewidth 1.000,  linetype 0 linewidth 1.000

set key left nobox

set datafile missing '-'

set xtics border in scale 1,0.5 nomirror rotate by -45  offset character 0, 0, 0 
set xtics ("8B" 8, "16B" 16, "32B" 32, "64B" 64, "128B" 128, "256B" 256, "512B" 512, "1KB" 1024, "2KB" 2048, "4KB" 4096, "8KB" 8192, "16KB" 16384, "32KB" 32768)
set ytics border in scale 0,0 mirror norotate  offset character 0, 0, 0 autofreq 

set title "Partial Data TITLE"
set ylabel YLABEL
set xlabel "Data Size"

set logscale x 2
#set logscale y 2
