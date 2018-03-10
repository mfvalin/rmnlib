#!/usr/bin/env gnuplot
set terminal wxt size 1280,1000 enhanced font 'Verdana,10' persist
#set terminal png size 600,400
#set output 'heatmaps.1.png'

set title "jitter map, x-axis (timestep), y-axis(core)"
unset key
set tic scale 0

# Color runs from white to green
#set palette rgbformula -7,2,-7
#set palette color
#set palette defined ( 0 "white", 3 "green" , 5 "red" )
set palette defined ( 0 "light-blue", 2 "cyan", 4 "green" , 8 "yellow", 16 "orange" , 32 "magenta" , 64 "red" , 99 "dark-red" )

set cbrange [0:100]
set cblabel "%jitter"
#unset cbtics

#set xrange [-0.5:4.5]
set xrange [1:300]
set yrange [1:352]

#set view map
set pm3d map
#set pm3d interpolate 2,2
set pm3d interpolate 0,0

#splot 'data.txt' matrix with image
splot 'data.txt' matrix

