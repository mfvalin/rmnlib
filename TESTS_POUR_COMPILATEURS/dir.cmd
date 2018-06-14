#!/usr/bin/env gnuplot
#set terminal wxt size 1280,1024 enhanced font 'Verdana,10' persist
set terminal png size 1600,1200
set output 'jitter.png'

set title "jitter map, x-axis (timestep), y-axis(core)"
unset key
set tic scale 0

# Color runs from white to green
#set palette rgbformula -7,2,-7
#set palette color
#set palette defined ( 0 "white", 3 "green" , 5 "red" )
#set palette defined ( 0 "white", 100 "dark-red" )
#set palette defined ( 0 "light-blue", 1 "cyan", 2 "green" , 4 "yellow", 8 "orange" , 12 "magenta" , 16 "red" , 99 "dark-red" )
set palette defined ( 0 "light-blue", 5 "green" , 7 "yellow", 8 "red" , 99 "dark-red" )

set cbrange [0:100]
set cblabel "%jitter"
#unset cbtics

#set xrange [-0.5:4.5]
set xrange [1:300]
set yrange [1:3600]

set view map
#set pm3d map
#set pm3d interpolate 2,2
#set pm3d interpolate 0,0

splot 'data.txt' matrix with image
#splot 'data.txt' matrix

