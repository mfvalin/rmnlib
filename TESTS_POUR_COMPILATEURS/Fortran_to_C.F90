program demo
implicit none
integer set_level, rpn_fortran_callback
external set_level,rpn_fortran_callback

integer p1a,p1b,istat,j,Lun_out
p1a =  10
p1b = -10
istat= rpn_fortran_callback('levels'  ,set_level,' ',p1a,p1b)

stop
end
integer function set_level ()
return
end function set_level
