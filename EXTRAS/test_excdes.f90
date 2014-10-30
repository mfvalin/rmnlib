program test_excdes
use iso_c_binding
implicit none
include 'excdes.inc'
external :: c_main
call fmain2cmain(c_main)
call dump_request_table()
stop
end
