Program tstaddr
integer :: ivar,ival
integer*8 addr,addr2,get_address_from
external get_address_from

ivar = 9
ival = -888
addr = loc(ivar)
addr2 = get_address_from(ivar)

print *,'addr =',addr
print *,'addr2=',addr2

call set_content_of_location(addr2,1,22)
print *,'Apres set'
call get_content_of_location(addr2,1,ival)

print *,'ival=',ival
stop
end
