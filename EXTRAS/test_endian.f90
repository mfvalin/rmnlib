program endian
integer, external :: is_little_endian, is_big_endian
if(is_little_endian()==1) then
print *,'this machine is little endian'
else
print *,'this machine is not little endian'
endif
if(is_big_endian()==1) then
 print *,'this machine is big endian'
else
 print *,'this machine is not big endian'
endif
stop
end
