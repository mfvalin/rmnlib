program inv
  integer :: nbits, power, izero, itop, ilow
  real :: R, rr, rrange,delta, low, high, newlow
1 read(5,*,err=2) low, high
  power = 2
  delta = 2.01
  rrange = 1234.56
  rrange = high - low
  do nbits = 2 , 16
    rrange = high - low
    power = power * 2
!   rrange = power * 19.5
    if(nbits>2) then
      rr = rrange/(power - delta)
    else
      rr = rrange/(power - 1.01)
    endif
    r = 1.0 / rr
    if(low<0 .and. high > 0) then
      izero = nint((0-low)*r)
      newlow = -rr*izero
!      print *,'newlow=',newlow
      izero = nint((0-newlow)*r)
    else
      newlow = low
      izero = 0
    endif
    ilow = nint((low-newlow)*r)
    itop = nint((high-newlow)*r)
    print *,power,rrange,rr,newlow,nint(rrange*R),power-1-rrange*R,power-itop-1,newlow + izero*rr, &
            newlow + itop*rr,itop,izero,ilow*rr+newlow
!    if(delta < 2.1) delta = delta * 1.02
  enddo
  goto 1
2 stop
end
