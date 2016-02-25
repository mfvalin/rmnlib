program inv
  integer :: i, power
  real :: R, range,delta
  power = 2
  delta = .55
  range = 1234.56
  range = 4096
  do i = 2 , 16
    power = power * 2
!   range = power * 19.5
    r = range/(power - delta)
    r = 1.0 / R
    print *,power,delta,range,r,nint(range*R),power-1-range*R,power-nint(range*R)-1
    if(delta < 2) delta = delta * 1.4
  enddo
  stop
end
