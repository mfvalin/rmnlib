program test_float_packer
  implicit none
  integer, parameter :: NDATA=500000
  real(kind=4), dimension(NDATA) :: original, unpacked
  real(kind=8), dimension(NDATA) :: delta
  integer, dimension(4) :: header
  integer, dimension(NDATA) :: stream
  integer :: i, nbits
  real(kind=4) :: errmax
  integer(kind=8), external :: f_gettimeofday
  integer(kind=8) :: t1, t2, t3
  integer :: time1, time2, tm1, tm2

  errmax = 1.0 / (2.0 ** 16)

!  do i=1,NDATA
!    original(i) = i *.001
!  enddo
  call random_number(original)
  original=max(original,NDATA*.000001)
  original(NDATA) = 1.0
!  original = -original
  time1 = 1000000
  time2 = 1000000
  do i=0,32
    original(1) = i * .000001
    t1 = f_gettimeofday()
    call float_packer(original,16,header,stream,NDATA)
    t2 = f_gettimeofday()
    call float_unpacker(unpacked,header,stream,NDATA,nbits)
    t3 = f_gettimeofday()
    tm1 = t2 - t1
    tm2 = t3 - t2
    time1 = min(time1,tm1)
    time2 = min(time2,tm2)
!    unpacked(NDATA) = 1.0
    delta = (unpacked-original)
    print 101,minval(original),maxval(original),minval(unpacked),maxval(unpacked), & ! 1.0-maxval(unpacked), &
              minval(delta),maxval(delta),errmax,sum(delta)/NDATA,sum(abs(delta))/NDATA
101 format(10F15.9,2F12.7)
  enddo
  print *,'pack time =',time1
  print *,'unpack time =',time2
stop
end program
