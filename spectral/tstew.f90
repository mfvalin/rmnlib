program tstew

integer, parameter :: ilev=1
integer, parameter :: lm=30
integer, parameter :: ilg=64

real w(2*ilg*ilev)
real w2(2*ilg*ilev)
real w3(2*ilg*ilev)

do i =1,2*ilg*ilev
  w(i) = .1 *i
enddo
w3 = w
w2 = w
ndiff = 0
do i =1,2*ilg*ilev
  if (w3(i) .ne. w(i)) ndiff = ndiff +1
enddo
print *,'ndiff=',ndiff

call ewfdr2(w2,w,w,ilg,ilev,lm,1.0,0)
ndiff = 0
do i =1,2*ilg*ilev
  if (w3(i) .ne. w(i)) ndiff = ndiff +1
enddo
print *,'ndiff=',ndiff
call ewfdr2(w,w,w,ilg,ilev,lm,1.0,0)

ndiff = 0
do i =1,2*ilg*ilev
  if (w2(i) .ne. w(i)) then
    ndiff = ndiff +1
    print *, 'i=',i
  endif
enddo
print *,'ndiff=',ndiff
end

