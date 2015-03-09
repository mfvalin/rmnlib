#!/bin/bash
compiler=${1:-gfortran}
cat <<EOT >f90_pointer_regression_0.f90
module pointers_nd

contains

function arrayptr1d(array,ni) result(p1d)
integer, intent(IN) :: ni
integer, dimension(ni), target :: array
integer, pointer, dimension(:) :: p1d
p1d=>array(1:ni)
end function arrayptr1d

function ptr1ds(ni,stride) result(p1d)
integer, intent(IN) :: ni,stride
integer, pointer, dimension(:) :: p1d
integer, pointer, dimension(:) :: p0d
integer :: i
allocate(p0d(ni))
p0d= [ (i, i=1,ni) ]
p1d=>p0d(1:ni:stride)
return
end function ptr1ds

function ptr1d(ni) result(p1d)
integer, intent(IN) :: ni
integer, pointer, dimension(:) :: p1d
integer :: i
allocate(p1d(ni))
p1d= [ (i, i=1,ni) ]
print *,'allocating array of size',ni
return
end function ptr1d

function ptr2d(ni,nj) result(p2d)
integer, intent(IN) :: ni,nj
integer, pointer, dimension(:,:) :: p2d
allocate(p2d(ni,nj))
return
end function ptr2d

function ptr3d(ni,nj,nk) result(p3d)
integer, intent(IN) :: ni,nj,nk
integer, pointer, dimension(:,:,:) :: p3d
allocate(p3d(ni,nj,nk))
return
end function ptr3d

end module pointers_nd
EOT

cat <<EOT >f90_pointer_regression_1.f90
subroutine print_ptr1d(copy_in,locarray,array,ni,i0,stride)
integer*8, intent(IN) :: locarray
integer, intent(IN) :: ni, i0, stride
integer, intent(IN), dimension(ni) :: array
logical :: same
integer :: errors, delta, i
character(len=*) :: copy_in

print *,' ------------------------------------------'
same = (locarray==loc(array))
if(locarray==loc(array))then
 copy_in=' O.K.    '
else
 copy_in=' COPY-IN,'
endif
errors=0
delta=array(2)-array(1)
do i=0,ni-1
  if(array(i+1) /= i0+i*stride) errors=errors+1
enddo
101 format(2z17.16,20i4)
!print 101,locarray,loc(array),array(1:ni)
102 format(A,A,I2,A,I2)
103 format(A,20i4)
if(copy_in/='         ' .or. errors/=0) then
  if(errors>0) then
!    print 102,'',' errors=',errors
    print 103,'expected: ',(I0+I*stride , I=0,ni-1)
    print 103,'got     : ',array(1:ni)
    if(copy_in==' O.K.    ')then
      copy_in=' ERROR'
    else
      copy_in=trim(copy_in)//'ERROR'
    endif
  endif
endif
end
EOT

cat <<EOT >f90_pointer_regression_2.f90
EOT

cat <<EOT >f90_pointer_regression_3.f90
EOT

cat <<EOT >f90_pointer_regression_4.f90
program test

interface
subroutine test1(bus2)
integer, dimension(20), target :: bus2
end subroutine test1
end interface

integer :: i
integer, dimension(20) :: bus

bus=[ (i, i=1,20) ]
call test1(bus)
stop
end
subroutine test1(bus2)
use pointers_nd
implicit none
integer, pointer, dimension(:) :: ptd
integer, pointer, dimension(:) :: p0d
integer, pointer, dimension(:) :: p1d
integer, pointer, dimension(:,:) :: p2d
integer*8 :: locarray
integer :: i
integer, dimension(20), target :: bus2
integer, dimension(30), target :: bus
character(len=17) :: copy_in

bus=[ (i, i=1,30) ]

p1d=>ptr1d(20)

locarray=loc(p1d(1))
call print_ptr1d(copy_in,locarray,p1d,10,1,1)
print 100,copy_in//'p1d=>ptr1d(10), passing p1d'

locarray=loc(p1d(1))
call print_ptr1d(copy_in,locarray,p1d(1),10,1,1)
print 100,copy_in//'p1d=>ptr1d(10), passing p1d(1)'

do I=5,5,3

print 100,'--------------------- I=',I,' ---------------------'
100 format(A,I1,A)
p0d(0:)=>p1d(I:I+9)
locarray=loc(p1d(I))
call print_ptr1d(copy_in,locarray,p0d,10,I,1)
print 100,copy_in//'1a- p0d(0:)=>p1d(I:I+9) passing p0d'

p0d(0:)=>p1d(I:I+9)
locarray=loc(p1d(I))
call print_ptr1d(copy_in,locarray,p0d(0),10,I,1)
print 100,copy_in//'1b- p0d(0:)=>p1d(I:I+9) passing p0d(0)'

p0d(0:)=>p1d(I:I+9)
locarray=loc(p1d(I))
call print_ptr1d(copy_in,locarray,p0d(0:9),10,I,1)
print 100,copy_in//'1c- p0d(0:)=>p1d(I:I+9) passing p0d(0:9)'

p0d(0:)=>bus(I:I+9)
locarray=loc(bus(I))
call print_ptr1d(copy_in,locarray,p0d,10,I,1)
print 100,copy_in//'2a- p0d(0:)=>bus(I:I+9) passing p0d'

p0d(0:)=>bus(I:I+9)
locarray=loc(bus(I))
call print_ptr1d(copy_in,locarray,p0d(0),10,I,1)
print 100,copy_in//'2b- p0d(0:)=>bus(I:I+9) passing p0d(0)'

p0d(0:)=>bus(I:I+9)
locarray=loc(bus(I))
call print_ptr1d(copy_in,locarray,(p0d(0)),10,I,1)
print 100,copy_in//'2c- p0d(0:)=>bus(I:I+9) passing (p0d(0)), ERRORS expected'

p0d(0:)=>bus(I:I+9)
ptd=>p0d
locarray=loc(bus(I))
call print_ptr1d(copy_in,locarray,ptd,10,I,1)
print 100,copy_in//'3a- p0d(0:)=>bus(I:I+9) ptd=>p0d passing ptd'

p0d(0:)=>bus(I:I+9)
ptd=>p0d
locarray=loc(bus(I))
call print_ptr1d(copy_in,locarray,ptd(0),10,I,1)
print 100,copy_in//'3b- p0d(0:)=>bus(I:I+9) ptd=>p0d passing ptd(0)'

p0d(0:)=>bus(I:I+9)
locarray=loc(bus(I))
call print_ptr1d(copy_in,locarray,p0d(0),10,I,1)
print 100,copy_in//'4 - p0d(0:)=>bus(I:I+9) passing p0d(0)'

p0d=>bus(I:I+9)
locarray=loc(bus(I))
p2d(0:1,1:5)=>p0d
call print_ptr1d(copy_in,locarray,p2d,10,I,1)
print 100,copy_in//'5a- p0d=>bus(I:I+9) p2d(0:1,1:5)=>p0d , passing p2d'

p0d=>bus(I:I+9)
locarray=loc(bus(I))
p2d(0:1,1:5)=>p0d
call print_ptr1d(copy_in,locarray,p2d(0,1),10,I,1)
print 100,copy_in//'5b- p0d=>bus(I:I+9) p2d(0:1,1:5)=>p0d , passing p2d(0,1)'

p0d=>bus(I:I+9)
locarray=loc(bus(I))
p2d(0:1,1:5)=>p0d(1:)
call print_ptr1d(copy_in,locarray,p2d,10,I,1)
print 100,copy_in//'6 - p0d=>bus(I:I+9) p2d(0:1,1:5)=>p0d(1:) , passing p2d'

p0d=>bus(I:I+9)
locarray=loc(bus(I))
p2d(0:1,1:5)=>p0d(1:10)
call print_ptr1d(copy_in,locarray,p2d,10,I,1)
print 100,copy_in//'7a- p0d=>bus(I:I+9) p2d(0:1,1:5)=>p0d(1:10) , passing p2d'

p0d=>bus(I:I+9)
locarray=loc(bus(I))
p2d(0:1,1:5)=>p0d(1:10)
call print_ptr1d(copy_in,locarray,p2d(0,1),10,I,1)
print 100,copy_in//'7b- p0d=>bus(I:I+9) p2d(0:1,1:5)=>p0d(1:10) , passing p2d(0,1)'

enddo
print 100,'----------------------- ',0,' ---------------------'

p0d(0:)=>p1d(1:)
locarray=loc(p1d(1))
call print_ptr1d(copy_in,locarray,p0d(0),10,1,1)
print 100,copy_in//'8 -p0d(0:)=>p1d(1:) passing p0d(0)'

p0d(0:)=>bus2(1:10)
locarray=loc(bus2)
call print_ptr1d(copy_in,locarray,p0d,10,1,1)
print 100,copy_in//'9a- p0d(0:)=>bus2(1:10) passing p0d'

p0d(0:)=>bus2(1:10)
locarray=loc(bus2)
call print_ptr1d(copy_in,locarray,p0d(0),10,1,1)
print 100,copy_in//'9b- p0d(0:)=>bus2(1:10) passing p0d(0)'

p0d(0:)=>bus(1:10)
locarray=loc(bus)
call print_ptr1d(copy_in,locarray,p0d,10,1,1)
print 100,copy_in//'0a- p0d(0:)=>bus(1:10) passing p0d'

locarray=loc(bus)
call print_ptr1d(copy_in,locarray,p0d(0),10,1,1)
print 100,copy_in//'0b- p0d(0:)=>bus(1:10) passing p0d(0)'

p2d(0:1,0:4)=>bus(1:10)
locarray=loc(bus)
call print_ptr1d(copy_in,locarray,p2d,10,1,1)
print 100,copy_in//'Aa- p2d(0:1,0:4)=>bus(1:10) , passing p2d'

p2d(0:1,0:4)=>p1d(1:)
locarray=loc(p1d(1))
call print_ptr1d(copy_in,locarray,p2d(0,0),10,1,1)
print 100,copy_in//'Ab- p2d(0:1,0:4)=>p1d(1:) passing p2d(0,0)'

p1d=>ptr1ds(20,1)
locarray=loc(p1d(1))
call print_ptr1d(copy_in,locarray,p1d,10,1,1)
print 100,copy_in//'B - p1d=>ptr1ds(20,1) , passing p1d'

p0d(0:)=>p1d(1:)
call print_ptr1d(copy_in,locarray,p0d(0),10,1,1)
print 100,copy_in//'C - p0d(0:)=>p1d(1:) , passing p0d(0)'

p1d=>ptr1ds(20,2)
locarray=loc(p1d(1))
call print_ptr1d(copy_in,locarray,p1d,10,1,2)
print 100,copy_in//'Da - p1d=>ptr1ds(20,2) , passing p1d, COPY-IN expected'

p0d(1:8)=>p1d(2:9)
locarray=loc(p1d(2))
call print_ptr1d(copy_in,locarray,p0d,8,3,2)
print 100,copy_in//'Dc - p1d=>ptr1ds(20,2) p0d(1:8)=>p1d(2:9), passing p0d, COPY-IN expected'

p0d(1:8)=>p1d(2:9)
locarray=loc(p1d(2))
call print_ptr1d(copy_in,locarray,p0d(1),8,3,2)
print 100,copy_in//'Dd - p1d=>ptr1ds(20,2) p0d(1:8)=>p1d(2:9), passing p0d(1), ERRORS expected'

p0d(1:8)=>p1d(2:9)
locarray=loc(p1d(2))
call print_ptr1d(copy_in,locarray,p0d(2),8,5,2)
print 100,copy_in//'De - p1d=>ptr1ds(20,2) p0d(1:8)=>p1d(2:9), passing p0d(2), ERRORS expected'

p0d(1:8)=>p1d(2:9)
locarray=loc(p1d(2))
call print_ptr1d(copy_in,locarray,p0d(1:8),8,3,2)
print 100,copy_in//'Df - p1d=>ptr1ds(20,2) p0d(1:8)=>p1d(2:9), passing p0d(1:8), COPY-IN expected'

p0d(0:)=>p1d(1:)
locarray=loc(p1d(1))
call print_ptr1d(copy_in,locarray,p0d(0),10,1,1)
print 100,copy_in//'E - p0d(0:)=>p1d(1:) , passing p0d(0)'

p0d(1:10)=>bus(1:10)
locarray=loc(bus(1))
p2d(1:2,1:5)=>p0d(1:10)
call print_ptr1d(copy_in,locarray,p2d,10,1,1)
print 100,copy_in//'Fa- p0d=>bus(1:10) p2d(1:2,1:5)=>p0d(1:10) , passing p2d'

p0d(1:10)=>bus(1:10)
locarray=loc(bus(1))
p2d(1:2,1:5)=>p0d(1:10)
call print_ptr1d(copy_in,locarray,p2d(1,1),10,1,1)
print 100,copy_in//'Fb- p0d=>bus(1:10) p2d(1:2,1:5)=>p0d(1:10) , passing p2d(1,1)'

p0d(1:10)=>bus2(5:14)
locarray=loc(bus2(5))
p2d(0:1,1:5)=>p0d(1:10)
call print_ptr1d(copy_in,locarray,p2d(0,1),10,5,1)
print 100,copy_in//'G - p0d=>bus2(5:14) p2d(0:1,1:5)=>p0d(1:10) , passing p2d(0,1)'

!p0d(1:)=>bus(1:10)
!locarray=loc(bus)
!p2d(0:1,1:5)=>p0d(1:10)
!call print_ptr1d(copy_in,locarray,p2d,10,1,1)
!print 100,copy_in//'p0d=bus(1:10) p2d(0:1,1:5)=>p0d(1:10) , passing p2d'

return
end
EOT
rm -f a.out pointers_nd.mod
set -x
${compiler} f90_pointer_regression_[0-4].f90
set +x
[[ -x ./a.out ]] && ./a.out
rm -f f90_pointer_regression_[0-4].f90 f90_pointer_regression_[0-4].o pointers_nd.mod a.out
