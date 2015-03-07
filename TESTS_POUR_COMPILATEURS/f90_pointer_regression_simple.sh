#!/bin/bash
compiler=${1:-gfortran}
cat <<EOT >f90_pointer_regression_0.f90
module pointers_nd

contains
function ptr1d(ni) result(p1d)
integer, intent(IN) :: ni
integer, pointer, dimension(:) :: p1d
integer :: i
allocate(p1d(ni))
p1d= [ (i, i=1,ni) ]
print *,'allocating array of size',ni
return
end function ptr1d

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
integer, dimension(*), target :: bus2
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
integer :: i, j
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

I=5

print 100,'--------------------- I=',I,' ---------------------'

100 format(A,I1,A)
101 format(A,2X,20I4)

p0d(0:)=>p1d(I:I+9)
locarray=loc(p1d(I))
call print_ptr1d(copy_in,locarray,p0d,10,I,1)
print 100,copy_in//'1a- p0d(0:)=>p1d(I:I+9) passing p0d'
print 101,'print p0d',p0d

p0d(0:9)=>p1d(I:I+9)
locarray=loc(p1d(I))
call print_ptr1d(copy_in,locarray,p0d,10,I,1)
print 100,copy_in//'1a- p0d(0:9)=>p1d(I:I+9) passing p0d'
print 101,'print p0d',p0d

p0d(0:9)=>p1d(I:I+9)
locarray=loc(p1d(I))
call print_ptr1d(copy_in,locarray,p0d(0),10,I,1)
print 100,copy_in//'1b- p0d(0:9)=>p1d(I:I+9) passing p0d(0)'

p0d(0:9)=>p1d(I:I+9)
locarray=loc(p1d(I))
call print_ptr1d(copy_in,locarray,p0d(0:9),10,I,1)
print 100,copy_in//'1c- p0d(0:9)=>p1d(I:I+9) passing p0d(0:9)'
print 101,'print p0d(0:9)',p0d(0:9)

p0d(0:9)=>bus(I:I+9)
locarray=loc(bus(I))
call print_ptr1d(copy_in,locarray,p0d,10,I,1)
print 100,copy_in//'2a- p0d(0:9)=>bus(I:I+9) passing p0d'

p0d(0:9)=>bus(I:I+9)
locarray=loc(bus(I))
call print_ptr1d(copy_in,locarray,p0d(0),10,I,1)
print 100,copy_in//'2b- p0d(0:9)=>bus(I:I+9) passing p0d(0)'

p0d(0:9)=>bus(I:I+9)
locarray=loc(bus(I))
call print_ptr1d(copy_in,locarray,(p0d(0)),10,I,1)
print 100,copy_in//'2c- p0d(0:9)=>bus(I:I+9) passing (p0d(0)), ERRORS expected'


return
end
EOT
rm -f a.out pointers_nd.mod
set -x
${compiler} f90_pointer_regression_[0-4].f90
set +x
[[ -x ./a.out ]] && ./a.out
rm -f f90_pointer_regression_[0-4].f90 f90_pointer_regression_[0-4].o pointers_nd.mod a.out
