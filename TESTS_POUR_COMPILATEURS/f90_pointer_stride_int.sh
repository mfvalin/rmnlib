#!/bin/bash
compiler=${1:-gfortran}
cat <<EOT >f90_pointer_regression_0.f90
module pointers_nd

contains

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
  print 103,'expected: ',(I0+I*stride , I=0,ni-1)
  print 103,'got     : ',array(1:ni)
103 format(A,20i4)
if(copy_in/='         ' .or. errors/=0) then
  if(errors>0) then
    if(copy_in==' O.K.    ')then
      copy_in=' ERROR'
    else
      copy_in=trim(copy_in)//'ERROR'
    endif
  endif
endif
end
EOT

cat <<EOT >f90_pointer_regression_4.f90
program test

call test1()

stop
end
subroutine test1()
use pointers_nd
implicit none
integer, pointer, dimension(:) :: p0d
integer, pointer, dimension(:) :: p1d
integer*8 :: locarray
integer :: i
character(len=17) :: copy_in

100 format(A,I1,A)


p1d=>ptr1ds(20,2)  ! array with a stride of 2

p0d(1:8)=>p1d(2:9)
locarray=loc(p1d(2))
call print_ptr1d(copy_in,locarray,p0d,8,3,2)
print 100,copy_in//'Dc - p1d=>ptr1ds(20,2) p0d(1:8)=>p1d(2:9), passing p0d, COPY-IN expected'

p0d(1:8)=>p1d(2:9)
locarray=loc(p1d(2))
call print_ptr1d(copy_in,locarray,p0d(1:8),8,3,2)
print 100,copy_in//'Df - p1d=>ptr1ds(20,2) p0d(1:8)=>p1d(2:9), passing p0d(1:8), COPY-IN expected'

return
end
EOT
rm -f a.out pointers_nd.mod
set -x
${compiler} f90_pointer_regression_[0-4].f90
set +x
echo "===== test with the ${compiler} compiler ===="
[[ ${compiler} == gfortran ||  ${compiler} == flang ||  ${compiler} == ifort* ]] &&  ${compiler} --version
[[ ${compiler} == pg* ]] &&  ${compiler} -V
[[ -x ./a.out ]] && ./a.out
rm -f f90_pointer_regression_[0-4].f90 f90_pointer_regression_[0-4].o pointers_nd.mod a.out
