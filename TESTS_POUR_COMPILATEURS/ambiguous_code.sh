#!/bin/bash
compiler=${1:-gfortran}
cat <<EOT >ambiguous.f90
program test
   implicit none
   integer, dimension(2) :: ambiguous = (/33,44/)
   integer :: answer

   answer = ambiguous(1)
   print *, answer

   contains
      integer function ambiguous(a)
      implicit none
         integer a
         ambiguous = a + a
         print *,'OOPS: function ambiguous called'
      end function ambiguous

end program
EOT
echo ===============================================================
echo no compiler error is an error
echo ===============================================================
rm -f a.out
${compiler} ambiguous.f90 
[[ -x ./a.out ]] && ./a.out
rm -f a.out ambiguous.o ambiguous.f90
