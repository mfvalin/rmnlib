#!/bin/bash
rm -f *.o *mod test_submodule.Abs
cat <<EOT >module_source.F90
module foo
  type :: custom
    integer :: v
    real    :: z
  end type
  interface bar
    module subroutine bar_integer(x)
      integer(kind=4), intent(in) :: x
    end subroutine
    module subroutine bar_real(x)
      real(kind=4), intent(in) :: x
    end subroutine
    module subroutine bar_real8(x)
      real(kind=8), intent(in) :: x
    end subroutine
    module subroutine bar_custom(x)
      type(custom), intent(in) :: x
    end subroutine
  end interface
end module
EOT
#
cat <<EOT >submodule_source.F90
submodule ( foo ) sub
contains
  module procedure bar_integer
    print *,'bar_integer :',x
  end procedure
  module procedure bar_real
    print *,'bar_real    :',x
  end procedure
  module procedure bar_real8
    print *,'bar_real8   :',x
  end procedure
  module procedure bar_custom
    print *,'bar_custom  :',x%v,x%z
  end procedure
end submodule
EOT
#
cat <<EOT >main_source.F90
program main
use foo
  integer :: i = -123
  real :: r = 1.5
  call bar(i)
  call bar(r)
  call bar(1.2345_8)
  call bar(custom(12345,5.4321))
  print *,'exiting main'
end program
EOT
${1:-ftn} -c module_source.F90 submodule_source.F90 
${1:-ftn} module_source.o submodule_source.o main_source.F90 -o test_submodule.Abs
./test_submodule.Abs
ls -alrt | grep -i 'mod$' | tail -5
ls -al module_source.o submodule_source.o
rm -f module_source.F90 submodule_source.F90 main_source.F90 *.o *mod test_submodule.Abs
# pgi, nvidia, flang : foo.mod,          foo-sub.mod
# xlf                : foo.mod,          foo_sub.smod
# gfortran           : foo.mod=foo.smod, foo@sub.smod
# ifort              : foo.mod,          foo@sub.smod
# cray-ftn           : FOO.mod,          SUB.mod
