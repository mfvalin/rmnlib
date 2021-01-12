#!/bin/bash
rm -f *.o *mod test_submodule.Abs
cat <<EOT >module_source.F90
module foo
  interface bar
    module subroutine bar_integer(x)
      integer(kind=4), intent(in) :: x
    end subroutine
    module subroutine bar_real(x)
      real(kind=4), intent(in) :: x
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
    print *,'bar_real :',x
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
  print *,'exiting main'
end program
EOT
${1:-ftn} module_source.F90 submodule_source.F90 main_source.F90 -o test_submodule.Abs
./test_submodule.Abs
ls -alrt | grep -i 'mod$' | tail -5
rm -f module_source.F90 submodule_source.F90 main_source.F90 *.o *mod test_submodule.Abs
# pgi, nvidia, flang : foo.mod,          foo-sub.mod
# xlf                : foo.mod,          foo_sub.smod
# gfortran           : foo.mod=foo.smod, foo@sub.smod
# ifort              : foo.mod,          foo@sub.smod
# cray-ftn           : FOO.mod,          SUB.mod
