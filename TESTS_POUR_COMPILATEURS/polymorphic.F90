module demo_poly
use ISO_C_BINDING
interface machin
  subroutine machin1(arg)
  real, intent(IN) :: arg
  end subroutine
  subroutine machin2(arg)
  integer, intent(IN) :: arg
  end subroutine
end interface
type :: truc
    type(C_PTR) :: p
contains
    procedure :: procedure1
    procedure :: procedure2
    GENERIC   :: procedure => procedure1, procedure2
end type

contains
  subroutine procedure1(dum, arg)
  class(truc), intent(IN) :: dum
  real, intent(IN) :: arg
  print *,arg
  end subroutine
  subroutine procedure2(dum, arg)
  class(truc), intent(IN) :: dum
  integer, intent(IN) :: arg
  print *,arg
  end subroutine
end module
program demo
use demo_poly
type(truc) :: t
call machin(234)
call machin(2.34)
call procedure2(t,123)
call t%procedure(123)
call procedure1(t,1.23)
call t%procedure(1.23)
end
subroutine machin1(arg)
real, intent(IN) :: arg
  print *,arg
end subroutine
subroutine machin2(arg)
integer, intent(IN) :: arg
  print *,arg
end subroutine
