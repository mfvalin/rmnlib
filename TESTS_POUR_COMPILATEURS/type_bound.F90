! same name for a type bound procedure in 2 different user defined types
! both types a and b have a bound procedure called machin
! compilers O.K. with this : gfortran, Intel Fortran, AMD aocc (flang), nvidia/pgi, allinea(ARM aarch64), IBM xlf
! some compilers do not like this
! using -Dmachin=machina -DMACHIN=machinb solves it
! declaring machin MACHIN as private also solves it
module mod_a
implicit none
#if defined(PRIVATE)
private machin
#endif
type:: a
 integer :: value
 contains
  procedure, NOPASS :: Machin => machin
end type
contains
subroutine machin()
print *,'machina'
end subroutine
end module

module mod_b
implicit none
#if defined(PRIVATE)
private MACHIN
#endif
type :: b
 contains
  procedure, NOPASS :: Machin => MACHIN
end type
contains
subroutine MACHIN()
print *,'machinb'
end subroutine
end module

program truc
use mod_a
use mod_b
implicit none
type(a) va
type(b) vb
call va%Machin()
call vb%Machin()
end
