#!/bin/bash
cat <<EOF >test.F90
subroutine poly(cp)
  use ISO_C_BINDING
  implicit none
  interface mptr
    function pr41d(pi,po,di) result(p)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      real, dimension(:), intent(IN), pointer :: po
      integer, dimension(:), intent(IN) :: di
      real, dimension(:), pointer :: p
    end function pr41d
    function pr42d(pi,po,di) result(p)
      import :: C_PTR
      type(C_PTR), intent(IN) :: pi
      real, dimension(:,:), intent(IN), pointer :: po
      integer, dimension(:), intent(IN) :: di
      real, dimension(:,:), pointer :: p
    end function pr42d
  end interface
  type(C_PTR), intent(IN) :: cp
  real, dimension(:), pointer       :: r41
  real, dimension(:,:), pointer     :: r42
  r41 => mptr(cp, r41, [-1, 2])
  print 1,'r41, size=',size(r41),' bounds=',lbound(r41,1),ubound(r41,1)
  r42 => mptr(cp, r42, [-1, 2, -2, 1])
  print 1,'r42, size=',size(r42),' bounds=',lbound(r42,1),ubound(r42,1),lbound(r42,2),ubound(r42,2)
1 format(A,I8,A,8I4)
end subroutine poly

function pr41d(pi,po,di) result(p)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: pi
  real, dimension(:), intent(IN), pointer :: po
  integer, dimension(:), intent(IN) :: di
  real, dimension(:), pointer :: p
  real, dimension(:), pointer :: t

  nullify(p)
  if(size(di) < 2) return
  call C_F_POINTER(pi, t, [1000000])
  p(di(1):di(2)) => t
end function pr41d

function pr42d(pi,po,di) result(p)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: pi
  real, dimension(:,:), intent(IN), pointer :: po
  integer, dimension(:), intent(IN) :: di
  real, dimension(:,:), pointer :: p
  real, dimension(:), pointer :: t

  nullify(p)
  if(size(di) < 4) return
  call C_F_POINTER(pi, t, [1000000])
  p(di(1):di(2),di(3):di(4)) => t
end function pr42d

program test
  use ISO_C_BINDING
  implicit none
  integer, dimension(1000000), target :: dummy
  type(C_PTR) :: cp
  cp = c_loc(dummy(1))
  call poly(cp)
end program
EOF
set -x
${1:-gfortran} --version
${1:-gfortran} test.F90
./a.out