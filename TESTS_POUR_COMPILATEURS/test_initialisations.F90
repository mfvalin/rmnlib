program test_initialisation
  implicit none
  integer :: val
  call sub(val)
  print *,'----------'
  call sub(val)
  print *,'----------'
  call sub(val)
end program
subroutine sub(val)
  implicit none
  integer, intent(INOUT) :: val
  type :: machin
    integer :: v = 12
  end type
  type(machin) :: a = machin(13)  ! variable initialisee
  type(machin) :: b               ! type initialise
  print *,'variable initialisee    machin a =',a%v
  print *,'type initialise (avant) machin b =',b%v
  a%v = a%v + 1
  b%v = b%v + 1
  print *,'type initialise (apres) machin b =',b%v
end subroutine
