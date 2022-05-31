program test_ccard
  implicit none
  character(len=16), dimension(3) :: incle
  character(len=16), dimension(3) :: def
  character(len=16), dimension(3) :: val
  integer :: it
  integer, external :: longueur
  character(len=16) :: scrap

  incle(1) = "a" ; def(1) = "111" ; val(1) = "aaa"
  incle(2) = "b" ; def(2) = "222" ; val(2) = "bbb"
  incle(3) = "c" ; def(3) = "333" ; val(3) = "ccc"
  it = -1
  call ccard(incle, def, val, 3, it)
  print *, 'a = ',trim(val(1))
  print *, 'b = ',trim(val(2))
  print *, 'c = ',trim(val(3))

  scrap = "         "
  print *,'longueur =', longueur(scrap)
end program
subroutine rmtcall
print *,'IN RMTCALL'
stop
end
