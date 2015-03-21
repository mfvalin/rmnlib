subroutine sub2(a,b,c)
  implicit none
  integer, dimension(*) :: a, b, c
  print *,'loc(a)=',loc(a)
  print *,'loc(b)=',loc(b)
  print *,'loc(c)=',loc(c)
  return
end subroutine
