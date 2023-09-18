module test_rank_mod
  implicit none
contains
  subroutine get_rank(this, expected)
    integer, dimension(..) :: this
    integer :: expected
    if(rank(this) .ne. expected) then
      print '(A,I2,A,I2)', 'ERROR: rank = ',rank(this), ' , expected ',expected
    endif
  end
end module
program test_rank
  use test_rank_mod
  implicit none
  integer, dimension(1) :: a1
  integer, dimension(1,2) :: a2
  call get_rank(a1, 1)
  call get_rank(a2, 2)
end

