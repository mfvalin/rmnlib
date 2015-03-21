subroutine sub1
  implicit none
  integer, parameter :: NI = 23
  integer, parameter :: Nj = 127
  integer, parameter :: NK = 7
  integer, dimension(NI,NJ,NK) :: array
  call sub2( array, array(:,:,1) , array(:,:,1:NK-1) )
  call sub2( array(1,1,2), array(:,:,2) , array(:,:,2:NK-1) )
  return
end subroutine sub1
