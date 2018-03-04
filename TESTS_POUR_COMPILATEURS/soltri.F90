subroutine soltri_m ( F_r, F_rhs, F_a, F_b, F_c, np, ni)
  integer, intent(IN) :: np, ni
  real, dimension(np,ni), intent(IN) :: F_a, F_b, F_c, F_rhs
  real, dimension(np,ni), intent(OUT) :: F_r

  integer :: i

  F_r(:,1) = F_rhs(:,1) * F_b(:,1)

  do i = 2, ni
    F_r(:,i) =  F_rhs(:,i) * F_b(:,i) - F_a(:,i)*F_r(:,i-1)
  enddo

  do i = ni-1, 1, -1
    F_r(:,i) =  F_r(:,i) - F_c(:,i)*F_r(:,i+1)
  enddo

  return
end
