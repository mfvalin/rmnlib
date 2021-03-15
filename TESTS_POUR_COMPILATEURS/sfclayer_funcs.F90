module sfclayer_funcs
  implicit none
  abstract interface
     subroutine stability_function(F_fm)
       real, intent(out) :: F_fm
     end subroutine stability_function
  end interface
  ! Stability functions
  public :: sf_stable_delage97, sf_unstable_delage92
contains
  subroutine sf_stable_delage97(F_fm)
    real, intent(out) :: F_fm
  end subroutine sf_stable_delage97
  subroutine sf_unstable_delage92(F_fm)
    real, intent(out) :: F_fm
  end subroutine sf_unstable_delage92
end module sfclayer_funcs

