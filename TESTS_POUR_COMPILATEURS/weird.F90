module rmn_fstd98
  use ISO_C_BINDING
  implicit none

  type :: fstd98
    integer     :: iun = -1                  ! Fortran unit number
  end type

  interface
    ! /*
    module function fstd98_is_rsf(this) result(is_rsf2)
      implicit none
      type(fstd98), intent(IN) :: this
      logical :: is_rsf2
    end function fstd98_is_rsf
  end interface
#if defined(BUG2)
    ! */
#endif

end module rmn_fstd98
