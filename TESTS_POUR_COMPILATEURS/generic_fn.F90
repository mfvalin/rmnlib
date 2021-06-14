
module my_mod
  implicit none
  interface my_mod_fn
    module procedure my_mod_fn_v1
    module procedure my_mod_fn_v2
  end interface my_mod_fn
contains
  subroutine my_mod_fn_v1(F_sfcfld_S, F_comm_S, F_ipe_master, F_altfld_S)
    implicit none
    character(len=*),intent(inout) :: F_sfcfld_S
    character(len=*),intent(in)    :: F_comm_S
    integer, intent(in), optional  :: F_ipe_master
    character(len=*), intent(inout), optional :: F_altfld_S
    print *,'my_mod_fn_v1'
  end subroutine my_mod_fn_v1
#if defined(REORDER)
  subroutine my_mod_fn_v2(F_sfcfld_S, F_comm_S, F_sfcfld2_S, F_ipe_master, F_altfld_S)
#else  
  subroutine my_mod_fn_v2(F_sfcfld_S, F_sfcfld2_S, F_comm_S, F_ipe_master, F_altfld_S)
#endif
    implicit none
    character(len=*), intent(inout) :: F_sfcfld_S
    character(len=*), intent(inout) :: F_sfcfld2_S
    character(len=*), intent(in)    :: F_comm_S
    integer, intent(in), optional   :: F_ipe_master
    character(len=*), intent(inout), optional :: F_altfld_S
    print *,'my_mod_fn_v2'
   end subroutine my_mod_fn_v2
end module
program test_it
  use my_mod
  implicit none
  character(len=32) :: str1, str2, str3

  call my_mod_fn(str1, 'flagada')                         ! call my_mod_fn_v1
  call my_mod_fn(str1, 'flagada', F_altfld_S=str2)        ! call my_mod_fn_v1
#if defined(REORDER)
  call my_mod_fn(str1, 'flagada', str2)                   ! call my_mod_fn_v2
  call my_mod_fn(str1, 'flagada', str2, F_altfld_S=str3)  ! call my_mod_fn_v2
#else
  call my_mod_fn(str1, str2, 'flagada')                   ! call my_mod_fn_v2
  call my_mod_fn(str1, str2, 'flagada', F_altfld_S=str3)  ! call my_mod_fn_v2
#endif
end 
