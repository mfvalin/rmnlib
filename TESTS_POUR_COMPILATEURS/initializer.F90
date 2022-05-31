module grid_assembly_module
  implicit none

  type :: grid_inner
    integer :: tag = -1
  end type

  type :: grid
    type(grid_inner), dimension(1) :: inner = grid_inner() ! This would be OK for intel if it were just a scalar variable
  contains
    procedure, pass, public :: get_num
  end type

contains

  function get_num(this) result(num)
    implicit none
    class(grid), intent(in) :: this
    integer :: num
    num = 0
  end function get_num

  subroutine create_grid(g)
    implicit none
    type(grid), intent(out) :: g
    g = grid() ! This is OK for both compilers
  end subroutine create_grid

end module grid_assembly_module

!--------------------------
! User module
module grid_user
  use grid_assembly_module
  implicit none

  type :: user
!       type(grid) :: g = grid()  ! crashes ifort, OK for gfortran
      type(grid) :: g = grid(grid_inner())          ! OK for both
  contains
    procedure, pass :: generate_crash
  end type user

contains

  subroutine generate_crash(this)
    implicit none
    class(user), intent(in) :: this
    integer :: dummy
    dummy = this % g % get_num()
  end subroutine generate_crash

  subroutine create(object)
    implicit none
    type(user), intent(out) :: object
!     object = user(grid())              ! crashes ifort, OK for gfortran
    object = user(grid(grid_inner()))          ! OK for both
  end subroutine create

  subroutine create_grid_out(g)
    implicit none
    type(grid), intent(out) :: g
    g = grid()                         ! This is OK for both compilers
  end subroutine create_grid_out

end module grid_user
