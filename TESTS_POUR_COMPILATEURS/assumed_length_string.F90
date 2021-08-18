program str_var
  implicit none
  interface
  subroutine make_string(str, lng)
    implicit none
    character(len=:), pointer, intent(OUT) :: str
    integer, intent(IN) :: lng
  end subroutine make_string
  end interface
  character(len=:), allocatable, target :: alloc_name
  character(len=:), pointer :: alloc_name3
  character(len=:), pointer :: alloc_name2
  character(len=:), pointer :: ptr_name

!  allocate(character(len=5) :: alloc_name)
  allocate(character(len=6) :: alloc_name2)
  alloc_name = 'Name'
  alloc_name2 = 'Nm2'
  ptr_name => alloc_name
  print *, 'len(alloc_name) =',len(alloc_name)
  print *, 'alloc_name ="'//alloc_name//'"'
  print *, 'len(alloc_name2) =',len(alloc_name2)
  print *, 'alloc_name2 ="'//alloc_name2//'"'
  call make_string(alloc_name3, 12)
  print *, 'len(alloc_name3) =',len(alloc_name3)
  print *, 'alloc_name3 ="'//alloc_name3//'"'
end program
subroutine make_string(str, lng)
  implicit none
  character(len=:), pointer, intent(OUT) :: str
  integer, intent(IN) :: lng
  allocate(character(len=lng) :: str)
  str = ' '
end subroutine make_string
