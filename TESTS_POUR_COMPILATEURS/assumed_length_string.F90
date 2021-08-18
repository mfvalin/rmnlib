program str_var
  implicit none
  character(len=:), allocatable, target :: alloc_name
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
end program
