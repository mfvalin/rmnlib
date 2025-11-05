module mod_040_1
  implicit none
  integer :: s1 = 1
end module
module mod_040_2
  implicit none
  integer :: s1 = 2
end module
program example_041
  use mod_040_1
  use mod_040_2
  implicit none
  print *,'s1 =', s1
end program

