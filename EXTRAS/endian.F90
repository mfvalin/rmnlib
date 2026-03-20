program endian
  use ISO_C_BINDING
  implicit none
  integer :: n
  common /endianness/ result
  integer(C_INT), dimension(2) :: result
  bind(C,name='C_endian_flag') :: /endianness/

  print *,'little endian =',(result(1)>0)

  return
end
