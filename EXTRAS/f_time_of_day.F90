function f_time_of_day() result(the_time)
  use iso_c_binding
  implicit none
  type, bind(C) :: timeval
    integer(C_LONG_LONG) :: sec
    integer(C_LONG_LONG) :: usec
  end type

  interface
    function gettime(tv,tz) result(code) BIND(C,name='gettimeofday')
      use iso_c_binding
      import :: timeval
      implicit none
      type(timeval), intent(OUT) :: tv
      type(C_PTR), value, intent(IN) :: tz
      integer(C_INT) :: code
    end function gettime
  end interface

  type(timeval) :: tv
  integer(C_INT) :: code
  integer *8 :: the_time

  code = gettime(tv,C_NULL_PTR)
  the_time = tv%sec
  the_time = the_time * 1000000 + tv%usec

end function f_time_of_day

function f_wall_time() result(the_time)
  use iso_c_binding
  implicit none
  integer *8 :: the_time

  integer *8, external :: f_time_of_day
  integer *8, save :: time_offset = -1

  if(time_offset < 0) time_offset = f_time_of_day()
  the_time = f_time_of_day() - time_offset
  
end function f_wall_time

function f_wall_time_since() result(the_time)
  use iso_c_binding
  implicit none
  integer *8 :: the_time

  integer *8, external :: f_time_of_day
  integer *8, save :: time_offset = -1
  integer *8 :: temp

  temp = f_time_of_day()
  if(time_offset < 0) time_offset = temp
  the_time = temp - time_offset
  time_offset = temp
  
end function f_wall_time_since

#if defined(SELF_TEST)
program self_test
implicit none
integer *8, external :: f_time_of_day, f_wall_time, f_wall_time_since
integer *8 :: t1, t2
t1 = f_time_of_day()
print *,'Printing something for timing purposes'
t2 = f_time_of_day()
print *,'time of day to print 1 line =',t2-t1,' microseconds'
t1 = f_time_of_day()
print *,'Printing something for timing purposes'
t2 = f_time_of_day()
print *,'time of day to print 1 line =',t2-t1,' microseconds'
t1 = f_wall_time()
print *,'Printing something for timing purposes'
t2 = f_wall_time()
print *,'wall time to print 1 line =',t2-t1,' microseconds'
t1 = f_wall_time()
print *,'Printing something for timing purposes'
t2 = f_wall_time()
print *,'wall time to print 1 line =',t2-t1,' microseconds'
t1 = f_wall_time()
print *,'Printing something for timing purposes'
t2 = f_wall_time()
t1 = f_wall_time_since()
print *,'wall time to print 1 line =',t2-t1,' microseconds'
print *,'wall time since =',f_wall_time_since()
print *,'wall time since =',f_wall_time_since()
print *,'wall time since =',f_wall_time_since()
stop
end
#endif