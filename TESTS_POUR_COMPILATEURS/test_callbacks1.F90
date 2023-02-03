program test_callback
use demo_functions9, only: test_integral9
implicit none
print *,'call test_integral9' ; call flush(6)
call test_integral9(1.1, 2.2)
end

