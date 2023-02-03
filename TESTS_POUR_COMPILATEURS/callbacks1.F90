module integrals9
   implicit none
   private
   public :: simpson9, integrable_function9

   abstract interface
      function integrable_function9(x) result(func)
         real, intent(in) :: x
         real :: func
      end function integrable_function9
   end interface

contains

   function simpson9(f, a, b) result(s)
      real, intent(in) :: a, b
      procedure(integrable_function9) :: f
      real :: s
      s = (b-a) / 6 * (f(a) + 4*f((a+b)/2) + f(b))
   end function simpson9

end module integrals9


module demo_functions9
   use integrals9, only: simpson9
   implicit none
   private
   public :: test_integral9

contains

   subroutine test_integral9(a, k)
      real, intent(in) :: a, k
      real :: pi
      pi = 3.1416
      print *, simpson9(f9, 0., pi)
      print *, simpson9(f9, 0., 2*pi)
   contains

      function f9(x) result(y)
         real, intent(in) :: x
         real :: y
         y = a*sin(k*x)
      end function f9

   end subroutine test_integral9

end module demo_functions9
