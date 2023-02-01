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

!==========================================================================

module integrals9a
   implicit none
   private
   public :: simpson9a, integrable_function9a

   abstract interface
      function integrable_function9a(x, fa, fk) result(func)
         real, intent(in) :: x, fa, fk
         real :: func
      end function integrable_function9a
   end interface

contains

   function simpson9a(f, a, b, fa, fk) result(s)
      real, intent(in) :: a, b, fa, fk
      procedure(integrable_function9a) :: f
      real :: s
      s = (b-a) / 6 * (f(a, fa, fk) + 4*f((a+b)/2, fa, fk) + f(b, fa, fk))
   end function simpson9a

end module integrals9a


module demo_functions9a
   use integrals9a, only: simpson9a
   implicit none
   private
   public :: test_integral9a

contains

   subroutine test_integral9a(a, k)
      real, intent(in) :: a, k
      real :: pi
      pi = 3.1416
      print *, simpson9a(f9a, 0., pi, a, k)
      print *, simpson9a(f9a, 0., 2*pi, a, k)
   contains

      function f9a(x, fa, fk) result(y)
         real, intent(in) :: x, fa, fk
         real :: y
         y = fa*sin(fk*x)
      end function f9a

   end subroutine test_integral9a

end module demo_functions9a
program test_integrals2
   ! use integrals2, only: integral_solve_vdep2, integral_solve_vdep3
   use demo_functions9, only: test_integral9
   use demo_functions9a, only: test_integral9a
   ! call integral_solve_vdep2()
   ! call integral_solve_vdep3(1.1, 2.2)
   print *,'call test_integral9a' ; call flush(6)
   call test_integral9a(1.1, 2.2)
   print *,'call test_integral9' ; call flush(6)
   call test_integral9(1.1, 2.2)
end program test_integrals2
