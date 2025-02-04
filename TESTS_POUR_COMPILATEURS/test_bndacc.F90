program tbndacc
   implicit none
   integer, parameter ::  Nb = 4, Jt =3
   real :: G(12,6) = 0.0
   integer :: Ip = 2, Ir = 4

   G(1,1:5) = (/   0.2,   21.0,   40.8,    4.7,    1.7 /)
   G(2,1:5) = (/  -3.1,  -41.2,  -30.9,   -0.9,   -4.6 /)
   G(3,1:5) = (/   0.5,   19.3,   33.3,    2.9,    8.0 /)
   G(4,1:5) = (/  11.1,   44.4,   11.1,    0.01,   18.0 /)
   G(5,1:5) = (/   6.8,   43.1,   16.7,    0.05,   24.7 /)

   call BndAcc(G,Nb,Ip,Ir,Jt)
CONTAINS
!
   subroutine bndacc(G,Nb,Ip,Ir,Jt)
!
      implicit none
      integer Ip, Ir, Jt, Nb
      real :: G(12,*), zero
      integer i,ig, k, l, lp1, mu, nbp1
!
      integer :: temp_max

      zero = 0.
      nbp1 = Nb + 1
         mu = min(Nb-1,Ir-Ip-1) !..mu will equal 1

            do l = 1, mu
               k = min(L,Jt-Ip)
               lp1 = l + 1
               ig = Ip + l

               do i = lp1, Nb
                  G(ig,i - k) = G(ig,i)
               end do

               do i = 1, k
                  G(ig,nbp1 - i) = zero
               end do
            end do
      print*, 'G(3,1:5)'
      print*, G(3,1:5)
      print*, "Should be"
      print*, "  19.30000       33.30000       2.900000      0.0000000E+00   8.000000"
      STOP

      return
   end subroutine bndacc

end program
