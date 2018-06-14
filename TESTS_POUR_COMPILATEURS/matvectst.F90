!---------------------------------- LICENCE BEGIN -------------------------------
! GEM - Library of kernel routines for the GEM numerical atmospheric model
! Copyright (C) 1990-2010 - Division de Recherche en Prevision Numerique
!                       Environnement Canada
! This library is free software; you can redistribute it and/or modify it
! under the terms of the GNU Lesser General Public License as published by
! the Free Software Foundation, version 2.1 of the License. This library is
! distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
! without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
! PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
! You should have received a copy of the GNU Lesser General Public License
! along with this library; if not, write to the Free Software Foundation, Inc.,
! 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
!---------------------------------- LICENCE END ---------------------------------
!
!*s/r exp_matvec - Jacobian-vector product subroutines
!
#if defined(SELF_TEST)
program test
  implicit none
  integer, parameter :: CENTERED_SHAPE=12, UPWIND_SHAPE=13, l_minx=-2, l_maxx=102, l_miny=-2, l_maxy=52, l_nk=80
  integer, parameter :: nvars=14
  real, dimension(l_minx:l_maxx, l_miny:l_maxy, l_nk, nvars) :: vec, prod
  real*8 :: dt
  integer :: n
  real :: stencil_eq1_h(l_minx:l_maxx, UPWIND_SHAPE  , l_miny:l_maxy, l_nk)
  real :: stencil_eq1_u(l_minx:l_maxx, CENTERED_SHAPE, l_miny:l_maxy, l_nk)
  real :: stencil_eq1_v(l_minx:l_maxx, CENTERED_SHAPE, l_miny:l_maxy, l_nk)
  real :: stencil_eq2_h(l_minx:l_maxx, CENTERED_SHAPE, l_miny:l_maxy, l_nk)
  real :: stencil_eq2_u(l_minx:l_maxx, UPWIND_SHAPE  , l_miny:l_maxy, l_nk)
  real :: stencil_eq2_v_interp(l_minx:l_maxx, l_miny:l_maxy, l_nk)
  real :: stencil_eq3_h(l_minx:l_maxx, CENTERED_SHAPE, l_miny:l_maxy,l_nk)
  real :: stencil_eq3_u_interp(l_minx:l_maxx, l_miny:l_maxy, l_nk)
  real :: stencil_eq3_v(l_minx:l_maxx, UPWIND_SHAPE, l_miny:l_maxy, l_nk)
  real :: stencil_adv_tr(l_minx:l_maxx, UPWIND_SHAPE, l_miny:l_maxy, l_nk)
  do n = 1, 1000
  call exp_matvec (vec, prod, nvars, dt, l_minx, l_maxx, l_miny, l_maxy, l_nk, &
       stencil_eq1_h, stencil_eq2_u, stencil_eq3_v, stencil_eq1_u, stencil_eq1_v, &
       stencil_eq2_h, stencil_eq3_h, stencil_adv_tr, stencil_eq2_v_interp, stencil_eq3_u_interp)
  enddo
  print *,maxval(prod)
end program
#endif
subroutine exp_matvec (vec, prod, nvars, dt, l_minx, l_maxx, l_miny, l_maxy, l_nk, &
          stencil_eq1_h, stencil_eq2_u, stencil_eq3_v, stencil_eq1_u, stencil_eq1_v, &
          stencil_eq2_h, stencil_eq3_h, stencil_adv_tr, stencil_eq2_v_interp, stencil_eq3_u_interp)
   ! Compute the action of the Jacobian on a given vector
   !
   !author
   !     Stephane Gaudreault -- September 2016
   !

   implicit none

   integer, parameter :: P=1, WWW=2, WW=3, W=4, E=5, EE=6, EEE=7, SSS=8, SS=9, S=10, N=11, NN=12, NNN=13, &
                         UPWIND_SHAPE=13
   integer, parameter :: www_stag=1, ww_stag=2, w_stag=3, e_stag=4, eee_stag=5, ee_stag=6, sss_stag=7, ss_stag=8, &
                         s_stag=9, n_stag=10, nn_stag=11, nnn_stag=12, CENTERED_SHAPE=12

   integer, intent(in) :: nvars, l_minx, l_maxx, l_miny, l_maxy, l_nk
   real, dimension(l_minx:l_maxx, l_miny:l_maxy, l_nk, nvars), intent(inout)  :: vec
   real, dimension(l_minx:l_maxx, l_miny:l_maxy, l_nk, nvars), intent(out) :: prod
   real*8, intent(in) :: dt
   real :: stencil_eq1_h(l_minx:l_maxx, UPWIND_SHAPE  , l_miny:l_maxy, l_nk)
   real :: stencil_eq1_u(l_minx:l_maxx, CENTERED_SHAPE, l_miny:l_maxy, l_nk)
   real :: stencil_eq1_v(l_minx:l_maxx, CENTERED_SHAPE, l_miny:l_maxy, l_nk)
   real :: stencil_eq2_h(l_minx:l_maxx, CENTERED_SHAPE, l_miny:l_maxy, l_nk)
   real :: stencil_eq2_u(l_minx:l_maxx, UPWIND_SHAPE  , l_miny:l_maxy, l_nk)
   real :: stencil_eq2_v_interp(l_minx:l_maxx, l_miny:l_maxy, l_nk)
   real :: stencil_eq3_h(l_minx:l_maxx, CENTERED_SHAPE, l_miny:l_maxy,l_nk)
   real :: stencil_eq3_u_interp(l_minx:l_maxx, l_miny:l_maxy, l_nk)
   real :: stencil_eq3_v(l_minx:l_maxx, UPWIND_SHAPE, l_miny:l_maxy, l_nk)
   real :: stencil_adv_tr(l_minx:l_maxx, UPWIND_SHAPE, l_miny:l_maxy, l_nk)

   integer :: i0, in, j0, jn, i0u, inu, j0v, jnv
   integer :: i, j, k, l
   real    :: u_vec_interp, v_vec_interp
   real*8 w1,w2,w3,w4,w5,w6, u1,u2,u3,u4,u5,u6, v1,v2,v3,v4,v5,v6
   real, dimension(l_minx:l_maxx, l_miny:l_maxy, l_nk) :: u_vec_on_g, v_vec_on_g, u_vec_on_v, v_vec_on_u

   j0 = 1
   jn = l_maxy - 2
   i0 = 1
   in = l_maxx - 2
!    do k = 1,l_nk
!       do j = j0, jn
!          do i = i0, in
!             !************************************************************
!             ! Compute Jacobian-vector for continuity, U and V equations *
!             !************************************************************
! 
!             prod(i,j,k,1) =  &
!                  stencil_eq1_h(i,WWW,j,k)      * vec(i-3,j,k,1) &
!                + stencil_eq1_h(i,WW ,j,k)      * vec(i-2,j,k,1) &
!                + stencil_eq1_h(i,W  ,j,k)      * vec(i-1,j,k,1) &
!                + stencil_eq1_h(i,P  ,j,k)      * vec(i  ,j,k,1) &
!                + stencil_eq1_h(i,E  ,j,k)      * vec(i+1,j,k,1) &
!                + stencil_eq1_h(i,EE ,j,k)      * vec(i+2,j,k,1) &
!                + stencil_eq1_h(i,EEE,j,k)      * vec(i+3,j,k,1)
! 
!             prod(i,j,k,1) =  prod(i,j,k,1) &
!                + stencil_eq1_h(i,SSS,j,k)      * vec(i,j-3,k,1) &
!                + stencil_eq1_h(i,SS ,j,k)      * vec(i,j-2,k,1) &
!                + stencil_eq1_h(i,S  ,j,k)      * vec(i,j-1,k,1) &
!                + stencil_eq1_h(i,N  ,j,k)      * vec(i,j+1,k,1) &
!                + stencil_eq1_h(i,NN ,j,k)      * vec(i,j+2,k,1) &
!                + stencil_eq1_h(i,NNN,j,k)      * vec(i,j+3,k,1)
! 
!             prod(i,j,k,1) =  dt * prod(i,j,k,1)
! 
!             prod(i,j,k,2) =  &
!                  stencil_eq2_h(i,www_stag,j,k) * vec(i-2,j,k,1) &
!                + stencil_eq2_h(i,ww_stag ,j,k) * vec(i-1,j,k,1) &
!                + stencil_eq2_h(i,w_stag  ,j,k) * vec(i  ,j,k,1) &
!                + stencil_eq2_h(i,e_stag  ,j,k) * vec(i+1,j,k,1) &
!                + stencil_eq2_h(i,ee_stag ,j,k) * vec(i+2,j,k,1) &
!                + stencil_eq2_h(i,eee_stag,j,k) * vec(i+3,j,k,1)
! 
!             prod(i,j,k,3) = &
!                  stencil_eq3_h(i,sss_stag,j,k) * vec(i,j-2,k,1) &
!                + stencil_eq3_h(i,ss_stag ,j,k) * vec(i,j-1,k,1) &
!                + stencil_eq3_h(i,s_stag  ,j,k) * vec(i,j  ,k,1) &
!                + stencil_eq3_h(i,n_stag  ,j,k) * vec(i,j+1,k,1) &
!                + stencil_eq3_h(i,nn_stag ,j,k) * vec(i,j+2,k,1) &
!                + stencil_eq3_h(i,nnn_stag,j,k) * vec(i,j+3,k,1)
!          end do
!       end do
! 
!       do j = j0, jn
!          do i = i0, in
!             prod(i,j,k,1) =  prod(i,j,k,1) &
!                + stencil_eq1_u(i,www_stag,j,k) * vec(i-3,j,k,2) &
!                + stencil_eq1_u(i,ww_stag ,j,k) * vec(i-2,j,k,2) &
!                + stencil_eq1_u(i,w_stag  ,j,k) * vec(i-1,j,k,2) &
!                + stencil_eq1_u(i,e_stag  ,j,k) * vec(i  ,j,k,2) &
!                + stencil_eq1_u(i,ee_stag ,j,k) * vec(i+1,j,k,2) &
!                + stencil_eq1_u(i,eee_stag,j,k) * vec(i+2,j,k,2)
! 
!             prod(i,j,k,2) =  prod(i,j,k,2) &
!                + stencil_eq2_u(i,P  ,j,k)      * vec(i  ,j,k,2) &
!                + stencil_eq2_u(i,WWW,j,k)      * vec(i-3,j,k,2) &
!                + stencil_eq2_u(i,WW ,j,k)      * vec(i-2,j,k,2) &
!                + stencil_eq2_u(i,W  ,j,k)      * vec(i-1,j,k,2) &
!                + stencil_eq2_u(i,E  ,j,k)      * vec(i+1,j,k,2) &
!                + stencil_eq2_u(i,EE ,j,k)      * vec(i+2,j,k,2) &
!                + stencil_eq2_u(i,EEE,j,k)      * vec(i+3,j,k,2)
! 
!             prod(i,j,k,2) =  prod(i,j,k,2) &
!                + stencil_eq2_u(i,SSS,j,k)      * vec(i,j-3,k,2) &
!                + stencil_eq2_u(i,SS ,j,k)      * vec(i,j-2,k,2) &
!                + stencil_eq2_u(i,S  ,j,k)      * vec(i,j-1,k,2) &
!                + stencil_eq2_u(i,N  ,j,k)      * vec(i,j+1,k,2) &
!                + stencil_eq2_u(i,NN ,j,k)      * vec(i,j+2,k,2) &
!                + stencil_eq2_u(i,NNN,j,k)      * vec(i,j+3,k,2)
! 
!             prod(i,j,k,2) =  dt * (prod(i,j,k,2) + stencil_eq2_v_interp(i,j,k) * v_vec_on_u(i,j,k))
!          end do
!       end do
!             
!       do j = j0, jn
!          do i = i0, in
!             prod(i,j,k,1) =  prod(i,j,k,1) &
!               + stencil_eq1_v(i,sss_stag,j,k) * vec(i,j-3,k,3) &
!               + stencil_eq1_v(i,ss_stag ,j,k) * vec(i,j-2,k,3) &
!               + stencil_eq1_v(i,s_stag  ,j,k) * vec(i,j-1,k,3) &
!               + stencil_eq1_v(i,n_stag  ,j,k) * vec(i,j  ,k,3) &
!               + stencil_eq1_v(i,nn_stag ,j,k) * vec(i,j+1,k,3) &
!               + stencil_eq1_v(i,nnn_stag,j,k) * vec(i,j+2,k,3)
!             prod(i,j,k,1) =  prod(i,j,k,1) * dt
! 
!             prod(i,j,k,3) = prod(i,j,k,3) &
!               + stencil_eq3_v(i,P  ,j,k)      * vec(i  ,j,k,3) &
!               + stencil_eq3_v(i,WWW,j,k)      * vec(i-3,j,k,3) &
!               + stencil_eq3_v(i,WW ,j,k)      * vec(i-2,j,k,3) &
!               + stencil_eq3_v(i,W  ,j,k)      * vec(i-1,j,k,3) &
!               + stencil_eq3_v(i,E  ,j,k)      * vec(i+1,j,k,3) &
!               + stencil_eq3_v(i,EE ,j,k)      * vec(i+2,j,k,3) &
!               + stencil_eq3_v(i,EEE,j,k)      * vec(i+3,j,k,3)
! 
!             prod(i,j,k,3) = prod(i,j,k,3) &
!               + stencil_eq3_v(i,SSS,j,k)      * vec(i,j-3,k,3) &
!               + stencil_eq3_v(i,SS ,j,k)      * vec(i,j-2,k,3) &
!               + stencil_eq3_v(i,S  ,j,k)      * vec(i,j-1,k,3) &
!               + stencil_eq3_v(i,N  ,j,k)      * vec(i,j+1,k,3) &
!               + stencil_eq3_v(i,NN ,j,k)      * vec(i,j+2,k,3) &
!               + stencil_eq3_v(i,NNN,j,k)      * vec(i,j+3,k,3)
! 
!             prod(i,j,k,3) = dt * (prod(i,j,k,3) + stencil_eq3_u_interp(i,j,k) * u_vec_on_v(i,j,k))
!          end do
!       end do
!    end do

100   continue

!         do l = 4,nvars
    do k = 1,l_nk
        do l = 4,nvars
      do j = j0, jn
!         do l = 4,nvars
          do i = i0, in
               !****************************************
               ! Compute Jacobian-vector for tracers   *
               !****************************************
               prod(i,j,k,l) = dt * (  stencil_adv_tr(i,P  ,j,k) * vec(i  ,j,k,l) &
                                     + stencil_adv_tr(i,WWW,j,k) * vec(i-3,j,k,l) &
                                     + stencil_adv_tr(i,WW ,j,k) * vec(i-2,j,k,l) &
                                     + stencil_adv_tr(i,W  ,j,k) * vec(i-1,j,k,l) &
                                     + stencil_adv_tr(i,E  ,j,k) * vec(i+1,j,k,l) &
                                     + stencil_adv_tr(i,EE ,j,k) * vec(i+2,j,k,l) &
                                     + stencil_adv_tr(i,EEE,j,k) * vec(i+3,j,k,l) &
                                     + stencil_adv_tr(i,SSS,j,k) * vec(i,j-3,k,l) &
                                     + stencil_adv_tr(i,SS ,j,k) * vec(i,j-2,k,l) &
                                     + stencil_adv_tr(i,S  ,j,k) * vec(i,j-1,k,l) &
                                     + stencil_adv_tr(i,N  ,j,k) * vec(i,j+1,k,l) &
                                     + stencil_adv_tr(i,NN ,j,k) * vec(i,j+2,k,l) &
                                     + stencil_adv_tr(i,NNN,j,k) * vec(i,j+3,k,l) )
          end do
        end do
      end do
   end do

!    vec(inu+1, :, :, 2)=0.d0
!    vec(:, jnv+1, :, 3)=0.d0
      

   return
end subroutine exp_matvec

