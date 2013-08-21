!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      SUBROUTINE ez_AMINMAX(MIN, MAX, FLD, NI, NJ)
      REAL MIN, MAX, FLD(NI,NJ)
      INTEGER NI, NJ
      REAL AMIN, AMAX
      MIN = AMIN(FLD, NI, NJ, 0)
      MAX = AMAX(FLD, NI, NJ, 0)
      RETURN
      END
   subroutine ez_applywgts(outfld, wts, idxs, infld, x, y, masque, ni_src, nj_src, ni_dst, nj_dst, n_wts)
   implicit none
   integer ni_src, nj_src, ni_dst, nj_dst, n_wts, n
   real :: x(ni_src,nj_src),y(ni_src,nj_src), dist, total_wgt
   real :: infld(ni_src*nj_src), outfld(ni_dst*nj_dst)
   real rmin, rmax
   integer i,j,k
   real :: wts(ni_dst, nj_dst, n_wts)
   integer :: idxs(ni_dst, nj_dst, n_wts), masque(ni_dst*nj_dst)
   integer :: ezgetval, ezgetopt, ier
   character(len=32) :: interopt, xtrapopt
   real xtrapval
   ier = ezgetopt('INTERP_DEGREE', interopt)
   ier = ezgetopt('EXTRAP_DEGREE', xtrapopt)
   ier = ezgetval('EXTRAP_VALUE', xtrapval)
!   print *, interopt, xtrapopt, xtrapval
   if (xtrapopt(1:5) == 'value') then
      ier = ezgetval('EXTRAP_VALUE', xtrapval)
      outfld = xtrapval
   else
      rmin = minval(infld)
      rmax = maxval(infld)
      rmin  = rmin - 0.1*(rmax-rmin)
      outfld = rmin
   endif
   do k=1,ni_dst*nj_dst
     i = mod((k-1),ni_dst) + 1
     j = 1+k/ni_dst
     if (masque(k) == 1) then
        outfld(k) = 0.0
        do n=1,n_wts
           if (idxs(i,j,n) < 1) exit
           outfld(k) = outfld(k)+wts(i,j,n)*infld(idxs(i,j,n))
        enddo
     endif
   enddo
   end subroutine ez_applywgts
   subroutine ez_avg(zout, xx, yy, ni_dst, nj_dst, zin, ni_src, nj_src, extension)
   implicit none
   integer udst
   integer ni_src, nj_src, ni_dst, nj_dst
   real, dimension(ni_src, nj_src) :: zin
   real, dimension(ni_dst, nj_dst) :: zout,xx,yy
   real, dimension(ni_dst) :: x, x_low, x_high
   real, dimension(nj_dst) :: y, y_low, y_high
   real, dimension(:,:), allocatable :: ztmp
   integer avg, extension
   integer low_bound_x, high_bound_x, low_bound_y, high_bound_y
   integer i, j, nix, njx, nkx, imid, jmid
   integer lcl_avg
   real dxcore, dycore, dxcoarse, dycoarse
   real total_area, xtile_min, ytile_min, xtile_max, ytile_max, xfrac, yfrac, area
   integer ez_cherche,ii,jj
   integer istart, iend, jstart, jend, compression_code, usr_datyp
   if (extension == 0) then
      low_bound_x = 1
      high_bound_x = ni_src
      allocate(ztmp(low_bound_x:high_bound_x, nj_src))
      do j=1,nj_src
         do i=1,ni_src
            ztmp(i,j) = zin(i,j)
         enddo
      enddo
   else if (extension == 1) then
      low_bound_x =  -ni_src
      high_bound_x = 2 * ni_src - 2
      allocate(ztmp(low_bound_x:high_bound_x, nj_src))
      do j=1,nj_src
         do i=1,ni_src-1
            ztmp(i,j) = zin(i,j)
            ztmp(1-ni_src+i,j) = zin(i,j)
            ztmp(ni_src-1+i,j) = zin(i,j)
         enddo
      enddo
   else if (extension == 2) then
      low_bound_x = 1 - ni_src
      high_bound_x = 2 * ni_src
      allocate(ztmp(low_bound_x:high_bound_x, nj_src))
      do j=1,nj_src
         do i=1,ni_src
            ztmp(i,j) = zin(i,j)
            ztmp(-ni_src+i,j) = zin(i,j)
            ztmp(ni_src+i,j) = zin(i,j)
         enddo
      enddo
   else
      print *, 'Invalid extension'
   endif
   x = xx(:,1)
   y = yy(1,:)
   if (x(1) > real(ni_src-1)) x(1) = 1.0
   x_low(1) = x(1)-0.5*(x(2)-x(1))
   do i=2,ni_dst
      x_low(i) = x(i)-0.5*(x(i)-x(i-1))
   enddo
   y_low(1) = max(1.0,y(1)-0.5*(y(2)-y(1)))
   do j=2,nj_dst
      y_low(j) = y(j)-0.5*(y(j)-y(j-1))
   enddo
   x_high(ni_dst) = x(ni_dst)+0.5*(x(ni_dst)-x(ni_dst-1))
   do i=1,ni_dst-1
      x_high(i) = x(i)+ 0.5*(x(i+1)-x(i))
   enddo
   y_high(nj_dst) = min(1.0*nj_src,y(nj_dst)+0.5*(y(nj_dst)-y(nj_dst-1)))
   do j=1,nj_dst-1
      y_high(j) = y(j)+0.5 * (y(j+1)-y(j))
   enddo
   do j=2,nj_dst-1
      jstart = int(y_low(j))
      jend   = nint(y_high(j))
      if ((0.5+real(jstart)) < y_low(j)) jstart = jstart + 1
!      if (real(jend) < y_high(j)) jend = jend + 1
      do i=1,ni_dst
         zout(i,j) = 0.0
         total_area = 0.0
         istart = int(x_low(i))
         iend   = nint(x_high(i))
         if ((0.5+real(istart)) < x_low(i)) istart = istart + 1
!         if (real(iend) < x_high(j)) iend = iend + 1
         do jj=jstart, jend
            ytile_min = real(jj)-0.5
            ytile_max = real(jj)+0.5
            yfrac = 1.0
            if (ytile_min < y_low(j)) then
               yfrac = ytile_max - y_low(j)
            endif
            if (ytile_max > y_high(j)) then
               yfrac = y_high(j) - ytile_min
            endif
            do ii=istart, iend
               xtile_min = real(ii)-0.5
               xtile_max = real(ii)+0.5
               xfrac = 1.0
               if (xtile_min < x_low(i)) then
                  xfrac = xtile_max - x_low(i)
               endif
               if (xtile_max > x_high(i)) then
                  xfrac = x_high(i) - xtile_min
               endif
               area =  xfrac*yfrac
               total_area = total_area + area
               zout(i,j) = zout(i,j) + ztmp(ii,jj) * area
            enddo
         enddo
         if (total_area /= 0.0) zout(i,j) = zout(i,j)/total_area
      enddo
   enddo
! Moyenne 1e rangee
  j = 1
  jstart = int(y_low(1))
  jend = nint(y_high(1))
  if (real(jstart) > y_low(1)) jstart = jstart - 1
  do i=1,ni_dst
      zout(i,j) = 0.0
      total_area = 0.0
      istart = int(x_low(i))
      iend   = nint(x_high(i))
      if ((0.5+real(istart)) < x_low(i)) istart = istart + 1
      if (real(iend) < x_high(i)) iend = iend + 1
      do jj=jstart, jend
         if (jj == 1) then
           ytile_min = 1.0
         else
           ytile_min = real(jj)-0.5
         endif
         ytile_max = real(jj)+0.5
         yfrac = ytile_max - ytile_min
         if (ytile_min < y_low(j)) then
            yfrac = ytile_max - y_low(j)
         endif
         if (ytile_max > y_high(j)) then
            yfrac = y_high(j) - ytile_min
         endif
         do ii=istart, iend
            xtile_min = real(ii)-0.5
            xtile_max = real(ii)+0.5
            xfrac = 1.0
            if (xtile_min < x_low(i)) then
               xfrac = xtile_max - x_low(i)
            endif
            if (xtile_max > x_high(i)) then
               xfrac = x_high(i) - xtile_min
            endif
            area =  xfrac*yfrac
            total_area = total_area + area
            zout(i,j) = zout(i,j) + ztmp(ii,jj) * area
         enddo
      enddo
         if (total_area /= 0.0) zout(i,j) = zout(i,j)/total_area
   enddo
! Moyenne rangee du haut
   j = nj_dst
   jstart = int(y(nj_dst))
   jend = nint(y_high(nj_dst))
   do i=1,ni_dst
      zout(i,j) = 0.0
      total_area = 0.0
      istart = int(x_low(i))
      if ((0.5+real(istart)) < x_low(i)) istart = istart + 1
      iend   = nint(x_high(i))
      do jj=jstart, jend
         if (jj == nj_src) then
            ytile_max = real(nj_src)
         else
            ytile_max = real(jj)+0.5
         endif
         ytile_min = real(jj)-0.5
         yfrac = ytile_max - ytile_min
         if (ytile_min < y_low(j)) then
            yfrac = ytile_max - y_low(j)
         endif
         if (ytile_max > y_high(j)) then
            yfrac = y_high(j) - ytile_min
         endif
         do ii=istart, iend
            xtile_min = real(ii)-0.5
            xtile_max = real(ii)+0.5
            xfrac = 1.0
            if (xtile_min < x_low(i)) then
               xfrac = xtile_max - x_low(i)
            endif
            if (xtile_max > x_high(i)) then
               xfrac = x_high(i) - xtile_min
            endif
            area =  xfrac*yfrac
            total_area = total_area + area
            zout(i,j) = zout(i,j) + ztmp(ii,jj) * area
         enddo
      enddo
         if (total_area /= 0.0) zout(i,j) = zout(i,j)/total_area
   enddo
! Moyenne 1e colonne
!    do j=1,nj_dst
!       jstart = int(y_low(j))
!       jend   = nint(y_high(j))
!       if (y_low(j) < 1.0) then
!         jstart = 1
!       endif
!       if (y_high(j) > (1.0*nj_dst)) then
!         jend = nj_src
!       endif
!       i=1
!       zout(i,j) = 0.0
!       total_area = 0.0
!       istart = int(x_low(1))
!       iend   = nint(x_high(1))
!       do jj=jstart, jend
!          if (jstart > 1) then
!             ytile_min = real(jj)-0.5
!          else
!             ytile_min = 1.0
!          endif
!          if (jend < nj_src) then
!             ytile_max = real(jj)+0.5
!          else
!            ytile_max = 1.0 * nj_src
!          endif
!          yfrac = ytile_max - ytile_min
!          if (ytile_min < y_low(j)) then
!             yfrac = ytile_max - y_low(j)
!          endif
!          if (ytile_max > y_high(j)) then
!             yfrac = y_high(j) - ytile_min
!          endif
!          do ii=istart, iend
!             if (ii == 1) then
!                xtile_min = 1.0
!             else
!                xtile_min = real(ii)-0.5
!             endif
!             xtile_max = real(ii)+0.5
!             xfrac = xtile_max - xtile_min
!             if (xtile_max > x_high(i)) then
!                xfrac = x_high(i) - xtile_min
!             endif
!             area =  xfrac*yfrac
!             total_area = total_area + area
!             zout(i,j) = zout(i,j) + ztmp(ii,jj) * area
!          enddo
!       enddo
!       if (total_area /= 0.0) zout(i,j) = zout(i,j)/total_area
!    enddo
! Moyenne derniere colonne
!    do j=1,nj_dst
!       jstart = int(y_low(j))
!       jend   = nint(y_high(j))
!       if (j == 1) then
!          jstart = 1
!       endif
!       if (j == nj_dst) then
!          jend = nj_src
!       endif
!       i=ni_dst
!       zout(i,j) = 0.0
!       total_area = 0.0
!       istart = int(x_low(ni_dst))
!       iend   = nint(x_high(ni_dst))
!       do jj=jstart, jend
!          if (jstart > 1) then
!             ytile_min = real(jj)-0.5
!          else
!             ytile_min = 1.0
!          endif
!          if (jend < nj_src) then
!             ytile_max = real(jj)+0.5
!          else
!             ytile_max = real(nj_src)
!          endif
!          yfrac = ytile_max - ytile_min
!          if (ytile_min < y_low(j)) then
!             yfrac = ytile_max - y_low(j)
!          endif
!          if (ytile_max > y_high(j)) then
!              yfrac = y_high(j) - ytile_min
!          endif
!          do ii=istart, iend
!             xtile_min = real(ii)-0.5
!             if (ii == ni_src) then
!                xtile_max = real(ni_src)
!             else
!                xtile_max = real(ii)+0.5
!             endif
!             xfrac = xtile_max - xtile_min
!             if (xtile_max > x_high(i)) then
!                xfrac = x_high(i) - xtile_min
!             endif
!             area =  xfrac*yfrac
!             total_area = total_area + area
!             zout(i,j) = zout(i,j) + ztmp(ii,jj) * area
!             enddo
!          enddo
!          if (total_area /= 0.0) zout(i,j) = zout(i,j)/total_area
!    enddo
   deallocate(ztmp)
   return
  end
   subroutine ez_avg_sph(zout, xx, yy, lats_dst, ni_dst, nj_dst, zin, ni_src, nj_src,extension)
   implicit none
   integer udst, extension
   integer ni_src, nj_src, ni_dst, nj_dst
   real, dimension(ni_src, nj_src) :: zin
   real, dimension(ni_dst, nj_dst) :: zout,xx,yy
   real, dimension(nj_dst) :: lats_dst
   integer avg
   real, parameter :: degre_a_radian = 0.017453295199
   real, dimension(:), allocatable :: x, y, y_low, y_high, amplif_lats
   real, dimension(:,:), allocatable :: ztmp, x_low, x_high
   integer i, j, nix, njx, nkx, imid, jmid
   integer lcl_avg
   real dxcore, dycore, dxcoarse, dycoarse
   real total_area, xtile_min, ytile_min, xtile_max, ytile_max, xfrac, yfrac, area
   integer ez_cherche,ii,jj
   integer istart, iend, jstart, jend, compression_code, usr_datyp
   integer low_bound_x, high_bound_x, low_bound_y, high_bound_y
   if (extension == 0) then
      low_bound_x = 1
      high_bound_x = ni_src
      allocate(ztmp(low_bound_x:high_bound_x, nj_src))
      do j=1,nj_src
         do i=1,ni_src
            ztmp(i,j) = zin(i,j)
         enddo
      enddo
   else if (extension == 1) then
      low_bound_x =  -ni_src
      high_bound_x = 2 * ni_src - 2
      allocate(ztmp(low_bound_x:high_bound_x, nj_src))
      do j=1,nj_src
         do i=1,ni_src-1
            ztmp(i,j) = zin(i,j)
            ztmp(1-ni_src+i,j) = zin(i,j)
            ztmp(ni_src-1+i,j) = zin(i,j)
         enddo
      enddo
   else if (extension == 2) then
      low_bound_x = 1 - ni_src
      high_bound_x = 2 * ni_src
      allocate(ztmp(low_bound_x:high_bound_x, nj_src))
      do j=1,nj_src
         do i=1,ni_src
            ztmp(i,j) = zin(i,j)
            ztmp(-ni_src+i,j) = zin(i,j)
            ztmp(ni_src+i,j) = zin(i,j)
         enddo
      enddo
   else
      print *, 'Invalid extension'
   endif
   allocate(x_low(ni_dst,nj_dst))
   allocate(y_low(nj_dst))
   allocate(x_high(ni_dst,nj_dst))
   allocate(y_high(nj_dst))
   allocate(x(ni_dst),y(nj_dst),amplif_lats(nj_dst))
   do j=1,nj_dst
      amplif_lats(j) = 1.0/cos(lats_dst(j)*degre_a_radian)
   enddo
   x = xx(:,1)
   y = yy(1,:)
   if (x(1) > real(ni_src-1)) x(1) = 1.0
   do j=1,nj_dst
      x_low(1,j) = x(1)-0.5*(x(2)-x(1))*amplif_lats(j)
      do i=2,ni_dst
         x_low(i,j) = x(i)-0.5*(x(i)-x(i-1))*amplif_lats(j)
      enddo
   enddo
   y_low(1) = max(1.0,y(1)-0.5*(y(2)-y(1)))
   do j=2,nj_dst
      y_low(j) =y(j)- 0.5*(y(j)-y(j-1))
   enddo
   do j=1,nj_dst
      x_high(ni_dst,j) = x(ni_dst)+0.5*(x(ni_dst)-x(ni_dst-1))*amplif_lats(j)
      do i=1,ni_dst-1
         x_high(i,j) = x(i)+0.5 * (x(i+1)-x(i))*amplif_lats(j)
      enddo
   enddo
   y_high(nj_dst) = min(1.0*nj_src,y(nj_dst)+0.5*(y(nj_dst)-y(nj_dst-1)))
   do j=1,nj_dst-1
      y_high(j) = y(j)+0.5 * (y(j+1)-y(j))
   enddo
   do j=2,nj_dst-1
      jstart = int(y_low(j))
      jend   = nint(y_high(j))
      if ((0.5+real(jstart)) < y_low(j)) jstart = jstart + 1
!      if (real(jend) < y_high(j)) jend = jend + 1
      do i=1,ni_dst
         zout(i,j) = 0.0
         total_area = 0.0
         istart = int(x_low(i,j))
         iend   = nint(x_high(i,j))
         if ((0.5+real(istart)) < x_low(i,j)) istart = istart + 1
!         if (real(iend) < x_high(j)) iend = iend + 1
         do jj=jstart, jend
            ytile_min = real(jj)-0.5
            ytile_max = real(jj)+0.5
            yfrac = 1.0
            if (ytile_min < y_low(j)) then
               yfrac = ytile_max - y_low(j)
            endif
            if (ytile_max > y_high(j)) then
               yfrac = y_high(j) - ytile_min
            endif
            do ii=istart, iend
               xtile_min = real(ii)-0.5
               xtile_max = real(ii)+0.5
               xfrac = 1.0
               if (xtile_min < x_low(i,j)) then
                  xfrac = xtile_max - x_low(i,j)
               endif
               if (xtile_max > x_high(i,j)) then
                  xfrac = x_high(i,j) - xtile_min
               endif
               area =  xfrac*yfrac
               total_area = total_area + area
               zout(i,j) = zout(i,j) + ztmp(ii,jj) * area
            enddo
         enddo
         if (total_area /= 0.0) zout(i,j) = zout(i,j)/total_area
      enddo
   enddo
! Moyenne 1e rangee
  j = 1
  jstart = int(y_low(1))
  jend = nint(y_high(1))
  if (real(jstart) > y_low(1)) jstart = jstart - 1
  do i=1,ni_dst
      zout(i,j) = 0.0
      total_area = 0.0
      istart = int(x_low(i,j))
      iend   = nint(x_high(i,j))
      if ((0.5+real(istart)) < x_low(i,j)) istart = istart + 1
      if (real(iend) < x_high(i,j)) iend = iend + 1
      do jj=jstart, jend
         if (jj == 1) then
           ytile_min = 1.0
         else
           ytile_min = real(jj)-0.5
         endif
         ytile_max = real(jj)+0.5
         yfrac = ytile_max - ytile_min
         if (ytile_min < y_low(j)) then
            yfrac = ytile_max - y_low(j)
         endif
         if (ytile_max > y_high(j)) then
            yfrac = y_high(j) - ytile_min
         endif
         do ii=istart, iend
            xtile_min = real(ii)-0.5
            xtile_max = real(ii)+0.5
            xfrac = 1.0
            if (xtile_min < x_low(i,j)) then
               xfrac = xtile_max - x_low(i,j)
            endif
            if (xtile_max > x_high(i,j)) then
               xfrac = x_high(i,j) - xtile_min
            endif
            area =  xfrac*yfrac
            total_area = total_area + area
            zout(i,j) = zout(i,j) + ztmp(ii,jj) * area
         enddo
      enddo
         if (total_area /= 0.0) zout(i,j) = zout(i,j)/total_area
   enddo
! Moyenne rangee du haut
   j = nj_dst
   jstart = int(y(nj_dst))
   jend = nint(y_high(nj_dst))
   do i=1,ni_dst
      zout(i,j) = 0.0
      total_area = 0.0
      istart = int(x_low(i,j))
      if ((0.5+real(istart)) < x_low(i,j)) istart = istart + 1
      iend   = nint(x_high(i,j))
      do jj=jstart, jend
         if (jj == nj_src) then
            ytile_max = real(nj_src)
         else
            ytile_max = real(jj)+0.5
         endif
         ytile_min = real(jj)-0.5
         yfrac = ytile_max - ytile_min
         if (ytile_min < y_low(j)) then
            yfrac = ytile_max - y_low(j)
         endif
         if (ytile_max > y_high(j)) then
            yfrac = y_high(j) - ytile_min
         endif
         do ii=istart, iend
            xtile_min = real(ii)-0.5
            xtile_max = real(ii)+0.5
            xfrac = 1.0
            if (xtile_min < x_low(i,j)) then
               xfrac = xtile_max - x_low(i,j)
            endif
            if (xtile_max > x_high(i,j)) then
               xfrac = x_high(i,j) - xtile_min
            endif
            area =  xfrac*yfrac
            total_area = total_area + area
            zout(i,j) = zout(i,j) + ztmp(ii,jj) * area
         enddo
      enddo
         if (total_area /= 0.0) zout(i,j) = zout(i,j)/total_area
   enddo
   deallocate(x_low, y_low, x_high, y_high, x, y, amplif_lats, ztmp)
   return
  end
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!**s/r qqqcal - compute spherical coordinates from cartesian coordinates
!
      subroutine ez_cal( lon,lat,xyz,n)
      implicit none
      integer n
      real xyz(3,n),lon(n),lat(n)
!
!author michel roch - april 1990
!
!revision
!       001 - michel roch - feb 94 - repackaging for integration in rmnlib
!
!*
!
      integer i
      real rad
!
      rad = 180./acos(-1.00)
!
         do 30 i=1,n
            lat(i) =asin(max(-1.00,min(1.0,xyz(3,i)))) * rad
            lon(i) =atan2( xyz(2,i),xyz(1,i) ) * rad
            lon(i) =amod( lon(i) , 360.0 )
            if(lon(i).lt.0.0) lon(i)=lon(i)+360.0
 30      continue
!
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_calcpolemodule(polemod, uu, vv, ni, ax, grtyp, grref)
      implicit none
      integer ni
      real polemod,uu(ni),vv(ni), ax(ni), module
      character*1 grtyp, grref
      integer i
      if (grtyp.eq.'Z'.and.grref.eq.'E') then
         polemod = 0.0
         do i=1,ni-1
            module = sqrt(uu(i)*uu(i)+vv(i)*vv(i))
            polemod = polemod + module*(ax(i+1)-ax(i))
         enddo
         polemod = polemod / 360.0
         return
      endif
      polemod = 0.0
      do i=1,ni
         polemod = polemod + sqrt(uu(i)*uu(i)+vv(i)*vv(i))
      enddo
      polemod = polemod / (1.0 * ni)
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_calcpoleval(poleval, z, ni, ax, grtyp, grref)
      implicit none
      integer ni, wrap
      real poleval,z(ni),ax(ni)
      character*1 grtyp, grref
      integer i
      if (grtyp.eq.'Z'.and.grref.eq.'E') then
         poleval = 0.0
         do i=1,ni-1
            poleval = poleval + z(i)*(ax(i+1)-ax(i))
         enddo
         poleval = poleval / 360.0
         return
      endif
      poleval = 0.0
      do i=1,ni
         poleval = poleval + z(i)
      enddo
      poleval = poleval / (1.0 * ni)
      return
      end
   subroutine ez_calcxy_y(wts, idxs, x, y, gdout_lat, gdout_lon, gdin_lat, gdin_lon, masque, ni_src, nj_src, ni_dst, nj_dst, num_wt&
     &s)
   implicit none
   integer ni_src, nj_src, ni_dst, nj_dst, num_wts
   real :: gdin_lon(ni_src,nj_src),gdin_lat(ni_src,nj_src), dist, total_wgt
   real :: gdout_lon(ni_dst, nj_dst), gdout_lat(ni_dst, nj_dst)
   real, dimension(:,:,:) :: wts(ni_dst, nj_dst, num_wts), x(ni_dst, nj_dst), y(ni_dst, nj_dst)
   real, dimension(:) :: tmpwgts(num_wts)
   integer :: idxs(ni_dst, nj_dst, num_wts), masque(ni_dst,nj_dst)
   integer :: tmp_idxs(16,2)
   real rx, ry
   integer i,j,k,ii,jj,iz
   real lon_min, lon_max, lat_min, lat_max
   integer valmax, locmax
   integer i1, j1, k1, exception
   logical point_found
   lon_min = minval(gdin_lon)
   lon_max = maxval(gdin_lon)
   lat_min = minval(gdin_lat)
   lat_max = maxval(gdin_lat)
   masque = 1
   where (gdout_lon < lon_min) masque = 0
   where (gdout_lon > lon_max) masque = 0
   where (gdout_lat < lat_min) masque = 0
   where (gdout_lat > lat_max) masque = 0
   wts = 1.0e30
   idxs = 0
   do j=1,nj_dst
      do i=1,ni_dst
         k = i + (j-1) * ni_dst
         if (masque(i,j) /= 0) then
            tmpwgts = 1.0e30
            locmax=1
            tmp_idxs = 0
            point_found = .false.
            do jj=1,nj_src
               do ii=1,ni_src
                  rx = (gdout_lon(i,j)- gdin_lon(ii,jj))
                  ry = (gdout_lat(i,j)- gdin_lat(ii,jj))
                  if (abs(rx) > 180.0) then
                     rx = abs(abs(rx) - 360.0)
                  endif
                 if (abs(ry) < 10.0 .and. abs(rx) < 10.0) then
                     dist = rx*rx + ry*ry
                     point_found = .true.
                     if (dist < tmpwgts(locmax)) then
                        tmpwgts(locmax) = dist
                        idxs(i,j,locmax) = ii + (jj-1)*ni_src
                        tmp_idxs(locmax, 1) = ii
                        tmp_idxs(locmax, 2) = jj
                        locmax = maxloc(tmpwgts, 1)
                    endif
                 endif
               enddo
            enddo
            if (.not.point_found) then
               masque(i,j) = 0
            endif
!          if (masque(i,j) == 1) then
!             call inside_or_outside(masque(i,j),x(i,j), y(i,j), gdout_lat(i,j),gdout_lon(i,j), gdin_lat, &
!                                     gdin_lon, ni_src, nj_src, tmpwgts, tmp_idxs, num_wts)
!          endif
            if (masque(i,j) == 1) then
               do iz=1,num_wts
                  wts(i,j,iz) = tmpwgts(iz)
               enddo
               if (wts(i,j,1) > 6371000.0) then
                  wts(i,j,1:num_wts) = 1.0e30
                  masque(i,j) = 0
               else
                  total_wgt = 0.0
                  do iz=1,num_wts
                     if (wts(i,j,iz) < 1.0e-10) then
                        print *, 'wts(i,j,iz) < 1.0e-10',i,j,iz,wts(i,j,iz)
                     endif
                     wts(i,j,iz) = max(wts(i,j,iz), 1.0e-10)
                     wts(i,j,iz) = 1.0 / wts(i,j,iz)
                  enddo
                  do iz=1,num_wts
                     total_wgt = total_wgt + wts(i,j,iz)
                  enddo
                  do iz=1,num_wts
                     wts(i,j,iz) = wts(i,j,iz) / total_wgt
                  enddo
               endif
            endif
         endif
      enddo
   enddo
   return
   end subroutine ez_calcxy_y
   subroutine ez_calcxy_y_m(wts, idxs, x, y, gdout_lat, gdout_lon, gdout_masque, gdin_lat, gdin_lon, gdin_masque, ni_src, nj_src, n&
     &i_dst, nj_dst, num_wts)
   implicit none
   integer ni_src, nj_src, ni_dst, nj_dst, num_wts
   real :: gdin_lon(ni_src,nj_src),gdin_lat(ni_src,nj_src), dist, total_wgt
   real :: gdout_lon(ni_dst, nj_dst), gdout_lat(ni_dst, nj_dst)
   real, dimension(:,:,:) :: wts(ni_dst, nj_dst, num_wts), x(ni_dst, nj_dst), y(ni_dst, nj_dst)
   real, dimension(:) :: tmpwgts(num_wts)
   integer :: idxs(ni_dst, nj_dst, num_wts), gdout_masque(ni_dst,nj_dst), gdin_masque(ni_src, nj_src)
   integer :: tmp_idxs(num_wts,2)
   real rx, ry
   integer i,j,k,ii,jj,iz
   real lon_min, lon_max, lat_min, lat_max
   integer valmax, locmax
   integer ier, i1, j1, k1, exception
   logical point_found
   integer ezgetval
   external ezgetval
   real dist_thresh
   lon_min = minval(gdin_lon)
   lon_max = maxval(gdin_lon)
   lat_min = minval(gdin_lat)
   lat_max = maxval(gdin_lat)
   gdout_masque = 1
   where (gdout_lon < lon_min) gdout_masque = 0
   where (gdout_lon > lon_max) gdout_masque = 0
   where (gdout_lat < lat_min) gdout_masque = 0
   where (gdout_lat > lat_max) gdout_masque = 0
   wts = 1.0e30
   idxs = 0
   ier = ezgetval('missing_distance_threshold', dist_thresh)
   do j=1,nj_dst
      do i=1,ni_dst
         k = i + (j-1) * ni_dst
         if (gdout_masque(i,j) /= 0) then
            tmpwgts = 1.0e30
            locmax=1
            tmp_idxs = 0
            point_found = .false.
            do jj=1,nj_src
               do ii=1,ni_src
                  if (gdin_masque(ii,jj) == 1) then
                     rx = (gdout_lon(i,j)- gdin_lon(ii,jj))
                     ry = (gdout_lat(i,j)- gdin_lat(ii,jj))
                     if (abs(rx) > 180.0) then
                        rx = abs(abs(rx) - 360.0)
                     endif
                     if (abs(ry) < dist_thresh .and. abs(rx) < dist_thresh) then
                        dist = rx*rx + ry*ry
                        point_found = .true.
                        if (dist < tmpwgts(locmax)) then
                           tmpwgts(locmax) = dist
                           idxs(i,j,locmax) = ii + (jj-1)*ni_src
                           tmp_idxs(locmax, 1) = ii
                           tmp_idxs(locmax, 2) = jj
                           locmax = maxloc(tmpwgts, 1)
                        endif
                     endif
                  endif
               enddo
            enddo
            if (.not.point_found) then
               gdout_masque(i,j) = 0
            endif
            if (gdout_masque(i,j) == 1) then
               call inside_or_outside(gdout_masque(i,j),x(i,j), y(i,j), gdout_lat(i,j),gdout_lon(i,j), gdin_lat, &
                                       gdin_lon, ni_src, nj_src, tmpwgts, tmp_idxs, num_wts)
            endif
            if (gdout_masque(i,j) == 1) then
               do iz=1,num_wts
                  wts(i,j,iz) = tmpwgts(iz)
               enddo
               if (wts(i,j,1) > 6371000.0) then
                  wts(i,j,1:num_wts) = 1.0e30
                  gdout_masque(i,j) = 0
               else
                  total_wgt = 0.0
                  do iz=1,num_wts
                     if (wts(i,j,iz) < 1.0e-10) then
                        print *, 'wts(i,j,iz) < 1.0e-10',i,j,iz,wts(i,j,iz)
                     endif
                     wts(i,j,iz) = max(wts(i,j,iz), 1.0e-10)
                     wts(i,j,iz) = 1.0 / wts(i,j,iz)
                  enddo
                  do iz=1,num_wts
                     total_wgt = total_wgt + wts(i,j,iz)
                  enddo
                  do iz=1,num_wts
                     wts(i,j,iz) = wts(i,j,iz) / total_wgt
                  enddo
               endif
            endif
         endif
      enddo
   enddo
   return
   end subroutine ez_calcxy_y_m
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!**S/R ez_CARTAUV - compute the components of the winds in the rotated system of
!                coordinates from the winds in the rotated cartesian space
!
      SUBROUTINE ez_CARTAUV(U, V, UVCART, LON, LAT, NI, NJ)
      implicit none
      INTEGER NI, NJ
      REAL    UVCART(3,NI*NJ), U(NI,NJ), V(NI,NJ), LON(NI,nj), LAT(ni,NJ)
!
!author michel roch - april 90
!
!arguments
!    out    U       - rotated component  U of the wind
!           V       - rotated component  V of the wind
!    in     xyz     - rotated winds in cartesian space
!           LON     - longitudes of the grid in the rotated system of coordinates
!           LAT     - latitudes of the grid in the rotated system of coordinates
!           NI      - E-W DIMENSION of the grid
!           NJ      - N-S dimension of the grid
!
!*
      INTEGER I, J, K
      REAL*8    A, B, C, D, E, F, DAR
      DAR = ACOS(-1.)/180.
      K   = 0
      DO 20 J=1,NJ
         DO 10 I=1,NI
            K      = K+1
            A      = COS(DAR*LON(I,j))
            B      = SIN(DAR*LON(I,j))
            E      = COS(DAR*LAT(i,J))
            F      = SIN(DAR*LAT(i,J))
            U(I,J) = (UVCART(2,K)*A) - (UVCART(1,K)*B)
            C      = (UVCART(1,K)*A) + (UVCART(2,K)*B)
            D      = SQRT(C**2 + UVCART(3,K)**2 )
            V(I,J) = SIGN(D, (UVCART(3,K)*E)-(C*F))
 10         CONTINUE
 20      CONTINUE
      RETURN
      END
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!**function cherche
!
	integer function ez_cherche(val, tableau, nbelem)
      implicit none
	real val
	integer nbelem
	real tableau(nbelem)
!
!AUTHOR     Yves Chartier               Jan 1991
!
!REVISION
!     Documentation update              Jul 1994      
!
!LANGUAGE   Fortran 77
!
!OBJECT (cherche)
!     Return the index of the table element equal or closest to "val",
!     using binary search. It is assumed that the table is sorted in ascending
!     order.
!
!FILES
!
!ARGUMENTS
! NAMES          I/O  TYPE  A/S        DESCRIPTION
! val             O    R     S         value to search for
! tableau         I    R     A         table of lookup values
! nbelem          I    I     S         length of table
!
!IMPLICIT
!
!MODULES
!
!*
	integer debut, milieu, fin
	debut = 1
	fin = nbelem
	milieu = (debut+fin)*0.5
10	if (milieu.ne.debut) then
          if (val.le.tableau(milieu)) then
	      fin   = milieu
          else
	      debut = milieu
	   endif
          milieu = (debut+fin)*0.5
          goto 10
	endif
	ez_cherche = milieu
	return
	end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_corrbgd(zout, ni,nj, hem)
      implicit none
      integer ni,nj,hem
      real zout(ni,nj)
      integer i,j
      real moyenne, somme
      if (hem.eq.0.or.hem.eq.2) then
         somme = 0.
         do i=1,ni
            somme = somme + zout(i,1)
         enddo
         moyenne = somme / (ni *1.0)
         do i=1,ni
            zout(i,1) = moyenne
         enddo
      endif
      if (hem.eq.0.or.hem.eq.1) then
         somme = 0.
         do i=1,ni
            somme = somme + zout(i,nj)
         enddo
         moyenne = somme / (ni *1.0)
         do i=1,ni
            zout(i,nj) = moyenne
         enddo
      endif
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!**s/r ez_crot - compute the rotation matrix that allows the transformation
!             from the spherical coordinate system to the rotated spherical
!             coordinate system.
!
      subroutine ez_crot( r, ri, lon1, lat1, lon2, lat2 )
      implicit none
      real r(3,3), ri(3,3)
      real lon1,lat1,lon2,lat2
!
!author michel roch - april 1990
!
!revision
!	001 - michel roch - feb 94 - repackaging for integration in rmnlib
!
!arguments
!    out    r       - rotation matrix from true to rotated coordinate system
!           ri      - rotation matrix from rotated to true coordinate system
!    in     lon1    - longitude on the unrotated coordinate system corresponding to
!                     the point (lat,lon)=(0,180) of the rotated coordinate system
!           lat1    - latitude on the unrotated coordinate system corresponding to
!                     the point (lat,lon)=(0,180) of the rotated coordinate system
!           lon2    - longitude on the unrotated coordinate system corresponding to
!                     a point (lat,lon) located on the equator of the rotated 
!                     coordinate system
!           lat2    - latitude on the unrotated coordinate system corresponding to
!                     a point (lat,lon) located on the equator of the rotated 
!                     coordinate system
!
!
!modules
      external ez_lac
!
!*
      integer i,j
      real a,b,c,d
      real xyz1(3),xyz2(3)
!
!     calcul des coordonnees cartesiennes des points r1 et r2
!
      call ez_lac(xyz1,lon1,lat1,1)
      call ez_lac(xyz2,lon2,lat2,1)
!
!     calcul de a=cos(alpha)
!
      a=(xyz1(1)*xyz2(1)) + (xyz1(2)*xyz2(2)) + (xyz1(3)*xyz2(3))
!
!     calcul de b=sin(alpha)
!
      b=sqrt(  ( (xyz1(2)*xyz2(3)) - (xyz2(2)*xyz1(3)) )**2 +          ( (xyz2(1)*xyz1(3)) - (xyz1(1)*xyz2(3)) )**2 +          ( (x&
     &yz1(1)*xyz2(2)) - (xyz2(1)*xyz1(2)) )**2  )
!
!     calcul de c=norm(-r1)
!
      c=sqrt( xyz1(1)**2 + xyz1(2)**2 + xyz1(3)**2 )
!
!     calcul de d=norm(r4)
!
      d=sqrt(  ( ( (a*xyz1(1)) - xyz2(1) ) / b )**2 +          ( ( (a*xyz1(2)) - xyz2(2) ) / b )**2 +          ( ( (a*xyz1(3)) - xy&
     &z2(3) ) / b )**2  )
!
      r(1,1)=     -xyz1(1)/c
      r(1,2)=     -xyz1(2)/c
      r(1,3)=     -xyz1(3)/c
      r(2,1)=  ( ((a*xyz1(1)) - xyz2(1)) / b)/d
      r(2,2)=  ( ((a*xyz1(2)) - xyz2(2)) / b)/d
      r(2,3)=  ( ((a*xyz1(3)) - xyz2(3)) / b)/d
      r(3,1)=  ( ( xyz1(2)*xyz2(3)) - (xyz2(2)*xyz1(3)) ) / b
      r(3,2)=  ( ( xyz2(1)*xyz1(3)) - (xyz1(1)*xyz2(3)) ) / b
      r(3,3)=  ( ( xyz1(1)*xyz2(2)) - (xyz2(1)*xyz1(2)) ) / b
!
!     matrice de rotation inverse
!
      do 10 i=1,3
      do 10 j=1,3
 10      ri(i,j)=r(j,i)
!
      return
      end
  subroutine ez_crot_2010( r, ri, Grd_xlon1, Grd_xlat1, Grd_xlon2, Grd_xlat2 )
  implicit none
  integer i,j
  real r(3,3), ri(3,3)
  real Grd_xlat1, Grd_xlon1,Grd_xlon2, Grd_xlat2
  real*8 Grd_rot_8(3,3),xyz1_8(3),xyz2_8(3),xyz3_8(3),b_8
  real   xyz1(3), xyz2(3)
!   Grd_xlat1 = 45.
!   Grd_xlon1 = 0.
!   Grd_xlat2 = 0.5
!   Grd_xlon2 = 90.5
!  Grd_xlon1 = Grd_xlon1 + 180.0
!  Grd_xlon2 = Grd_xlon2 + 180.0
  call ez_crot(r, ri, Grd_xlon1, Grd_xlat1, Grd_xlon2, Grd_xlat2)
  print *, 'r et ri original'
  print *, '-- r  ----------'
  print *, r
  print *, '-- ri ----------'
  print *, ri
  print *, '################'
  call ez_lac_8 ( xyz1_8, Grd_xlon1, Grd_xlat1, 1 )
  call ez_lac_8 ( xyz2_8, Grd_xlon2, Grd_xlat2, 1 )
  xyz3_8=xyz2_8
!    Compute r_3= r_1Xr_2 normalized (New North Pole)
  xyz3_8(1)=xyz1_8(2)*xyz2_8(3)-xyz1_8(3)*xyz2_8(2)
  xyz3_8(2)=xyz1_8(3)*xyz2_8(1)-xyz1_8(1)*xyz2_8(3)
  xyz3_8(3)=xyz1_8(1)*xyz2_8(2)-xyz1_8(2)*xyz2_8(1)
! Normalize
  b_8=sqrt(xyz3_8(1)**2 +xyz3_8(2)**2+xyz3_8(3)**2)
  xyz3_8(1)=xyz3_8(1)/b_8
  xyz3_8(2)=xyz3_8(2)/b_8
  xyz3_8(3)=xyz3_8(3)/b_8
!    Compute r_2= r_3Xr_1
  xyz2_8(1)=xyz3_8(2)*xyz1_8(3)-xyz3_8(3)*xyz1_8(2)
  xyz2_8(2)=xyz3_8(3)*xyz1_8(1)-xyz3_8(1)*xyz1_8(3)
  xyz2_8(3)=xyz3_8(1)*xyz1_8(2)-xyz3_8(2)*xyz1_8(1)
!  Now r_1, r_2 and r_3 are the rotated axes
  Grd_rot_8(1,1)=xyz1_8(1)
  Grd_rot_8(1,2)=xyz1_8(2)
  Grd_rot_8(1,3)=xyz1_8(3)
  Grd_rot_8(2,1)=xyz2_8(1)
  Grd_rot_8(2,2)=xyz2_8(2)
  Grd_rot_8(2,3)=xyz2_8(3)
  Grd_rot_8(3,1)=xyz3_8(1)
  Grd_rot_8(3,2)=xyz3_8(2)
  Grd_rot_8(3,3)=xyz3_8(3)
  r = real(Grd_rot_8)
  print *, Grd_xlon1, Grd_xlat1, Grd_xlon2, Grd_xlat2
  print *, Grd_rot_8
  print *, '--------------------'
  print *, r
  do j=1,3
    do i=1,3
      ri(i,j) = r(j,i)
    enddo
  enddo
  print *, '--------------------'
  print *, ri
  return
  end subroutine ez_crot_2010
!    program testlat
!    real lons(2,2), lats(2,2)
!    real uv, lat, lon
!
!    integer pt_in_quad, res
!
!    lons(1,1) = 285.0
!    lons(2,1) = 288.0
!    lons(2,2) = 287.0
!    lons(1,2) = 286.0
!
!    lats(1,1) = 45.0
!    lats(2,1) = 43.0
!    lats(2,2) = 48.0
!    lats(1,2) = 47.0
!
!    lat = 45.5
!    lon = 285.5
!
!    res = pt_in_quad(lon,lat,lons(1,1),lats(1,1),lons(2,1), lats(2,1), &
!                      lons(1,2),lats(1,2),lons(2,2), lats(2,2))
!
!
!    call ez_uvfllc2d(u,v,lat, lon, lats, lons)
!    print *, u,v,lat, lon,lats, lons
!    print *
!    print *,'res = ', res
!
!    lat = lats(1,1)
!    lon = lons(1,1)
!    call ez_uvfllc2d(u,v,lat, lon, lats, lons)
!    print *, u,v,lat, lon,lats, lons
!    res = pt_in_quad(lon,lat,lons(1,1),lats(1,1),lons(2,1), lats(2,1), &
!                      lons(1,2),lats(1,2),lons(2,2), lats(2,2))
!    print *,'res = ', res
!    print *
!
!    lat = lats(2,1)
!    lon = lons(2,1)
!    call ez_uvfllc2d(u,v,lat, lon, lats, lons)
!    print *, u,v,lat, lon,lats, lons
!    res = pt_in_quad(lon,lat,lons(1,1),lats(1,1),lons(2,1), lats(2,1), &
!                      lons(1,2),lats(1,2),lons(2,2), lats(2,2))
!    print *,'res = ', res
!    print *
!
!    lat = lats(1,2)
!    lon = lons(1,2)
!    call ez_uvfllc2d(u,v,lat, lon, lats, lons)
!    print *, u,v,lat, lon,lats, lons
!    res = pt_in_quad(lon,lat,lons(1,1),lats(1,1),lons(2,1), lats(2,1), &
!                      lons(1,2),lats(1,2),lons(2,2), lats(2,2))
!    print *,'res = ', res
!    print *
!
!    lat = lats(2,2)
!    lon = lons(2,2)
!    call ez_uvfllc2d(u,v,lat, lon, lats, lons)
!    print *, u,v,lat, lon,lats, lons
!    res = pt_in_quad(lon,lat,lons(1,1),lats(1,1),lons(2,1), lats(2,1), &
!                      lons(1,2),lats(1,2),lons(2,2), lats(2,2))
!    print *,'res = ', res
!    print *
!
!    stop
!    end program testlat
   subroutine ez_uvfllc2d(u, v, x, y, x0, y0, x1, y1, x2, y2, x3, y3)
   implicit none
   real :: u, v, lat, lon, x, y
   real a,b,c,d,e, f, g,h, i
   real dx1, dx2, dy1, dy2, som_x ,som_y
   real x0, y0, x1, y1, x2, y2, x3, y3, q
   real, dimension(3, 3) ::  invmat
   som_x = x0 - x1 + x2 - x3
   som_y = y0 - y1 + y2 - y3
   dx1 = x1 - x2
   dx2 = x3 - x2
   dy1 = y1 - y2
   dy2 = y3 - y2
   g = (dy2*som_x-som_y*dx2)/(dx1*dy2-dx2*dy1)
   h = (dx1*som_y-som_x*dy1)/(dx1*dy2-dx2*dy1)
   i = 1.0
   a = x1 - x0 + g * x1
   b = x3 - x0 + h * x3
   c = x0
   d = y1 - y0 + g * y1
   e = y3 - y0 + h * y3
   f = y0
   invmat(1,3) = e * i - f * h
   invmat(2,3) = c * h - b * i
   invmat(3,3) = b * f - c * e
   invmat(1,2) = f * g - d * i
   invmat(2,2) = a * i - c * g
   invmat(3,2) = c * d - a * f
   invmat(1,1) = d * h - e * g
   invmat(2,1) = b * g - a * h
   invmat(3,1) = a * e - b * d
   q = invmat(1,1) * x + invmat(2,1) * y + invmat(3,1)
   if (q /= 0.0) then
      u = (invmat(1,3) * x + invmat(2,3) * y + invmat(3,3)) / q
      v = (invmat(1,2) * x + invmat(2,2) * y + invmat(3,2)) / q
      if (abs(u) < 0.01) u = abs(u)
      if (abs(v) < 0.01) v = abs(u)
   else
      u = -1.0
      v = -1.0
   endif
   end subroutine ez_uvfllc2d
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_fillaxzx(axdst, ax, ni, i1, i2)
      integer ni, i1, i2
      real axdst(i1:i2), ax(ni)
      integer i
      print *, ni, i1, i2
      do i=1,ni
         axdst(i) = ax(i)
         print *, axdst(i), ax(i)
      enddo
      axdst(0) = ax(ni-1) - 360.0
      axdst(ni)=ax(1)+360.0
      axdst(ni+1)=ax(2)+360.0
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_fillayzs(aydst, ay, nj, j1, j2)
      integer nj, j1, j2
      real aydst(j1:j2), ay(nj)
      aydst(-1) = -180.0 - ay(2)
      aydst(0)  = -180.0 - ay(1)
      aydst(1)  = ay(1)
      aydst(2)  = ay(2)
      aydst(3)  = ay(3)
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_fillnpole(zout, zin, ni, j1, j2, valpole)
      implicit none
      integer ni,j,i,j1, j2
      real zout(ni, 4), zin(ni,j1:j2), valpole
      do j=1,3
         do i=1,ni
            zout(i,j) = zin(i,j2-3+j)
         enddo
      enddo
      do i=1,ni
         zout(i,4) = valpole
      enddo
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_fillspole(zout, zin, ni, j1, j2, valpole)
      implicit none
      integer i,j,ni,j1,j2
      real zout(ni, 4),zin(ni,j1:j2),valpole
      do j=j1,j1+2
         do i=1,ni
            zout(i,j-j1+2) = zin(i,j)
         enddo
      enddo
      do i=1,ni
         zout(i,1) = valpole
      enddo
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_gausslat(lats, aylat, ni,nj)
      integer ni,nj
      real lats(ni,nj)
      real aylat(nj)
      integer i,j
      do j=1,nj
         do i=1,ni
            lats(i,j) = aylat(j)
         enddo
      enddo
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!**s/p gdwfllw - passe des composantes speed, psi
!                aux composantes u,v selon le type de grille.
      subroutine ez_gdwfllw(z1,z2,xlon,li,lj,grtyp,ig1,ig2,ig3,ig4)
      implicit none
      integer li,lj
      real z1(li,lj), z2(li,lj), xlon(li,lj)
      character*1 grtyp
      integer ig1,ig2,ig3,ig4
      external cigaxg
!
!auteur- y. chartier - avril 91
!
!langage  - ratfor
!
!objet(gdwfllw)
!         - passe de vent de grille (composantes u et v)
!         - a vitesse et direction.
!
!librairies
!
!appel    - call gdwfllw(spd,psi,li,lj,iyp,xg1,xg2,xg3,xg4)
!
!modules  - xgaig
!
!arguments
!  in/out - spd   - a l'entree contient la vitesse du vent et
!                   a la sortie la composante u.
!  in/out - psi   - a l'entree contient la direction du vent et
!                   a la sortie la composante v.
!   in    - li    - premiere dimension des champs spd et psi
!   in    - lj    - deuxieme dimension des champs spd et psi
!   in    - igtyp  - type de grille (voir ouvrir)
!   in    - xg1   - ** descripteur de grille (reel),
!   in    - xg2   -    igtyp = 'n', pi, pj, d60, dgrw
!   in    - xg3   -    igtyp = 'l', lat0, lon0, dlat, dlon,
!   in    - xg4   -    igtyp = 'a', 'b', 'g', xg1 = 0. global,
!                                                 = 1. nord
!                                                 = 2. sud **
!
!messages - "erreur mauvaise grille (gdwfllw)"
!
!-------------------------------------------------------------
!
!
!
!     * 1.866025=(1+sin60),   6.371e+6=earth radius in meters.
!
!     rdtodg = 180/pie, dgtord = pie/180
      real pie,rdtodg,dgtord
      data pie    /3.1415926535898/
      data rdtodg /57.295779513082/
      data dgtord /1.7453292519943e-2/
!
      integer i,j,k,ntot,npart,ier
      real psi,u,v
      real xg1, xg2, xg3, xg4
      character*1 gtypout
      real xglst(20)
      integer nxg
      real lat45,lat50,delx,dely,alpha
      real uuu,vvv
      integer un
      real x1(2*li*lj),y1(2*li*lj),lat(2*li*lj)
!     les #define qui suivent rendent le code plus lisible
      if (grtyp .eq. '!') then
         call ez_lamb_gdwfllw(z1,z2,xlon,li,lj,grtyp,ig1,ig2,ig3,ig4,x1,y1,lat)
      endif
      if (grtyp .eq. 'N') then
         call cigaxg(grtyp,xg1,xg2,xg3,xg4,ig1,ig2,ig3,ig4)
         do 10 i=1,li
            do 20 j=1,lj
               psi =xlon(i,j)+xg4-z2(i,j)
               u = cos(psi*dgtord)*z1(i,j)
               v = sin(psi*dgtord)*z1(i,j)
               z1(i,j) = u
               z2(i,j) = v
 20         continue
 10      continue
         return
      endif
      if (grtyp .eq. 'S') then
         call cigaxg(grtyp,xg1,xg2,xg3,xg4,ig1,ig2,ig3,ig4)
         do 30 i=1,li
            do 40 j=1,lj
               psi =180.0 - xlon(i,j)+xg4-z2(i,j)
               u = cos(psi*dgtord)*z1(i,j)
               v = sin(psi*dgtord)*z1(i,j)
               z1(i,j) = u
               z2(i,j) = v
 40         continue
 30      continue
         return
      endif
      if (grtyp.eq.'A'.or.grtyp.eq.'B'.or.grtyp.eq.'G'.or.      grtyp.eq.'L') then
         do 50 i=1,li
            do 60 j=1,lj
               psi = 270.0 - z2(i,j)
               u = cos(psi*dgtord)*z1(i,j)
               v = sin(psi*dgtord)*z1(i,j)
               z1(i,j) = u
               z2(i,j) = v
 60         continue
 50      continue
         return
      endif
 600  format('0',' erreur, mauvaise grille (gdwfllw) - grtyp = ', A1)
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!********************************************************************
!**
!**
      subroutine ez_genpole(vpolnor,vpolsud,fld,ni,nj,vecteur,grtyp,hem,      x,y,z,lat,lon,glat,ordint)
      implicit none
      real vpolnor,vpolsud
      integer ni,nj
      real fld(ni,nj)
      character*1 grtyp
      logical vecteur
      integer hem,ordint
      real x(2,ni),y(2,ni),z(2,ni),lat(2,ni),lon(2,ni),glat(*)
!     
!     * 1.866025=(1+sin60),   6.371e+6=earth radius in meters.      
!     
!     rdtodg = 180/pie, dgtord = pie/180        
      real pie,rdtodg,dgtord
      data pie    /3.1415926535898/
      data rdtodg /57.295779513082/
      data dgtord /1.7453292519943e-2/
!         
      integer global, nord, sud, sudnord, nordsud
      integer absolu, relatif
      parameter (global = 0)
      parameter (nord   = 1)
      parameter (sud    = 2)
      parameter (sudnord= 0)
      parameter (nordsud= 1)
      parameter (absolu = 0)
      parameter (relatif = 1)
      integer voisin, lineair, cubique
      integer oui, abort, valeur, maximum, minimum
      parameter (voisin  =   0)
      parameter (lineair =   1)
      parameter (cubique =   3)
      parameter (oui     =   1)
      parameter (maximum =   4)
      parameter (minimum =   5)
      parameter (valeur  =   6)
      parameter (abort   =  13)
!--   common gaussgd
      integer maxroots
      parameter (maxroots=5120)
      real roots(maxroots),lroots(maxroots)
      common /ez_gaussgd/ roots,lroots
!----------------------
      integer i,j
      real*8 x2,y2,xy,x3,y3,x2y,xy2
      real pi,pj,dlon,temp
      real*8 a(6,6),b(6),vpole,det,deta,detb,detc
      integer l(6),m(6)
      integer n,nglat,ier
      real lat1,lat2,dlat,xla0
      integer ilat1,ilat2
!      if (vecteur) then
!         return
!      endif
      if (grtyp.ne.'G'.and.grtyp.ne.'A') then
         print *
         print *,     ' <GENPOLE> BAD GRID TYPE'
         print *,     '           THIS FUNCTION IS DESIGNED ONLY FOR A AND G GRIDS'
         print *
         goto 999
      endif
      if (hem.ne. global) then
         nglat = nj*2
      else
         nglat = nj
      endif
      if (grtyp.eq.'G') then
         call ez_glat(glat,lroots,nglat,hem)
      else
         dlat = 180./float(nglat)
         xla0 = -90. + 0.5*dlat
         do j=1,nglat
            glat(j)=xla0+(j-1)*dlat
         enddo
      endif
      do n=1,2
         if (n.eq.nord) then
            lat2  = glat(nglat-1)
            lat1  = glat(nglat)
            if (hem.eq.global) then
               ilat1 = nj
               ilat2 = nj-1
            else if (hem.eq.nord) then
               ilat1 = nj
               ilat2 = nj-1
            else
               ilat1 = 1
               ilat2 = 2
            endif
         else
            lat1  = glat(1)
            lat2  = glat(2)
            if (hem.eq.global) then
               ilat1 = 1
               ilat2 = 2
            else if (hem.eq.nord) then
               ilat1 = nj
               ilat2 = nj-1
            else
               ilat1 = 1
               ilat2 = 2
            endif
         endif
         dlon = 360. / float(ni)
         do i=1,ni
            lat(1,i)=lat1
            lat(2,i)=lat2
            lon(1,i)=(i-1)*dlon
            lon(2,i)=lon(1,i)
            z(1,i)=fld(i,ilat1)
            z(2,i)=fld(i,ilat2)
         enddo
         pi = 0.0
         pj = 0.0
         call ez_vxyfll(x,y,lat,lon,2*ni,1000.0,0.0,pi,pj,n)
         do j=1,6
            b(j)=0.0
            do i=1,6
               a(i,j)=0.0
            enddo
         enddo
         do j=1,2
            do i=1,ni
               xy=x(j,i)*y(j,i)
               x2=x(j,i)*x(j,i)
               x3=x2*x(j,i)
               y2=y(j,i)*y(j,i)
               y3=y2*y(j,i)
               x2y=x2*y(j,i)
               xy2=x(j,i)*y2
               a(1,1)=a(1,1) + 1
               a(2,1)=a(2,1) + x(j,i)
               a(3,1)=a(3,1) + y(j,i)
               a(4,1)=a(4,1) + x2
               a(5,1)=a(5,1) + xy
               a(6,1)=a(6,1) + y2
               a(2,2)=a(2,2) + x2
               a(3,2)=a(3,2) + xy
               a(4,2)=a(4,2) + x3
               a(5,2)=a(5,2) + x2y
               a(6,2)=a(6,2) + xy2
               a(3,3)=a(3,3) + y2
               a(4,3)=a(4,3) + x2y
               a(5,3)=a(5,3) + xy2
               a(6,3)=a(6,3) + y3
               a(4,4)=a(4,4) + x2 * x2
               a(5,4)=a(5,4) + x3 * y(j,i)
               a(6,4)=a(6,4) + x2 * y2
               a(5,5)=a(5,5) + x2 * y2
               a(6,5)=a(6,5) + x(j,i) * y3
               a(6,6)=a(6,6) + y2*y2
               b(1) = b(1) + z(j,i)
               b(2) = b(2) + z(j,i)*x(j,i)
               b(3) = b(3) + z(j,i)*y(j,i)
               b(4) = b(4) + z(j,i)*x2
               b(5) = b(5) + z(j,i)*xy
               b(6) = b(6) + z(j,i)*y2
            enddo
         enddo
         do i=1,6
            do j=i+1,6
               a(i,j)=a(j,i)
            enddo
         enddo
         if (ordint.eq.cubique) then
            call ez_mtxinv8(a,l,m,6)
            vpole = 0.0
            do i=1,6
               vpole= vpole + a(i,1)*b(i)
            enddo
         endif
         if (ordint.eq.lineair) then
            det  = a(1,1)*(a(2,2)*a(3,3)-a(3,2)*a(2,3))            - a(2,1)*(a(1,2)*a(3,3)-a(3,2)*a(1,3))            + a(3,1)*(a(1,&
     &2)*a(2,3)-a(2,2)*a(1,3))
            deta = b(1)*(a(2,2)*a(3,3)-a(3,2)*a(2,3))            - a(2,1)*(b(2)*a(3,3)-a(3,2)*b(3))            + a(3,1)*(b(2)*a(2,3&
     &)-a(2,2)*b(3))
            detb = a(1,1)*(b(2)*a(3,3)-a(3,2)*b(3))            - b(1)*(a(1,2)*a(3,3)-a(3,2)*a(1,3))            + a(3,1)*(a(1,2)*b(3&
     &)-b(2)*a(1,3))
            detc = a(1,1)*(a(2,2)*b(3)-b(2)*a(2,3))            - a(2,1)*(a(1,2)*b(3)-b(2)*a(1,3))            + b(1)*(a(1,2)*a(2,3)-&
     &a(2,2)*a(1,3))
            if (det.ne.0.0) then
               deta = deta/det
               detb = detb/det
               detc = detc/det
            endif
            if (det.ne.0.0) then
               vpole= deta
            else
               vpole = b(1)/a(1,1)
            endif
         endif
         if (ordint.eq.voisin) then
            vpole = b(1)/a(1,1)
         endif
         if (n.eq.nord) then
            vpolnor=real(vpole)
         else
            vpolsud=real(vpole)
         endif
      enddo
      if (ordint.ne.voisin.and.ordint.ne.lineair.and.ordint.ne.cubique) then
         print *, 'Tout un probleme mon gars'
      endif
 999  return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!**s/r ezgfllfxy - compute a latitude and longitude on the true earth from a 
!		 x and y coordinate on a rotated latitude/longitude frame
!		 of reference.
!
      subroutine ez_gfllfxy(lonp,latp,lon,lat,n,xlat1,xlon1,xlat2,xlon2)
      implicit none
      integer n
      real xlon1,xlat2,xlat1,xlon2
      real lonp(n),latp(n),lon(n),lat(n)
!     
!
!author Michel Roch      - feb 1994
!revision 001
!       Yves Chartier    - march 1994
!           argument change from xlon1,xlat1,xlon2,xlat2 
!                           to   xlat1,xlon1,xlat2,xlon2
!           replacement of logical switch "first" by
!           saved values of "oldlat1,oldlat2,oldlon1,oldlon2"
!                          
!arguments
!    out    lonp    - longitude on the unrotated coordinate system corresponding to
!                     the point (lat,lon) of the rotated coordinate system
!           latp    - latitude on the unrotated coordinate system corresponding to
!                     the point (lat,lon) of the rotated coordinate system
!    in     lon     - longitude on the rotated spherical coordinate system
!           lat     - latitude on the rotated spherical coordinate system 
!           xlat1     - latitude on the unrotated coordinate system corresponding to
!                     the point (lat,lon)=(0,180) of the rotated coordinate system
!           xlon1     - longitude on the unrotated coordinate system corresponding to
!                     the point (lat,lon)=(0,180) of the rotated coordinate system
!           xlat2     - longitude on the unrotated coordinate system corresponding to
!                     a point (lat,lon) located on the equator of the rotated
!                     coordinate system
!           xlon2     - latitude on the unrotated coordinate system corresponding to
!                     a point (lat,lon) located on the equator of the rotated
!                     coordinate system
!	    n 	    - dimension of the fields
!
!
!implicits
      real r(3,3),ri(3,3)
      common /ez_qqqmrot/ r,ri
      call ezgfllfxy(lonp, latp, lon, lat, r, ri, n, xlat1, xlon1, xlat2, xlon2)
      return
!     
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!**s/r ezgfxyfll - compute a latitude and longitude on the true earth from a 
!		 x and y coordinate on a rotated latitude/longitude frame
!		 of reference.
!
      subroutine ez_gfxyfll(lonp,latp,lon,lat,n,xlat1,xlon1,xlat2,xlon2)
      implicit none
      integer n
      real xlon1,xlat2,xlat1,xlon2
      real lonp(n),latp(n),lon(n),lat(n)
!     
!
!author Michel Roch      - feb 1994
!revision 001
!       Yves Chartier    - march 1994
!           argument change from xlon1,xlat1,xlon2,xlat2 
!                           to   xlat1,xlon1,xlat2,xlon2
!           replacement of logical switch "first" by
!           saved values of "oldlat1,oldlat2,oldlon1,oldlon2"
!                          
!arguments
!    out    lonp    - longitude on the unrotated coordinate system corresponding to
!                     the point (lat,lon) of the rotated coordinate system
!           latp    - latitude on the unrotated coordinate system corresponding to
!                     the point (lat,lon) of the rotated coordinate system
!    in     lon     - longitude on the rotated spherical coordinate system
!           lat     - latitude on the rotated spherical coordinate system 
!           xlat1     - latitude on the unrotated coordinate system corresponding to
!                     the point (lat,lon)=(0,180) of the rotated coordinate system
!           xlon1     - longitude on the unrotated coordinate system corresponding to
!                     the point (lat,lon)=(0,180) of the rotated coordinate system
!           xlat2     - longitude on the unrotated coordinate system corresponding to
!                     a point (lat,lon) located on the equator of the rotated
!                     coordinate system
!           xlon2     - latitude on the unrotated coordinate system corresponding to
!                     a point (lat,lon) located on the equator of the rotated
!                     coordinate system
!	    n 	    - dimension of the fields
!
!
!implicits
      real r(3,3),ri(3,3)
      common /ez_qqqmrot/ r,ri
      call ezgfxyfll(lonp, latp, lon, lat, r, ri, n, xlat1, xlon1, xlat2, xlon2)
      return
!     
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_gggdint_nw(zo,px,py,npts,ay,z,i1,i2,j1,j2)
      implicit none
!*******
!Auteur: Y.Chartier, drpn
!        Fevrier 1991
!
!Objet:  Interpolation bi-cubique de points a partir 
!        d'une grille gaussienne.
!*******
      integer npts,i1,i2,j1,j2,degree
      real zo(npts),px(npts),py(npts)
      real ay(j1:j2),cy(6)
      real z(i1:i2,j1:j2)
!
!  npts   : nombre de points a interpoler
!  i1:i2  : dimension de la grille source selon x
!  j1:j2  : dimension de la grille source selon y
!  zo     : vecteur de sortie contenant les valeurs interpolees
!  px     : vecteur contenant la position x des points que l'on
!           veut interpoler
!  py     : vecteur contenant la position y des points que l'on
!           veut interpoler
!  ay     : vecteur contenant la pos. des points sur l'axe des Y.
!  cy     : vecteur contenant une table de differences selon Y.
!  z      : valeurs de la grille source.
!
!**********************************************************************
!
!  *   *   *   *
!  
!  *   *   *   *
!        #        .eq.>   pt (x,y)
!  *  (=)  *   *  .eq.> = pt (i, j)
!
!  *   *   *   *
!
!
      integer i, j, m, n,stride
      real*8 x, x1, x2, x3, x4
      real*8 b1, b2,  b3,  b4
      real*8 b11, b12, b13, b14
      real*8 y,y1,y2,y3,y4
      real*8 y11, y12, y13, y14
      real*8 ay1, ay2, ay3, ay4
      real*8 fa, fa2, fa3, fa4
      real*8 a1,a2,a3,a4,c1,c2,c3,c4,c5,c6
      real*8 cubic, dx,dy,z1,z2,z3,z4
      real*8 zlin, zz1, zz2, zdx
      cubic(z1,z2,z3,z4,dx)=((((z4-z1)*0.1666666666666 + 0.5*(z2-z3))      *dx  + 0.5*(z1+z3)-z2)*dx  + z3-0.1666666666666*z4-0.5* &
     &     z2-0.3333333333333*z1)*dx+z2
      zlin(zz1,zz2,zdx) = zz1 + (zz2 - zz1) * zdx
      fa(a1,a2,a3,a4,x,x1,x2,x3)=a1+(x-x1)*(a2+(x-x2)*(a3+a4*(x-x3)))
      fa2(c1,a1,a2)=c1*(a2-a1)
      fa3(c1,c2,c3,a1,a2,a3)=c2*(c3*(a3-a2)-c1*(a2-a1))
      fa4(c1,c2,c3,c4,c5,c6,a1,a2,a3,a4)=c4*(c5*(c6*(a4-a3)-c3*      (a3-a2)) - c2*(c3*(a3-a2)-c1*(a2-a1)))
      do n=1,npts
         i = min(i2-2,max(i1+1,ifix(px(n))))
         j = min(j2-2,max(j1+1,ifix(py(n))))
         dx = px(n) - i
         y1=cubic(dble(z(i-1,j-1)),dble(z(i  ,j-1)),dble(z(i+1,j-1)),dble(z(i+2,j-1)),dx)
         y2=cubic(dble(z(i-1,j  )),dble(z(i  ,j  )),dble(z(i+1,j  )),dble(z(i+2,j  )),dx)
         y3=cubic(dble(z(i-1,j+1)),dble(z(i  ,j+1)),dble(z(i+1,j+1)),dble(z(i+2,j+1)),dx)
         y4=cubic(dble(z(i-1,j+2)),dble(z(i  ,j+2)),dble(z(i+1,j+2)),dble(z(i+2,j+2)),dx)
         y = ay(j) + (ay(j+1)-ay(j))*(py(n)-j)
!     interpolation finale selon y
         ay1=ay(j-1)
         ay2=ay(j)
         ay3=ay(j+1)
         ay4=ay(j+2)
         cy(1) = 1.0 / (ay2 - ay1)
         cy(2) = 1.0 / (ay3 - ay1)
         cy(3) = 1.0 / (ay3 - ay2)
         cy(4) = 1.0 / (ay4 - ay1)
         cy(5) = 1.0 / (ay4 - ay2)
         cy(6) = 1.0 / (ay4 - ay3)
         y11 = y1
         y12 = fa2(dble(cy(1)),y1,y2)
         y13 = fa3(dble(cy(1)),dble(cy(2)),dble(cy(3)),y1,y2,y3)
         y14 = fa4(dble(cy(1)),dble(cy(2)),dble(cy(3)),dble(cy(4)),         dble(cy(5)),dble(cy(6)),y1,y2,y3,y4)
         zo(n) = fa(y11,y12,y13,y14,y,ay1,ay2,ay3)
      enddo
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_gggdint_w(zo,px,py,npts,ay,z,ni,j1,j2,wrap)
      implicit none
!*******
!Auteur: Y.Chartier, drpn
!        Fevrier 1991
!
!Objet:  Interpolation bi-cubique de points a partir 
!        d'une grille gaussienne.
!*******
      integer npts,ni,i1,i2,j1,j2,wrap,limite
      real zo(npts),px(npts),py(npts)
      real ay(j1:j2),cy(6)
      real z(ni,j1:j2)
!
!  npts   : nombre de points a interpoler
!  i1:i2  : dimension de la grille source selon x
!  j1:j2  : dimension de la grille source selon y
!  zo     : vecteur de sortie contenant les valeurs interpolees
!  px     : vecteur contenant la position x des points que l'on
!           veut interpoler
!  py     : vecteur contenant la position y des points que l'on
!           veut interpoler
!  ay     : vecteur contenant la pos. des points sur l'axe des Y.
!  cy     : vecteur contenant une table de differences selon Y.
!  z      : valeurs de la grille source.
!
!**********************************************************************
!
!  *   *   *   *
!  
!  *   *   *   *
!        #        .eq.>   pt (x,y)
!  *  (=)  *   *  .eq.> = pt (i, j)
!
!  *   *   *   *
!
!
!     
!  cy(i,1) = 1.0 / (x2-x1)
!  cy(i,2) = 1.0 / (x3-x1)
!  cy(i,3) = 1.0 / (x3-x2)
!  cy(i,4) = 1.0 / (x4-x1)
!  cy(i,5) = 1.0 / (x4-x2)
!  cy(i,6) = 1.0 / (x4-x3)
!
!  structure identique pour cy(j,1..6)
      integer i, j, m, n,stride
      integer imoins1, iplus1, iplus2
      real*8 x, x1, x2, x3, x4
      real*8 b1, b2,  b3,  b4
      real*8 b11, b12, b13, b14
      real*8 y,y1,y2,y3,y4
      real*8 y11, y12, y13, y14
      real*8 ay1, ay2, ay3, ay4
      real*8 fa, fa2, fa3, fa4
      real*8 a1,a2,a3,a4,c1,c2,c3,c4,c5,c6
      integer voisin, lineair, cubique
      integer oui, abort, valeur, maximum, minimum
      parameter (voisin  =   0)
      parameter (lineair =   1)
      parameter (cubique =   3)
      parameter (oui     =   1)
      parameter (maximum =   4)
      parameter (minimum =   5)
      parameter (valeur  =   6)
      parameter (abort   =  13)
!  definition des fonctions in-line
      real*8 cubic, dx,dy,z1,z2,z3,z4
      real*8 zlin, zz1, zz2, zdx
      cubic(z1,z2,z3,z4,dx)=((((z4-z1)*0.1666666666666 + 0.5*(z2-z3))      *dx  + 0.5*(z1+z3)-z2)*dx  + z3-0.1666666666666*z4-0.5* &
     &     z2-0.3333333333333*z1)*dx+z2
      zlin(zz1,zz2,zdx) = zz1 + (zz2 - zz1) * zdx
      fa(a1,a2,a3,a4,x,x1,x2,x3)=a1+(x-x1)*(a2+(x-x2)*(a3+a4*(x-x3)))
      fa2(c1,a1,a2)=c1*(a2-a1)
      fa3(c1,c2,c3,a1,a2,a3)=c2*(c3*(a3-a2)-c1*(a2-a1))
      fa4(c1,c2,c3,c4,c5,c6,a1,a2,a3,a4)=c4*(c5*(c6*(a4-a3)-c3*      (a3-a2)) - c2*(c3*(a3-a2)-c1*(a2-a1)))
         do n=1,npts
            limite = ni +2 -wrap
            i = min(ni-2+wrap,max(1,max(2-wrap,ifix(px(n)))))
            j = min(j2-2,max(j1,ifix(py(n))))
            imoins1 = i-1
            iplus1 = i+1
            iplus2 = i+2
            if (wrap.gt.0.and.(i.le.1).or.i.ge.(ni-1)) then
               imoins1 = mod(limite+i-1,limite)
               iplus1  = mod(limite+i+1,limite)
               iplus2  = mod(limite+i+2,limite)
               if (imoins1.eq.0) imoins1 = ni
               if (i.eq.0) i = ni
               if (iplus1.eq.0) iplus1 = ni
               if (iplus2.eq.0) iplus2 = ni
               if (wrap.eq.1) then
                  if (iplus2.eq.ni) iplus2 = 2
                  if (imoins1.eq.ni) imoins1=ni-1
               endif
            endif
            dx = px(n) - i
            y1=cubic(dble(z(imoins1,j-1)),dble(z(i,j-1)),dble(z(iplus1,j-1)),dble(z(iplus2,j-1)),dx)
            y2=cubic(dble(z(imoins1,j  )),dble(z(i,j  )),dble(z(iplus1,j  )),dble(z(iplus2,j  )),dx)
            y3=cubic(dble(z(imoins1,j+1)),dble(z(i,j+1)),dble(z(iplus1,j+1)),dble(z(iplus2,j+1)),dx)
            y4=cubic(dble(z(imoins1,j+2)),dble(z(i,j+2)),dble(z(iplus1,j+2)),dble(z(iplus2,j+2)),dx)
            y = ay(j) + (ay(j+1)-ay(j))*(py(n)-j)
!     interpolation finale selon y
            ay1=ay(j-1)
            ay2=ay(j)
            ay3=ay(j+1)
            ay4=ay(j+2)
            cy(1) = 1.0 / (ay2 - ay1)
            cy(2) = 1.0 / (ay3 - ay1)
            cy(3) = 1.0 / (ay3 - ay2)
            cy(4) = 1.0 / (ay4 - ay1)
            cy(5) = 1.0 / (ay4 - ay2)
            cy(6) = 1.0 / (ay4 - ay3)
            y11 = y1
            y12 = fa2(dble(cy(1)),y1,y2)
            y13 = fa3(dble(cy(1)),dble(cy(2)),dble(cy(3)),y1,y2,y3)
            y14 = fa4(dble(cy(1)),dble(cy(2)),dble(cy(3)),dble(cy(4)),            dble(cy(5)),dble(cy(6)),y1,y2,y3,y4)
            zo(n) = fa(y11,y12,y13,y14,y,ay1,ay2,ay3)
         enddo
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!**s/r ez_ggll2gd - computes the grid co-ordinates of a point on
!                a gaussian grid
!
      subroutine ez_ggll2gd(x,y,xlat,xlon,npts,ni,nj,hem,lroots)
      implicit none
!*--------------------------------------------------------------------
      integer global, nord, sud, sudnord, nordsud
      integer absolu, relatif
      parameter (global = 0)
      parameter (nord   = 1)
      parameter (sud    = 2)
      parameter (sudnord= 0)
      parameter (nordsud= 1)
      parameter (absolu = 0)
      parameter (relatif = 1)
      integer npts, ni, nj
      real x(npts), y(npts), xlat(npts), xlon(npts)
      real lroots(nj)
      real abs,del,epsphi
      integer ez_cherche
      external ez_cherche
      integer i,j,guess,hem,indy
      real tmplat, dellat, dellon, xlat0, xlon0
      dellon = 360.0 / real(ni)
      xlon0 = 0.0
      do 10 i = 1, npts
         x(i) = (xlon(i) - xlon0)/dellon + 1.0
 10   continue
      do i=1,npts
         indy = ez_cherche(xlat(i),lroots,nj)
         if (indy .ge. nj) indy = nj - 1
         y(i)= real(indy)+(xlat(i)-lroots(indy))/         (lroots(indy+1)-lroots(indy))
      enddo
      return
      end
!-----------------------------------------------------------------
   subroutine qqq_ezget_mask_zones(mask_zones, x, y, ni_out, nj_out, mask_in, ni_in, nj_in)
   implicit none
   integer :: ni_out, nj_out, ni_in, nj_in
   integer :: mask_zones(ni_out, nj_out),mask_in(ni_in, nj_in)
   real    :: x(ni_out, nj_out), y(ni_out, nj_out)
   real    :: rx, ry
   integer :: i,j, k, l, ix, iy, nix, niy, nmissing
   integer :: codes(8)
   integer :: original_mask, all_pts_present, three_points_present, two_points_present, &
      one_point_present, one_point_missing, all_pts_missing, nearest_point_missing, outside_src_grid
   all_pts_missing           = 0
   one_point_present         = 1
   two_points_present        = 2
   three_points_present      = 3
   all_pts_present           = 4
   nearest_point_missing     = 5
   original_mask             = 6
   outside_src_grid          = 7
   do j=1,nj_out
      do i=1,ni_out
         ix = int(x(i,j))
         iy = int(y(i,j))
         nix = nint(x(i,j))
         niy = nint(y(i,j))
         if (ix<1.or.ix>ni_in.or.iy<1.or.iy>nj_in) then
            mask_zones(i,j) = outside_src_grid
         else
            if (mask_in(nix,niy) == 0) then
               mask_zones(i,j) = nearest_point_missing
            endif
            nmissing = 0
            do k=1,2
               do l=1,2
                  if (mask_in(ix+k-1,iy+l-1) == 0) then
                     nmissing = nmissing+1
                  endif
               enddo
            enddo
            select case (nmissing)
               case (0)
                  mask_zones(i,j) = all_pts_present
               case (1)
                  mask_zones(i,j) = three_points_present
               case (2)
                  mask_zones(i,j) = two_points_present
               case (3)
                  mask_zones(i,j) = one_point_present
               case (4)
                  mask_zones(i,j) = all_pts_missing
            end select
         endif
      enddo
   enddo
   end subroutine qqq_ezget_mask_zones
   subroutine qqq_ezsint_mask(mask_out, x, y, ni_out, nj_out, mask_in, ni_in, nj_in)
   implicit none
   integer :: ni_out, nj_out, ni_in, nj_in
   integer :: mask_out(ni_out, nj_out),mask_in(ni_in, nj_in)
   real    :: x(ni_out, nj_out), y(ni_out, nj_out)
   real    :: rx, ry
   integer :: i,j, k, l, ix, iy, nix, niy, nmissing, ier
   integer ezgetopt
   external ezgetopt
   character(len=32) :: value
   ier = ezgetopt('cloud_interp_alg', value)
   mask_out = 1
   do j=1,nj_out
      do i=1,ni_out
         ix = int(x(i,j))
         iy = int(y(i,j))
         nix = nint(x(i,j))
         niy = nint(y(i,j))
         if (ix<1.or.ix>ni_in.or.iy<1.or.iy>nj_in) then
            mask_out(i,j) = 0
         else if (mask_in(nix,niy) == 0) then
            mask_out(i,j) = 0
         endif
      enddo
   enddo
   if (value(1:6) == 'linear') then
      do j=1,nj_out-1
         do i=1,ni_out-1
            if (mask_out(i,j) == 1) then
               ix = int(x(i,j))
               iy = int(y(i,j))
               if (mask_in(ix+1,iy) == 0 .or.mask_in(ix,iy+1) == 0 .or. mask_in(ix+1,iy+1) == 0) then
                  mask_out(i,j) = 0
               endif
            endif
         enddo
      enddo
   endif
   end subroutine qqq_ezsint_mask
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!*********************************************************************
!**s/r ez_glat - calcul des latitudes d'une grille gaussienne
!
!  auteur: Yves Chartier. Mars 1991.
!****
!
      subroutine ez_glat(latroots,groots,nj,hem)
      implicit none
      integer nj, hem,ier
      integer global, nord, sud, sudnord, nordsud
      integer absolu, relatif
      parameter (global = 0)
      parameter (nord   = 1)
      parameter (sud    = 2)
      parameter (sudnord= 0)
      parameter (nordsud= 1)
      parameter (absolu = 0)
      parameter (relatif = 1)
!     
!     * 1.866025=(1+sin60),   6.371e+6=earth radius in meters.      
!     
!     rdtodg = 180/pie, dgtord = pie/180        
      real pie,rdtodg,dgtord
      data pie    /3.1415926535898/
      data rdtodg /57.295779513082/
      data dgtord /1.7453292519943e-2/
!         
      external dgauss
      real latroots(*), groots(*)
      integer j,npoly
      real temp
      if (hem .ne. GLOBAL) then
         npoly = nj * 2
      else
         npoly = nj
      endif
      call dgauss(npoly, groots, global)
      do j=1,npoly/2
         temp = groots(j)
         groots(j)=groots(npoly+1-j)
         groots(npoly+1-j)=temp
      enddo
      if (hem .ne. nord) then
         do j=1,nj
            latroots(j)= 90. - rdtodg * acos(groots(j))
         enddo
      endif
      if (hem .eq. NORD) then
         do 20 j=1,nj
            latroots(j)=90.0-rdtodg*acos(groots(j+nj))
 20      continue
      endif
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_iaxint(zo,px,npts,ax,z,ni, i1,i2,ordint)
      implicit none
!*******
!Auteur: Y.Chartier, drpn
!        Fevrier 1991
!
!Objet:  Interpolation bi-cubique de points a partir 
!        d'une grille source irreguliere.
!*******
      integer npts,ni,i1,i2,ordint
      real zo(npts),px(npts)
      real ax(i1:i2)
      real z(ni)
!
!  npts   : nombre de points a interpoler
!  i1:i2  : dimension de la grille source selon x
!  zo     : vecteur de sortie contenant les valeurs interpolees
!  px     : vecteur contenant la position x des points que l'on
!           veut interpoler
!  ax     : vecteur contenant la pos. des points sur l'axe des X.
!  cx     : vecteur contenant une table de differences selon X.
!  z      : valeurs de la grille source.
!
!***************************************************************************
!
!  *   *   *   *
!  
!  *   *   *   *
!        #        .eq.>   pt (x,y)
!  *  (=)  *   *  .eq.> = pt (i, j)
!
!  *   *   *   *
!
!
!     
!  structure identique pour cy(j,1..6)
      real a11,a12,a13,a14
      real b1
      real x1,x2,x3
      integer i, n
      real a1,a2,a3,a4,x,c1,c2,c3,c4,c5,c6
      real fa, fa2, fa3, fa4
!     definition des fonctions in-line
      integer voisin, lineair, cubique
      integer oui, abort, valeur, maximum, minimum
      parameter (voisin  =   0)
      parameter (lineair =   1)
      parameter (cubique =   3)
      parameter (oui     =   1)
      parameter (maximum =   4)
      parameter (minimum =   5)
      parameter (valeur  =   6)
      parameter (abort   =  13)
      real cubic, dx,dy,z1,z2,z3,z4
      cubic(z1,z2,z3,z4,dx)=((((z4-z1)*0.1666666666666 + 0.5*(z2-z3))      *dx  + 0.5*(z1+z3)-z2)*dx  + z3-0.1666666666666*z4-0.5* &
     &     z2-0.3333333333333*z1)*dx+z2
      real zlin, zz1, zz2, zdx
      zlin(zz1,zz2,zdx) = zz1 + (zz2 - zz1) * zdx
      fa(a1,a2,a3,a4,x,x1,x2,x3)=a1+(x-x1)*(a2+(x-x2)*(a3+a4*(x-x3)))
      fa2(c1,a1,a2)=c1*(a2-a1)
      fa3(c1,c2,c3,a1,a2,a3)=c2*(c3*(a3-a2)-c1*(a2-a1))
      fa4(c1,c2,c3,c4,c5,c6,a1,a2,a3,a4)=c4*(c5*(c6*(a4-a3)-      c3*(a3-a2)) - c2*(c3*(a3-a2)-c1*(a2-a1)))
      if (ordint.eq.cubique) then
         do 10 n=1,npts
            i = min(ni-2,max(2,ifix(px(n))))
            x = ax(i) + (ax(i+1)-ax(i))*(px(n)-i)
            a11 = z(i-1)
            a12 = fa2((1.0/(ax(i)-ax(i-1))),z(i-1),z(i))
            a13 = fa3((1.0/(ax(i)-ax(i-1))),1.0/(ax(i+1)-ax(i-1)), 1.0/(ax(i+1)-ax(i)),z(i-1),z(i),z(i+1))
            a14 = fa4((1.0/(ax(i)-ax(i-1))),1.0/(ax(i+1)-ax(i-1)), 1.0/(ax(i+1)-ax(i)),1.0/(ax(i+2)-ax(i-1)),1.0/(ax(i+2)-ax(i)),1.&
     &0/(ax(i+2)-ax(i+1)),z(i-1),z(i),z(i+1),z(i+2))
            zo(n)  = fa(a11,a12,a13,a14,x,ax(i-1),ax(i),ax(i+1))
 10      continue
         endif
         if (ordint.eq.lineair) then
            do 20 n=1,npts
               i = min(i2-1,max(i1,ifix(px(n))))
               x = ax(i) + (ax(i+1)-ax(i))*(px(n)-i)
               dx = (x - ax(i))/(ax(i+1)-ax(i))
               zo(n) = zlin((z(i)),z(i+1),dx)
 20         continue
         endif
         if (ordint.eq.voisin) then
            do 30 n=1,npts
               i = min(i2,max(i1,nint(px(n))))
               zo(n) = z(i)
 30         continue
         endif
         return
         end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_irgdint_1_nw(zo,px,py,npts,ax,ay,z,ni,nj)
      implicit none
      integer npts,ni,nj,wrap,limite
      integer i, j,n, iplus1
      real zo(npts),px(npts),py(npts)
      real ax(ni),ay(nj)
      real z(ni,nj)
      real*8 x, y, x1, x2, y1,y2,dx,dy
      real*8 zlin, zz1, zz2, zdx
      zlin(zz1,zz2,zdx) = zz1 + (zz2 - zz1) * zdx
      do n=1,npts
         i = min(ni-1,max(1,ifix(px(n))))
         j = min(nj-1,max(1,ifix(py(n))))
         x1=ax(i)
         x2=ax(i+1)
         x = ax(i) + (x2-x1)*(px(n)-i)
         y = ay(j) + (ay(j+1)-ay(j))*(py(n)-j)
         dx = (x - x1)/(x2-x1)
         dy = (y - ay(j))/(ay(j+1)-ay(j))
         y1 = zlin(dble(z(i,j)),dble(z(i+1,j)),dx)
         y2 = zlin(dble(z(i,j+1)),dble(z(i+1,j+1)),dx)
         zo(n) = zlin(y1,y2,dy)
      enddo
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_irgdint_1_w(zo,px,py,npts,ax,ay,z,ni,j1,j2,wrap)
      implicit none
      integer npts,ni,nj,wrap,limite
      integer i, j,j1,j2,n, iplus1
      real zo(npts),px(npts),py(npts)
      real ax(ni),ay(j1:j2)
      real z(ni,j1:j2)
      real*8 x, y, x1, x2, y1,y2,dx,dy
      real*8 zlin, zz1, zz2, zdx
      zlin(zz1,zz2,zdx) = zz1 + (zz2 - zz1) * zdx
      do n=1,npts
         limite = ni+2-wrap
         i = min(ni-2+wrap,max(1,ifix(px(n))))
         j = min(j2-1,max(j1+1, ifix(py(n))))
         if (j.lt.0) then
            j = j-1
         endif
         iplus1 = i+1
         x1=ax(i)
         if (iplus1.le.ni) then
            x2 = ax(iplus1)
         endif
         if (wrap.gt.0.and.(i.eq.(ni-2+wrap))) then
            iplus1  = mod(limite+i+1,limite)
            x2=ax(2)+ax(ni)
         endif
         x = x1 + (x2-x1)*(px(n)-i)
         y = ay(j) + (ay(j+1)-ay(j))*(py(n)-j)
         dx = (x - x1)/(x2-x1)
         dy = (y - ay(j))/(ay(j+1)-ay(j))
         y1 = zlin(dble(z(i,j)),dble(z(iplus1,j)),dx)
         y2 = zlin(dble(z(i,j+1)),dble(z(iplus1,j+1)),dx)
         zo(n) = zlin(y1,y2,dy)
      enddo
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_irgdint_3_nw(zo,px,py,npts,ax,ay,cx,cy,z,i1,i2,j1,j2)
      implicit none
!*******
!     Auteur: Y.Chartier, drpn
!     Fevrier 1991
!     
!     Objet:  Interpolation bi-cubique de points a partir 
!     d'une grille source irreguliere.
!*******
      integer npts,i1,i2,j1,j2,degree
      real zo(npts),px(npts),py(npts)
      real fa, fa2, fa3, fa4
      real ax(i1:i2),ay(j1:j2),cx(i1:i2,6),cy(j1:j2,6)
      real z(i1:i2,j1:j2)
!     
!     npts   : nombre de points a interpoler
!     1:ni  : dimension de la grille source selon x
!     1:nj  : dimension de la grille source selon y
!     zo     : vecteur de sortie contenant les valeurs interpolees
!     px     : vecteur contenant la position x des points que l'on
!              veut interpoler
!     py     : vecteur contenant la position y des points que l'on
!              veut interpoler
!     ax     : vecteur contenant la pos. des points sur l'axe des X.
!     ay     : vecteur contenant la pos. des points sur l'axe des Y.
!     cx     : vecteur contenant une table de differences selon X.
!     cy     : vecteur contenant une table de differences selon Y.
!     z      : valeurs de la grille source.
!     
!***************************************************************************
!     
!     *   *   *   *
!     
!     *   *   *   *
!           #        .eq.>   pt (x,y)
!     *  (=)  *   *  .eq.> = pt (i, j)
!     
!     *   *   *   *
!     
!     
!     
!     cx(i,1) = 1.0 / (x2-x1)
!     cx(i,2) = 1.0 / (x3-x1)
!     cx(i,3) = 1.0 / (x3-x2)
!     cx(i,4) = 1.0 / (x4-x1)
!     cx(i,5) = 1.0 / (x4-x2)
!     cx(i,6) = 1.0 / (x4-x3)
!     
!  structure identique pour cy(j,1..6)
      integer i, j, m, n,stride
      real*8 a11,a12,a13,a14,a21,a22,a23,a24
      real*8 a31,a32,a33,a34,a41,a42,a43,a44
      real*8 b1,b2,b3,b4,b11,b12,b13,b14
      real*8 x1,x2,x3,x4,y1,y2,y3,y4
      real*8 a1,a2,a3,a4,x,y,c1,c2,c3,c4,c5,c6
      integer voisin, lineair, cubique
      integer oui, abort, valeur, maximum, minimum
      parameter (voisin  =   0)
      parameter (lineair =   1)
      parameter (cubique =   3)
      parameter (oui     =   1)
      parameter (maximum =   4)
      parameter (minimum =   5)
      parameter (valeur  =   6)
      parameter (abort   =  13)
!     definition des fonctions in-line
      real*8 cubic, dx,dy,z1,z2,z3,z4
      cubic(z1,z2,z3,z4,dx)=((((z4-z1)*0.1666666666666 + 0.5*(z2-z3))      *dx  + 0.5*(z1+z3)-z2)*dx  + z3-0.1666666666666*z4-0.5* &
     &     z2-0.3333333333333*z1)*dx+z2
      fa(a1,a2,a3,a4,x,x1,x2,x3)=a1+(x-x1)*(a2+(x-x2)*(a3+a4*(x-x3)))
      fa2(c1,a1,a2)=c1*(a2-a1)
      fa3(c1,c2,c3,a1,a2,a3)=c2*(c3*(a3-a2)-c1*(a2-a1))
      fa4(c1,c2,c3,c4,c5,c6,a1,a2,a3,a4)=c4*(c5*(c6*(a4-a3)-      c3*(a3-a2)) - c2*(c3*(a3-a2)-c1*(a2-a1)))
      do n=1,npts
         i = min(i2-2,max(i1+1,ifix(px(n))))
         j = min(j2-2,max(j1+1,ifix(py(n))))
         x = ax(i) + (ax(i+1)-ax(i))*(px(n)-i)
         y = ay(j) + (ay(j+1)-ay(j))*(py(n)-j)
         x1=ax(i-1)
         x2=ax(i)
         x3=ax(i+1)
         x4=ax(i+2)
         y1=ay(j-1)
         y2=ay(j)
         y3=ay(j+1)
         y4=ay(j+2)
!     interpolation 1ere rangee selon x
         z1=z(i-1,j-1)
         z2=z(i  ,j-1)
         z3=z(i+1,j-1)
         z4=z(i+2,j-1)
         a11 = z1
         a12 = fa2(dble(cx(i,1)),z1,z2)
         a13 = fa3(dble(cx(i,1)),dble(cx(i,2)),dble(cx(i,3)),z1,z2,z3)
         a14 = fa4(dble(cx(i,1)),dble(cx(i,2)),dble(cx(i,3)),dble(cx(i,4)),dble(cx(i,5)),dble(cx(i,6)),         z1,z2,z3,z4)
         b1  = fa(a11,a12,a13,a14,x,x1,x2,x3)
!     interpolation 2eme rangee selon x
         z1=z(i-1,j)
         z2=z(i  ,j)
         z3=z(i+1,j)
         z4=z(i+2,j)
         a21 = z1
         a22 = fa2(dble(cx(i,1)),z1,z2)
         a23 = fa3(dble(cx(i,1)),dble(cx(i,2)),dble(cx(i,3)),z1,z2,z3)
         a24 = fa4(dble(cx(i,1)),dble(cx(i,2)),dble(cx(i,3)),dble(cx(i,4)),         dble(cx(i,5)),dble(cx(i,6)),z1,z2,z3,z4)
         b2  = fa(a21,a22,a23,a24,x,x1,x2,x3)
!     interpolation 3eme rangee selon x
         z1=z(i-1,j+1)
         z2=z(i  ,j+1)
         z3=z(i+1,j+1)
         z4=z(i+2,j+1)
         a31 = z1
         a32 = fa2(dble(cx(i,1)),z1,z2)
         a33 = fa3(dble(cx(i,1)),dble(cx(i,2)),dble(cx(i,3)),z1,z2,z3)
         a34 = fa4(dble(cx(i,1)),dble(cx(i,2)),dble(cx(i,3)),dble(cx(i,4)),         dble(cx(i,5)),dble(cx(i,6)),z1,z2,z3,z4)
         b3  = fa(a31,a32,a33,a34,x,x1,x2,x3)
!     interpolation 4eme rangee selon x
         z1=z(i-1,j+2)
         z2=z(i  ,j+2)
         z3=z(i+1,j+2)
         z4=z(i+2,j+2)
         a41 = z1
         a42 = fa2(dble(cx(i,1)),z1,z2)
         a43 = fa3(dble(cx(i,1)),dble(cx(i,2)),dble(cx(i,3)),z1,z2,z3)
         a44 = fa4(dble(cx(i,1)),dble(cx(i,2)),dble(cx(i,3)),dble(cx(i,4)),         dble(cx(i,5)),dble(cx(i,6)),z1,z2,z3,z4)
         b4  = fa(a41,a42,a43,a44,x,x1,x2,x3)
!     interpolation finale selon y
         b11 = b1
         b12 = fa2(dble(cy(j,1)),b1,b2)
         b13 = fa3(dble(cy(j,1)),dble(cy(j,2)),dble(cy(j,3)),b1,b2,b3)
         b14 = fa4(dble(cy(j,1)),dble(cy(j,2)),dble(cy(j,3)),dble(cy(j,4)),         dble(cy(j,5)),dble(cy(j,6)),b1,b2,b3,b4)
         zo(n) = fa(b11,b12,b13,b14,y,y1,y2,y3)
      enddo
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_irgdint_3_w(zo,px,py,npts,ax,ay,cx,cy,z,ni,j1,j2,wrap)
      implicit none
!*******
!     Auteur: Y.Chartier, drpn
!     Fevrier 1991
!     
!     Objet:  Interpolation bi-cubique de points a partir 
!     d'une grille source irreguliere.
!*******
      integer npts,ni,j1,j2,degree,wrap,limite
      integer imoins1, iplus1, iplus2
      real zo(npts),px(npts),py(npts)
      real ax(ni),ay(j1:j2),cx(ni,6),cy(j1:j2,6)
      real z(ni,j1:j2)
!     
!     npts   : nombre de points a interpoler
!     1:ni  : dimension de la grille source selon x
!     1:nj  : dimension de la grille source selon y
!     zo     : vecteur de sortie contenant les valeurs interpolees
!     px     : vecteur contenant la position x des points que l'on
!              veut interpoler
!     py     : vecteur contenant la position y des points que l'on
!              veut interpoler
!     ax     : vecteur contenant la pos. des points sur l'axe des X.
!     ay     : vecteur contenant la pos. des points sur l'axe des Y.
!     cx     : vecteur contenant une table de differences selon X.
!     cy     : vecteur contenant une table de differences selon Y.
!     z      : valeurs de la grille source.
!     
!***************************************************************************
!     
!     *   *   *   *
!     
!     *   *   *   *
!           #        .eq.>   pt (x,y)
!     *  (=)  *   *  .eq.> = pt (i, j)
!     
!     *   *   *   *
!     
!     
!     
!     cx(i,1) = 1.0 / (x2-x1)
!     cx(i,2) = 1.0 / (x3-x1)
!     cx(i,3) = 1.0 / (x3-x2)
!     cx(i,4) = 1.0 / (x4-x1)
!     cx(i,5) = 1.0 / (x4-x2)
!     cx(i,6) = 1.0 / (x4-x3)
!     
!  structure identique pour cy(j,1..6)
      integer i, j, m, n
      real*8 a11,a12,a13,a14,a21,a22,a23,a24
      real*8 a31,a32,a33,a34,a41,a42,a43,a44
      real*8 b1,b2,b3,b4,b11,b12,b13,b14
      real x1,x2,x3,x4,y1,y2,y3,y4
      real*8 z1,z2,z3,z4
      real*8 fa, fa2, fa3, fa4
      real*8 a1,a2,a3,a4,x,y,c1,c2,c3,c4,c5,c6
      integer voisin, lineair, cubique
      integer oui, abort, valeur, maximum, minimum
      parameter (voisin  =   0)
      parameter (lineair =   1)
      parameter (cubique =   3)
      parameter (oui     =   1)
      parameter (maximum =   4)
      parameter (minimum =   5)
      parameter (valeur  =   6)
      parameter (abort   =  13)
!     definition des fonctions in-line
      fa(a1,a2,a3,a4,x,x1,x2,x3)=a1+(x-x1)*(a2+(x-x2)*(a3+a4*(x-x3)))
      fa2(c1,a1,a2)=c1*(a2-a1)
      fa3(c1,c2,c3,a1,a2,a3)=c2*(c3*(a3-a2)-c1*(a2-a1))
      fa4(c1,c2,c3,c4,c5,c6,a1,a2,a3,a4)=c4*(c5*(c6*(a4-a3)-      c3*(a3-a2)) - c2*(c3*(a3-a2)-c1*(a2-a1)))
!zzzzzz$OMP PARALLEL
!zzzzzz$OMP DO PRIVATE(n)
      do n=1,npts
         i = min(ni-2+wrap,max(1,max(2-wrap,ifix(px(n)))))
         j = min(j2-2,max(j1+1,ifix(py(n))))
         imoins1 = i-1
         iplus1 = i+1
         iplus2 = i+2
         if (wrap.eq.1.and.(i.le.1.or.i.ge.(ni-wrap))) then
            if (i.eq.1) then
               imoins1 = ni-1
               iplus1  = 2
               iplus2  = 3
               x1 = ax(ni-1) - 360.0
               x2 = ax(1)
               x3 = ax(2)
               x4 = ax(3)
            endif
            if (i.eq.(ni-1)) then
               imoins1 = ni-2
               iplus1  = ni
               iplus2  = 2
               x1 = ax(ni-2)
               x2 = ax(ni-1)
               x3 = ax(ni)
               x4 = ax(2)+360.0
            endif
         elseif (wrap.eq.2.and.(i.le.1.or.i.gt.(ni-wrap))) then
            if (i.eq.1) then
               imoins1 = ni
               iplus1  = 2
               iplus2  = 3
               x1 = ax(ni) - 360.0
               x2 = ax(1)
               x3 = ax(2)
               x4 = ax(3)
            endif
            if (i.eq.(ni-1)) then
               imoins1 = ni - 2
               iplus1  = ni
               iplus2  = 1
               x1 = ax(ni-2)
               x2 = ax(ni-1)
               x3 = ax(ni)
               x4 = ax(1) + 360.0
            endif
            if (i.eq.ni) then
               imoins1 = ni - 1
               iplus1  = 1
               iplus2  = 2
               x1 = ax(ni-1)
               x2 = ax(ni)
               x3 = ax(1) + 360.0
               x4 = ax(2) + 360.0
            endif
            if (i.ne.1.and.i.ne.(ni-1).and.i.ne.ni) then
               print *, 'Maudit probleme'
               print *, 'i, ni, x = ', i, ni, x
            endif
         else
            x1=ax(imoins1)
            x2=ax(i)
            x3=ax(iplus1)
            x4=ax(iplus2)
         endif
         x = x2 + (x3-x2)*(px(n)-i)
         y = ay(j) + (ay(j+1)-ay(j))*(py(n)-j)
         y1=ay(j-1)
         y2=ay(j)
         y3=ay(j+1)
         y4=ay(j+2)
!     interpolation 1ere rangee selon x
         z1=z(imoins1,j-1)
         z2=z(i  ,j-1)
         z3=z(iplus1,j-1)
         z4=z(iplus2,j-1)
         a11 = z1
         a12 = fa2(dble(cx(i,1)),z1,z2)
         a13 = fa3(dble(cx(i,1)),dble(cx(i,2)),dble(cx(i,3)),z1,z2,z3)
         a14 = fa4(dble(cx(i,1)),dble(cx(i,2)),dble(cx(i,3)),         dble(cx(i,4)),dble(cx(i,5)),dble(cx(i,6)),         dble(z1),d&
     &ble(z2),dble(z3),dble(z4))
         b1  = fa(a11,a12,a13,a14,x,x1,x2,x3)
!     interpolation 2eme rangee selon x
         z1=z(imoins1,j)
         z2=z(i  ,j)
         z3=z(iplus1,j)
         z4=z(iplus2,j)
         a21 = z1
         a22 = fa2(dble(cx(i,1)),z1,z2)
         a23 = fa3(dble(cx(i,1)),dble(cx(i,2)),dble(cx(i,3)),z1,z2,z3)
         a24 = fa4(dble(cx(i,1)),dble(cx(i,2)),dble(cx(i,3)),         dble(cx(i,4)),dble(cx(i,5)),dble(cx(i,6)),dble(z1),dble(z2),d&
     &ble(z3),dble(z4))
         b2  = fa(a21,a22,a23,a24,x,x1,x2,x3)
!     interpolation 3eme rangee selon x
         z1=z(imoins1,j+1)
         z2=z(i  ,j+1)
         z3=z(iplus1,j+1)
         z4=z(iplus2,j+1)
         a31 = z1
         a32 = fa2(dble(cx(i,1)),z1,z2)
         a33 = fa3(dble(cx(i,1)),dble(cx(i,2)),dble(cx(i,3)),z1,z2,z3)
         a34 = fa4(dble(cx(i,1)),dble(cx(i,2)),dble(cx(i,3)),dble(cx(i,4)),         dble(cx(i,5)),dble(cx(i,6)),dble(z1),dble(z2),d&
     &ble(z3),dble(z4))
         b3  = fa(a31,a32,a33,a34,x,x1,x2,x3)
!     interpolation 4eme rangee selon x
         z1=z(imoins1,j+2)
         z2=z(i  ,j+2)
         z3=z(iplus1,j+2)
         z4=z(iplus2,j+2)
         a41 = z1
         a42 = fa2(dble(cx(i,1)),z1,z2)
         a43 = fa3(dble(cx(i,1)),dble(cx(i,2)),dble(cx(i,3)),z1,z2,z3)
         a44 = fa4(dble(cx(i,1)),dble(cx(i,2)),dble(cx(i,3)),dble(cx(i,4)),         dble(cx(i,5)),dble(cx(i,6)),dble(z1),dble(z2),d&
     &ble(z3),dble(z4))
         b4  = fa(a41,a42,a43,a44,x,x1,x2,x3)
!     interpolation finale selon y
         b11 = b1
         b12 = fa2(dble(cy(j,1)),b1,b2)
         b13 = fa3(dble(cy(j,1)),dble(cy(j,2)),dble(cy(j,3)),b1,b2,b3)
         b14 = fa4(dble(cy(j,1)),dble(cy(j,2)),dble(cy(j,3)),         dble(cy(j,4)),dble(cy(j,5)),dble(cy(j,6)),b1,b2,b3,b4)
         zo(n) = fa(b11,b12,b13,b14,y,y1,y2,y3)
      enddo
!zzzzzz$OMP END DO
!zzzzzz$OMP END PARALLEL
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_irgdint_3_wnnc(zo,px,py,npts,ax,ay,z,ni,j1,j2,wrap)
      implicit none
!*******
!     Auteur: Y.Chartier, drpn
!     Fevrier 1991
!     
!     Objet:  Interpolation bi-cubique de points a partir 
!     d'une grille source irreguliere.
!*******
      integer npts,ni,nj,j1,j2,wrap,limite
      integer imoins1, iplus1, iplus2
      real zo(npts),px(npts),py(npts)
      real ax(ni),ay(j1:j2)
      real z(ni,j1:j2)
!     
!     npts   : nombre de points a interpoler
!     1:ni  : dimension de la grille source selon x
!     1:nj  : dimension de la grille source selon y
!     zo     : vecteur de sortie contenant les valeurs interpolees
!     px     : vecteur contenant la position x des points que l'on
!              veut interpoler
!     py     : vecteur contenant la position y des points que l'on
!              veut interpoler
!     ax     : vecteur contenant la pos. des points sur l'axe des X.
!     ay     : vecteur contenant la pos. des points sur l'axe des Y.
!     cx     : vecteur contenant une table de differences selon X.
!     cy     : vecteur contenant une table de differences selon Y.
!     z      : valeurs de la grille source.
!     
!***************************************************************************
!     
!     *   *   *   *
!     
!     *   *   *   *
!           #        .eq.>   pt (x,y)
!     *  (=)  *   *  .eq.> = pt (i, j)
!     
!     *   *   *   *
!     
!     
!     
!     cx(i,1) = 1.0 / (x2-x1)
!     cx(i,2) = 1.0 / (x3-x1)
!     cx(i,3) = 1.0 / (x3-x2)
!     cx(i,4) = 1.0 / (x4-x1)
!     cx(i,5) = 1.0 / (x4-x2)
!     cx(i,6) = 1.0 / (x4-x3)
!     
!  structure identique pour cy(j,1..6)
      integer i, j, m, n
      real*8 fa, fa2, fa3, fa4
      real*8 a11,a12,a13,a14,a21,a22,a23,a24
      real*8 a31,a32,a33,a34,a41,a42,a43,a44
      real*8 b1,b2,b3,b4,b11,b12,b13,b14
      real*8 x1,x2,x3,x4,y1,y2,y3,y4
      real*8 a1,a2,a3,a4,x,y,c1,c2,c3,c4,c5,c6
      real*8 cx(6),cy(6)
      integer voisin, lineair, cubique
      integer oui, abort, valeur, maximum, minimum
      parameter (voisin  =   0)
      parameter (lineair =   1)
      parameter (cubique =   3)
      parameter (oui     =   1)
      parameter (maximum =   4)
      parameter (minimum =   5)
      parameter (valeur  =   6)
      parameter (abort   =  13)
!     definition des fonctions in-line
      real*8 cubic, dx,dy,z1,z2,z3,z4
      cubic(z1,z2,z3,z4,dx)=((((z4-z1)*0.1666666666666 + 0.5*(z2-z3))      *dx  + 0.5*(z1+z3)-z2)*dx  + z3-0.1666666666666*z4-0.5* &
     &     z2-0.3333333333333*z1)*dx+z2
      fa(a1,a2,a3,a4,x,x1,x2,x3)=a1+(x-x1)*(a2+(x-x2)*(a3+a4*(x-x3)))
      fa2(c1,a1,a2)=c1*(a2-a1)
      fa3(c1,c2,c3,a1,a2,a3)=c2*(c3*(a3-a2)-c1*(a2-a1))
      fa4(c1,c2,c3,c4,c5,c6,a1,a2,a3,a4)=c4*(c5*(c6*(a4-a3)-      c3*(a3-a2)) - c2*(c3*(a3-a2)-c1*(a2-a1)))
      limite = ni+2-wrap
      do n=1,npts
         i = min(ni-2+wrap,max(1,max(2-wrap,ifix(px(n)))))
         j = min(j2-2,max(j1+1,ifix(py(n))))
         imoins1 = i-1
         iplus1 = i+1
         iplus2 = i+2
         if (wrap.eq.1.and.(i.le.1.or.i.ge.(ni-wrap))) then
            if (i.eq.1) then
               imoins1 = ni-1
               iplus1  = 2
               iplus2  = 3
               x1 = ax(ni-1) - 360.0
               x2 = ax(1)
               x3 = ax(2)
               x4 = ax(3)
            endif
            if (i.eq.(ni-1)) then
               imoins1 = ni-2
               iplus1  = ni
               iplus2  = 1
               x1 = ax(ni-2)
               x2 = ax(ni-1)
               x3 = ax(ni)
               x4 = ax(2)+360.0
            endif
         elseif (wrap.eq.2.and.(i.le.1.or.i.gt.(ni-wrap))) then
            if (i.eq.1) then
               imoins1 = ni
               iplus1  = 2
               iplus2  = 3
               x1 = ax(ni) - 360.0
               x2 = ax(1)
               x3 = ax(2)
               x4 = ax(3)
            endif
            if (i.eq.(ni-1)) then
               imoins1 = ni - 2
               iplus1  = ni
               iplus2  = 1
               x1 = ax(ni-2)
               x2 = ax(ni-1)
               x3 = ax(ni)
               x4 = ax(1) + 360.0
            endif
            if (i.eq.ni) then
               imoins1 = ni - 1
               iplus1  = 1
               iplus2  = 2
               x1 = ax(ni-1)
               x2 = ax(ni)
               x3 = ax(1) + 360.0
               x4 = ax(2) + 360.0
            endif
            if (i.ne.1.and.i.ne.(ni-1).and.i.ne.ni) then
               print *, 'Maudit probleme'
               print *, 'i, ni, x = ', i, ni, x
            endif
         else
            x1=ax(imoins1)
            x2=ax(i)
            x3=ax(iplus1)
            x4=ax(iplus2)
         endif
         x = x2 + (x3-x2)*(px(n)-i)
         y = ay(j) + (ay(j+1)-ay(j))*(py(n)-j)
         cx(1) = 1.0/(x2-x1)
         cx(2) = 1.0/(x3-x1)
         cx(3) = 1.0/(x3-x2)
         cx(4) = 1.0/(x4-x1)
         cx(5) = 1.0/(x4-x2)
         cx(6) = 1.0/(x4-x3)
         y1=ay(j-1)
         y2=ay(j)
         y3=ay(j+1)
         y4=ay(j+2)
!     interpolation 1ere rangee selon x
         z1=z(imoins1,j-1)
         z2=z(i  ,j-1)
         z3=z(iplus1,j-1)
         z4=z(iplus2,j-1)
         a11 = z1
         a12 = fa2(cx(1),z1,z2)
         a13 = fa3(cx(1),cx(2),cx(3),z1,z2,z3)
         a14 = fa4(cx(1),cx(2),cx(3),cx(4),cx(5),cx(6),         z1,z2,z3,z4)
         b1  = fa(a11,a12,a13,a14,x,x1,x2,x3)
!     interpolation 2eme rangee selon x
         z1=z(imoins1,j)
         z2=z(i  ,j)
         z3=z(iplus1,j)
         z4=z(iplus2,j)
         a21 = z1
         a22 = fa2(cx(1),z1,z2)
         a23 = fa3(cx(1),cx(2),cx(3),z1,z2,z3)
         a24 = fa4(cx(1),cx(2),cx(3),cx(4),         cx(5),cx(6),z1,z2,z3,z4)
         b2  = fa(a21,a22,a23,a24,x,x1,x2,x3)
!     interpolation 3eme rangee selon x
         z1=z(imoins1,j+1)
         z2=z(i  ,j+1)
         z3=z(iplus1,j+1)
         z4=z(iplus2,j+1)
         a31 = z1
         a32 = fa2(cx(1),z1,z2)
         a33 = fa3(cx(1),cx(2),cx(3),z1,z2,z3)
         a34 = fa4(cx(1),cx(2),cx(3),cx(4),         cx(5),cx(6),z1,z2,z3,z4)
         b3  = fa(a31,a32,a33,a34,x,x1,x2,x3)
!     interpolation 4eme rangee selon x
         z1=z(imoins1,j+2)
         z2=z(i  ,j+2)
         z3=z(iplus1,j+2)
         z4=z(iplus2,j+2)
         a41 = z1
         a42 = fa2(cx(1),z1,z2)
         a43 = fa3(cx(1),cx(2),cx(3),z1,z2,z3)
         a44 = fa4(cx(1),cx(2),cx(3),cx(4),         cx(5),cx(6),z1,z2,z3,z4)
         b4  = fa(a41,a42,a43,a44,x,x1,x2,x3)
!     interpolation finale selon y
         cy(1) = 1.0 / (y2-y1)
         cy(2) = 1.0 / (y3-y1)
         cy(3) = 1.0 / (y3-y2)
         cy(4) = 1.0 / (y4-y1)
         cy(5) = 1.0 / (y4-y2)
         cy(6) = 1.0 / (y4-y3)
         b11 = b1
         b12 = fa2(cy(1),b1,b2)
         b13 = fa3(cy(1),cy(2),cy(3),b1,b2,b3)
         b14 = fa4(cy(1),cy(2),cy(3),cy(4),         cy(5),cy(6),b1,b2,b3,b4)
         zo(n) = fa(b11,b12,b13,b14,y,y1,y2,y3)
      enddo
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!**s/r ez_lac transformation from a set of points (lat,lon) in the spherical 
!             coordinate system to cartesian space
!
      subroutine ez_lac( xyz,lon,lat,n)
      implicit none
      integer n
      real xyz(3,n),lon(n),lat(n)
!
!author michel roch - april 90
!
!revision
!       001 - michel roch - feb 94 - repackaging for integration in rmnlib
!
!arguments
!    out    xyz     - coordinates in cartesian space
!    in     lon     - longitudes in spherical coordinates
!           lat     - latitudes  in spherical coordinates
!           n       - dimension of the fields
!
!*
      integer i,k
      real dar
      dar = acos(-1.)/180.
!     
      k=0
      do 30 i=1,n
         k=k+1
         xyz(1,k)=cos(dar*lat(i))*cos(dar*lon(i))
         xyz(2,k)=cos(dar*lat(i))*sin(dar*lon(i))
         xyz(3,k)=sin(dar*lat(i))
 30   continue
!     
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!**s/r ez_lac transformation from a set of points (lat,lon) in the spherical 
!             coordinate system to cartesian space
!
      subroutine ez_lac_8( xyz,lon,lat,n)
      implicit none
      integer n
      real*8 xyz(3,n)
      real lon(n),lat(n)
!
!author michel roch - april 90
!
!revision
!       001 - michel roch - feb 94 - repackaging for integration in rmnlib
!
!arguments
!    out    xyz     - coordinates in cartesian space
!    in     lon     - longitudes in spherical coordinates
!           lat     - latitudes  in spherical coordinates
!           n       - dimension of the fields
!
!*
      integer i,k
      real dar
      dar = acos(-1.)/180.
!     
      k=0
      do 30 i=1,n
         k=k+1
         xyz(1,k)=cos(dar*lat(i))*cos(dar*lon(i))
         xyz(2,k)=cos(dar*lat(i))*sin(dar*lon(i))
         xyz(3,k)=sin(dar*lat(i))
 30   continue
!     
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_lamb_gdwfllw(z1,z2,xlon,li,lj,grtyp,ig1,ig2,ig3,ig4,x1,y1,xlat)
      implicit none
      integer li,lj
      real z1(li*lj), z2(li*lj), xlon(li*lj)
      character*1 grtyp
      integer ig1,ig2,ig3,ig4
      real x1(li*lj,2),y1(li*lj,2),xlat(li*lj,2)
      real delx,dely,uuu,vvv,alpha,psi
      integer i,j
!     
!     * 1.866025=(1+sin60),   6.371e+6=earth radius in meters.      
!     
!     rdtodg = 180/pie, dgtord = pie/180        
      real pie,rdtodg,dgtord
      data pie    /3.1415926535898/
      data rdtodg /57.295779513082/
      data dgtord /1.7453292519943e-2/
      do i=1,li*lj
         xlat(i,1) = 45.0
         xlat(i,2) = 50.0
      enddo
      call ez_lambfll(x1(1,1),y1(1,1),xlat(1,1),xlon(1),li*lj,grtyp,ig1,ig2,ig3,ig4)
      call ez_lambfll(x1(1,2),y1(1,2),xlat(1,2),xlon(1),li*lj,grtyp,ig1,ig2,ig3,ig4)
      do i=1,li*lj
         delx = x1(i,2) - x1(i,1)
         dely = y1(i,2) - y1(i,1)
         psi = 270.0 -  z2(i)
         uuu = cos(psi*dgtord)* z1(i)
         vvv = sin(psi*dgtord)* z1(i)
         alpha = atan2(dely,delx) - 0.5*pie
         z1(i) = uuu*cos(alpha)-vvv*sin(alpha)
         z2(i) = uuu*sin(alpha)+vvv*cos(alpha)
      enddo
!     
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_lamb_llwfgdw(z1,z2,xlon,li,lj,grtyp,ig1,ig2,ig3,ig4,x1,y1,xlat)
      implicit none
      integer li,lj
      real z1(li*lj), z2(li*lj), xlon(li*lj)
      character*1 grtyp
      integer ig1,ig2,ig3,ig4
      real x1(li*lj,2),y1(li*lj,2),xlat(li*lj,2)
      real delx,dely,uuu,vvv,alpha,psi
      integer i,j,ier
!     
!     * 1.866025=(1+sin60),   6.371e+6=earth radius in meters.      
!     
!     rdtodg = 180/pie, dgtord = pie/180        
      real spd0,dir0
      real pie,rdtodg,dgtord
      data pie    /3.1415926535898/
      data rdtodg /57.295779513082/
      data dgtord /1.7453292519943e-2/
      do i=1,li*lj
         xlat(i,1) = 45.0
         xlat(i,2) = 50.0
      enddo
      call ez_lambfll(x1(1,1),y1(1,1),xlat(1,1),xlon(1),li*lj,grtyp,ig1,ig2,ig3,ig4)
      call ez_lambfll(x1(1,2),y1(1,2),xlat(1,2),xlon(1),li*lj,grtyp,ig1,ig2,ig3,ig4)
      do i=1,li*lj
         delx = x1(i,2) - x1(i,1)
         dely = y1(i,2) - y1(i,1)
         alpha = pie*0.50 - atan2(dely,delx)
         uuu = z1(i)
         vvv = z2(i)
         z1(i) = uuu*cos(alpha)-vvv*sin(alpha)
         z2(i) = uuu*sin(alpha)+vvv*cos(alpha)
         spd0=sqrt(z1(i)*z1(i)+z2(i)*z2(i))
         if( (spd0.eq. 0.0))then
            dir0= 0.0
         else
            if( (z1(i).eq. 0.0))then
               if( (z2(i).ge. 0.0))then
                  dir0= 180.0
               else
                  dir0= 0.0
               endif
            else
               dir0=270.0 - rdtodg*atan2(z2(i),z1(i))
            endif
         endif
         dir0=amod(amod(dir0,360.0)+360.0,360.0)
         z1(i)=spd0
         z2(i)=dir0
      enddo
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_lambfll(x,y,xlat,xlon,npts,grtyp,ig1,ig2,ig3,ig4)
      implicit none
      integer npts
      character*1 grtyp
      real x(npts),y(npts),xlat(npts),xlon(npts)
      integer ig1,ig2,ig3,ig4
      real xg(15)
      character*1 gtypout
      real xlat11,xlon11,xlatninj,xlonninj,dx,dy,latin1,latin2
      real yaxislat,yaxislon
      real x11,y11
      integer i,nxg
      integer lclig1,lclig2,lclig3,lclig4
      lclig1=ig1
      lclig2=ig2
      lclig3=ig3
      lclig4=ig4
      nxg =15
      if (grtyp.eq.'!') then
         call igaxg95(gtypout,xg,15,grtyp,lclig1,lclig2,lclig3,lclig4)
         if (gtypout.eq.'H') then
            xlat11 =   xg( 1)
            xlon11 =   xg( 2)
            xlatninj = xg(10)
            xlonninj = xg(11)
            yaxislat = 0.5 * (xlat11 + xlatninj)
            yaxislon = xg(5)
            latin1   = xg(6)
            latin2   = xg(7)
            dx       = xg(3)*1000.0
            dy       = xg(4)*1000.0
            call ez_lambxyfll99(x11,y11,xlat11,xlon11,1,            latin1,latin2,yaxislat,yaxislon)
            call ez_lambxyfll99(x,y,xlat,xlon,npts,            latin1,latin2,yaxislat,yaxislon)
            do i=1,npts
               x(i) = 1.0 + (x(i) - x11)/dx
               y(i) = 1.0 + (y(i) - y11)/dy
            enddo
         endif
      endif
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_lambllfxy99(lat,lon,x,y,n,      latin1,latin2,yaxislat,yaxislon)
      implicit none
      integer n
      real x(n),y(n),lat(n),lon(n)
      real lat11,lon11,latninj,lonninj,latin1,latin2,yaxislat,yaxislon
      real pi,pisur4,d2r,rn,rphi1,rphi2,r,f,rtan,rho,theta,rhozero,dlon
      integer i
      pisur4 = atan(1.0)
      pi     = 4.0 * pisur4
      d2r    = pi / 180.0
      r = 6370997.0
      rphi1 = d2r * latin1
      rphi2 = d2r * latin2
      if (rphi1.eq.rphi2) then
         rn = sin(rphi1)
      else
         rn = log(cos(rphi1)/cos(rphi2))
         rn = rn / log((tan(pisur4+0.5*rphi2))/tan(pisur4+0.5*rphi1))
      endif
      rtan = tan(pisur4 + rphi1 * 0.5)
      f = (cos(rphi1)*(rtan ** rn))/rn
      rhozero = r * f / (tan(pisur4+yaxislat*d2r*.5)**rn)
      do i = 1,n
         rho = sign(1.0,rn) * sqrt(x(i)*x(i)+((rhozero-y(i))**2))
         theta = atan(x(i)/(rhozero - y(i)))
         lat(i) = (2.0 * atan((r*f/rho)**(1.0/rn)) - 0.5*pi)/d2r
         lon(i) = theta/(d2r*rn) + yaxislon
      enddo
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_lambxyfll99(x,y,lat,lon,n,      latin1,latin2,yaxislat,yaxislon)
      implicit none
      integer n
      real x(n),y(n),lat(n),lon(n)
      real lat11,lon11,latninj,lonninj,latin1,latin2,yaxislat,yaxislon
      real pi,pisur4,d2r,rn,rphi1,rphi2,r,f,rtan,rho,theta,rhozero,dlon,tmplat
      integer i
      pisur4 = atan(1.0)
      pi     = 4.0 * pisur4
      d2r    = pi / 180.0
      r = 6370997.0
      rphi1 = d2r * latin1
      rphi2 = d2r * latin2
      if (rphi1.eq.rphi2) then
         rn = sin(rphi1)
      else
         rn = log(cos(rphi1)/cos(rphi2))
         rn = rn / log((tan(pisur4+0.5*rphi2))/tan(pisur4+0.5*rphi1))
      endif
      rtan = tan(pisur4 + rphi1 * 0.5)
      f = (cos(rphi1)*(rtan ** rn))/rn
      rhozero = r * f / (tan(pisur4+yaxislat*d2r*.5)**rn)
      do i = 1,n
         tmplat = lat(i)
         if (tmplat.gt.90.0) tmplat = 89.95
         rho   = r * f / (tan(pisur4+tmplat*0.5*d2r)**rn)
         dlon = lon(i) - yaxislon
         if (dlon.lt.-180) then
            dlon = dlon + 360.
         else if (dlon.gt.180.) then
            dlon = dlon - 360.
         endif
         theta = rn * (d2r * dlon)
         x(i) = rho * sin(theta)
         y(i) = rhozero - rho*cos(theta)
      enddo
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!**s/r ll2ergd - conversion de coordonnees lat-lon a pts de grille
      subroutine ez_ll2ergd(px,py,xlat,xlon,npts,ni,nj,grtyp,      ig1,ig2,ig3,ig4)
      implicit none
      integer global, nord, sud, sudnord, nordsud
      integer absolu, relatif
      parameter (global = 0)
      parameter (nord   = 1)
      parameter (sud    = 2)
      parameter (sudnord= 0)
      parameter (nordsud= 1)
      parameter (absolu = 0)
      parameter (relatif = 1)
!     
!     * 1.866025=(1+sin60),   6.371e+6=earth radius in meters.      
!     
!     rdtodg = 180/pie, dgtord = pie/180        
      real pie,rdtodg,dgtord
      data pie    /3.1415926535898/
      data rdtodg /57.295779513082/
      data dgtord /1.7453292519943e-2/
!         
      external cigaxg,ez_llll2gd,permut
      integer npts, ni, nj
      real px(npts),py(npts),xlat(npts),xlon(npts)
      character*1 grtyp
      integer ig1,ig2,ig3,ig4
      real dellat, dellon, xlat0, xlon0
      real xlat1,xlat2,xlon1,xlon2
      real xlatgf(npts),xlongf(npts)
      integer ier
      integer i,j
      real rinc
      call cigaxg(grtyp,xlat1,xlon1,xlat2,xlon2,ig1,ig2,ig3,ig4)
      call ez_gfxyfll(xlon,xlat,xlongf,xlatgf,npts,      xlat1,xlon1,xlat2,xlon2)
      if (grtyp.eq.'E') then
         dellon = 360.0 / real(ni-1)
         xlon0 = 0.0
         dellat = 180.0 / real(nj)
         xlat0 = -90. + 0.5*dellat
         call ez_llll2gd(px,py,xlatgf,xlongf,npts,         xlat0,xlon0,dellat,dellon, 0.0)
      endif
      return
      end
!-----------------------------------------------------------------
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!**s/r ez_ll2igd - conversion de coordonnees lat-lon a pts de grille
      subroutine ez_ll2igd(px,py,xlat,xlon,npts,ni,nj,grtyp,grref,      ig1,ig2,ig3,ig4,ax,ay,coordflag)
      implicit none
      integer global, nord, sud, sudnord, nordsud
      integer absolu, relatif
      parameter (global = 0)
      parameter (nord   = 1)
      parameter (sud    = 2)
      parameter (sudnord= 0)
      parameter (nordsud= 1)
      parameter (absolu = 0)
      parameter (relatif = 1)
      integer coordflag
      external cigaxg,ez_vxyfll,ez_llll2gd,permut
      integer i, npts, ni, nj
      real px(npts),py(npts),xlat(npts),xlon(npts)
      character*1 grtyp, grref
      integer ig1,ig2,ig3,ig4
      real ax(ni), ay(nj)
      real pi,pj,dgrw,d60
      real dlat, dlon, xlat0, xlon0
      real xlat1,xlon1,xlat2,xlon2
      real lonref
      integer indx, indy
      integer ez_cherche
      external ez_cherche
      if (grref .eq. 'N') then
         call cigaxg(grref,  PI, PJ, D60, DGRW, ig1, ig2, ig3, ig4)
         call ez_vxyfll(px, py, xlat, xlon, npts, d60,dgrw,pi,pj,1)
      endif
      if (grref .eq. 'S') then
         call cigaxg(grref,  PI, PJ, D60, DGRW, ig1, ig2, ig3, ig4)
         call ez_vxyfll(px, py, xlat, xlon, npts, d60,dgrw,pi,pj,2)
      endif
      if (grref.eq.'L') then
         call cigaxg(grref,xlat0,xlon0,dlat,dlon,ig1,ig2,ig3,ig4)
         if (ax(1).lt.0.0) then
            lonref = -180.0
         else
            lonref = 0.0
         endif
         call ez_llll2gd(px,py,xlat,xlon,npts,xlat0,xlon0,dlat,dlon,lonref)
         do i=1,npts
            px(i) = px(i)-1.0
            py(i) = py(i)-1.0
         enddo
      endif
      if (grref.eq.'E') then
         call cigaxg(grref,xlat1,xlon1,xlat2,xlon2,ig1,ig2,ig3,ig4)
         call ez_gfxyfll(xlon,xlat,px,py,npts,xlat1,xlon1,xlat2,xlon2)
      endif
      if (coordflag .eq. relatif) then
         do 10 i=1,npts
            indx = ez_cherche(px(i),ax,ni)
            indy = ez_cherche(py(i),ay,nj)
            if (indx .ge. ni) indx = ni - 1
            if (indy .ge. nj) indy = nj - 1
            px(i) = real(indx)+(px(i)-ax(indx))/(ax(indx+1)-ax(indx))
            py(i) = real(indy)+(py(i)-ay(indy))/(ay(indy+1)-ay(indy))
 10      continue
      endif
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!**s/r ll2rgd - conversion de coordonnees lat-lon a pts de grille
      subroutine ez_ll2rgd(px,py,xlat,xlon,npts,ni,nj,grtyp,      ig1,ig2,ig3,ig4,sym,lroots)
      implicit none
      integer global, nord, sud, sudnord, nordsud
      integer absolu, relatif
      parameter (global = 0)
      parameter (nord   = 1)
      parameter (sud    = 2)
      parameter (sudnord= 0)
      parameter (nordsud= 1)
      parameter (absolu = 0)
      parameter (relatif = 1)
!     
!     * 1.866025=(1+sin60),   6.371e+6=earth radius in meters.      
!     
!     rdtodg = 180/pie, dgtord = pie/180        
      real pie,rdtodg,dgtord
      data pie    /3.1415926535898/
      data rdtodg /57.295779513082/
      data dgtord /1.7453292519943e-2/
!         
      integer voisin, lineair, cubique
      integer oui, abort, valeur, maximum, minimum
      parameter (voisin  =   0)
      parameter (lineair =   1)
      parameter (cubique =   3)
      parameter (oui     =   1)
      parameter (maximum =   4)
      parameter (minimum =   5)
      parameter (valeur  =   6)
      parameter (abort   =  13)
      external cigaxg,ez_vxyfll,ez_llll2gd,permut
      integer npts, ni, nj
      real px(npts),py(npts),xlat(npts),xlon(npts)
      character*1 grtyp
      integer ig1,ig2,ig3,ig4
      logical sym
      real pi,pj,dgrw,d60
      real dellat, dellon, xlat0, xlon0
      real lambx1, lamby1, res
      real lroots(nj)
      real clat, clon
      real xg(20)
      character*1 gtyout,gtyin
      real swlat,swlon,dx,latin1,latin2,yaxislon
      integer i,j
      if (grtyp.eq.'N') then
         call cigaxg(grtyp,pi,pj,d60,dgrw,ig1,ig2,ig3,ig4)
         call ez_vxyfll(px,py,xlat,xlon,npts,d60,dgrw,pi,pj,NORD)
         return
      endif
      if (grtyp.eq.'S')  then
         call cigaxg(grtyp,pi,pj,d60,dgrw,ig1,ig2,ig3,ig4)
         call ez_vxyfll(px,py,xlat,xlon,npts,d60,dgrw,pi,pj,SUD)
         return
      endif
      if (grtyp.eq.'T') then
         call cigaxg(grtyp,d60,dgrw,clat,clon,ig1,ig2,ig3,ig4)
         call ez_vtxyfll(px,py, xlat, xlon, clat, clon, d60, dgrw, ni, nj, npts)
         return
      endif
      if (grtyp.eq.'A') then
         dellon = 360.0 / real(ni)
         xlon0 = 0.0
         if (ig1 .eq. GLOBAL) then
            dellat = 180.0 / real(nj)
            xlat0 = -90.0 + dellat * 0.5
         endif
         if (ig1 .eq. NORD) then
            dellat = 90.0 / real(nj)
            xlat0 =  dellat * 0.5
         endif
         if (ig1 .eq. SUD) then
            dellat = 90.0 / real(nj)
            xlat0 = -90.0 + dellat * 0.5
         endif
         do i=1,npts
            if (xlon(i).lt.0.0) xlon(i) = xlon(i) + 360.0
         enddo
         call ez_llll2gd(px,py,xlat,xlon,npts,xlat0, xlon0, dellat, dellon, 0.0)
         return
      endif
      if (grtyp.eq.'B') then
         dellon = 360.0 / real(ni-1)
         xlon0 = 0.0
         if (ig1 .eq. GLOBAL) then
            dellat = 180.0 / real(nj-1)
            xlat0 = -90.0
         endif
         if (ig1 .eq. NORD) then
            dellat = 90.0 / real(nj-1)
            xlat0 =  0.0
         endif
         if (ig1 .eq. SUD) then
            dellat = 90.0 / real(nj-1)
            xlat0 = -90.0
         endif
         do i=1,npts
            if (xlon(i).lt.0.0) xlon(i) = xlon(i) + 360.0
         enddo
         call ez_llll2gd(px,py,xlat,xlon,npts,xlat0, xlon0, dellat, dellon, 0.0)
         return
      endif
      if (grtyp .eq. 'G') then
         dellon = 360.0 / real(ni)
         xlon0 = 0.0
         do i=1,npts
            if (xlon(i).lt.0.0) xlon(i) = xlon(i) + 360.0
         enddo
         if (ig1.eq.GLOBAL) then
            call ez_ggll2gd(px,py,xlat,xlon,npts,ni,nj,ig1,lroots)
         else if  (ig1 .eq. NORD) then
            dellat = 90.0 / real(nj)
            xlat0 =  dellat * 0.5
            call ez_llll2gd(px,py,xlat,xlon,npts,xlat0, xlon0, dellat, dellon, 0.0)
         else
            dellat = 90.0 / real(nj)
            xlat0 = -90.0 + dellat * 0.5
            call ez_llll2gd(px,py,xlat,xlon,npts,xlat0, xlon0, dellat, dellon, 0.0)
         endif
         return
      endif
      if (grtyp .eq. 'L') then
         call cigaxg(grtyp,xlat0,xlon0,dellat,dellon,ig1,ig2,ig3,ig4)
         do 10 i=1,npts
            if (xlon(i).lt.xlon0) then
               xlon(i) = xlon(i) + 360.0
            endif
            if (xlon(i).gt.(xlon0 + ni*dellon)) then
               xlon(i) = xlon(i) - 360.0
            endif
 10      continue
         call ez_llll2gd(px,py,xlat,xlon,npts,xlat0,xlon0,dellat,dellon, 0.0)
         return
      endif
      if (grtyp.eq.'E') then
         call ez_ll2ergd(px,py,xlat,xlon,npts,ni,nj,grtyp,      ig1,ig2,ig3,ig4)
         return
      endif
      if (grtyp.eq.'!') then
         call ez_lambfll(px,py,xlat,xlon,npts,grtyp,ig1,ig2,ig3,ig4)
         return
      endif
      print *,'<ez_ll2rgd> bad grid type for type: ',grtyp
      print *,'         any further processing will create scrap'
      print *,'         stopping immediately !'
      stop
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_llflamb(xlat,xlon,x,y,npts,grtyp,ig1,ig2,ig3,ig4)
      implicit none
      integer npts
      real xlat(npts),xlon(npts)
      real x(npts),y(npts)
      character*1 grtyp,gtypout
      integer ig1,ig2,ig3,ig4,nxg
      real xg(15)
      real xlat11,xlon11,xlatninj,xlonninj,dx,dy,latin1,latin2
      real yaxislat,yaxislon
      real x11,y11
      integer i
      nxg =15
      if (grtyp.eq.'!') then
         call igaxg95(gtypout,xg,15,grtyp,ig1,ig2,ig3,ig4)
         if (gtypout.eq.'H') then
            xlat11 =   xg( 1)
            xlon11 =   xg( 2)
            xlatninj = xg(10)
            xlonninj = xg(11)
            yaxislon = xg(5)
            yaxislat = 0.5 * (xlat11 + xlatninj)
            latin1   = xg(6)
            latin2   = xg(7)
            dx       = xg(3)*1000.0
            dy       = xg(4)*1000.0
            call ez_lambxyfll99(x11,y11,xlat11,xlon11,1,            latin1,latin2,yaxislat,yaxislon)
            do i=1,npts
               x(i) = x11 + dx * (x(i) - 1.0)
               y(i) = y11 + dy * (y(i) - 1.0)
            enddo
            call ez_lambllfxy99(xlat,xlon,x,y,npts,            latin1,latin2,yaxislat,yaxislon)
         endif
      endif
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!**s/r ez_llll2gd - computes the grid co-ordinates of a point
!     
      subroutine ez_llll2gd(x,y,dlat,dlon,npts,xlat0,xlon0, dellat, dellon, lonref)
      implicit none
!*--------------------------------------------------------------------
      integer npts
      real x(npts), y(npts), dlat(npts), dlon(npts)
      real xlat0, xlon0, dellat, dellon, lonref
      integer i
      if (lonref.eq.-180.0) then
         do i=1,npts
            if (dlon(i).gt.180.0) then
               dlon(i) = dlon(i) - 360.0
            endif
         enddo
      endif
      do 10 i=1,npts
         x(i) = (dlon(i) - xlon0)/dellon + 1.0
         y(i) = (dlat(i) - xlat0)/dellat + 1.0
 10   continue
      return
      end
!-----------------------------------------------------------------
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!**     s/r ez_llwfgdw - converts from grid wind components to std
!       meteorological speed and direction
   subroutine ez_llwfgdw(z1,z2,xlon,li,lj,grtyp,ig1,ig2,ig3,ig4)
   implicit none
   integer li,lj
   real z1(li,lj), z2(li,lj), xlon(li,lj)
   character*1 grtyp
   integer ig1,ig2,ig3,ig4
!
!       AUTHOR     Yves Chartier              April 1991
!
!       REVISION
!       Documentation update             Jul 1994
!
!       LANGUAGE   Fortran 77
!
!       OBJECT (gdw2llw)
!       Converts grid wind components to meteorological wind speed and direction,
!       independent of geographical projection.
!       On output, a 270 degree wind is a westerly wind, meaning U is +ve and
!       V is zero.
!
!       FILES
!
!       ARGUMENTS
!       NAMES          I/O  TYPE  A/S        DESCRIPTION
!       z1           I/O   R     A         On entry, contains the U grid component
!       of wind. On exit, contains wind speed.
!       z2           I/O   R     A         On entry, contains the V component
!       of wind. On exit, contains wind dir.
!       xlon           I     R     A         Array of longitudes
!       li             I     I     S         X dimension of the grid
!       lj             I     I     S         Y dimension of the grid
!       grtyp          I     C     S         Grid type
!       ig1-2-3-4      I     I     S         Grid descriptors
!
!       IMPLICIT
!       include
!
!       MODULES
   external cigaxg
!       *
!
!-------------------------------------------------------------
!
!
!
!       * 1.866025=(1+sin60),   6.371e+6=earth radius in meters.
!
!       rdtodg = 180/pie, dgtord = pie/180
!
!     
!     * 1.866025=(1+sin60),   6.371e+6=earth radius in meters.      
!     
!     rdtodg = 180/pie, dgtord = pie/180        
      real pie,rdtodg,dgtord
      data pie    /3.1415926535898/
      data rdtodg /57.295779513082/
      data dgtord /1.7453292519943e-2/
!         
   real xg1, xg2, xg3, xg4, dgrw
   character*1 gtypout
   real xg(20)
   integer nxg
   real delx,dely,alpha
   real uuu,vvv
   integer i,j,k,n,ntot
   real spd0, dir0
   integer npts,ier
   real x1(2*li*lj),y1(2*li*lj),lat(2*li*lj)
!  les #define qui suivent rendent le code plus lisible
   if (grtyp .eq. '!') then
      call ez_lamb_llwfgdw(z1,z2,xlon,li,lj,grtyp,ig1,ig2,ig3,ig4,x1,y1,lat)
      return
   endif
   if (grtyp.eq. 'N')then
      call cigaxg(grtyp,xg1,xg2,xg3,xg4,ig1,ig2,ig3,ig4)
      do j=1,lj
         do i=1,li
            spd0=sqrt(z1(i,j)*z1(i,j)+z2(i,j)*z2(i,j))
            if (spd0.eq. 0.0)then
               dir0= 0.0
            else
              if (z1(i,j).eq. 0.0)then
                 if (z2(i,j).ge. 0.0)then
                    dir0= xlon(i,j)+xg4-90.0
                 else
                    dir0= xlon(i,j)+xg4+90.0
                 endif
              else
                 dir0=xlon(i,j)+xg4-rdtodg*atan2(z2(i,j),z1(i,j))
              endif
            endif
            dir0=amod(amod(dir0,360.0)+360.0,360.0)
            z1(i,j)=spd0
            z2(i,j)=dir0
           enddo
      enddo
      return
   endif
   if (grtyp.eq. 'S')then
      call cigaxg(grtyp,xg1,xg2,xg3,xg4,ig1,ig2,ig3,ig4)
      do j=1,lj
         do i=1,li
            spd0=sqrt(z1(i,j)*z1(i,j)+z2(i,j)*z2(i,j))
            if (spd0.eq. 0.0)then
               dir0= 0.0
            else
               if (z1(i,j).eq. 0.0)then
                  if (z2(i,j).ge. 0.0)then
                     dir0= 90.0 - xlon(i,j)+xg4
                  else
                     dir0= 270.0 - xlon(i,j)+xg4
                 endif
               else
                 dir0=180.0-xlon(i,j)+xg4-rdtodg*atan2(z2(i,j),z1(i,j))
               endif
            endif
            dir0=amod(amod(dir0,360.0)+360.0,360.0)
            z1(i,j)=spd0
            z2(i,j)=dir0
         enddo
      enddo
      return
   endif
   if (grtyp.eq.'A'.or.grtyp.eq.'B'.or.grtyp.eq.'G'.or.grtyp.eq.'L')then
      do j=1,lj
         do i=1,li
            spd0=sqrt(z1(i,j)*z1(i,j)+z2(i,j)*z2(i,j))
            if (spd0.eq. 0.0)then
               dir0= 0.0
            else
               if (z1(i,j).eq. 0.0)then
                  if (z2(i,j).ge. 0.0)then
                     dir0= 180.0
                  else
                     dir0= 0.0
                  endif
               else
                  dir0=270.0 - rdtodg*atan2(z2(i,j),z1(i,j))
               endif
            endif
            dir0=amod(amod(dir0,360.0)+360.0,360.0)
            z1(i,j)=spd0
            z2(i,j)=dir0
         enddo
      enddo
      return
   endif
   write(6, 600) grtyp
 600   format('0',' erreur, bad grid type (llwfgdw) - grtyp = ', a11)
   return
   end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_mdup(out,in,npts)
      integer npts
      real out(npts),in(npts)
      integer i
      do i=1,npts
         out(i)=in(i)
      enddo
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_module(u,v,li,lj)
      integer li,lj
      real u(li,lj), v(li,lj)
      integer i,j
      do 10 i=1,li
         do 20 j=1,lj
            u(i,j)=sqrt(u(i,j)*u(i,j)+v(i,j)*v(i,j))
 20      continue
 10   continue
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!**S/R MTXINV - INVERT A MATRIX 
! 
      SUBROUTINE  EZ_MTXINV8 (A,L,M,N)
      implicit none
      INTEGER N
      REAL*8 A(N,N)
      INTEGER L(N), M(N)
! 
!Author
!          R. Sarrazin (1992)REAL*8 version for MTXINV
!
!Object 
!          to invert a matrix (in double precision)
!
!Revision
! 001      C. Thibeault (Apr 80) Documentation
! 002      M. Valin (Nov 86) More logical algorithm
! 003      R. Sarrazin (1992) Real*8 of MTXINV
!
!Arguments
!
!          - Input/Output -
! A        input matrix, destroyed in computation and replaced by 
!          resultant inverse.  A must be a general matrix.
!
!          - Input -
! L        work vector of length N
! M        work vector of length N
! N        order of matrix A
!
!Notes
!          This routine based closely upon IBM-SSP subroutine MTXINV.  
!          The standard Gauss-Jordan method is used.  If the matrix 
!          is singular, a message is printed and program execution is 
!          terminated.
!
!*
      INTEGER I, J, K
      REAL*8 HOLD, BIGA
! 
!        SEARCH FOR LARGEST ELEMENT 
! 
      DO 80 K=1,N
        L(K)=K
        M(K)=K
        BIGA=A(K,K)
        DO 20 J=K,N
        DO 20 I=K,N
          IF( ABS(BIGA).LT.ABS(A(I,J))) THEN
            BIGA=A(I,J)
            L(K)=I
            M(K)=J
          ENDIF
20      CONTINUE
! 
!     INTERCHANGE ROWS
! 
      J=L(K)
      IF(J.GT.K) THEN
        DO 30 I=1,N
          HOLD=-A(K,I)
          A(K,I)=A(J,I)
30        A(J,I) =HOLD
      ENDIF
! 
!     INTERCHANGE COLUMNS 
! 
      I=M(K)
      IF(I.GT.K) THEN
        DO 40 J=1,N
          HOLD=-A(J,K)
          A(J,K)=A(J,I)
40        A(J,I) =HOLD
      ENDIF
! 
!     DIVIDE COLUMN BY MINUS PIVOT (VALUE OF PIVOT ELEMENT IS 
!     CONTAINED IN BIGA)
! 
      IF(BIGA.EQ.0)THEN
        WRITE (6,601)
        STOP
      ENDIF
      DO 51 I=1,K-1
51    A(I,K)=A(I,K)/(-BIGA)
      DO 52 I=K+1,N
52    A(I,K)=A(I,K)/(-BIGA)
! 
!        REDUCE MATRIX
! 
      DO 65 I=1,N
        HOLD=A(I,K)
        IF(I.NE.K)THEN
          DO 61 J=1,K-1
61        A(I,J)=HOLD*A(K,J)+A(I,J)
          DO 62 J=K+1,N
62        A(I,J)=HOLD*A(K,J)+A(I,J)
        ENDIF
65    CONTINUE
! 
!     DIVIDE ROW BY PIVOT 
! 
      DO 71 J=1,K-1
71    A(K,J)=A(K,J)/BIGA
      DO 72 J=K+1,N
72    A(K,J)=A(K,J)/BIGA
! 
!     REPLACE PIVOT BY RECIPROCAL 
! 
      A(K,K)=1.0/BIGA
   80 CONTINUE
! 
!     FINAL ROW AND COLUMN INTERCHANGE
! 
      DO 150 K=N-1,1,-1
        I=L(K)
        IF(I.GT.K) THEN
          DO 110 J=1,N
            HOLD=A(J,K)
            A(J,K)=-A(J,I)
110         A(J,I) =HOLD
        ENDIF
        J=M(K)
        IF(J.GT.K) THEN
          DO 130 I=1,N
            HOLD=A(K,I)
            A(K,I)=-A(J,I)
130         A(J,I) =HOLD
        ENDIF
150   CONTINUE
      RETURN
! 
601   FORMAT ('0**ERROR** SUBROUTINE MTXINV WAS CALLED TO INVERT A',          ' SINGULAR MATRIX')
! 
      END
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_mxm(a,nar,b,nac,c,nbc)
      integer nar,nac,nbc,i,j,k
      real a(nar,1),b(nac,1),c(nar,1)
      call mxm(a,nar,b,nac,c,nbc)
      return
      end
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
   subroutine ez_nwtncof(cx,cy,ax,ay,ni,nj,i1,i2,j1,j2,extension)
!*******
!     Auteur: Y. Chartier, drpn
!     Fevrier 1991
!
!Objet:  Calcul de coefficients uti1ises dans la forme newtonienne
!        de l'interpolation de Lagrange.
!
!*************************************************************
!   -----*-------------*------#------*----------*------->
!        x1            x2     x      x3         x4
!
!*************************************************************
!     cx(i,1) = 1.0 / (x2-x1)
!     cx(i,2) = 1.0 / (x3-x1)
!     cx(i,3) = 1.0 / (x3-x2)
!     cx(i,4) = 1.0 / (x4-x1)
!     cx(i,5) = 1.0 / (x4-x2)
!     cx(i,6) = 1.0 / (x4-x3)
!
!     structure identique pour cy(j,1..6)
!*******
   implicit none
   integer  :: ni,nj,i1,i2,j1,j2,extension
   real ::   x1,x2,x3,x4
   integer :: limite,imoins1,iplus1,iplus2,wrap
   real, dimension(:) :: ax(ni),ay(j1:j2)
   real, dimension(:,:) :: cx(ni,6),cy(j1:j2,6)
   logical sequence_ok
   integer i,j
   sequence_ok = .true.
   do i=1,ni-1
      if (ax(i+1) <= ax(i)) then
         sequence_ok = .false.
         exit
      endif
   enddo
   if (.not.sequence_ok) then
      print *, 'Probleme detecte dans EZ_NWTNCOF code 998'
      print *, '(EZ_NWTNCOF) Probleme : x1..x4  : ', ax(i), ax(i+1)
      print *, 'EZ_NWTNCOF CALL EXIT'
      call exit(13)
   endif
   do j=1,nj-1
      if (ay(j+1) <= ay(j)) then
         sequence_ok = .false.
         exit
      endif
   enddo
   if (.not.sequence_ok) then
      print *, 'Probleme detecte dans EZ_NWTNCOF code 999'
      print *, '(EZ_NWTNCOF) Probleme : y1..y4  : ', ay(j), ay(j+1)
      print *, 'EZ_NWTNCOF CALL EXIT'
      call exit(13)
   endif
   do i=1,ni
      do j=1,6
         cx(i,j) = 1.0
      enddo
   enddo
   do i=1,6
      do j=j1,j2
         cy(j,i) = 1.0
      enddo
   enddo
   do i=2,ni-2
      cx(i,1) = 1. / (ax(i  ) - ax(i-1))
      cx(i,2) = 1. / (ax(i+1) - ax(i-1))
      cx(i,3) = 1. / (ax(i+1) - ax(i  ))
      cx(i,4) = 1. / (ax(i+2) - ax(i-1))
      cx(i,5) = 1. / (ax(i+2) - ax(i  ))
      cx(i,6) = 1. / (ax(i+2) - ax(i+1))
   enddo
   do j=j1+1,j2-2
      cy(j,1) = 1. / (ay(j  ) - ay(j-1))
      cy(j,2) = 1. / (ay(j+1) - ay(j-1))
      cy(j,3) = 1. / (ay(j+1) - ay(j  ))
      cy(j,4) = 1. / (ay(j+2) - ay(j-1))
      cy(j,5) = 1. / (ay(j+2) - ay(j  ))
      cy(j,6) = 1. / (ay(j+2) - ay(j+1))
   enddo
   if (extension.eq.1) then
      x1 = ax(1) - (ax(ni) - ax(ni-1))
      x2 = ax(1)
      x3 = ax(2)
      x4 = ax(3)
      cx(1,1) = 1. / (x2-x1)
      cx(1,2) = 1. / (x3-x1)
      cx(1,3) = 1. / (x3-x2)
      cx(1,4) = 1. / (x4-x1)
      cx(1,5) = 1. / (x4-x2)
      cx(1,6) = 1. / (x4-x3)
      x1 = ax(ni-2)
      x2 = ax(ni-1)
      x3 = ax(ni)
      x4 = ax(ni) + (ax(2)-ax(1))
      cx(ni-1,1) = 1. / (x2-x1)
      cx(ni-1,2) = 1. / (x3-x1)
      cx(ni-1,3) = 1. / (x3-x2)
      cx(ni-1,4) = 1. / (x4-x1)
      cx(ni-1,5) = 1. / (x4-x2)
      cx(ni-1,6) = 1. / (x4-x3)
   endif
   if (extension.eq.2) then
      x1 = ax(1) - (360.0 - ax(ni))
      x2 = ax(1)
      x3 = ax(2)
      x4 = ax(3)
      cx(1,1) = 1. / (x2-x1)
      cx(1,2) = 1. / (x3-x1)
      cx(1,3) = 1. / (x3-x2)
      cx(1,4) = 1. / (x4-x1)
      cx(1,5) = 1. / (x4-x2)
      cx(1,6) = 1. / (x4-x3)
      x1 = ax(ni-2)
      x2 = ax(ni-1)
      x3 = ax(ni)
      x4 = ax(1) + 360.0
      cx(ni-1,1) = 1. / (x2-x1)
      cx(ni-1,2) = 1. / (x3-x1)
      cx(ni-1,3) = 1. / (x3-x2)
      cx(ni-1,4) = 1. / (x4-x1)
      cx(ni-1,5) = 1. / (x4-x2)
      cx(ni-1,6) = 1. / (x4-x3)
      x1 = ax(ni-1)
      x2 = ax(ni)
      x3 = ax(1)+360.0
      x4 = ax(2)+360.0
      cx(ni,1) = 1. / (x2-x1)
      cx(ni,2) = 1. / (x3-x1)
      cx(ni,3) = 1. / (x3-x2)
      cx(ni,4) = 1. / (x4-x1)
      cx(ni,5) = 1. / (x4-x2)
      cx(ni,6) = 1. / (x4-x3)
   endif
   return
   end subroutine ez_nwtncof
!********************************************************************
!**
!**
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_nwtncof2(cx,cy,ax,ay,i1,i2,j1,j2)
!*******
!     Auteur: Y. Chartier, drpn
!     Fevrier 1991
!     
!Objet:  Calcul de coefficients uti1ises dans la forme newtonienne
!        de l'interpolation de Lagrange.
!
!*************************************************************
!   -----*-------------*------#------*----------*------->
!        x1            x2     x      x3         x4
!
!*************************************************************
!     cx(i,1) = 1.0 / (x2-x1)
!     cx(i,2) = 1.0 / (x3-x1)
!     cx(i,3) = 1.0 / (x3-x2)
!     cx(i,4) = 1.0 / (x4-x1)
!     cx(i,5) = 1.0 / (x4-x2)
!     cx(i,6) = 1.0 / (x4-x3)
!
!     structure identique pour cy(j,1..6)
!*******
      implicit none
      integer ni,nj,i1,j1,i2,j2
      real cx(i1:i2,6),cy(j1:j2,6),ax(i1:i2),ay(j1:j2)
      integer i,j
      print *, ax
      print *
      print *, ay
      do 10 i=i1+1,i2-2
         cx(i,1) = 1. / (ax(i  ) - ax(i-1))
         cx(i,2) = 1. / (ax(i+1) - ax(i-1))
         cx(i,3) = 1. / (ax(i+1) - ax(i  ))
         cx(i,4) = 1. / (ax(i+2) - ax(i-1))
         cx(i,5) = 1. / (ax(i+2) - ax(i  ))
         cx(i,6) = 1. / (ax(i+2) - ax(i+1))
 10   continue
      do 20 j=j1+1,j2-2
         cy(j,1) = 1. / (ay(j  ) - ay(j-1))
         cy(j,2) = 1. / (ay(j+1) - ay(j-1))
         cy(j,3) = 1. / (ay(j+1) - ay(j  ))
         cy(j,4) = 1. / (ay(j+2) - ay(j-1))
         cy(j,5) = 1. / (ay(j+2) - ay(j  ))
         cy(j,6) = 1. / (ay(j+2) - ay(j+1))
         print *, (cy(j,i),i=1,6)
 20   continue
      return
      end
!********************************************************************
!**
!**
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!****************************************************************************
      subroutine ez_polrint(vpolnor,vpolsud,zi,ni,nj,grtyp,grref,hem,vecteur,ax,ay)
      implicit none
      integer ni,nj,hem
      real zi(ni,nj)
      real ax(ni),ay(nj)
      character*1 grtyp,grref
      logical vecteur
      integer n
      real vpolnor, vpolsud, sum
      integer global, nord, sud, sudnord, nordsud
      integer absolu, relatif
      parameter (global = 0)
      parameter (nord   = 1)
      parameter (sud    = 2)
      parameter (sudnord= 0)
      parameter (nordsud= 1)
      parameter (absolu = 0)
      parameter (relatif = 1)
!
!Definition des variables locales
!
      integer i,j
      integer n1, n2, s1, s2
      if(vecteur) then
         return
      endif
      if(grtyp.eq.'L'.or.grtyp.eq.'N'.or.grtyp.eq.'S'.or.grtyp.eq.'!'.or.(grtyp.eq.'Z'.and.grref.ne.'E')) then
         return
      endif
      if (grtyp .eq. 'B') then
         vpolnor = zi(1, nj)
         vpolsud = zi(1, 1)
         return
      endif
      if(grtyp .eq. 'A' .or. grtyp .eq. 'G'.or. (grtyp.eq.'Z'.and.grref.eq.'E')) then
         sum = 0.0
         do i=1,ni
            sum = sum + zi(i,nj)
         enddo
         vpolnor = sum / (1.0*ni)
         sum = 0.0
         do i=1,ni
            sum = sum + zi(i,1)
         enddo
         vpolsud = sum / (1.0*ni)
      endif
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_rgdint_0(zo,px,py,npts,z,ni,j1,j2)
      implicit none
      integer npts,ni,j1,j2,degree,wrap,i,j,n
      real zo(npts),px(npts),py(npts)
      real z(ni,j1:j2)
      do n=1,npts
         i = min(ni,max(1,nint(px(n))))
         j = min(j2,max(j1,nint(py(n))))
         zo(n)=z(i,j)
      enddo
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_rgdint_1_nw(zo,px,py,npts,z,ni,j1,j2)
      implicit none
      integer npts,ni,j1,j2,i,j,n
      real zo(npts),px(npts),py(npts)
      real z(ni,j1:j2)
      real*8 dx,dy,y2,y3
      real*8 zlin, zz1, zz2, zdx
      zlin(zz1,zz2,zdx) = zz1 + (zz2 - zz1) * zdx
      do n=1,npts
         i = min(ni-1,max(1,ifix(px(n))))
         j = min(j2-1,max(j1,ifix(py(n))))
         dx = px(n) - i
         dy = py(n) - j
         y2=zlin(dble(z(i,j  )),dble(z(i+1,j  )),dx)
         y3=zlin(dble(z(i,j+1)),dble(z(i+1,j+1)),dx)
         zo(n)=zlin(y2,y3,dy)
      enddo
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_rgdint_1_w(zo,px,py,npts,z,ni,j1,j2,wrap)
      implicit none
      integer npts,ni,j1,j2,wrap,i,j,n,limite,iplus1
      real zo(npts),px(npts),py(npts)
      real z(ni,j1:j2)
      real*8 dx, dy, y2, y3
      real*8 zlin, zz1, zz2, zdx
      zlin(zz1,zz2,zdx) = zz1 + (zz2 - zz1) * zdx
      limite = ni +2 - wrap
      do n=1,npts
         i = min(ni-2+wrap,max(1,ifix(px(n))))
         j = min(j2-1,max(j1,ifix(py(n))))
         iplus1 = i + 1
         if (wrap.gt.0.and.(i.eq.(ni-2+wrap))) then
            iplus1  = mod(limite+i+1,limite)
         endif
         dx = px(n) - i
         dy = py(n) - j
         y2=zlin(dble(z(i,j  )),dble(z(iplus1,j  )),dx)
         y3=zlin(dble(z(i,j+1)),dble(z(iplus1,j+1)),dx)
         zo(n)=zlin(y2,y3,dy)
      enddo
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_rgdint_3_nw(zo,px,py,npts,z,ni,j1,j2)
!*******
!Auteur: Y.Chartier, drpn
!        Fevrier 1991
!
!Objet:  Interpolation bi-cubique de points a partir d'une grille
!        source reguliere.
!        
!*******
      implicit none
      integer npts,ni,j1,j2
      real zo(npts),px(npts),py(npts)
      real z(ni,j1:j2)
!
!  npts   : nombre de points a interpoler
!  j1:nj  : dimension de la grille source selon y
!  zo     : vecteur de sortie contenant les valeurs interpolees
!  px     : vecteur contenant la position x des points que l'on
!         : veut interpoler
!  py     : vecteur contenant la position y des points que l'on
!         : veut interpoler
!  z      : valeurs de la grille source.
!
!===========================================
!
!     *   *   *   *
!     
!     *   *   *   *
!           #        ==>   pt (x,y)
!     *  (=)  *   *  ==> = pt (iind, jind)
!
!     *   *   *   *
!
!===========================================
      real*8 y1,y2,y3,y4
      integer m,n,i,j
      real*8 cubic, dx,dy,z1,z2,z3,z4
      cubic(z1,z2,z3,z4,dx)=((((z4-z1)*0.1666666666666 + 0.5*(z2-z3))      *dx  + 0.5*(z1+z3)-z2)*dx  + z3-0.1666666666666*z4-0.5* &
     &     z2-0.3333333333333*z1)*dx+z2
      do n=1,npts
         i = min(ni-2,max(2,ifix(px(n))))
         j = min(j2-2,max(j1+1,ifix(py(n))))
         dx = px(n) - i
         dy = py(n) - j
         y1=cubic(dble(z(i-1,j-1)),dble(z(i  ,j-1)),dble(z(i+1,j-1)),dble(z(i+2,j-1)),dx)
         y2=cubic(dble(z(i-1,j  )),dble(z(i  ,j  )),dble(z(i+1,j  )),dble(z(i+2,j  )),dx)
         y3=cubic(dble(z(i-1,j+1)),dble(z(i  ,j+1)),dble(z(i+1,j+1)),dble(z(i+2,j+1)),dx)
         y4=cubic(dble(z(i-1,j+2)),dble(z(i  ,j+2)),dble(z(i+1,j+2)),dble(z(i+2,j+2)),dx)
         zo(n)=cubic(y1,y2,y3,y4,dy)
      enddo
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_rgdint_3_w(zo,px,py,npts,z,ni,j1,j2,wrap)
!*******
!Auteur: Y.Chartier, drpn
!        Fevrier 1991
!
!Objet:  Interpolation bi-cubique de points a partir d'une grille
!        source reguliere.
!        
!*******
      implicit none
      integer npts,ni,j1,j2,degree,wrap
      real zo(npts),px(npts),py(npts)
      real z(ni,j1:j2)
!
!  npts   : nombre de points a interpoler
!  i1:i2  : dimension de la grille source selon x
!  j1:nj  : dimension de la grille source selon y
!  zo     : vecteur de sortie contenant les valeurs interpolees
!  px     : vecteur contenant la position x des points que l'on
!         : veut interpoler
!  py     : vecteur contenant la position y des points que l'on
!         : veut interpoler
!  z      : valeurs de la grille source.
!
!  wrap est est le facteur de "wrap around" dans le cadre d'une grille globale
!  pour une grille de type 'A' ou 'G', wrap = 2
!  pour une grille de type 'B', wrap = 1
!  dans tous les autres case wrap = 0
!
!===========================================
!
!     *   *   *   *
!     
!     *   *   *   *
!           #        ==>   pt (x,y)
!     *  (=)  *   *  ==> = pt (iind, jind)
!
!     *   *   *   *
!
!===========================================
      real*8 y1,y2,y3,y4
      integer m,n,i,j,stride
      integer imoins1, iplus1, iplus2, limite
      real*8 cubic, dx,dy,z1,z2,z3,z4
      cubic(z1,z2,z3,z4,dx)=((((z4-z1)*0.1666666666666 + 0.5*(z2-z3))      *dx  + 0.5*(z1+z3)-z2)*dx  + z3-0.1666666666666*z4-0.5* &
     &     z2-0.3333333333333*z1)*dx+z2
      limite = ni+2-wrap
!zzzzzz$OMP PARALLEL
!zzzzzz$OMP DO private(n, i, j, imoins1, iplus1, iplus2, y1, y2, y3, y4) 
      do n=1,npts
         i = min(ni-2+wrap,max(1,max(2-wrap,ifix(px(n)))))
         j = min(j2-2,max(j1+1,ifix(py(n))))
         if (wrap.gt.0) then
            imoins1 = mod(limite+i-1,limite)
            iplus1  = mod(limite+i+1,limite)
            iplus2  = mod(limite+i+2,limite)
            if (imoins1.eq.0) imoins1 = ni
            if (i.eq.0) i = ni
            if (iplus1.eq.0) iplus1 = ni
            if (iplus2.eq.0) iplus2 = ni
            if (wrap.eq.1) then
               if (iplus2.eq.ni) iplus2 = 2
               if (imoins1.eq.ni) imoins1=ni-1
            endif
         else
            imoins1 = i-1
            iplus1  = i+1
            iplus2  = i+2
         endif
         dx = px(n) - i
         dy = py(n) - j
         y1=cubic(dble(z(imoins1,j-1)),dble(z(i ,j-1)),dble(z(iplus1,j-1)),dble(z(iplus2,j-1)),dx)
         y2=cubic(dble(z(imoins1,j  )),dble(z(i ,j  )),dble(z(iplus1,j  )),dble(z(iplus2,j  )),dx)
         y3=cubic(dble(z(imoins1,j+1)),dble(z(i ,j+1)),dble(z(iplus1,j+1)),dble(z(iplus2,j+1)),dx)
         y4=cubic(dble(z(imoins1,j+2)),dble(z(i ,j+2)),dble(z(iplus1,j+2)),dble(z(iplus2,j+2)),dx)
         zo(n)=cubic(y1,y2,y3,y4,dy)
      enddo
!zzzzzz$OMP END DO
!zzzzzz$OMP END PARALLEL
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_rgdint_3_wnnc(zo,px,py,npts,z,ni,j1,j2,wrap)
!*******
!Auteur: Y.Chartier, drpn
!        Fevrier 1991
!
!Objet:  Interpolation bi-cubique de points a partir d'une grille
!        source reguliere.
!        
!*******
      implicit none
      integer npts,ni,j1,j2,degree,wrap
      real zo(npts),px(npts),py(npts)
      real z(ni,j1:j2)
!
!  npts   : nombre de points a interpoler
!  i1:i2  : dimension de la grille source selon x
!  j1:nj  : dimension de la grille source selon y
!  zo     : vecteur de sortie contenant les valeurs interpolees
!  px     : vecteur contenant la position x des points que l'on
!         : veut interpoler
!  py     : vecteur contenant la position y des points que l'on
!         : veut interpoler
!  z      : valeurs de la grille source.
!
!  wrap est est le facteur de "wrap around" dans le cadre d'une grille globale
!  pour une grille de type 'A' ou 'G', wrap = 2
!  pour une grille de type 'B', wrap = 1
!  dans tous les autres case wrap = 0
!
!===========================================
!
!     *   *   *   *
!     
!     *   *   *   *
!           #        ==>   pt (x,y)
!     *  (=)  *   *  ==> = pt (iind, jind)
!
!     *   *   *   *
!
!===========================================
      real*8 y1,y2,y3,y4
      integer m,n,i,j,stride
      integer imoins1, iplus1, iplus2, limite
      integer voisin, lineair, cubique
      integer oui, abort, valeur, maximum, minimum
      parameter (voisin  =   0)
      parameter (lineair =   1)
      parameter (cubique =   3)
      parameter (oui     =   1)
      parameter (maximum =   4)
      parameter (minimum =   5)
      parameter (valeur  =   6)
      parameter (abort   =  13)
      real*8 cubic, dx,dy,z1,z2,z3,z4
      cubic(z1,z2,z3,z4,dx)=((((z4-z1)*0.1666666666666 + 0.5*(z2-z3))      *dx  + 0.5*(z1+z3)-z2)*dx  + z3-0.1666666666666*z4-0.5* &
     &     z2-0.3333333333333*z1)*dx+z2
      limite = ni+2-wrap
      do n=1,npts
         i = min(ni-2+wrap,max(1,max(2-wrap,ifix(px(n)))))
         j = min(j2-2,max(j1+1,ifix(py(n))))
         if (wrap.gt.0.and.(i.le.1).or.i.ge.(ni-1)) then
            imoins1 = mod(limite+i-1,limite)
            iplus1  = mod(limite+i+1,limite)
            iplus2  = mod(limite+i+2,limite)
            if (imoins1.eq.0) imoins1 = ni
            if (i.eq.0) i = ni
            if (iplus1.eq.0) iplus1 = ni
            if (iplus2.eq.0) iplus2 = ni
            if (wrap.eq.1) then
               if (iplus2.eq.ni) iplus2 = 2
               if (imoins1.eq.ni) imoins1=ni-1
            endif
         else
            imoins1 = i-1
            iplus1  = i+1
            iplus2  = i+2
         endif
         dx = px(n) - i
         dy = py(n) - j
         y1=cubic(dble(z(imoins1,j-1)),dble(z(i ,j-1)),dble(z(iplus1,j-1)),dble(z(iplus2,j-1)),dx)
         y2=cubic(dble(z(imoins1,j  )),dble(z(i ,j  )),dble(z(iplus1,j  )),dble(z(iplus2,j  )),dx)
         y3=cubic(dble(z(imoins1,j+1)),dble(z(i ,j+1)),dble(z(iplus1,j+1)),dble(z(iplus2,j+1)),dx)
         y4=cubic(dble(z(imoins1,j+2)),dble(z(i ,j+2)),dble(z(iplus1,j+2)),dble(z(iplus2,j+2)),dx)
         zo(n)=cubic(y1,y2,y3,y4,dy)
      enddo
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ezsincoslatlon(lat,lon,sinlat,sinlon,coslat,coslon,npts)
      implicit none
      integer npts
      real lat(npts),lon(npts),sinlat(npts),sinlon(npts),coslat(npts),coslon(npts)
      integer i
      real dar
      dar = 3.14159265358979323846/180.0
      do i=1,npts
         sinlat(i)  = sin(dar*lat(i))
         coslat(i)  = cos(dar*lat(i))
         sinlon(i)  = sin(dar*lon(i))
         coslon(i)  = cos(dar*lon(i))
      enddo
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!**S/R UVACART  - compute the winds in the cartesian space from
!                 the components
!
      SUBROUTINE EZ_UVACART( XYZ, U, V, LON, LAT, NI, NJ)
      implicit none
      INTEGER NI, NJ
      REAL    U(NI,NJ), V(NI,NJ), XYZ(3,NI*NJ), LON(NI,NJ), LAT(NI,NJ)
!
!author michel roch - april 90
!
!arguments
!    out    xyz   - unrotated winds in cartesian space
!    IN     U     - unrotated component  U of the wind
!           V     - unrotated component  V of the wind
!           LON   - longitudes of the grid in the unrotated system of coordinates
!           LAT   - latitudes  of the grid in the unrotated system of coordinates
!           NI    - E-W DIMENSION of the grid
!           NJ    - N-S dimension of the grid
!
!*
      INTEGER I, J, K
      REAL*8    A, B, C, D, DAR
      DAR = ACOS(-1.)/180.
      K   = 0
      DO 20 J=1,NJ
         DO 10 I=1,NI
            K        = K+1
            A        = SIN(DAR*LON(I,J))
            B        = COS(DAR*LON(I,J))
            C        = SIN(DAR*LAT(I,J))
            D        = COS(DAR*LAT(I,J))
            XYZ(1,K) = -(U(I,J)*A) - (V(I,J)*B*C)
            XYZ(2,K) =  (U(I,J)*B) - (V(I,J)*A*C)
            XYZ(3,K) =   V(I,J)*D
 10         CONTINUE
 20      CONTINUE
      RETURN
      END
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!**s/r ez_vllfxy(i) - computes the grid co-ordinates of a point
!
      subroutine ez_vllfxy(dlat,dlon,x,y,ni,nj,d60,dgrw,pi,pj,nhem)
      implicit none
!arguments                                                              
!   out   - dlat - latitude in degrees (-90 to +90, positive n).        
!         - dlon - longitude in degrees (-180 to +180, positive e).     
!   in    - x    - x-co-ordinate of the point as measured with pole     
!                  as origin                                            
!         - y    - y-co-ordinate of the point as measured with pole     
!                  as origin                                            
!         - d60  - grid length (in metres) of the polar stereographic   
!                  grid at 60 degrees                                   
!         - dgrw - orientation of greenwich meridian with respect to    
!                  the grid (in degrees)                                
!         - nhem - 1 for northern hemisphere                            
!                  2 for southern hemisphere                            
!                                                                       
!notes    - the companion routine xyfll, which computes the grid        
!           co-ordinates given the latitude and longitude, is also      
!           available.                                                  
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!     
!     * 1.866025=(1+sin60),   6.371e+6=earth radius in meters.      
!     
!     rdtodg = 180/pie, dgtord = pie/180        
      real pie,rdtodg,dgtord
      data pie    /3.1415926535898/
      data rdtodg /57.295779513082/
      data dgtord /1.7453292519943e-2/
!         
      integer global, nord, sud, sudnord, nordsud
      integer absolu, relatif
      parameter (global = 0)
      parameter (nord   = 1)
      parameter (sud    = 2)
      parameter (sudnord= 0)
      parameter (nordsud= 1)
      parameter (absolu = 0)
      parameter (relatif = 1)
      integer ni,nj, nhem
      real x(ni,nj), y(ni,nj), dlat(ni,nj), dlon(ni,nj)
      real d60, dgrw, pi, pj
      real*8 x1, y1
      real*8 re,re2,r2,rlat, rlon
      integer i,j
      re=1.866025d0*6.371e+6/d60
      re2=re**2
      do j=1,nj
         do i=1,ni
            x1 = x(i,j) - pi
            y1 = y(i,j) - pj
            if (x1 .eq. 0.  .and. y1 .eq. 0.)  then
               rlat=90.0d0
               rlon=0.0d0
            endif
!     
!     calculate longitude in map coordinates.                         
!     
            if(x1 .eq. 0.0d0) rlon=sign(90.d0,y1)
            if(x1 .ne. 0.0d0) rlon=datan(y1/x1)*dble(rdtodg)
            if(x1.lt. 0.0d0) rlon=rlon+sign(180.d0,y1)
!     
!     * adjust longitude for grid orientation.                          
!     
            rlon=rlon-dgrw
!     if(rlon.gt. 180.) rlon=rlon-360.
!     if(rlon.lt.-180.) rlon=rlon+360. 
            if(rlon.lt.0.0d0) rlon=rlon+3.6d2
!     
!     * calculate latitude.                                             
!     
            r2=x1*x1+y1*y1
            rlat=(re2-r2)/(re2+r2)
            rlat=max(-1.0d0, min(rlat, 1.0d0))
            rlat= dasin(rlat)*dble(rdtodg)
!     change signs if in southern hemisphere.                         
!     
            if(nhem .eq. 2) then
               rlat=-rlat
               rlon=-rlon
               if(rlon.lt.0.0d0) rlon=rlon+360.0d0
            endif
            dlat(i,j)=rlat
            dlon(i,j)=rlon
         enddo
      enddo
!-----------------------------------------------------------------------
!     
!     
      return
      end
!-----------------------------------------------------------------
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!**s/r ez_vrotf - rotation of the components of the wind once interpolated
!              to the rotated grid of the model
!
      subroutine ez_vrotf(u, v, lonp, latp, lon, lat, ro, xyz, uvcart, ni, nj)
      implicit none
      integer ni, nj
      real    u(ni,nj), lonp(ni,nj), lon(ni,nj), uvcart(3,ni*nj),          ro(3,3), v(ni,nj),  latp(ni,nj), lat(ni,nj), xyz(3,ni*nj&
     &)
!
!author michel roch - april 1990
!
!revision
!	001 - yvon bourrassa - mai/juin 1993 - remove dynamic allocation 
!	                       change calling sequence
!	002 - michel roch - documentation
!
!arguments
!   in/out  u         composante u du vent sur grille non tournee en entree
!                     composante u du vent sur grille tournee en sortie
!           v         composante v du vent sur grille non tournee en entree
!                     composante v du vent sur grille tournee en sortie
!    in     lonp      longitudes d'origine dans le systeme non tourne 
!           latp      latitudes d'origine dans le systeme non tourne
!           lon       longitudes de grille variable dans le systeme tourne
!           lat       latitudes de grille variable dans le systeme tourne
!           ro        matrice de transformation du systeme non tourne 
!                     au systeme de coordonnees tourne
!           ni        dimension e-o de la grille a sortir
!           nj        dimension n-s de la grille a sortir
!    out    uvcart    champ de travail
!           xyz       champ de travail
!
!
      external ez_uvacart, mxm, ez_cartauv
!     calcul des vent en espace cartesiennes
      call  ez_uvacart(xyz, u, v, lonp, latp, ni, nj)
!     calcul des vents dans l'espace cartesien avec rotation
      call mxm(ro, 3, xyz, 3, uvcart, ni*nj)
!     calcul des composantes de vent dans le systeme de
!                coordonnees tourne par rapport a la geographie reelle
      call ez_cartauv(u, v, uvcart, lon, lat, ni, nj)
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!**s/r qqqvrotf2 - rotation of the rotated components of the wind to the
!                  non-rotated grid of the model
!
      subroutine ez_vrotf2(u, v, lonp, latp, lon, lat, ro, xyz, uvcart, ni, nj)
      implicit none
      integer ni, nj
      real    u(ni,nj), lonp(ni,nj), lon(ni,nj), uvcart(3,ni*nj),          ro(3,3), v(ni,nj),  latp(ni,nj), lat(ni,nj), xyz(3,ni*nj&
     &)
!
!author michel roch - april 1990
!
!revision
!	001 - yvon bourrassa - mai/juin 1993 - remove dynamic allocation 
!	                       change calling sequence
!	002 - michel roch - documentation
!
!arguments
!   in/out  u         composante u du vent sur grille non tournee en entree
!                     composante u du vent sur grille tournee en sortie
!           v         composante v du vent sur grille non tournee en entree
!                     composante v du vent sur grille tournee en sortie
!    in     lonp      longitudes d'origine dans le systeme non tourne 
!           latp      latitudes d'origine dans le systeme non tourne
!           lon       longitudes de grille variable dans le systeme tourne
!           lat       latitudes de grille variable dans le systeme tourne
!           ro        matrice de transformation du systeme non tourne 
!                     au systeme de coordonnees tourne
!           ni        dimension e-o de la grille a sortir
!           nj        dimension n-s de la grille a sortir
!    out    uvcart    champ de travail
!           xyz       champ de travail
!
!
      external ez_uvacart, ez_mxm, ez_cartauv
!     calcul des vent en espace cartesiennes
      call  ez_uvacart(xyz, u, v, lon, lat, ni, nj)
!     calcul des vents dans l'espace cartesien avec rotation
      call ez_mxm(ro, 3, xyz, 3, uvcart, ni*nj)
!     calcul des composantes de vent dans le systeme de
!                coordonnees tourne par rapport a la geographie reelle
      call ez_cartauv(u, v, uvcart, lonp, latp, ni, nj)
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine testprojt
      implicit none
      integer i,j,ni,nj
      real clat, clon, d60, dgrw
      real lat(11,11), lon(11,11)
      real tlat(5,5), tlon(5,5)
      real tlat2(5,5), tlon2(5,5)
      real x(5,5),y(5,5)
      ni = 11
      nj = 11
      clat = 45.0
      clon = 260.0
      d60 = 100000.0
      dgrw = 0.0
      do j=1,5
         do i=1,5
            tlat(i,j) = 35.0 + (j-1)*5
            tlon(i,j) = 250.0 +(i-1)*5
         enddo
      enddo
      call ez_vtxyfll(x, y, tlat, tlon, clat, clon, d60, dgrw, ni, nj, 25)
      do j=1,5
         do i=1,5
            print *, i,j,tlat(i,j),tlon(i,j),x(i,j),y(i,j)
         enddo
      enddo
      print *, '***************************************************'
      call ez_vtllfxy(tlat2, tlon2, x, y, clat, clon, d60, dgrw, ni, nj, 25)
      do j=1,5
         do i=1,5
            print *, i,j,tlat2(i,j)-tlat(i,j),tlon2(i,j) - tlon(i,j)
         enddo
      enddo
      stop
      end
      subroutine ez_vtxyfll(x, y, lat, lon, clat, clon, d60, dgrw, ni, nj, n)
      implicit none
!     
!     * 1.866025=(1+sin60),   6.371e+6=earth radius in meters.      
!     
!     rdtodg = 180/pie, dgtord = pie/180        
      real pie,rdtodg,dgtord
      data pie    /3.1415926535898/
      data rdtodg /57.295779513082/
      data dgtord /1.7453292519943e-2/
!         
      integer i, n, ni, nj
      real x(n), y(n), lat(n), lon(n), clat, clon, d60, dgrw
      real r,k,lat0,lon0
      real offsetx, offsety,sinclat,cosclat,sinclon,cosclon
      r = 6371000.0
      sinclat = sin (clat * dgtord)
      cosclat = cos (clat * dgtord)
      sinclon = sin (clon * dgtord)
      cosclon = cos (clon * dgtord)
      offsetx = (-ni-1) * 0.5
      offsety = (-nj-1) * 0.5
      do i=1,n
         k = 2.0 / (1.0 + sinclat*sin(lat(i) * dgtord)+ cosclat* cos(lat(i)*dgtord)*cos(dgtord*(lon(i)-clon)))
         x(i) = r * k * cos(lat(i)*dgtord) * sin(dgtord*(lon(i)-clon))
         y(i) = r * k * (cosclat*sin(lat(i) * dgtord) -  (sinclat * cos(lat(i)*dgtord)*cos(dgtord*(lon(i)-clon))))
         x(i) = x(i) / d60 - offsetx
         y(i) = y(i) / d60 - offsety
      enddo
      return
      end
      subroutine ez_vtllfxy(lat, lon, x, y, clat, clon, d60, dgrw, ni, nj, n)
      implicit none
!     
!     * 1.866025=(1+sin60),   6.371e+6=earth radius in meters.      
!     
!     rdtodg = 180/pie, dgtord = pie/180        
      real pie,rdtodg,dgtord
      data pie    /3.1415926535898/
      data rdtodg /57.295779513082/
      data dgtord /1.7453292519943e-2/
!         
      integer i, n, ni, nj
      real x(n), y(n), lat(n), lon(n), clat, clon, d60, dgrw
      real r,k,lat0,lon0
      real offsetx, offsety,sinclat,cosclat,sinclon,cosclon
      real rho, c, a, b, temp
      r = 6371000.0
      sinclat = sin (clat * dgtord)
      cosclat = cos (clat * dgtord)
      sinclon = sin (clon * dgtord)
      cosclon = cos (clon * dgtord)
      offsetx = (-ni-1) * 0.5
      offsety = (-nj-1) * 0.5
      do i=1,n
         x(i) = (x(i) + offsetx) * d60
         y(i) = (y(i) + offsety) * d60
         rho = sqrt(x(i)*x(i) + y(i)*y(i))
         if (rho.eq.0.0) then
            lat(i) = clat
            lon(i) = clon
         else
            c = 2.0 * atan(rho/(2.0*r))
            temp = cos(c)*sinclat + (y(i) * sin(c) * cosclat)/rho
            lat(i) = rdtodg * asin(max(-1.0, min(temp, 1.0)))
            lon(i) = clon + rdtodg * atan(((x(i)*sin(c))/            (rho*cosclat*cos(c)-y(i)*sinclat*sin(c))))
            a = x(i) * sin(c)
            b = rho*cosclat*cos(c)-y(i)*sinclat*sin(c)
            lon(i) = clon + rdtodg * atan2(a,b)
         endif
         lon(i) = mod(mod(lon(i),360.0)+360.0,360.0)
      enddo
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!**s/r ez_vxyfll - computes the grid co-ordinates of a point
!
      subroutine ez_vxyfll(x,y,dlat,dlon,npts,d60,dgrw,pi,pj,nhem)
      implicit none
!author   - j.d. henderson  -  feb 75
!        
!revision 001   c. thibeault  -  nov 79  documentation
!revision 002   y. chartier   -  feb 91  vectorization
!                                                     
!language - fortran
!                  
!object(vxyfll)
!     - computes the grid co-ordinates measured from the pole of a
!       point, given the latitude and longitude in degrees.
!
!libraries
!         - source  rmnsourcelib,id=rmnp     deck=vxyfll
!         - object  rmnlib,id=rmnp
!
!usage    - call vxyfll(x,y,dlat,dlon,npts,d60,dgrw,pi,pj,nhem)
!
!arguments
!   out   - x    - x-co-ordinate of the point as measured with pole
!                  as origin
!         - y    - y-co-ordinate of the point as measured with pole
!                  as origin
!   in    - dlat - latitude in degrees (-90 to +90, positive n)
!         - dlon - longitude in degrees (-180 to +180, positive e)
!         - d60  - grid length (in metres) of the polar stereographic
!                  grid at 60 degrees
!         - dgrw - orientation of greenwich meridian with respect to
!                  the grid (in degrees)
!         - nhem - 1 for northern hemisphere
!                  2 for southern hemisphere
!                                                                      
!notes    - the companion routine llfxy, which computes the latitude
!         - and longitude given the grid-coordinates, 
!         - is also available.
!*--------------------------------------------------------------------
!     
!     * 1.866025=(1+sin60),   6.371e+6=earth radius in meters.      
!     
!     rdtodg = 180/pie, dgtord = pie/180        
      real pie,rdtodg,dgtord
      data pie    /3.1415926535898/
      data rdtodg /57.295779513082/
      data dgtord /1.7453292519943e-2/
!         
      integer global, nord, sud, sudnord, nordsud
      integer absolu, relatif
      parameter (global = 0)
      parameter (nord   = 1)
      parameter (sud    = 2)
      parameter (sudnord= 0)
      parameter (nordsud= 1)
      parameter (absolu = 0)
      parameter (relatif = 1)
      integer npts, nhem
      real x(npts), y(npts), dlat(npts), dlon(npts)
      real d60, dgrw, pi, pj
      real*8 re,rlon,rlat,sinlat,r
      integer i
      re=1.866025d0*6.371d+6/d60
!     
      if (nhem.eq.NORD) then
         do 10 i=1,npts
            rlon=dgtord*(dlon(i)+dgrw)
            rlat=dgtord*dlat(i)
            sinlat=sin(rlat)
            r=re*sqrt((1.d0-sinlat)/(1.d0+sinlat))
            x(i)=r*cos(rlon) + pi
            y(i)=r*sin(rlon) + pj
 10      continue
         return
      endif
      if (nhem.eq.SUD) then
         do 20 i=1,npts
            rlon = dlon(i)
            if (rlon.gt.180.0d0) rlon = rlon - 360.0d0
            rlon=dgtord*(-rlon+dgrw)
            rlat=dgtord*(-dlat(i))
            sinlat=sin(rlat)
            r=re*sqrt((1.d0-sinlat)/(1.d0+sinlat))
            x(i)=r*cos(rlon)+pi
            y(i)=r*sin(rlon)+pj
 20      continue
      endif
      return
      end
!-----------------------------------------------------------------
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_xpngdag2(zout,zi,ni,nj,j1,j2,hem,symetrie)
      implicit none
      integer global, nord, sud, sudnord, nordsud
      integer absolu, relatif
      parameter (global = 0)
      parameter (nord   = 1)
      parameter (sud    = 2)
      parameter (sudnord= 0)
      parameter (nordsud= 1)
      parameter (absolu = 0)
      parameter (relatif = 1)
      external permut
      integer i1,i2,j1,j2,ni,nj
      integer hem,symetrie
      character*1 grtyp
      integer i,j,njsur2
      real zout(ni,j1:j2)
      real zi(ni,nj)
      integer sym
      real sign
      integer ier,ii
      if (symetrie.eq.0) then
         sign = -1.0
      else
         sign = 1.0
      endif
      if (hem .eq. nord) then
         do j=1,nj
            do i=1,ni
               zout(i,j)  = zi(i,j)
            enddo
         enddo
         do j=1,nj
            do i=1,ni
               zout(i,-j+1)  = sign * zi(i,j)
            enddo
         enddo
      endif
      if (hem .eq. sud) then
         do j=1,nj
            do i=1,ni
               zout(i,j)  = zi(i,j)
            enddo
         enddo
         do j=1,nj
            do i=1,ni
               zout(i,nj+j)  = sign * zi(i,nj-j+1)
            enddo
         enddo
      endif
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_xpngdb2(zout,zi,ni,nj,j1,j2,hem,symetrie)
      implicit none
      integer global, nord, sud, sudnord, nordsud
      integer absolu, relatif
      parameter (global = 0)
      parameter (nord   = 1)
      parameter (sud    = 2)
      parameter (sudnord= 0)
      parameter (nordsud= 1)
      parameter (absolu = 0)
      parameter (relatif = 1)
      external permut
      integer i1,i2,j1,j2,ni,nj
      integer hem,symetrie
      character*1 grtyp
      integer i,j,njsur2
      real zout(ni,j1:j2)
      real zi(ni,nj)
      integer sym
      real sign
      integer ier,ii
      if (symetrie.eq.0) then
         sign = -1.0
      else
         sign = 1.0
      endif
      if (hem .eq. nord) then
         do j=1,nj
            do i=1,ni
               zout(i,j)  = zi(i,j)
            enddo
         enddo
         do j=2,nj
            do i=1,ni
               zout(i,2-j)  = sign * zi(i,j)
            enddo
         enddo
      endif
      if (hem .eq. sud) then
         do j=1,nj
            do i=1,ni
               zout(i,j)  = zi(i,j)
            enddo
         enddo
         do j=2,nj
            do i=1,ni
               zout(i,nj+j-1)  = sign * zi(i,nj-j+1)
            enddo
         enddo
      endif
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_xtrap(zo, px, py, npts, z, ni,nj,ordint,codxtrap,valxtrap)
      implicit none
      integer npts,ni,nj
      real zo(npts),px(npts),py(npts)
      real z(ni,nj)
      integer ordint,codxtrap
      integer n, i, j, i1, j1, offl, offr
      real rmin, rmax, tempmin, tempmax, valxtrap
      integer voisin, lineair, cubique
      integer oui, abort, valeur, maximum, minimum
      parameter (voisin  =   0)
      parameter (lineair =   1)
      parameter (cubique =   3)
      parameter (oui     =   1)
      parameter (maximum =   4)
      parameter (minimum =   5)
      parameter (valeur  =   6)
      parameter (abort   =  13)
      if (ordint .eq. cubique) then
         offr = 2
         offl = 1
      else
         offr = 0
         offl = 0
      endif
      rmin = z(1,1)
      rmax = z(1,1)
      do j=1,nj
         do i=1,ni
            if(z(i,j).lt.rmin) rmin = z(i,j)
            if(z(i,j).gt.rmax) rmax = z(i,j)
         enddo
      enddo
      tempmin = rmin - 0.05*(rmax - rmin)
      tempmax = rmax + 0.05*(rmax - rmin)
      rmin = tempmin
      rmax = tempmax
      if (codxtrap.eq.voisin) then
         do n=1,npts
            i = ifix(px(n))
            j = ifix(py(n))
            if (i.lt.1.or.j.lt.1.or.i.gt.ni.or.j.gt.nj) then
               i1 = min(ni, max(1, nint(px(n))))
               j1 = min(nj, max(1, nint(py(n))))
               zo(n) = z(i1,j1)
            endif
         enddo
      endif
      if (codxtrap .eq. minimum) then
         do n=1,npts
            zo(n) = rmin
         enddo
      endif
      if (codxtrap .eq. maximum) then
         do n=1,npts
            zo(n) = rmax
         enddo
      endif
      if (codxtrap .eq. valeur) then
         do n=1,npts
            zo(n) = valxtrap
         enddo
      endif
      return
      end
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine ez_rgoptc(op, val, flag)
      implicit none
      character *8 op, val
      logical flag
      integer ier,ezsetopt, ezgetopt
!flag = .true.  mode set
!flag = .false. mode get
      integer voisin, lineair, cubique
      integer oui, abort, valeur, maximum, minimum
      parameter (voisin  =   0)
      parameter (lineair =   1)
      parameter (cubique =   3)
      parameter (oui     =   1)
      parameter (minimum =   4)
      parameter (maximum =   5)
      parameter (valeur  =   6)
      parameter (abort   =  13)
      logical flgxtrap,outlmit
      integer codxtrap, ordint
      real valxtrap
      common /ez_qqqxtrp0/ ordint, flgxtrap, codxtrap, valxtrap, outlmit
      character*3 localop,localval
      localop =op(1:3)
      localval=val(1:3)
      call up2low(localop,localop)
      call up2low(localval,localval)
      if (flag) then
         if (localop.eq.  'ext') then
            if (localval .eq.  'oui') then
               codxtrap = oui
               ier = ezsetopt('extrap_degree', 'do_nothing')
            else if (localval.eq.  'abo') then
               codxtrap = abort
               ier = ezsetopt('extrap_degree', 'abort')
            else if (localval .eq.  'max') then
               codxtrap = maximum
               ier = ezsetopt('extrap_degree', 'maximum')
            else if (localval .eq.  'min') then
               codxtrap = minimum
               ier = ezsetopt('extrap_degree', 'minimum')
            else if (localval .eq.  'voi') then
               codxtrap = voisin
               ier = ezsetopt('extrap_degree', 'nearest')
            else if (localval .eq.  'val') then
               codxtrap = valeur
               ier = ezsetopt('extrap_degree', 'value')
            else
               print *, ' <rgoptc>: mauvaise valeur pour val'
               print *, '           val = ', val
               print *, '           val initialisee a ''abort'''
               codxtrap = abort
               ier = ezsetopt('extrap_degree', 'abort')
            endif
         else if (localop.eq.'int') then
            if (localval .eq.  'voi') then
               ordint = voisin
               ier = ezsetopt('interp_degree', 'nearest')
            else if (localval.eq.  'lin') then
               ordint = lineair
               ier = ezsetopt('interp_degree', 'linear')
            else if (localval.eq.  'cub') then
               ordint = cubique
               ier = ezsetopt('interp_degree', 'cubic')
            else
               print *, ' <rgoptc>: mauvaise valeur pour val'
               print *, '           val = ', val
               print *, '           val initialisee a ''cubique'''
               ordint = cubique
               ier = ezsetopt('interp_degree', 'cubic')
            endif
         else
            print *, ' <rgoptc>: mauvaise valeur pour op'
            print *,             '     op devrait etre egal a ''extrap'' ou ''interp'''
         endif
      else
         if (localop .eq.  'ext') then
            ier = ezgetopt('extrap_degree', val)
         endif
         if (localop .eq.  'int') then
            ier = ezgetopt('interp_degree', val)
         endif
      endif
      return
      end
      subroutine ez_rgopti(op, val, flag)
      implicit none
      character*8 op
      integer val
      logical flag
      integer ier,ezsetval,ezgetval,ezsetopt,ezgetopt
!     flag = .true.  mode set
!     flag = .false. mode get
      integer voisin, lineair, cubique
      integer oui, abort, valeur, maximum, minimum
      parameter (voisin  =   0)
      parameter (lineair =   1)
      parameter (cubique =   3)
      parameter (oui     =   1)
      parameter (minimum =   4)
      parameter (maximum =   5)
      parameter (valeur  =   6)
      parameter (abort   =  13)
      logical flgxtrap,outlmit
      integer codxtrap, ordint
      real valxtrap
      common /ez_qqqxtrp0/ ordint, flgxtrap, codxtrap, valxtrap, outlmit
      data codxtrap / oui /
      data flgxtrap / .false. /
      data ordint   / 3 /
      real rval
      character*3 localop
      character*16 local_val
      localop=op(1:3)
      call up2low(localop,localop)
      if (flag) then
         if (localop .eq.  'ext') then
            if (val .eq.  voisin .or.  val .eq.  lineair             .or.  val .eq. cubique) then
               if (val.eq.100.or.val.eq.0) then
                  ier = ezsetopt('interp_degree','nearest')
               elseif (val.eq.1) then
                  ier = ezsetopt('interp_degree','linear')
               elseif (val.eq.3) then
                  ier = ezsetopt('interp_degree','cubic')
               endif
            else
               valxtrap = real(val)
            endif
            ier = ezsetval('extrap_value',valxtrap)
         else
            if (localop .eq.  'int') then
               if (val.eq.100.or.val.eq.0) then
                  ier = ezsetopt('interp_degree','nearest')
               elseif (val.eq.1) then
                  ier = ezsetopt('interp_degree','linear')
               elseif (val.eq.3) then
                  ier = ezsetopt('interp_degree','cubic')
               else
                  print *, '<ez_rgopti> Erreur!'
               endif
            endif
         endif
      else
         if (localop.eq.'ext') then
            ier = ezgetval('extrap_value',rval)
            val = nint(rval)
         else if (localop .eq.  'int') then
            ier = ezgetopt('interp_degree', local_val)
            if (local_val.eq.'nearest') then
               val = 0
            elseif (local_val.eq.'linear') then
               val = 1
            else
               val = 3
            endif
         endif
      endif
      return
      end
      subroutine ez_rgoptr(op, val, flag)
      implicit none
      character*8 op
      real val
      logical flag
      integer ier, ezsetval,ezgetval
!     flag = .true.  mode set
!     flag = .false. mode get
      integer voisin, lineair, cubique
      integer oui, abort, valeur, maximum, minimum
      parameter (voisin  =   0)
      parameter (lineair =   1)
      parameter (cubique =   3)
      parameter (oui     =   1)
      parameter (minimum =   4)
      parameter (maximum =   5)
      parameter (valeur  =   6)
      parameter (abort   =  13)
      logical flgxtrap,outlmit
      integer codxtrap, ordint
      real valxtrap
      common /ez_qqqxtrp0/ ordint, flgxtrap, codxtrap, valxtrap, outlmit
      character*3 localop
      localop=op(1:3)
      call up2low(localop,localop)
      if (flag) then
         ier = ezsetval('extrap_value',val)
      else
         ier = ezgetval('extrap_value',val)
      endif
      return
      end
   subroutine lorenzo_mask_fill(fld, masque, ni, nj, methode)
   implicit none
   integer ni,nj,methode
   real, dimension(:,:) :: fld(ni,nj)
   integer, dimension(:,:) :: masque(ni,nj)
   integer i,j,ii,last_i
   real rmin, rmax
! methode
!  1- Predicteur de Lorenzo
!  2- Minimum
!  3- Remplissage horizontal
   rmin = minval(fld)
   rmax = maxval(fld)
   select case (methode)
   case(1)
!       do i=1,ni
!          if (masque(i,1) == 0) then
!             fld(i,1) = rmin
!          endif
!       enddo
!
!       do j=1,nj
!          if (masque(1,j) == 0) then
!             fld(1,j) = rmin
!          endif
!       enddo
      do j=2,nj
         do i=2,ni
            if (masque(i,j) == 0) then
               fld(i,j) = fld(i-1,j) + fld(i,j-1) - fld(i-1,j-1)
               if (fld(i,j) < rmin) fld(i,j) = rmin
               if (fld(i,j) > rmax) fld(i,j) = rmax
            endif
         enddo
      enddo
   case(2)
      do j=1,nj
         do i=1,ni
            if (masque(i,j) == 0) then
               fld(i,j) = rmin
            endif
         enddo
      enddo
   case(3)
      do j=1,nj
         i = 1
         last_i = 1
         if (masque(i,j) == 0) then  
            do
               i = i+1
               if (masque(i,j) /= 0) exit
               if (i == ni) then
                  fld(1:ni,j) = rmin
                  exit
               endif
            enddo
            if (i < ni) then
               do ii=last_i,i-1
                  fld(ii,j) = fld(i,j)
               enddo
            endif
         endif
         do
            i = i+1
            if (i >= ni+1) exit
            if (masque(i,j) == 0) then
               fld(i,j) = fld(last_i,j)
            endif
         last_i = i
         enddo
      enddo
   end select
   return
   end subroutine lorenzo_mask_fill
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!**s/p gfw2llw - converts from u-v (grid) wind components to
!                standard meteorological speed and direction
!
      subroutine llwfgfw(z1,z2,xlat,xlon,li,lj,grtyp,ig1,ig2,ig3,ig4)
      implicit none
      integer li,lj
      real z1(li,lj), z2(li,lj), xlat(li,lj),xlon(li,lj)
      character*1 grtyp
      integer ig1,ig2,ig3,ig4
      external cigaxg
!
!auteur- y. chartier - april 94
!
!langage- fortran
!
!objet(gfw2llw)
!     - passe de vent de grille (composantes u et v)
!     - a vitesse et direction.
!
!librairies
!
!appel- call gfw2llw(spd,psi,li,lj,iyp,xg1,xg2,xg3,xg4)
!
!modules- xgaig
!
!arguments
!     in/out - spd   - a l'entree contient la composante u
!     a la sortie la vitesse.
!     in/out - dir   - a l'entree contient la composante v
!     a la sortie la direction
!     in    - li    - premiere dimension des champs spd et dir
!     in    - lj    - deuxieme dimension des champs spd et dir
!     in    - igtyp  - type de grille (voir ouvrir)
!     in    - xg1   - ** descripteur de grille (reel),
!     in    - xg2   -    igtyp = 'n', pi, pj, d60, dgrw
!     in    - xg3   -    igtyp = 'l', lat0, lon0, dlat, dlon,
!     in    - xg4   -    igtyp = 'a', 'b', 'g', xg1 = 0. global,
!                                                   = 1. nord
!                                                   = 2. sud **
!
!messages- "erreur mauvaise grille (gfw2llw)"
!
!-------------------------------------------------------------
!
      real r(3,3),ri(3,3)
      common /qqqmrot/ r,ri
!
      integer i,j,ier
      real xlat1,xlon1,xlat2,xlon2
      real :: xlatgf(li,lj),xlongf(li,lj)
      real :: uvcart(3, li, lj), xyz(3, li, lj)
      call cigaxg(grtyp,xlat1,xlon1,xlat2,xlon2,ig1,ig2,ig3,ig4)
      call ez_crot( r, ri, xlon1, xlat1, xlon2, xlat2 )
      call ez_gfxyfll(xlon,xlat,xlongf,xlatgf,li*lj,      xlat1,xlon1,xlat2,xlon2)
      call ez_vrotf2(z1,z2,xlon,xlat,xlongf,xlatgf,ri,xyz,uvcart,li,lj)
      call ez_llwfgdw(z1,z2,xlongf,li,lj,'L',0,0,0,0)
      return
      end
   subroutine inside_or_outside(masque,x, y, out_lat,out_lon, gdin_lat, gdin_lon, ni_src, nj_src, wgts, idxs, num_wgts)
   implicit none
   integer :: masque, ni_src, nj_src, num_wgts
   real out_lat, out_lon, gdin_lat(ni_src, nj_src), gdin_lon(ni_src, nj_src)
   real :: wgts(num_wgts), x, y
   integer :: idxs(num_wgts,2), i,j, ix
   logical pt_in_quad
   external pt_in_quad
   ix = minloc(wgts, 1)
   i = min(max(2,idxs(ix,1)),ni_src-1)
   j = min(max(2,idxs(ix,2)),nj_src-1)
   
   if (pt_in_quad(out_lon, out_lat, gdin_lon(i-1,j-1), gdin_lat(i-1,j-1), &
         gdin_lon(i,j-1), gdin_lat(i,j-1), gdin_lon(i,j), gdin_lat(i,j), &
         gdin_lon(i-1,j), gdin_lat(i-1,j))) then
         masque = 1
         call ez_uvfllc2d(x, y,out_lon, out_lat, &
            gdin_lon(i-1,j-1), gdin_lat(i-1,j-1), &
            gdin_lon(i,j-1), gdin_lat(i,j-1), &
            gdin_lon(i,j), gdin_lat(i,j), &
            gdin_lon(i-1,j), gdin_lat(i-1,j))
         x = x + (i-1)
         y = y + (j-1)
   
   else if (pt_in_quad(out_lon, out_lat, gdin_lon(i,j-1), gdin_lat(i,j-1), &
         gdin_lon(i+1,j-1), gdin_lat(i+1,j-1), gdin_lon(i+1,j), gdin_lat(i+1,j), &
         gdin_lon(i,j), gdin_lat(i,j))) then
         masque = 1
         call ez_uvfllc2d(x, y,out_lon, out_lat, &
            gdin_lon(i,j-1), gdin_lat(i,j-1), &
            gdin_lon(i+1,j-1), gdin_lat(i+1,j-1), &
            gdin_lon(i+1,j), gdin_lat(i+1,j), &
            gdin_lon(i,j), gdin_lat(i,j))
         x = x + (i)
         y = y + (j-1)
!  Cas 3 - Coin inferieur droit
   else if (pt_in_quad(out_lon, out_lat, gdin_lon(i-1,j), gdin_lat(i-1,j), &
         gdin_lon(i,j), gdin_lat(i,j), gdin_lon(i,j+1), gdin_lat(i,j+1), &
         gdin_lon(i-1,j+1), gdin_lat(i-1,j+1))) then
         masque = 1
         call ez_uvfllc2d(x, y,out_lon, out_lat, &
            gdin_lon(i-1,j), gdin_lat(i-1,j), &
            gdin_lon(i,j), gdin_lat(i,j), &
            gdin_lon(i,j+1), gdin_lat(i,j+1), &
            gdin_lon(i-1,j+1), gdin_lat(i-1,j+1))
         x = x + (i-1)
         y = y + (j)
!  Cas 4 - Coin inferieur gauche
   else if (pt_in_quad(out_lon, out_lat, gdin_lon(i,j), gdin_lat(i,j), &
         gdin_lon(i+1,j), gdin_lat(i+1,j), gdin_lon(i+1,j+1), gdin_lat(i+1,j+1), &
         gdin_lon(i,j+1), gdin_lat(i,j+1))) then
         masque = 1
         call ez_uvfllc2d(x, y,out_lon, out_lat, &
            gdin_lon(i,j), gdin_lat(i,j), &
            gdin_lon(i+1,j), gdin_lat(i+1,j), &
            gdin_lon(i+1,j+1), gdin_lat(i+1,j+1), &
            gdin_lon(i,j+1), gdin_lat(i,j+1))
         x = x + (i)
         y = y + (j)
   else
      masque = 0
      x = -1.0
      y = -1.0
   endif
   return
   end subroutine inside_or_outside
   logical function pt_in_quad(x,y,x1,y1,x2,y2,x3,y3,x4,y4)
   implicit none
   real x,y,x1,y1,x2,y2,x3,y3,x4,y4
   logical pt_in_triangle
   external pt_in_triangle
   pt_in_quad = .false.
   if (pt_in_triangle(x,y,x1,y1,x2,y2,x3,y3)) then
      pt_in_quad = .true.
   else
      pt_in_quad = pt_in_triangle(x,y,x1,y1,x3,y3,x4,y4)
   endif
   return
   end function pt_in_quad
   logical function pt_in_triangle(x,y,x1,y1,x2,y2,x3,y3)
   implicit none
   real x,y,x1,y1,x2,y2,x3,y3
   real a,b,c,d, det, lambda1, lambda2, lambda3
   pt_in_triangle = .false.
   a = x1 - x3
   b = x2 - x3
   c = y1 - y3
   d = y2 - y3
   det = 1.0 / (a * d - b * c)
   lambda1 = (d * (x - x3) - b * (y - y3)) * det
   lambda2 = (a * (y - y3) - c * (x - x3)) * det
   lambda3 = 1.0 - lambda1 - lambda2
   if (lambda1 < 0.0 .or. lambda1 > 1.0) then
      return
   else if (lambda2 < 0.0 .or. lambda2 > 1.0) then
      return
   else if (lambda3 < 0.0 .or. lambda3 > 1.0) then
      return
   else
      pt_in_triangle =  .true.
   endif
   return
   end function pt_in_triangle
!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!**s/p rgll2gd - passe des composantes speed, psi
!                aux composantes u,v selon le type de grille.
      subroutine rgll2gd(spdo,psio,xlon,li,lj,grtyp,ig1,ig2,ig3,ig4)
      implicit none
      integer li,lj
      real spdo(li,lj), psio(li,lj), xlon(li,lj)
      character*1 grtyp
      integer ig1,ig2,ig3,ig4
      external cigaxg
!
!auteur- y. chartier - avril 91
!     
!langage  - ratfor
!
!objet(rgll2gd)
!         - passe de vent de grille (composantes u et v)
!         - a vitesse et direction. 
!
!librairies
!
!appel    - call rgll2gd(spd,psi,li,lj,iyp,xg1,xg2,xg3,xg4)
!
!modules  - xgaig
!
!arguments
!  in/out - spd   - a l'entree contient la vitesse du vent et
!                   a la sortie la composante u.
!  in/out - psi   - a l'entree contient la direction du vent et
!                   a la sortie la composante v.
!   in    - li    - premiere dimension des champs spd et psi
!   in    - lj    - deuxieme dimension des champs spd et psi
!   in    - igtyp  - type de grille (voir ouvrir)
!   in    - xg1   - ** descripteur de grille (reel),
!   in    - xg2   -    igtyp = 'n', pi, pj, d60, dgrw
!   in    - xg3   -    igtyp = 'l', lat0, lon0, dlat, dlon,
!   in    - xg4   -    igtyp = 'a', 'b', 'g', xg1 = 0. global,
!                                                 = 1. nord
!                                                 = 2. sud **
!
!messages - "erreur mauvaise grille (rgll2gd)"
!
!-------------------------------------------------------------
!
!
!     
!     * 1.866025=(1+sin60),   6.371e+6=earth radius in meters.      
!     
!     rdtodg = 180/pie, dgtord = pie/180        
      real pie,rdtodg,dgtord
      data pie    /3.1415926535898/
      data rdtodg /57.295779513082/
      data dgtord /1.7453292519943e-2/
!         
      integer i,j
      real psi,u,v
      real xg1, xg2, xg3, xg4
      if (grtyp .eq. 'N') then
         call cigaxg(grtyp,xg1,xg2,xg3,xg4,ig1,ig2,ig3,ig4)
         do 10 i=1,li
            do 20 j=1,lj
               psi =xlon(i,j)+xg4-psio(i,j)
               u = cos(psi*dgtord)*spdo(i,j)
               v = sin(psi*dgtord)*spdo(i,j)
               spdo(i,j) = u
               psio(i,j) = v
 20         continue
 10      continue
         return
      endif
      if (grtyp .eq. 'S') then
         call cigaxg(grtyp,xg1,xg2,xg3,xg4,ig1,ig2,ig3,ig4)
         do 30 i=1,li
            do 40 j=1,lj
               psi =180.0 - xlon(i,j)+xg4-psio(i,j)
               u = cos(psi*dgtord)*spdo(i,j)
               v = sin(psi*dgtord)*spdo(i,j)
               spdo(i,j) = u
               psio(i,j) = v
 40         continue
 30      continue
         return
      endif
      if (grtyp.eq.'A'.or.grtyp.eq.'B'.or.grtyp.eq.'G'.or.      grtyp.eq.'L') then
         do 50 i=1,li
            do 60 j=1,lj
               psi = 270.0 - psio(i,j)
               u = cos(psi*dgtord)*spdo(i,j)
               v = sin(psi*dgtord)*spdo(i,j)
               spdo(i,j) = u
               psio(i,j) = v
 60         continue
 50      continue
         return
      endif
 600  format('0',' erreur, mauvaise grille (rgll2gd) - grtyp = ', A1)
      return
      end
