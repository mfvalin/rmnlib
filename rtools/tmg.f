      subroutine tmg_init ( id, string )
      implicit none
*
      character* (*) string
      integer id
!     under AIX fort_hpminit can be either a ricochet call to tmg_init_f
!     or the routine using the hpm library
      character*12 tmg_opt
      logical tmg_go4it
      common /tmg_c/ tmg_opt
      common /tmg_l/ tmg_go4it
      call getenvc ('TMG_ON',tmg_opt)
      tmg_go4it=tmg_opt.eq.'YES'
*
      if (.not.tmg_go4it) return
      call fort_hpminit ( id, string )
      return
      end
      subroutine tmg_init_f ( id, string )
      implicit none
*
      character* (*) string
      integer id
      character*12 tmg_opt
      logical tmg_go4it
      common /tmg_c/ tmg_opt
      common /tmg_l/ tmg_go4it
*
      integer maxtmg,my_pe
      parameter (maxtmg=100)
      character*12 evy
      character*60 msg(maxtmg)
      real*8 tmg_of(maxtmg), tmg_cf(maxtmg),
     $       tmg_ot(maxtmg), tmg_ct(maxtmg), c1, r1
      common /tmg_1/ tmg_of, tmg_cf, tmg_ot, tmg_ct, c1, r1
      common /tmg_2/ evy,msg
      common /tmg_3/ my_pe
*
      call getenvc ('TMG_ON',tmg_opt)
      tmg_go4it=.false.
      if (tmg_opt.eq.'YES') tmg_go4it=.true.
*
      if (.not.tmg_go4it) return
*
*
*     ---------------------------------------------------------------
      my_pe = id
      evy   = string
      tmg_cf= 0.d0
      tmg_ct= 0.d0
      c1    = 2.d0/1.0d9
      r1    = 1.0d6
      msg   = '@#$%^&'
*     ---------------------------------------------------------------
*
      return
      end
*
      subroutine tmg_terminate ( id )
      implicit none
*
      integer id
      character*12 tmg_opt
      logical tmg_go4it
      common /tmg_c/ tmg_opt
      common /tmg_l/ tmg_go4it
*
      if (.not.tmg_go4it) return
!     under AIX fort_hpmterminate can be either a ricochet call to tmg_terminate_f
!     or the routine using the hpm library
      call fort_hpmterminate ( id )
      return
      end
      subroutine tmg_terminate_f ( id )
      implicit none
*
      integer id
      character*12 tmg_opt
      logical tmg_go4it
      common /tmg_c/ tmg_opt
      common /tmg_l/ tmg_go4it
*
      integer maxtmg,my_pe
      parameter (maxtmg=100)
      character*12 evy
      character*60 msg(maxtmg)
      real*8 tmg_of(maxtmg), tmg_cf(maxtmg),
     $       tmg_ot(maxtmg), tmg_ct(maxtmg), c1, r1
      common /tmg_1/ tmg_of, tmg_cf, tmg_ot, tmg_ct, c1, r1
      common /tmg_2/ evy,msg
      common /tmg_3/ my_pe
*
      integer i
*
      if (.not.tmg_go4it) return
*
*     ---------------------------------------------------------------
*
      if (evy(1:3).ne.'YES') then
         do i = 1, maxtmg
         if (msg(i).ne.'@#$%^&') then
             if (tmg_ct(i).lt.1.)
     $       write (6,200) my_pe,msg(i),tmg_ct(i),
     $                 tmg_cf(i),tmg_cf(i)/tmg_ct(i)/r1
             if ((tmg_ct(i).ge.1.).and.(tmg_ct(i).lt.10000.))
     $       write (6,201) my_pe,msg(i),tmg_ct(i),
     $                 tmg_cf(i),tmg_cf(i)/tmg_ct(i)/r1
             if (tmg_ct(i).ge.10000.)
     $       write (6,202) my_pe,msg(i),tmg_ct(i),
     $                 tmg_cf(i),tmg_cf(i)/tmg_ct(i)/r1
         endif
         end do
      endif
 200  format ('###TMG: PE=',i3,1x,a8,f12.8,f20.0,f10.3)
 201  format ('###TMG: PE=',i3,1x,a8,f12.4,f20.0,f10.3)
 202  format ('###TMG: PE=',i3,1x,a8,f12.0,f20.0,f10.3)
*     ---------------------------------------------------------------
*
      return
      end
*
      subroutine tmg_tstart ( id, string )
      implicit none
*
      character* (*) string
      integer id
      character*12 tmg_opt
      logical tmg_go4it
      common /tmg_c/ tmg_opt
      common /tmg_l/ tmg_go4it
*
      if (.not.tmg_go4it) return
!     under AIX fort_hpmtstart can be either a ricochet call to tmg_start_f
!     or the routine using the hpm library
      call fort_hpmtstart ( id, string)
      return
      end
      subroutine tmg_start ( id, string )
      implicit none
*
      character* (*) string
      integer id
      character*12 tmg_opt
      logical tmg_go4it
      common /tmg_c/ tmg_opt
      common /tmg_l/ tmg_go4it
*
      if (.not.tmg_go4it) return
!     under AIX fort_hpmstart can be either a ricochet call to tmg_start_f
!     or the routine using the hpm library
      call fort_hpmstart ( id, string)
      return
      end
      subroutine tmg_start_f ( id, string )
      implicit none
*
      character* (*) string
      integer id
*
      character*12 tmg_opt
      logical tmg_go4it
      common /tmg_c/ tmg_opt
      common /tmg_l/ tmg_go4it
*
      integer maxtmg,my_pe
      parameter (maxtmg=100)
      character*12 evy
      character*60 msg(maxtmg)
      real*8 rtools_wtime
      real*8 tmg_of(maxtmg), tmg_cf(maxtmg),
     $       tmg_ot(maxtmg), tmg_ct(maxtmg), c1, r1
      external rtools_wtime
      common /tmg_1/ tmg_of, tmg_cf, tmg_ot, tmg_ct, c1, r1
      common /tmg_2/ evy,msg
      common /tmg_3/ my_pe
*
*     ---------------------------------------------------------------
*
      if (.not.tmg_go4it) return
*
      if (id.gt.maxtmg) return
      msg(id)    = string
      tmg_of(id) =        0.
      tmg_ot(id) = rtools_wtime()
*     ---------------------------------------------------------------
*
      return
      end
*
      subroutine tmg_tstop ( id )
      implicit none
*
      integer id
      character*12 tmg_opt
      logical tmg_go4it
      common /tmg_c/ tmg_opt
      common /tmg_l/ tmg_go4it
*
      if (.not.tmg_go4it) return
!     under AIX fort_hpmtstop can be either a ricochet call to tmg_stop_f
!     or the routine using the hpm library
      call fort_hpmtstop ( id )
      return
      end
      subroutine tmg_stop ( id )
      implicit none
*
      integer id
      character*12 tmg_opt
      logical tmg_go4it
      common /tmg_c/ tmg_opt
      common /tmg_l/ tmg_go4it
*
      if (.not.tmg_go4it) return
!     under AIX fort_hpmstop can be either a ricochet call to tmg_stop_f
!     or the routine using the hpm library
      call fort_hpmstop ( id )
      return
      end
      subroutine tmg_stop_f ( id )
      implicit none
*
      integer id
*
      character*12 tmg_opt
      logical tmg_go4it
      common /tmg_c/ tmg_opt
      common /tmg_l/ tmg_go4it
*
      integer maxtmg,my_pe
      parameter (maxtmg=100)
      character*12 evy
      character*60 msg(maxtmg)
      real*8 tmg_of(maxtmg), tmg_cf(maxtmg),
     $       tmg_ot(maxtmg), tmg_ct(maxtmg), c1, r1
      real*8 rtools_wtime
      external rtools_wtime
      common /tmg_1/ tmg_of, tmg_cf, tmg_ot, tmg_ct, c1, r1
      common /tmg_2/ evy,msg
      common /tmg_3/ my_pe
*
      integer i
*     ---------------------------------------------------------------
*
      if (.not.tmg_go4it) return
*
      if (id.gt.maxtmg) return
      tmg_cf(id) = 0.
      tmg_ct(id) = tmg_ct(id) + (rtools_wtime() - tmg_ot(id))
*
      if (evy(1:3).eq.'YES') then
          if (tmg_ct(id).lt.1.)
     $    write (6,200) my_pe,msg(id),tmg_ct(id),
     $              tmg_cf(id),tmg_cf(id)/tmg_ct(id)/r1
          if ((tmg_ct(id).ge.1.).and.(tmg_ct(id).lt.10000.))
     $    write (6,201) my_pe,msg(id),tmg_ct(id),
     $              tmg_cf(id),tmg_cf(id)/tmg_ct(id)/r1
          if (tmg_ct(id).ge.10000.)
     $    write (6,202) my_pe,msg(id),tmg_ct(id),
     $              tmg_cf(id),tmg_cf(id)/tmg_ct(id)/r1
         tmg_cf(id) = 0.
         tmg_ct(id) = 0.
      endif
*
 200  format ('###TMG: PE=',i3,1x,a8,f12.8,f20.0,f10.3)
 201  format ('###TMG: PE=',i3,1x,a8,f12.4,f20.0,f10.3)
 202  format ('###TMG: PE=',i3,1x,a8,f12.0,f20.0,f10.3)
*     ---------------------------------------------------------------
*
      return
      end
