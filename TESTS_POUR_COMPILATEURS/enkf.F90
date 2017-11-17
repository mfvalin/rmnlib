subroutine calxhxt(xhxt,rholoc,trlloc,trllocsum,trlnov,trlnovsum,ireg)
 implicit none
!
! subroutine calxhxt
!
! authors Peter Houtekamer and Herschel Mitchell
!         July 2004
!
! For each subensemble, compute pht.
!
!input variables
!         rholoc: localizing matrix
!         trlnov:  array of dimension (nsubens,nnovs,mxreg,npart) with
!           values of Hx for each member of the ensemble.
!         trlnovsum: ensemble mean values for each subensemble in trlnov
!         trlloc:  array of dimension (nsubens,ngridsv,npart) with values of
!           the trial field for each member of the ensemble.
!         trllocsum: ensemble mean values for each subensemble in trlloc
!         ireg:    number of the region
!output variables:
!         xhxt:    product of trlloc with the transpose of trlnov.
 real*8, dimension(:,:,:,:), intent(in)  :: trlnov
 real*8, dimension(:,:,:),   intent(in)  :: trlloc,trlnovsum
 real*8, dimension(:,:),     intent(in)  :: trllocsum,rholoc
 integer,                    intent(in)  :: ireg
 real*8, dimension(:,:,:),   intent(inout) :: xhxt

 real*8, dimension(:,:), allocatable :: trllocbar,trlnovbar

 integer :: ier,igridsv,inovs,ipart,ngridsv,mpar,nnovs,npart,nsubens
 real*8  :: alpha,beta,con,dum,rho,sumparts

!         nsubens: number of members per subensemble
 nsubens=size(trlnov,1)
!         npart:   number of subensembles
 npart=size(trlnov,4)
!         ngridsv: number of model coordinates.
 ngridsv=size(xhxt,1)
!         nnovs:   length of the vector of interpolated guess fields (Hx).
 nnovs=size(xhxt,2)
 !==========================================================================
!  allocate(trllocbar(npart,ngridsv),stat=ier)
 allocate(trllocbar(ngridsv,npart),stat=ier)
 allocate(sumparts(max(nnovs,ngridsv)))
 mpar=npart-1
 con=1.0D0/dble(mpar)
!  do igridsv=1,ngridsv
!   sumparts=0.0D0
!   do ipart=1,npart
!    sumparts=sumparts+trllocsum(igridsv,ipart)
!   enddo
!   do ipart=1,npart
!    trllocbar(ipart,igridsv)=con*(sumparts-trllocsum(igridsv,ipart))
!   enddo
!  enddo
!$OMP PARALLEL DO PRIVATE(ipart,igridsv,igridsv0) SHARED(sumparts,trllocsum,trllocbar,con)
do igridsv0=1,ngridsv,256
  igridsv1 = min(ngridsv,igridsv0+255)
  sumparts(igridsv0:igridsv1) = trllocsum(igridsv0:igridsv1,1)
  do ipart=2,npart
    sumparts(igridsv0:igridsv1)=sumparts(igridsv0:igridsv1)+trllocsum(igridsv0:igridsv1,ipart)
  enddo
  do ipart=1,npart
    trllocbar(igridsv0:igridsv1,ipart)=con*(sumparts(igridsv0:igridsv1)-trllocsum(igridsv0:igridsv1,ipart))
  enddo
enddo
!$OMP END PARALLEL DO
!===============================================================================

!  allocate(trlnovbar(npart,nnovs),stat=ier)
 allocate(trlnovbar(nnovs,npart),stat=ier)
 dum=dble(nsubens)
!  do inovs=1,nnovs
!   sumparts=0.0D0
!   do ipart=1,npart
!    sumparts=sumparts+trlnovsum(inovs,ireg,ipart)
!   enddo
!   do ipart=1,npart
!    trlnovbar(ipart,inovs)=dum*(sumparts-trlnovsum(inovs,ireg,ipart))
!   enddo
!  enddo
 sumparts(1:nnovs)=0.0D0
 do ipart=1,npart
   do inovs=1,nnovs
     sumparts(inovs)=sumparts(inovs)+trlnovsum(inovs,ireg,ipart)
   enddo
 enddo
!$OMP PARALLEL DO PRIVATE(ipart,innovs)
 do ipart=1,npart
   do inovs=1,nnovs
      trlnovbar(inovs,ipart)=dum*(sumparts(inovs)-trlnovsum(inovs,ireg,ipart))
   enddo
 enddo
!$OMP END PARALLEL DO

 alpha=1.0D0
 beta=0.0D0

!$OMP PARALLEL DO PRIVATE(ipart)
 do ipart=1,npart

! note: this comment illustrates the action of library routine dgemm
! (it is not Fortran code intended to be re-activated).
!      do inovs=1,nnovs
!       do igridsv=1,ngridsv
!        do iens=1,nsubens
!         xhxt(igridsv,inovs,ipart)=xhxt(igridsv,inovs,ipart)+ &
!          trlloc(iens,igridsv,ipart)*trlnov(iens,inovs,ireg,ipart)
!        continue
!       continue
!      continue

  call dgemm('T','N',ngridsv,nnovs,nsubens,alpha, &
      trlloc(1,1,ipart),nsubens, &
      trlnov(1,1,ireg,ipart),nsubens,beta, &
      xhxt(1,1,ipart),ngridsv)

 enddo
!$OMP END PARALLEL DO
 con=1.0D0/dble(mpar*nsubens-1)

! !$OMP PARALLEL PRIVATE(igridsv,sumparts,rho,ipart,sumparts2)
! !$OMP DO
 do inovs=1,nnovs
!   do igridsv=1,ngridsv
!    sumparts=0.0D0
!    do ipart=1,npart
!     sumparts=sumparts+xhxt(igridsv,inovs,ipart)
!    enddo
!    rho=rholoc(igridsv,inovs)
!    do ipart=1,npart
!     xhxt(igridsv,inovs,ipart)=rho*con*( &
!       (sumparts-xhxt(igridsv,inovs,ipart)) - &
!       trlnovbar(inovs,ipart)*trllocbar(igridsv,ipart))
!    enddo
!   enddo
   do ipart=1,npart
     do igridsv=1,ngridsv
       sumparts(igridsv)=sumparts(igridsv)+xhxt(igridsv,inovs,ipart)
     enddo
   enddo
!$OMP PARALLEL DO PRIVATE(ipart,igridsv)
   do ipart=1,npart
     do igridsv=1,ngridsv
       xhxt(igridsv,inovs,ipart)=rholoc(igridsv,inovs)*con*( &
       (sumparts(igridsv)-xhxt(igridsv,inovs,ipart)) - &
       trlnovbar(inovs,ipart)*trllocbar(igridsv,ipart))   ! on peut sortir trlnovbar(inovs,ipart) de vette boucle
     enddo
   enddo
!$OMP END PARALLEL DO   
 enddo
! !$OMP END DO
! !$OMP END PARALLEL

 deallocate(trllocbar,stat=ier)
 deallocate(trlnovbar,stat=ier)

 return
end subroutine calxhxt
 
