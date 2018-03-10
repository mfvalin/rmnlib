      program simu
!
!  to compile on cray
!     ftn -DMPI -O3 -o simu_opt30 simu_opt30_bench_2.F
!  to compile with OpenMPI (or Mpich)
!     mpif90 -DMPI -O3 -o simu_opt30 simu_opt30_bench_2.F
!  to run, use aprun on cray, mpirun with OpenMPI (or Mpich)
!
      use ISO_C_BINDING
      implicit none
#ifdef MPI
      include 'mpif.h'
      common /mpi_world/ OWN_COMM_WORLD
      integer(C_SIZE_T), parameter :: L128 = 128
      integer :: OWN_COMM_WORLD, host_id
      double precision T1,T2
      real :: TMIN,TMAX,TSQR,TSUM,TAVG
      real :: tjit
      integer, dimension(0:21) :: tj
      real, dimension(:), pointer :: TIMES, BARRIER
      real, dimension(:,:), pointer :: ALLTIMES, NTIMES
      double precision :: TIME0
      character(len=1), dimension(L128) :: hostname
      character(len=8), dimension(:), pointer :: hosts_8
      character(len=L128) :: host_128
      integer :: my_proc, total_procs, err
      integer :: pe_to, pe_from, pe0, total0
!       int gethostname(char *name, size_t len);
      interface
        integer(C_INT) function f_gethostid()BIND(C,name='gethostid')
          import :: C_INT
        end function f_gethostid
        integer(C_INT) function f_gethostname(name,nchar)
     %                   BIND(C,name='gethostname')
          import :: C_INT, C_CHAR, C_SIZE_T
          integer(C_SIZE_T), intent(IN), value :: nchar
          character(C_CHAR), dimension(nchar), intent(OUT) :: name
        end function f_gethostname
      end interface
#endif
      real  R8DIM
      real t0
      real t
      real, dimension(:,:,:), pointer :: tabl, old_tabl, old_old_tabl
      integer i,j, MAXSTEPS, DIM, NREP, nsteps
      common /tables/ cosj, cosi,sini,sinj,deltat
      real, dimension(:), pointer :: cosj, cosi,sini,sinj
      real, dimension(:,:), pointer :: buffer
      integer BUFDIM
      real deltat
!      integer omp_get_thread_num
      real AVG
      real pi
      real  one
      parameter(one=1.0)
      integer pe_me,pe_npes
      integer  itime1, itime2 , time_base


      my_proc=0
      total_procs=1
#ifdef MPI
      call mpi_init(err)
      call mpi_comm_rank(MPI_COMM_WORLD,pe0,err)
      call mpi_comm_size(MPI_COMM_WORLD,total0,err)
!       print *,'rank in world=',my_proc
      host_id = f_gethostid()
      host_128 = ' '
      i = f_gethostname(hostname,L128)
      do i = 1,L128
        if(hostname(i) == achar(0)) exit
        host_128(i:i) = hostname(i)
      enddo
      allocate(hosts_8(total0))
      call MPI_gather(host_128(1:8), 8, MPI_CHAR, 
     %                hosts_8      , 8, MPI_CHAR, 
     %                0, MPI_COMM_WORLD, err)
      host_id = abs(host_id)
      call MPI_COMM_SPLIT(MPI_COMM_WORLD, host_id, pe0, 
     %                    OWN_COMM_WORLD, err)
      T1=MPI_Wtime()                                      ! start of run
      call mpi_comm_rank(OWN_COMM_WORLD,my_proc,err)
!       print *,'rank in own world=',my_proc
      call mpi_comm_size(OWN_COMM_WORLD,total_procs,err)
      pe_to=mod(my_proc+1,total_procs)
      pe_from=my_proc-1
      if(pe_from .lt. 0) pe_from=pe_from+total_procs
#endif
!       if(my_proc .eq. 0)then
!         call system('echo running on = `hostname` ')
!         print 1, 'host = '//trim(host_128)
!       endif
      open(5,file='nsteps',form='FORMATTED')
      NREP=1
      MAXSTEPS=600
      DIM=1024
      read(5,*,iostat=err)MAXSTEPS,DIM,NREP
      R8DIM=DIM
      if(pe0 .eq. 0)
     %  write(6,1)
     %       ' Will integrate forward for ',abs(MAXSTEPS),' steps',
     %       ' grid size=',DIM,' NREP=',NREP,' iostat=',err
#ifdef MPI
     %      ,' proc ',my_proc,' of ',total_procs
     %      ,' pe_to ',pe_to,' pe_from ',pe_from
#endif
1     format(A,I4,A,A,I4,A,I2,A,I3,A,I4,A,I4,A,I4,A,I4)
      close(5)
!======================================================================
!     INITIALISATION, setup initial conditions and needed tables
!======================================================================
      pi=acos(-one)
!      print *,'PI=',pi
      allocate(sini(0:DIM-1))
      allocate(sinj(0:DIM-1))
      allocate(cosi(0:DIM-1))
      allocate(cosj(0:DIM-1))
      BUFDIM = 16*DIM
      allocate(buffer(BUFDIM,4))
      if(my_proc .eq.0) then
            do i=0,DIM-1
                  sini(i) = sin(2*PI*i/R8DIM)
                  cosi(i) = cos(2*PI*i/R8DIM)
                  sinj(i) = sini(i)
                  cosj(i) = cosj(i)
            enddo
      endif
#ifdef MPI
      if(total_procs .gt. 1) then
            call mpi_bcast(sini,DIM,MPI_REAL,0,OWN_COMM_WORLD,err)
            call mpi_bcast(sinj,DIM,MPI_REAL,0,OWN_COMM_WORLD,err)
            call mpi_bcast(cosi,DIM,MPI_REAL,0,OWN_COMM_WORLD,err)
            call mpi_bcast(cosj,DIM,MPI_REAL,0,OWN_COMM_WORLD,err)
      else
            print *,'ONE MPI process, no sin/cos bcst needed'
      endif
#endif
      allocate(tabl(-2:DIM+1,-2:DIM+1,0:2))
      allocate(old_tabl(-2:DIM+1,-2:DIM+1,0:2))
      allocate(old_old_tabl(-2:DIM+1,-2:DIM+1,0:2))
      do j=-2,DIM+1
         do i=-2,DIM+1
            tabl(i,j,0) = 0.0
            tabl(i,j,1) = 0.0
            tabl(i,j,2) = 0.0
         enddo
      enddo
      if( my_proc .eq.0) then
      do j=-2,DIM+1
         do i=-2,DIM+1
            tabl(i,j,0) = .1*cos(5*j/R8DIM)*sin(5*i/R8DIM)
            tabl(i,j,1) = - .1*sin(5*j/R8DIM)*cos(5*i/R8DIM)
            tabl(i,j,2) = .1
            old_tabl(i,j,0) = tabl(i,j,0)
            old_tabl(i,j,1) = tabl(i,j,1)
            old_tabl(i,j,2) = tabl(i,j,2)
            old_old_tabl(i,j,0) = old_tabl(i,j,0)
            old_old_tabl(i,j,1) = old_tabl(i,j,1)
            old_old_tabl(i,j,2) = old_tabl(i,j,2)
         enddo
      enddo
      endif
#ifdef MPI
      if(total_procs .gt. 1) then
            call mpi_bcast(tabl,(DIM+4)*(DIM+4)*3,
     %           MPI_REAL,0,OWN_COMM_WORLD,err)
            call mpi_bcast(old_tabl,(DIM+4)*(DIM+4)*3,
     %           MPI_REAL,0,OWN_COMM_WORLD,err)
            call mpi_bcast(old_old_tabl,(DIM+4)*(DIM+4)*3,
     %           MPI_REAL,0,OWN_COMM_WORLD,err)
      else
            print *,'ONE MPI process, no tabl bcst needed'
      endif
      allocate(TIMES(MAXSTEPS))
      allocate(BARRIER(MAXSTEPS))
#endif
      if(my_proc .eq.0) then
!             call signature(tabl(-2,-2,0),DIM+4,DIM+4,AVG,1)
!             call signature(tabl(-2,-2,1),DIM+4,DIM+4,AVG,2)
!             call signature(tabl(-2,-2,2),DIM+4,DIM+4,AVG,3)
      endif
      t = 0.0
      call step_euler    (old_old_tabl, old_tabl, tabl, -1, t,DIM)
      call step_euler    (old_tabl, tabl, old_old_tabl, -1, t,DIM)
      call step_euler    (tabl, old_old_tabl, old_tabl, -1, t,DIM)
!======================================================================
!     BOUCLE SUR LE TEMPS
!======================================================================
      deltat=1.0/(R8DIM*100)
      nsteps=0
 10   if (nsteps+6 .le. abs(MAXSTEPS)) then
#ifdef MPI
         call MPI_barrier(OWN_COMM_WORLD,err)
         TIME0 = MPI_Wtime()
#endif
         call step_euler    (old_old_tabl, old_tabl, tabl, 1, t,DIM)
!          call pseudo_halo(buffer,BUFDIM,4,NREP,pe_to,pe_from)
         t = t + deltat
         nsteps=nsteps+1
#ifdef MPI
         TIMES(nsteps) = MPI_Wtime() - TIME0
!          TIME0 = MPI_Wtime()
         call MPI_barrier(OWN_COMM_WORLD,err)
!          BARRIER(nsteps) = MPI_Wtime() - TIME0
         TIME0 = MPI_Wtime()
#endif
         call step_leapfrog (old_tabl, tabl, old_old_tabl, 1, t,DIM)
!          call pseudo_halo(buffer,BUFDIM,4,NREP,pe_to,pe_from)
         t = t + deltat
         nsteps=nsteps+1
#ifdef MPI
         TIMES(nsteps) = MPI_Wtime() - TIME0
!          TIME0 = MPI_Wtime()
         call MPI_barrier(OWN_COMM_WORLD,err)
!          BARRIER(nsteps) = MPI_Wtime() - TIME0
         TIME0 = MPI_Wtime()
#endif
         call step_euler    (tabl, old_old_tabl, old_tabl, 1, t,DIM)
!          call pseudo_halo(buffer,BUFDIM,4,NREP,pe_to,pe_from)
         t = t + deltat
         nsteps=nsteps+1
#ifdef MPI
         TIMES(nsteps) = MPI_Wtime() - TIME0
!          TIME0 = MPI_Wtime()
         call MPI_barrier(OWN_COMM_WORLD,err)
!          BARRIER(nsteps) = MPI_Wtime() - TIME0
         TIME0 = MPI_Wtime()
#endif
         call step_leapfrog (old_old_tabl, old_tabl, tabl, 1, t,DIM)
!          call pseudo_halo(buffer,BUFDIM,4,NREP,pe_to,pe_from)
         t = t + deltat
         nsteps=nsteps+1
#ifdef MPI
         TIMES(nsteps) = MPI_Wtime() - TIME0
!          TIME0 = MPI_Wtime()
         call MPI_barrier(OWN_COMM_WORLD,err)
!          BARRIER(nsteps) = MPI_Wtime() - TIME0
         TIME0 = MPI_Wtime()
#endif
         call step_euler    (old_tabl, tabl, old_old_tabl, 1, t,DIM)
!          call pseudo_halo(buffer,BUFDIM,4,NREP,pe_to,pe_from)
         t = t + deltat
         nsteps=nsteps+1
#ifdef MPI
         TIMES(nsteps) = MPI_Wtime() - TIME0
!          TIME0 = MPI_Wtime()
         call MPI_barrier(OWN_COMM_WORLD,err)
!          BARRIER(nsteps) = MPI_Wtime() - TIME0
         TIME0 = MPI_Wtime()
#endif
         call step_leapfrog (tabl, old_old_tabl, old_tabl, 1, t,DIM)
!          call pseudo_halo(buffer,BUFDIM,4,NREP,pe_to,pe_from)
         t = t + deltat
         nsteps=nsteps+1
#ifdef MPI
         TIMES(nsteps) = MPI_Wtime() - TIME0
!          TIME0 = MPI_Wtime()
!          call MPI_barrier(OWN_COMM_WORLD,err)
!          BARRIER(nsteps) = MPI_Wtime() - TIME0
#endif
        if( mod(nsteps,67).eq.0 .and. my_proc.eq.-1 )
     %      write(6,'(A,f9.7)') 'T=',t
         goto 10
      endif
!======================================================================
!     FIN DE LA BOUCLE SUR LE TEMPS
!======================================================================

      if(my_proc .eq. -1)then   ! code is deactivated
         if(nsteps .eq. 3000 .and. DIM .eq.1024 )then
            if (AVG .gt. -2.753E-03 .and. AVG .lt.  -2.752E-03)then
                  write(6,*)'HYDRO TEST RAN SUCCESSFULLY'
            else
                  write(6,*)'HYDRO TEST FAILED'
            endif
         else
            write(6,*)'current dimension is ',DIM
            write(6,*)'current number of timesteps is ',nsteps
         endif
      endif
!======================================================================
!     IMPRESSION DES DIAGNOSTICS
!======================================================================
!       if(my_proc .eq.0) then
!             call signature(tabl(-2,-2,0),DIM+4,DIM+4,AVG,1)
!             call signature(tabl(-2,-2,1),DIM+4,DIM+4,AVG,2)
!       endif
!       call signature(tabl(-2,-2,2),DIM+4,DIM+4,AVG,3)
#ifdef MPI
      T2=MPI_Wtime()                                      ! end of run
!       print *,'Total Run time = ',T2-T1
      allocate(ALLTIMES(nsteps,total0))
      allocate(NTIMES(nsteps,total_procs))
      TIMES = TIMES * 1000.0  ! convert to milliseconds
      call MPI_gather(TIMES,  nsteps, MPI_REAL, 
     %                NTIMES, nsteps, MPI_REAL, 
     %                0, OWN_COMM_WORLD, err)
      call MPI_allgather(TIMES,    nsteps, MPI_REAL, 
     %                ALLTIMES, nsteps, MPI_REAL, 
     %                MPI_COMM_WORLD, err)                  ! get times from all cores, all nodes
      if (my_proc == 0) then
	print *,'node time(sec) =',T2-T1,', ',
     %          'node minimum(ms) =',minval(NTIMES(1:nsteps:2,:)),
     %                               minval(NTIMES(2:nsteps:2,:))
	tmin = minval(ALLTIMES(1:nsteps:2,:))
	do j = 1, total_procs
	  do i = 1, nsteps, 2
	    NTIMES(i,j) = min(99.0 ,(NTIMES(i,j) - tmin)/tmin*100.0)
	  enddo
	enddo
	tmin = minval(ALLTIMES(2:nsteps:2,:))
	do j = 1, total_procs
	  do i = 2, nsteps, 2
	    NTIMES(i,j) = min(99.0 ,(NTIMES(i,j) - tmin)/tmin*100.0)
	  enddo
	enddo
	print *,'node jitter(%) =',maxval(NTIMES(1:nsteps:2,:)),
     %                             maxval(NTIMES(2:nsteps:2,:))
	open(unit=11,file=trim(host_128)//'.euler',form='FORMATTED')
	do j = 1, total_procs
	  write(11,11)nint(NTIMES(1:nsteps:2,j))
	enddo
	close(unit=11)
	open(unit=11,file=trim(host_128)//'.leapfrog',form='FORMATTED')
	do j = 1, total_procs
	  write(11,11)nint(NTIMES(2:nsteps:2,j))
	enddo
	close(unit=11)
      endif
11    format(1000I3)
!       goto 999
      if (pe0 == 0) then
      print *,'global min(ms) =',minval(ALLTIMES(1:nsteps:2,:)),','
     %                          ,minval(ALLTIMES(2:nsteps:2,:))
      print *,'global max(ms) =',maxval(ALLTIMES(1:nsteps:2,:)),','
     %                          ,maxval(ALLTIMES(2:nsteps:2,:))
      call analyze2d(hosts_8, ALLTIMES(1:nsteps:2,1),
     %               nsteps/2,1,1)                             ! node 0, PE 0, odd
      call analyze2d(hosts_8, ALLTIMES(1:nsteps:2,:),
     %               nsteps/2,total_procs,total0/total_procs)  ! all nodes, all PEs on node, odd
      call analyze2d(hosts_8,ALLTIMES(2:nsteps:2,1),
     %               nsteps/2,1,1)                             ! node 0, PE 0, even
      call analyze2d(hosts_8, ALLTIMES(2:nsteps:2,:),
     %               nsteps/2,total_procs,total0/total_procs)  ! all nodes, all PEs on node, even
!       call analyze2d(hosts_8,ALLTIMES(1:nsteps,1),
!      %               nsteps,1,1)
!       call analyze2d(hosts_8,ALLTIMES(1:nsteps,1),
!      %               nsteps,1,2)
!       hosts_8 = 'Global'
      endif
      goto 999
      call MPI_allreduce(TIMES, ALLTIMES(:,1), nsteps+1, 
     %     MPI_REAL, MPI_MAX, OWN_COMM_WORLD, err)
      call MPI_allreduce(TIMES, ALLTIMES(:,2), nsteps+1, 
     %     MPI_REAL, MPI_MIN, OWN_COMM_WORLD, err)
      if(my_proc .eq. 0) then
        tmax = maxval(ALLTIMES(1:nsteps:2,1))
        tmin = minval(ALLTIMES(1:nsteps:2,2))
        tjit = (tmax-tmin)/tmin*100
        write(6,22)host_128(1:8)//'(odd) : min, max, jitter% =',
     %            tmin*1000,tmax*1000,min(tjit,999.0)
!         call analyze(host_128(1:8)//': Real Time (odd)   =',
!      %               TIMES(1:nsteps:2)*1000.0,nsteps/2)
      endif
      call MPI_barrier(MPI_COMM_WORLD,err)
      if(my_proc .eq. 0) then
        tmax = maxval(ALLTIMES(2:nsteps:2,1))
        tmin = minval(ALLTIMES(2:nsteps:2,2))
        tjit = (tmax-tmin)/tmin*100
        write(6,22)host_128(1:8)//'(even): min, max, jitter% =',
     %            tmin*1000,tmax*1000,min(tjit,999.0)
!         call analyze(host_128(1:8)//': Real Time (even)  =',
!      %               TIMES(2:nsteps:2)*1000.0,nsteps/2)
      endif
      call MPI_barrier(MPI_COMM_WORLD,err)
2     format(A,F6.3,a,4f7.4)

!       allocate(ALLTIMES(0:nsteps,total_procs))
      call MPI_allreduce(TIMES, ALLTIMES(:,1), nsteps+1, 
     %     MPI_REAL, MPI_MAX, MPI_COMM_WORLD, err)
      call MPI_allreduce(TIMES, ALLTIMES(:,2), nsteps+1, 
     %     MPI_REAL, MPI_MIN, MPI_COMM_WORLD, err)
!       call MPI_allgather(TIMES,nsteps+1,MPI_REAL,
!      %                   ALLTIMES,nsteps+1,MPI_REAL,
!      %                   OWN_COMM_WORLD,err)
      if(pe0 .eq. 0) then
!         do i = 1, nsteps
!           TIMES(i) = maxval(ALLTIMES(i,:)) - minval(ALLTIMES(i,:))
!         enddo
!         call analyze(host_128(1:8)//':    Node Time Diff =',
!      %               TIMES(1:nsteps)*1000.0,nsteps)
        tmax = maxval(ALLTIMES(1:nsteps:2,1))
        tmin = minval(ALLTIMES(1:nsteps:2,2))
        tjit = (tmax-tmin)/tmin*100
        write(6,22)'summary(odd) : min, max, jitter% =',
     %            tmin*1000,tmax*1000,min(tjit,999.0)
        tj = 0
        do i = 1,nsteps,2
          tjit = (ALLTIMES(i,1) - ALLTIMES(i,2)) / ALLTIMES(i,2) * 100
          tjit = min(20.0,tjit)
          tj(nint(tjit)) = tj(nint(tjit)) + 1
        enddo
        write(6,33)'tj(odd)  =',tj(0:20),sum(tj(0:20))
        tmax = maxval(ALLTIMES(2:nsteps:2,1))
        tmin = minval(ALLTIMES(2:nsteps:2,2))
        tjit = (tmax-tmin)/tmin*100
        write(6,22)'summary(even): min, max, jitter% =',
     %            tmin*1000,tmax*1000,min(tjit,999.0)
        tj = 0
        do i = 2,nsteps,2
          tjit = (ALLTIMES(i,1) - ALLTIMES(i,2)) / ALLTIMES(i,2) * 100
          tjit = min(20.0,tjit)
          tj(nint(tjit)) = tj(nint(tjit)) + 1
        enddo
        write(6,33)'tj(even) =',tj(0:20),sum(tj(0:20))
        write(6,33)'tj(zzz)   ',(i,i=0,20)
22    format(A,10F6.1)
33    format(A,25I4)
      endif
      goto 999

      call MPI_barrier(MPI_COMM_WORLD,err)
      if(my_proc .eq. 0) then
        call analyze(host_128(1:8)//': Barrier Time=',
     %               BARRIER(1:nsteps)*1000.0,nsteps)
      endif
!       TMIN=BARRIER(1)
!       TMAX=TMIN
!       TSQR=TMAX*TMAX
!       TSUM=TMIN
!       do i=2,nsteps
!         TMIN=min(TMIN,BARRIER(i))
!         TMAX=max(TMAX,BARRIER(i))
!         TSQR=TSQR+(BARRIER(i))*(BARRIER(i))
!         TSUM=TSUM+BARRIER(i)
!       enddo
!       TAVG=(TSUM)/nsteps
!       TSQR=sqrt(TSQR/nsteps-TAVG*TAVG)
!       if(my_proc .eq. 0) print 2,'Barrier_Time=',T2-T1,
!      %   ' avg,dev,min,max=',TAVG,TSQR,TMIN,TMAX
      call MPI_allgather(BARRIER,nsteps+1,MPI_REAL,
     %                   ALLTIMES,nsteps+1,MPI_REAL,
     %                   OWN_COMM_WORLD,err)
      call MPI_barrier(MPI_COMM_WORLD,err)
      if(my_proc .eq. 0) then
        do i = 1, nsteps
          TIMES(i) = maxval(ALLTIMES(i,:)) - minval(ALLTIMES(i,:))
        enddo
        call analyze(host_128(1:8)//': Cross Node Barrier Time=',
     %               TIMES(1:nsteps)*1000.0,nsteps)
!         TMIN=TIMES(1)
!         TMAX=TMIN
!         TSQR=TMAX*TMAX
!         TSUM=TMIN
!         do i=2,nsteps
!           TMIN=min(TMIN,TIMES(i))
!           TMAX=max(TMAX,TIMES(i))
!           TSQR=TSQR+(TIMES(i))*(TIMES(i))
!           TSUM=TSUM+TIMES(i)
!         enddo
!         print *,'===================================='
!         print 2,'Cross_Node_Barrier_Time    =',T2-T1,
!      %    ' avg,dev,min,max=',TAVG,TSQR,TMIN,TMAX
      endif
999   call mpi_finalize(err)
#endif
      stop
      end
      subroutine step_euler(old_old_tabl, old_tabl, tabl, iter, t, N)
      real  R8DIM
      integer iter,var,N
      real t
      real         tabl(-2:N+1,-2:N+1,0:2)
      real     old_tabl(-2:N+1,-2:N+1,0:2)
      real old_old_tabl(-2:N+1,-2:N+1,0:2)
      common /tables/ cosj, cosi,sini,sinj,deltat
      real, dimension(:), pointer :: cosj, cosi,sini,sinj
      real deltat
      real sint
      integer omp_get_thread_num,inum
      external omp_get_thread_num
      R8DIM=N
      if (iter.ge.0) then
         sint = sin(t)
!$OMP PARALLEL
!$OMP DO PRIVATE(i,j)
         do j=0,N-1
            do i=0,N-1
c     CALCUL DES DERIVEES
               tabl(i,j,0) = old_tabl(i,j,0)+DELTAT*(-old_tabl(i,j,0)*
     %               (R8DIM*(.08333333333333333333*old_tabl(i-2,j,0)-.66
     %666666666666666666*old_tabl(i-1,j,0) +         .666666666666666666
     %66*old_tabl(i+1,j,0)-.08333333333333333333*old_tabl(i+2,j,0)))
     %              -old_tabl(i,j,1)* (R8DIM*(.08333333333333333333*old_
     %tabl(i,j-2,0)-.66666666666666666666*old_tabl(i,j-1,0) +         .6
     %6666666666666666666*old_tabl(i,j+1,0)-.08333333333333333333*old_ta
     %bl(i,j+2,0)))
     %              -9.81* (R8DIM*(.08333333333333333333*old_tabl(i-2,j,
     %2)-.66666666666666666666*old_tabl(i-1,j,2) +         .666666666666
     %66666666*old_tabl(i+1,j,2)-.08333333333333333333*old_tabl(i+2,j,2)
     %)) + .001* ( (262144.0*(-.08333333333333333333*old_tabl(i-2,j,0)+1
     %.33333333333333333333*old_tabl(i-1,j,0) -         2.5*old_tabl(i,j
     %,0) + 1.33333333333333333333*old_tabl(i+1,j,0)-.083333333333333333
     %33*old_tabl(i+2,j,0))) +  (262144.0*(-.08333333333333333333*old_ta
     %bl(i,j-2,0)+1.33333333333333333333*old_tabl(i,j-1,0) -         2.5
     %*old_tabl(i,j,0) + 1.33333333333333333333*old_tabl(i,j+1,0)-.08333
     %333333333333333*old_tabl(i,j+2,0))))
     %              + .01*cosj(j)*sini(i)*sint)
            enddo
            do i=0,N-1
               tabl(i,j,1) = old_tabl(i,j,1)+DELTAT*(-old_tabl(i,j,1)*
     %               (R8DIM*(.08333333333333333333*old_tabl(i-2,j,1)-.66
     %666666666666666666*old_tabl(i-1,j,1) +         .666666666666666666
     %66*old_tabl(i+1,j,1)-.08333333333333333333*old_tabl(i+2,j,1)))
     %              -old_tabl(i,j,1)* (R8DIM*(.08333333333333333333*old_
     %tabl(i,j-2,1)-.66666666666666666666*old_tabl(i,j-1,1) +         .6
     %6666666666666666666*old_tabl(i,j+1,1)-.08333333333333333333*old_ta
     %bl(i,j+2,1)))
     %              -9.81* (R8DIM*(.08333333333333333333*old_tabl(i,j-2,
     %2)-.66666666666666666666*old_tabl(i,j-1,2) +         .666666666666
     %66666666*old_tabl(i,j+1,2)-.08333333333333333333*old_tabl(i,j+2,2)
     %)) + .001* ( (262144.0*(-.08333333333333333333*old_tabl(i-2,j,1)+1
     %.33333333333333333333*old_tabl(i-1,j,1) -         2.5*old_tabl(i,j
     %,1) + 1.33333333333333333333*old_tabl(i+1,j,1)-.083333333333333333
     %33*old_tabl(i+2,j,1))) +  (262144.0*(-.08333333333333333333*old_ta
     %bl(i,j-2,1)+1.33333333333333333333*old_tabl(i,j-1,1) -         2.5
     %*old_tabl(i,j,1) + 1.33333333333333333333*old_tabl(i,j+1,1)-.08333
     %333333333333333*old_tabl(i,j+2,1))))
     %              - .01*sinj(j)*cosi(i)*sint)
            enddo
            do i=0,N-1
               tabl(i,j,2) = old_tabl(i,j,2)+
     %             DELTAT*(- (R8DIM*(.08333333333333333333* (old_tabl(i-
     %2,j,2)*old_tabl(i-2,j,0))-.66666666666666666666* (old_tabl(i-1,j,2
     %)*old_tabl(i-1,j,0)) +         .66666666666666666666* (old_tabl(i+
     %1,j,2)*old_tabl(i+1,j,0))-.08333333333333333333* (old_tabl(i+2,j,2
     %)*old_tabl(i+2,j,0))))- (R8DIM*(.08333333333333333333* (old_tabl(i
     %,j-2,2)*old_tabl(i,j-2,1))-.66666666666666666666* (old_tabl(i,j-1,
     %2)*old_tabl(i,j-1,1)) +         .66666666666666666666* (old_tabl(i
     %,j+1,2)*old_tabl(i,j+1,1))-.08333333333333333333* (old_tabl(i,j+2,
     %2)*old_tabl(i,j+2,1)))))
            enddo
         enddo
!$OMP END DO
!$OMP END PARALLEL
      endif
      do var=0,2
         do j=0,N-1
            do i=-2,-1
               tabl(i,j,var) = tabl(i+N,j,var)
               tabl(i+N+2,j,var) = tabl(i+2,j,var)
            enddo
         enddo
      enddo
      do var=0,2
         do j=-2,-1
            do i=0,N-1
               tabl(i,j,var) = tabl(i,j+N,var)
               tabl(i,j+N+2,var) = tabl(i,j+2,var)
            enddo
         enddo
      enddo
      end
      subroutine step_leapfrog(old_old_tabl, old_tabl, tabl, iter, t,N)
      real  R8DIM
      integer iter,var,N
      real t
      real         tabl(-2:N+1,-2:N+1,0:2)
      real     old_tabl(-2:N+1,-2:N+1,0:2)
      real old_old_tabl(-2:N+1,-2:N+1,0:2)
      common /tables/ cosj, cosi,sini,sinj,deltat
      real, dimension(:), pointer :: cosj, cosi,sini,sinj
      real deltat
      real sint
      R8DIM=N
      if (iter.ge.0) then
         sint = sin(t)
!$OMP PARALLEL
!$OMP DO PRIVATE(i,j)
         do j=0,N-1
            do i=0,N-1
c     CALCUL DES DERIVEES
              tabl(i,j,0) = old_old_tabl(i,j,0)+2*DELTAT*(-old_tabl(i,j,
     %0)*
     %               (R8DIM*(.08333333333333333333*old_tabl(i-2,j,0)-.66
     %666666666666666666*old_tabl(i-1,j,0) +         .666666666666666666
     %66*old_tabl(i+1,j,0)-.08333333333333333333*old_tabl(i+2,j,0)))
     %              -old_tabl(i,j,1)* (R8DIM*(.08333333333333333333*old_
     %tabl(i,j-2,0)-.66666666666666666666*old_tabl(i,j-1,0) +         .6
     %6666666666666666666*old_tabl(i,j+1,0)-.08333333333333333333*old_ta
     %bl(i,j+2,0)))
     %              -9.81* (R8DIM*(.08333333333333333333*old_tabl(i-2,j,
     %2)-.66666666666666666666*old_tabl(i-1,j,2) +         .666666666666
     %66666666*old_tabl(i+1,j,2)-.08333333333333333333*old_tabl(i+2,j,2)
     %)) + .001* ( (262144.0*(-.08333333333333333333*old_tabl(i-2,j,0)+1
     %.33333333333333333333*old_tabl(i-1,j,0) -         2.5*old_tabl(i,j
     %,0) + 1.33333333333333333333*old_tabl(i+1,j,0)-.083333333333333333
     %33*old_tabl(i+2,j,0))) +  (262144.0*(-.08333333333333333333*old_ta
     %bl(i,j-2,0)+1.33333333333333333333*old_tabl(i,j-1,0) -         2.5
     %*old_tabl(i,j,0) + 1.33333333333333333333*old_tabl(i,j+1,0)-.08333
     %333333333333333*old_tabl(i,j+2,0))))
     %              + .01*cosj(j)*sini(i)*sint)
            enddo
            do i=0,N-1
              tabl(i,j,1) = old_old_tabl(i,j,1)+2*DELTAT*(-old_tabl(i,j,
     %1)*
     %               (R8DIM*(.08333333333333333333*old_tabl(i-2,j,1)-.66
     %666666666666666666*old_tabl(i-1,j,1) +         .666666666666666666
     %66*old_tabl(i+1,j,1)-.08333333333333333333*old_tabl(i+2,j,1)))
     %              -old_tabl(i,j,1)* (R8DIM*(.08333333333333333333*old_
     %tabl(i,j-2,1)-.66666666666666666666*old_tabl(i,j-1,1) +         .6
     %6666666666666666666*old_tabl(i,j+1,1)-.08333333333333333333*old_ta
     %bl(i,j+2,1)))
     %              -9.81* (R8DIM*(.08333333333333333333*old_tabl(i,j-2,
     %2)-.66666666666666666666*old_tabl(i,j-1,2) +         .666666666666
     %66666666*old_tabl(i,j+1,2)-.08333333333333333333*old_tabl(i,j+2,2)
     %)) + .001* ( (262144.0*(-.08333333333333333333*old_tabl(i-2,j,1)+1
     %.33333333333333333333*old_tabl(i-1,j,1) -         2.5*old_tabl(i,j
     %,1) + 1.33333333333333333333*old_tabl(i+1,j,1)-.083333333333333333
     %33*old_tabl(i+2,j,1))) +  (262144.0*(-.08333333333333333333*old_ta
     %bl(i,j-2,1)+1.33333333333333333333*old_tabl(i,j-1,1) -         2.5
     %*old_tabl(i,j,1) + 1.33333333333333333333*old_tabl(i,j+1,1)-.08333
     %333333333333333*old_tabl(i,j+2,1))))
     %              - .01*sinj(j)*cosi(i)*sint)
            enddo
            do i=0,N-1
               tabl(i,j,2) = old_old_tabl(i,j,2)+
     %             2*DELTAT*(- (R8DIM*(.08333333333333333333* (old_tabl(
     %i-2,j,2)*old_tabl(i-2,j,0))-.66666666666666666666* (old_tabl(i-1,j
     %,2)*old_tabl(i-1,j,0)) +         .66666666666666666666* (old_tabl(
     %i+1,j,2)*old_tabl(i+1,j,0))-.08333333333333333333* (old_tabl(i+2,j
     %,2)*old_tabl(i+2,j,0))))- (R8DIM*(.08333333333333333333* (old_tabl
     %(i,j-2,2)*old_tabl(i,j-2,1))-.66666666666666666666* (old_tabl(i,j-
     %1,2)*old_tabl(i,j-1,1)) +         .66666666666666666666* (old_tabl
     %(i,j+1,2)*old_tabl(i,j+1,1))-.08333333333333333333* (old_tabl(i,j+
     %2,2)*old_tabl(i,j+2,1)))))
            enddo
         enddo
!$OMP END DO
!$OMP END PARALLEL
      endif
      do var=0,2
         do j=0,N-1
            do i=-2,-1
               tabl(i,j,var) = tabl(i+N,j,var)
               tabl(i+N+2,j,var) = tabl(i+2,j,var)
            enddo
         enddo
      enddo
      do var=0,2
         do j=-2,-1
            do i=0,N-1
               tabl(i,j,var) = tabl(i,j+N,var)
               tabl(i,j+N+2,var) = tabl(i,j+2,var)
            enddo
         enddo
      enddo
      end
      subroutine signature8(Z,ni,nj,AVG)
      real AVG
      integer ni,nj
      real Z(ni,nj)
      real  sum
      real  zmin,zmax
      sum=0
      zmin=z(1,1)
      zmax=z(1,1)
      do j=1,nj
      do i=1,ni
         sum=sum+z(i,j)
         zmin=min(zmin,z(i,j))
         zmax=max(zmax,z(i,j))
      enddo
      enddo
      AVG=sum/(ni*nj)
      print *,'Signature =',zmin,sum/(ni*nj),zmax
      return
      end
      subroutine signature(Z,ni,nj,AVG,ix)
      real AVG
      integer ni,nj,ix
      real Z(ni,nj)
      real  sum
      real zmin,zmax
      sum=0
      zmin=z(1,1)
      zmax=z(1,1)
      do j=1,nj
      do i=1,ni
         sum=sum+z(i,j)
         zmin=min(zmin,z(i,j))
         zmax=max(zmax,z(i,j))
      enddo
      enddo
      AVG=sum/(ni*nj)
      return
      write(6,'(a,i1,a,3f15.10)')
     %     'Min,Avg,Max(',ix,') =',zmin,sum/(ni*nj),zmax
      return
      end
	integer function time_base()
	time_base=0
	return
	end
      subroutine pseudo_halo(buffer,n1,n2,nrep,pe_to,pe_from)
#ifdef MPI
      include 'mpif.h'
      common /mpi_world/ OWN_COMM_WORLD
      integer :: OWN_COMM_WORLD
      integer n1,n2,nrep,pe_from,pe_to
      real buffer(n1,n2)
      integer STATUS(MPI_STATUS_SIZE),err,i
      logical, save :: first=.true.

!      if(pe_to .eq. pe_from) return
!       if ( first) print *,'pseudo_halo'
      first=.false.
      do i=1,nrep
         call mpi_sendrecv(buffer(1,1),n1,MPI_REAL,pe_to,0,
     %                     buffer(1,2),n1,MPI_REAL,pe_from,0,
     %                     OWN_COMM_WORLD,STATUS,err)
      enddo
#endif
      end

      subroutine analyze2d(msg,t,nt,ns,nn)
        implicit none
        integer, intent(IN) :: nt, ns, nn
        real*4, dimension(nt,ns,nn), intent(IN) :: t
        character(len=*), dimension(ns,nn) :: msg
        integer :: i, j, k
        real*4 :: tmax, tmin
        real*8 :: tsum, tsqr, tavg, tdev
        real*4 :: tpct, mpct
        integer, dimension(0:20) :: tp
        integer :: ip

        tmax = maxval(t)
        tmin = minval(t)
        mpct = 0
        print 2,'Analyzing',nt,' time frames on',
     %           nn,' node(s) with',ns,' cores'
        do k = 1, nn
	  tp = 0
	  do j = 1, ns
	  do i = 1, nt
	    tpct = 0
	    if(tmin > 0) tpct = ( (t(i,j,k)-tmin)/tmin )*100.0
	    mpct = max(tpct,mpct)
	    ip = min(20, nint(tpct))
	    tp(ip) = tp(ip) + 1
	  enddo
	  enddo
	  ip = ns*nt
	  write(6,1)trim(msg(1,k)),' min,max,maxjit%,jt',
     %            tmin,tmax,mpct,tp,ip
        enddo
1       format(A10,A,3F8.1,21I6,I8)
2       format(A,3(I6,A))
      end subroutine analyze2d

      subroutine analyze(msg,t,n)
        implicit none
        integer, intent(IN) :: n
        real*4, dimension(n), intent(IN) :: t
        character(len=*) :: msg
        integer :: i
        real*4 :: tmax, tmin
        real*8 :: tsum, tsqr, tavg, tdev
        real*4 :: tpct
        integer, dimension(0:20) :: tp

        tmax = t(1)
        tmin = t(1)
        tsum = t(1)
        tsqr = t(1)*t(1)
        do i = 2, n
          tmax = max(tmax,t(i))
          tmin = min(tmin,t(i))
          tsum = tsum + t(i)
          tsqr = tsqr + t(i)*t(i)
        enddo
        tavg = tsum / n
        tdev = sqrt(tsqr/n-tavg*tavg)
        tpct = 0
        if(tmin > 0) tpct = ( (tmax-tmin)/tmin )*100
        write(6,1)trim(msg),' min,max,avg,dev,jit% ',
     %           tmin,tmax,tavg,tdev,min(tpct,999.9)
        call flush(6)
1       format(A,A,20F8.1)
      end subroutine analyze
