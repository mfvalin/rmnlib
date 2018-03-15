program tsoltri
  use ISO_C_BINDING
  implicit none
  include 'mpif.h'

  integer, parameter :: NREP = 20
  interface
    subroutine intrp_biquint_yx(f, r, ni, ninj, nk, np, x, y) bind(C,name='intrp_biquint_yx')         !InTf!
      import :: C_INT, C_FLOAT, C_DOUBLE                                                          !InTf!
      real(C_FLOAT), dimension(*), intent(IN) :: f                                                !InTf!
      real(C_FLOAT), dimension(*), intent(OUT) :: r                                               !InTf!
      real(C_DOUBLE), intent(IN), value :: x, y                                                   !InTf!
      integer(C_INT), intent(IN), value :: ni, ninj, nk, np                                       !InTf!
    end subroutine intrp_biquint_yx                                                                 !InTf!
    subroutine intrp_biquint_yx_mono(f, r, ni, ninj, nk, np, x, y) bind(C,name='intrp_biquint_yx_mono') !InTf!
      import :: C_INT, C_FLOAT, C_DOUBLE                                                          !InTf!
      real(C_FLOAT), dimension(*), intent(IN) :: f                                                !InTf!
      real(C_FLOAT), dimension(*), intent(OUT) :: r                                               !InTf!
      real(C_DOUBLE), intent(IN), value :: x, y                                                   !InTf!
      integer(C_INT), intent(IN), value :: ni, ninj, nk, np                                       !InTf!
    end subroutine intrp_biquint_yx_mono                                                            !InTf!
  end interface
#define FXY(A,B,C) (1.1*(A)**5 + 1.2*(A)**4 + 1.3*(A)**3 + 1.4*(A)**2 + (A)*1.5 + 2.1*(B)**5 + 2.2*(B)**4 + 2.3*(B)**3 + 2.4*(B)**2 + (B)*2.5 + (C))
  integer, parameter :: RP=2000    ! repeat count
  integer, parameter :: NI=31
  integer, parameter :: NJ=27
  integer, parameter :: NK=41
  integer, parameter :: NP=180
  integer, parameter :: HX=3
  integer, parameter :: HY=3
  integer, parameter :: NR=25
  real(C_FLOAT), dimension(1-HX:NI+HX , 1-HY:NJ+HY , NK) :: f
  real(C_FLOAT), dimension(NP,NK) :: r
  real(C_DOUBLE), dimension(NP) :: x, y
  integer j, k, nidim, ninjdim
  integer :: irep, ierr, rank, csiz, i, host_id, OWN_COMM_WORLD, my_proc, total_procs
  real, dimension(:), pointer :: ttmp1
  real, dimension(:,:), pointer :: ttmp2
  real*8 :: t0, t1, ttot
  integer(C_SIZE_T), parameter :: L128 = 128
  character(len=1), dimension(L128) :: hostname
  character(len=8), dimension(:), pointer :: hosts_8
  character(len=L128) :: host_128
  interface
    integer(C_INT) function f_gethostid()BIND(C,name='gethostid')
      import :: C_INT
    end function f_gethostid
    integer(C_INT) function f_gethostname(name,nchar) BIND(C,name='gethostname')
      import :: C_INT, C_CHAR, C_SIZE_T
      integer(C_SIZE_T), intent(IN), value :: nchar
      character(C_CHAR), dimension(nchar), intent(OUT) :: name
    end function f_gethostname
  end interface

  call mpi_init(ierr)
  call mpi_comm_rank(MPI_COMM_WORLD,rank,ierr)
  call mpi_comm_size(MPI_COMM_WORLD,csiz,ierr)
  host_id = f_gethostid()
  host_128 = ' '
  i = f_gethostname(hostname,L128)
  do i = 1,L128
    if(hostname(i) == achar(0)) exit
    host_128(i:i) = hostname(i)
  enddo
  allocate(hosts_8(csiz))
  call MPI_gather(host_128(1:8), 8, MPI_CHAR, hosts_8, 8, MPI_CHAR, 0, MPI_COMM_WORLD, ierr)
  host_id = abs(host_id)
  call MPI_COMM_SPLIT(MPI_COMM_WORLD, host_id, rank, OWN_COMM_WORLD, ierr)
  T1=MPI_Wtime()                                      ! start of run
  call mpi_comm_rank(OWN_COMM_WORLD,my_proc,ierr)
!       print *,'rank in own world=',my_proc
  call mpi_comm_size(OWN_COMM_WORLD,total_procs,ierr)
  allocate(ttmp1(NREP))
  allocate(ttmp2(NREP,csiz))

  r = 9999.99
  do k = 1 , NK
    do j = 1-HY , NJ+HY
      do i = 1-HX , NI+HX
!        f(i,j,k) = i + j  + k
        f(i,j,k) = FXY(i*1.0 , j*1.0 , k*1.0)
      enddo
    enddo
!    print *,f(1,1,k),f(2,2,k)
  enddo
  do i = 1 , NP
    x(i) = i*ni/np*.9 - .999
    y(i) = i*nj/np*.9 - .999
  enddo
  nidim = NI + 2*HX
  ninjdim = nidim * (NJ + HY*2)
  ttmp1 = 0
  ttmp2 = 0

  do irep = 1 , NREP
    call mpi_barrier(MPI_COMM_WORLD,ierr)
    t0 = MPI_wtime()
    do k = 1,RP
      do i = 1 , NP
        call intrp_biquint_yx( f(1,1,1), r(i,1), nidim, ninjdim, NK, NP, x(i), y(i) )
      enddo
    enddo
    t1 = MPI_wtime() - t0
    ttmp1(irep) = t1
    call mpi_barrier(MPI_COMM_WORLD,ierr)
!     tavg(irep) = sum(ttmp(1:csiz))/csiz
!     tmax(irep) = maxval(ttmp(1:csiz))
!     tmin(irep) = minval(ttmp(1:csiz))
!     if(rank == 0) call analyze('soltri',ttmp*1000,csiz)
  enddo
  ttmp1 = ttmp1 * 1000.0   ! convert to milliseconds
  call mpi_gather(ttmp1,NREP,MPI_REAL,ttmp2,NREP,MPI_REAL,0,MPI_COMM_WORLD,ierr)
  if(rank == 0) then
    do i = 1, NREP
      call analyze('biquint',ttmp2(i,:),csiz)
    enddo
!     print 1,tavg(1:10)
!     print 1,tmax(1:10) - tmin(1:10)
!     print 1,tmin(1:10)
!     print 1,tmax(1:10)
  endif

  call mpi_finalize(ierr)

1 format(20f7.3)
end
subroutine soltri_m ( F_r, F_rhs, F_a, F_b, F_c, np, ni)
  integer, intent(IN) :: np, ni
  real, dimension(np,ni), intent(IN) :: F_a, F_b, F_c, F_rhs
  real, dimension(np,ni), intent(OUT) :: F_r

  integer :: i

  F_r(:,1) = F_rhs(:,1) * F_b(:,1)

  do i = 2, ni
    F_r(:,i) =  F_rhs(:,i) * F_b(:,i) - F_a(:,i)*F_r(:,i-1)
  enddo

  do i = ni-1, 1, -1
    F_r(:,i) =  F_r(:,i) - F_c(:,i)*F_r(:,i+1)
  enddo

  return
end
subroutine analyze(msg,t,n)
  implicit none
  integer, intent(IN) :: n
  real, dimension(n), intent(IN) :: t
  character(len=*) :: msg
  integer :: i
  real*8 :: tmax, tmin, tsum, tsqr, tavg, tdev

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
  write(6,1)trim(msg),' min,max,avg,dev,jit% ',tmin,tmax,tavg,tdev,( (tmax-tmin)/tmin )*100
1 format(A,A,20F8.1)
end subroutine analyze
