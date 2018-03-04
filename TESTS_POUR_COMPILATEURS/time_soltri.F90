program tsoltri
  implicit none
  include 'mpif.h'

  integer, parameter :: NP = 2000
  integer, parameter :: NI = 1000
  real, dimension(NP,NI) :: A, B, C, R, RHS
  integer :: irep, ierr, rank, csiz, i
  real*8, dimension(:), pointer :: ttmp
  real*8 :: t0, t1, ttot

  A = 1.0
  B = .90
  c = .80
  RHS = 1.5

  call mpi_init(ierr)
  call mpi_comm_rank(MPI_COMM_WORLD,rank,ierr)
  call mpi_comm_size(MPI_COMM_WORLD,csiz,ierr)
  allocate(ttmp(csiz))

  do irep = 1 , 10
    call mpi_barrier(MPI_COMM_WORLD,ierr)
    t0 = MPI_wtime()
    do i = 1, 100
      call soltri_m ( r, rhs, a, b, c, np, ni)
    enddo
    t1 = MPI_wtime() - t0
    call mpi_barrier(MPI_COMM_WORLD,ierr)
    call mpi_gather(t1,1,MPI_DOUBLE_PRECISION,ttmp,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
!     tavg(irep) = sum(ttmp(1:csiz))/csiz
!     tmax(irep) = maxval(ttmp(1:csiz))
!     tmin(irep) = minval(ttmp(1:csiz))
    if(rank == 0) call analyze('soltri',ttmp*1000,csiz)
  enddo
  if(rank == 0) then
!     print 1,tavg(1:10)
!     print 1,tmax(1:10) - tmin(1:10)
!     print 1,tmin(1:10)
!     print 1,tmax(1:10)
  endif

  call mpi_finalize(ierr)

1 format(20f6.3)
end
subroutine analyze(msg,t,n)
  implicit none
  integer, intent(IN) :: n
  real*8, dimension(n), intent(IN) :: t
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
1 format(A,A,20F6.1)
end subroutine analyze
