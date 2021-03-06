program tsoltri
  use ISO_C_BINDING
  implicit none
  include 'mpif.h'

  integer, parameter :: NP = 2000
  integer, parameter :: NI = 1000
  integer, parameter :: NREP = 20
  real, dimension(NP,NI) :: A, B, C, R, RHS
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

  A = 1.0
  B = .90
  c = .80
  RHS = 1.5

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

  do irep = 1 , NREP
    call mpi_barrier(MPI_COMM_WORLD,ierr)
    t0 = MPI_wtime()
    do i = 1, 40
      call soltri_m ( r, rhs, a, b, c, np, ni)
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
      call analyze('soltri',ttmp2(i,:),csiz)
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
