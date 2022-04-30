! module nowin_shared_allocate
!   use ISO_C_BINDING
!   use mpi
!   implicit none
! #if defined(STAND_ALONE)
!   integer, parameter :: C_KEY_T = C_INT32_T
!   integer, parameter :: SIZEOF_SHMID_DS = 112
!   integer, parameter :: IPC_PRIVATE = 0
!   integer, parameter :: IPC_CREAT = 512
!   integer, parameter :: IPC_EXCL = 1024
!   integer, parameter :: IPC_RMID = 0
!   integer, parameter :: C_USECONDS_T = C_INT32_T
!   type, bind(C) :: shmid_ds
!     integer(C_INT), dimension(SIZEOF_SHMID_DS/4) :: x
!   end type
!   interface
!     function c_usleep(useconds) result(ok) BIND(C,name='usleep')  ! sleep in microseconds
!       import :: C_INT, C_USECONDS_T
!       integer(C_USECONDS_T), intent(IN), value :: useconds
!       integer(C_INT) :: ok
!     end function c_usleep
!     function c_shmget(key, siz, flags) result(memid) bind(C,name='shmget')
!       import :: C_INT, C_KEY_T, C_SIZE_T
!       integer(C_KEY_T), intent(IN),  value :: key
!       integer(C_SIZE_T), intent(IN), value :: siz
!       integer(C_INT), intent(IN),    value :: flags
!       integer(C_INT) :: memid
!     end function c_shmget
!     function c_shmat(memid, shmaddr, flags) result(addr) bind(C,name='shmat')
!       import :: C_INT, C_PTR
!       integer(C_INT), intent(IN),    value :: memid
!       type(C_PTR), intent(IN),       value :: shmaddr
!       integer(C_INT), intent(IN),    value :: flags
!       type(C_PTR) :: addr
!     end function c_shmat
!     function c_shmdt(shmaddr) result(ok) bind(C,name='shmdt')
!       import :: C_INT, C_PTR
!       type(C_PTR), intent(IN),       value :: shmaddr
!       integer(C_INT) :: ok
!     end function c_shmdt
!     function c_shmctl(memid, cmd, buf) result(ok) bind(C,name='shmctl')
!       import :: C_INT, shmid_ds
!       integer(C_INT), intent(IN),    value :: memid
!       integer(C_INT), intent(IN),    value :: cmd
!       type(shmid_ds), intent(OUT)          :: buf
!       integer(C_INT) :: ok
!     end function c_shmctl
!   end interface
! #else
! #include <iso_c_binding_extras.hf>
! #endif
! contains
! end module

program test_mpi_finalize
  use ISO_C_BINDING
  use mpi
  implicit none
#include <iso_c_binding_extras.hf>
  interface
    subroutine MPI_Nowin_allocate_shared(size, comm, base, ierr)
      import :: C_SIZE_T, C_PTR
      implicit none
      integer(C_SIZE_T), INTENT(IN) :: size      ! size of shared memory segment in bytes
      integer, intent(IN)           :: comm      ! communicator (all PEs MUST BE in same SMP node)
      type(C_PTR), INTENT(OUT)      :: base      ! base local address of shared memory segment
      integer, intent(OUT)          :: ierr      ! error return (MPI_SUCCESS
    end subroutine MPI_Nowin_allocate_shared
  end interface

  integer :: my_rank, my_size, ierr, junk
  INTEGER :: window, node_comm
  integer(C_INT32_T), dimension(:), pointer, volatile :: flag
  type(C_PTR) :: cflag
  integer :: i, t, ok
  integer(C_INT) :: shmid, status
  integer(C_SIZE_T) :: shmsize

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, my_size, ierr)
  write(0,*)'MPI global process',my_rank+1,' of',my_size
  call MPI_COMM_SPLIT_TYPE(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, my_rank, MPI_INFO_NULL, node_comm, ierr)
  call MPI_Comm_rank(node_comm, my_rank, ierr)
  call MPI_Comm_size(node_comm, my_size, ierr)

  shmsize = ((my_size + 4095) / 4096) * 4096 * 4  ! multiple of 4096 integers
  call MPI_Nowin_allocate_shared(shmsize, node_comm, cflag, ierr)

  call c_f_pointer(cflag, flag, [shmsize/4] )
  flag(my_rank+1) = my_rank+1

  call MPI_Barrier(MPI_COMM_WORLD, ierr)

  write(0,*)'MPI node process',my_rank+1,' of',my_size,' flag =',flag(my_rank+1)

  if(my_rank == 0) read(5,*) junk
  call MPI_Barrier(MPI_COMM_WORLD, ierr)

  if(my_rank == 0) read(5,*) junk

  if(my_rank == 0) then                  ! rank 0 starts the merry go round
    flag(1) = 0
  else
    do while(flag(my_rank) .ne. 0)       ! wait for rank -1 to be ready to finalize
      ok = c_usleep(500000)               ! 500 msec
    enddo
    flag(my_rank+1) = 0                  ! tell rank + 1 that we are ready to finalize
  endif

  do while(flag(my_size) .ne. 0)         ! wait for last rank to be ready before calling finalize
    ok = c_usleep(10000)
  enddo

  write(0,*)'process',my_rank+1,' mpi_finalize'
1 call mpi_finalize(ierr)
end
