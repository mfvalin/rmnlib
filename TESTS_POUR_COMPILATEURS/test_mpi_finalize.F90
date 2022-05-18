program test_mpi_finalize
  use ISO_C_BINDING
  use mpi
  implicit none
  interface
    subroutine MPI_Soft_finalize_prep(comm, rank, ierr)
      implicit none
      integer, intent(IN)    :: comm
      integer, intent(OUT)   :: rank
      integer, intent(INOUT) :: ierr
    end subroutine MPI_Soft_finalize_prep
    subroutine MPI_Soft_finalize(ierr)
      implicit none
      integer, intent(INOUT) :: ierr
    end subroutine MPI_Soft_finalize
  end interface

  integer :: my_rank, my_size, ierr, junk, node_rank

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, my_size, ierr)
  write(0,*)'MPI global process',my_rank+1,' of',my_size

  call MPI_Soft_finalize_prep(MPI_COMM_WORLD, node_rank, ierr)

  call MPI_Barrier(MPI_COMM_WORLD, ierr)

  if(my_rank == 0) read(5,*) junk  
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  if(my_rank == 0) read(5,*) junk

  call MPI_Soft_finalize(ierr)

end
