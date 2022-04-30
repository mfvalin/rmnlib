! Copyright (C) 2022  Environnement et Changement climatique Canada
!
! This is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation,
! version 2.1 of the License.
!
! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! Author:
!     M. Valin,   Recherche en Prevision Numerique, 2022

! modeled after MPI_Win_allocate_shared
! no MPI window id is returned
! no disp_unit is used
! no info is used
! size : used only by process with rank == 0, size shared memory segment in bytes
! base : type(C_PTR) is returned as in 2008 syntax instead of INTEGER(KIND=MPI_ADDRESS_KIND)
! comm, ierr : same as MPI_Win_allocate_shared
! It is the user’s responsibility to ensure that the communicator comm represents a group of processes 
! that can create a shared memory segment that can be accessed by all processes in the group
! the allocation is handled in such a way that the shared memory segment
! will be automatically freed when the last process terminates
subroutine MPI_Nowin_allocate_shared(size, comm, base, ierr)
  use mpi
  use ISO_C_BINDING
  implicit none
#include <iso_c_binding_extras.hf>
  integer(C_SIZE_T), INTENT(IN) :: size      ! size of shared memory segment in bytes
  integer, intent(IN)           :: comm      ! communicator (all PEs MUST BE in same SMP node)
  type(C_PTR), INTENT(OUT)      :: base      ! base local address of shared memory segment
  integer, intent(OUT)          :: ierr      ! error return (MPI_SUCCESS
  integer(C_INT) :: my_rank, shmid, status
  type(shmid_ds) :: ds

  base = C_NULL_PTR
  status = -1
  shmid = -1
  call MPI_Comm_rank(comm, my_rank, ierr)
  if(ierr .ne. MPI_SUCCESS) return

  if(my_rank == 0) then                                    ! rank 0 in SMP node creates the shared memory segment
    shmid = c_shmget(IPC_PRIVATE, size, IPC_CREAT+(6*64))  ! create shared memory segment
    if(shmid .ne. -1) then                                 ! no creation error
      base   = c_shmat(shmid, C_NULL_PTR, 0)               ! attach to segment
      status = c_shmctl(shmid, IPC_RMID, ds)               ! mark segment for deletion
    endif
  endif

  call MPI_Bcast(shmid, 1, MPI_INTEGER, 0, comm, ierr)     ! broadcast shared memory segment id
  if(shmid == -1) ierr = MPI_ERROR                         ! bad segment id
  if(ierr .ne. MPI_SUCCESS) return

  if(my_rank .ne. 0 ) base = c_shmat(shmid, C_NULL_PTR, 0) ! rank 0 already attached

  if(.not. C_ASSOCIATED(base)) then                        ! error atttaching to segment
    ierr = MPI_ERROR
  else
    ierr = MPI_SUCCESS
  endif
  return
end subroutine MPI_Nowin_allocate_shared
