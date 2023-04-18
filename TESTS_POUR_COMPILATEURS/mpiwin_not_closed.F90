program test_window
   !! Test whether MPI dies when a window is created but not freed before MPI_Finalize
   use mpi
   use, intrinsic :: iso_fortran_env
   use, intrinsic :: iso_c_binding
   integer, dimension(10) :: window_array
   integer :: myrank, numproc, ierr
   integer :: created_window

   call MPI_Init(ierr)
   call MPI_Comm_size(MPI_COMM_WORLD,numproc,ierr)
   call MPI_Comm_Rank(MPI_COMM_WORLD,myrank,ierr)

   write(0,'("Rank ",I0,"/",I0," initialized")') myrank+1, numproc

   call MPI_Win_Create(window_array, int(10,kind=MPI_ADDRESS_KIND), &
                       1, MPI_INFO_NULL, MPI_COMM_WORLD, created_window,ierr)

   write(0,'("Rank ",I0," created window")') myrank+1

   call MPI_Finalize(ierr)

   write(0,'(" Rank ",I0," finalized")') myrank+1

end program


