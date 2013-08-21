        program tata
	implicit none
	include 'mpif.h'
	integer local,iitot,ierr
	real *8, wt1,wt2
        external dummy
        call rpn_comm_init(dummy,local,iitot,0,0)
        print *,'local=',local,' total=',iitot
	call rpn_comm_softbarrier_init(MPI_COMM_WORLD)
	wt1=mpi_wtime()
	call rpn_comm_softbarrier(MPI_COMM_WORLD)
	wt2=mpi_wtime()
	print *,'SYNC TIME=',wt2-wt1
        call rpn_comm_finalize(ierr)
!       call some_sub(%val(12),%val(123))
        stop
        end
        subroutine dummy(nx,ny)
        return
        end
