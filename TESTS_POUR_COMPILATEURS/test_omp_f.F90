subroutine OpenMPTest1()
        use omp_lib;
 
!       write(*,*) "Running OpenMP Test 1: Environment variables"
!       write(*,*) "Number of threads :",omp_get_num_threads()
!       write(*,*) "Number of CPU's available:",omp_get_num_procs()
!        call omp_set_num_threads(8) ! set the number of threads to 8
!       write(*,*) "#Threads outside the parallel section:",omp_get_num_threads()
        !below we start a parallel section
        !$OMP PARALLEL
        write(*,*) "Number of threads in a parallel section :",omp_get_num_threads()
        write(*,*) "Currently in thread with ID = ",omp_get_thread_num()
        !$OMP END PARALLEL
 
end subroutine OpenMPTest1
program test
use omp_lib
write(*,*) "Running OpenMP Test 1: Environment variables"
write(*,*) "Number of threads :",omp_get_num_threads()
write(*,*) "Number of CPU's available:",omp_get_num_procs()
write(*,*) "#Threads outside the parallel section:",omp_get_num_threads()
call OpenMPTest1
write(*,*) "=================================================="
write(*,*) "set the number of threads to 8"
call omp_set_num_threads(8) ! set the number of threads to 8
call OpenMPTest1
stop
end
