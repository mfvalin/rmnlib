! File shared.f90
SUBROUTINE MySub(x) BIND(C,NAME="MySub")
   USE ISO_C_BINDING
   REAL(C_DOUBLE), VALUE :: x
   WRITE(*,*) "MySub: x=",x
END SUBROUTINE   

