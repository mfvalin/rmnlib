PROGRAM DLFCN_Test
   USE ISO_C_BINDING
   USE ISO_C_UTILITIES
   USE DLFCN
   IMPLICIT NONE
   
   ! Local variables:
   CHARACTER(KIND=C_CHAR,LEN=1024) :: dll_name, sub_name
   TYPE(C_PTR) :: handle=C_NULL_PTR
   TYPE(C_FUNPTR) :: funptr=C_NULL_FUNPTR
   INTEGER(C_INT) :: status

   ! The dynamic subroutine has a simple interface:
   ABSTRACT INTERFACE
      SUBROUTINE MySub(x) BIND(C)
         USE ISO_C_BINDING
         REAL(C_DOUBLE), VALUE :: x
      END SUBROUTINE
   END INTERFACE
   PROCEDURE(MySub), POINTER :: dll_sub ! Dynamically-linked procedure
   
   call register_dl_routines
   WRITE(*,*) "Enter the name of the DL and the name of the DL subroutine:"
   READ(*,"(A)") dll_name ! Enter "shared.so"
   READ(*,"(A)") sub_name ! Enter "MySub"
   
   ! Open the DL:
   handle=DL_Open(TRIM(dll_name)//C_NULL_CHAR, IOR(RTLD_NOW, RTLD_GLOBAL))
      ! The use of IOR is not really proper...wait till Fortran 2008  
   IF(.NOT.C_ASSOCIATED(handle)) THEN
      WRITE(*,*) "Error in dlopen: ", C_F_STRING(DL_Error())
      STOP
   END IF
   
   ! Find the subroutine in the DL:
   funptr=DL_Sym(handle,TRIM(sub_name)//C_NULL_CHAR)
   IF(.NOT.C_ASSOCIATED(funptr)) THEN
      WRITE(*,*) "Error in dlsym: ", C_F_STRING(DL_Error())
      STOP
   END IF
   ! Now convert the C function pointer to a Fortran procedure pointer
   CALL C_F_PROCPOINTER(CPTR=funptr, FPTR=dll_sub)
   ! Finally, invoke the dynamically-linked subroutine:
   CALL dll_sub(1.0_c_double)
   
   ! Now close the DL:
   status=DL_Close(handle)
   IF(status/=0) THEN
      WRITE(*,*) "Error in dlclose: ", C_F_STRING(DL_Error())
      STOP
   END IF

END PROGRAM

