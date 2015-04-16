program fcmain
! fake fortran main for C program that calls fortran routines that
! might require fortran runtime library initialization
!
! s.f90 "-DMY_C_MAIN='test_main'"  fcmain.F90 test_main.o
!
!==================================================================================
! simple example
!
! cat mydemo.c
!int test_main(int argc, char**argv)
!{
!  int i;
!  for(i=0;i<argc;i++) printf("arg %d = '%s'\n",i,argv[i]);
!  return(0);
!}
!
!==================================================================================
! example for tcl interpreter
!
!  cat mytcl.c
! #include <tcl.h>
! int
! test_main(argc, argv)
!     int argc;                   /* Number of command-line arguments. */
!     char **argv;                /* Values of command-line arguments. */
! {
!     Tcl_Main(argc, argv, Tcl_AppInit);
!     return 0;                   /* Needed only to prevent compiler
! warning. */
! }
! int
! Tcl_AppInit(interp)
!     Tcl_Interp *interp;         /* Interpreter for application. */
! {
!     if (Tcl_Init(interp) == TCL_ERROR) {
!         return TCL_ERROR;
!     }
!     Tcl_SetVar(interp, "tcl_rcFileName", "~/tclshrc.tcl", TCL_GLOBAL_ONLY);
!     return TCL_OK;
! }
!==================================================================================
! example for wish interpreter
! cat mywish.c
! #include <tcl.h>
! #include <tk.h>
! int
! test_main(
!     int argc,                   /* Number of command-line arguments. */
!     char **argv)                /* Values of command-line arguments. */
! {
!     Tk_Main(argc, argv, Tcl_AppInit);
!     return 0;                   /* Needed only to prevent compiler
! warning. */
! }
! 
! int
! Tcl_AppInit(interp)
!     Tcl_Interp *interp;         /* Interpreter for application. */
! {
!     if (Tcl_Init(interp) == TCL_ERROR) {
!         return TCL_ERROR;
!     }
!     if (Tk_Init(interp) == TCL_ERROR) {
!         return TCL_ERROR;
!     }
!     Tcl_StaticPackage(interp, "Tk", Tk_Init, Tk_SafeInit);
!     Tcl_SetVar(interp, "tcl_rcFileName", "~/.wishrc", TCL_GLOBAL_ONLY);
!     return TCL_OK;
! }
!==================================================================================
! example for python interpreter
! s.f90 "-DMY_C_MAIN='test_main'"  fcmain.F90 mypython.o -lpython2.7
!
! cat mypython.c
! test_main(int argc, char** argv)
! {
!   Py_Initialize();
!   Py_Main(argc, argv);
!   Py_Finalize();
! }
!==================================================================================

  use ISO_C_BINDING
  implicit none
  integer(C_INT) :: nargs
  integer :: i, length, status
  character(len=4096) :: argument
  character(len=1), dimension(:), pointer :: arg1
  type(C_PTR), dimension(:), pointer :: argv
  type(C_PTR) :: argtab
!
  interface   ! call real C main
    function c_main(nargs,argv) result(status) BIND(C,name=MY_C_MAIN)
    import
    implicit none
    integer, intent(IN), value :: nargs      ! int argc
    type(C_PTR), intent(IN), value :: argv   ! char **argv
    integer :: status
    end function c_main
  end interface
!
  nargs = command_argument_count()
  allocate(argv(0:nargs+1))       ! allocate table of C pointers
  argv = C_NULL_PTR               ! initialize to NULL pointers
  do i=0,nargs
    call get_command_argument(i,argument,length,status)       ! get argument i
    allocate(arg1(length+1))                                  ! allocate byte array with room for null terminator
    arg1 = transfer(trim(argument)//achar(0),arg1,length+1)   ! copy null terminated string into C compatible char array
    argv(i) = C_LOC(arg1(1))                                  ! put pointer into slot i of array of argument pointers
  enddo
  argtab = C_LOC(argv(0))          ! pointer to array of pointers to individual arguments
  status = c_main(nargs,argtab)    ! call our C main, ginving it what it expects (int argc, char**argv)
  stop
end
