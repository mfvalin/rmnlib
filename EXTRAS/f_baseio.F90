!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!int c_fnom(int *iun,char *nom,char *type,int lrec)
!ftnword f77name(fnom)(ftnword *iun,char *nom,char *type,ftnword *flrec,F2Cl l1,F2Cl l2)
! ====================================================
! fnom (open a file with attributes), see c_fnom for
! argument description
! this is a Fortran interface to the active routine written in C.
! the fortran functions (qqqfopen,qqqfclos) that c_fnom may have to call
! are passed as callbacks (address of function to call)
! this helps isolate c_baseio from any need to know fortran names and types
! ====================================================
module fnom_helpers
  use ISO_C_BINDING
  interface
    subroutine c_fnom_ext(qqqfopen,qqqfclos) bind(C,name='c_fnom_externals')
      import
      type(C_FUNPTR), intent(IN), value :: qqqfopen
      type(C_FUNPTR), intent(IN), value :: qqqfclos
    end subroutine c_fnom_ext
    function cfnom(iun,name,opti,reclen) result (status) bind(C,name='c_fnom')
      import
      integer(C_INT), intent(INOUT) :: iun
      integer(C_INT), intent(IN), value :: reclen
      character(C_CHAR), dimension(*), intent(IN) :: name,opti
!       type(C_PTR), intent(IN),value :: name,opti
!       type(C_FUNPTR), intent(IN), value :: qqqfopen
!       type(C_FUNPTR), intent(IN), value :: qqqfclos
      integer :: status
    end function cfnom
    FUNCTION ftnclos(iun) result(status) bind(C,name='F90clos_for_c')
      integer, intent(IN) :: iun
      integer :: status
    end FUNCTION ftnclos
    FUNCTION qqqf7op_c(iun,c_name,lrec,rndflag,unfflag,lmult,leng) result(status) bind(C,name='QQQf7op_for_c')
      import
      integer(C_INT), intent(IN), value :: iun, lrec, rndflag, unfflag, lmult, leng
      character(C_CHAR), dimension(leng), intent(IN) :: c_name
      integer :: status
    end FUNCTION qqqf7op_c
    function cqqqfnom(iun,name,ftyp,flrec,lname,lftyp) result(status) bind(C,name='F_qqqfnom')
      import
      integer(C_INT), intent(IN), value :: iun, lname, lftyp
      integer(C_INT), intent(OUT) :: flrec
      character(C_CHAR), dimension(lname), intent(OUT) :: name
      character(C_CHAR), dimension(lftyp), intent(OUT) :: ftyp
      integer :: status
    end function cqqqfnom
  end interface
end module fnom_helpers

subroutine traceback_from_c() bind(C,name='f_tracebck') ! C code calls f_tracebck()
  call tracebck  ! unfortunately does nothing with most compilers
end subroutine traceback_from_c

function fnom(iun,name,opti,reclen) result (status)
  use ISO_C_BINDING
  use fnom_helpers
  implicit none
  integer, intent(INOUT) :: iun
  integer, intent(IN) :: reclen
  character(len=*), intent(IN) :: name,opti
  integer :: status

  character(C_CHAR), dimension(len(trim(name))+1), target :: name1
  character(C_CHAR), dimension(len(trim(opti))+1), target :: opti1

  name1 = transfer(trim(name)//achar(0),name1)
  opti1 = transfer(trim(opti)//achar(0),opti1)

! int c_fnom(int *iun,char *nom,      char *type,     int lrec )
  call c_fnom_ext(C_FUNLOC(qqqf7op_c), C_FUNLOC(ftnclos))
  status = cfnom(iun, name1, opti1, reclen)
end function fnom

!int c_fnom(int *iun,char *nom,char *type,int lrec) 
! function fnom_for_c(iun,name,opti,reclen) result(status) bind(C,name='c_fnom')
!   use ISO_C_BINDING
!   use fnom_helpers
!   implicit none
!   type(C_PTR), intent(IN), value :: iun
!   integer(C_INT), intent(IN), value :: reclen
!   type(C_PTR), intent(IN), value :: name,opti
!   integer :: status
! 
!   status = cfnom(iun,name,opti,reclen)
!   return
! end function fnom_for_c

!int F_qqqfnom(int iun,char *nom,char *type,int *flrec,int l1,int l2)
function qqqfnom(iun,name,ftyp,flrec) result(status)  ! get filename, properties, record length  info from unit number
  use ISO_C_BINDING
  use fnom_helpers
  implicit none
  integer, intent(IN) :: iun
  integer, intent(OUT) :: flrec
  character(len=*), intent(OUT) :: name,ftyp
  integer :: status

  character(len=1), dimension(len(name)) :: name1
  character(len=1), dimension(len(ftyp)) :: ftyp1
  integer :: lname, lftyp, i

  lname = len(name)
  lftyp = len(ftyp)
  status = cqqqfnom(iun,name1,ftyp1,flrec,lname,lftyp)
  do i = 1 , lftyp
    ftyp(i:i) = ftyp1(i)
  enddo
  do i = 1 , lname
    name(i:i) = name1(i)
  enddo

end function qqqfnom

! C callable function (called by c_fnom) to address file open operations
! that must be performed by the Fortran library
function qqqf7op_c(iun,c_name,lrec,rndflag,unfflag,lmult,leng) result(status) bind(C,name='QQQf7op_for_c') ! for C callback
  use ISO_C_BINDING
  implicit none
  integer(C_INT), intent(IN), value :: iun, lrec, rndflag, unfflag, lmult, leng
  character(C_CHAR), dimension(leng), intent(IN) :: c_name
  integer :: status

  integer lng, i
  character(len=4096) :: name
  integer, external :: qqqf7op

  name = ' '
  lng = leng
  do i=1,lng
    name(i:i) = c_name(i)
  enddo
!          qqqf7op(iun,name       ,lrec,rndflag,unfflag,lmult)
  status = qqqf7op(iun,name(1:lng),lrec,rndflag,unfflag,lmult)
  return
end function qqqf7op_c

! function to perform file open operations that must be performed by the Fortran library
INTEGER FUNCTION qqqf7op(iun,name,lrec,rndflag,unfflag,lmult)
  integer, intent(IN) :: iun,lrec
  character(len=*), intent(IN) :: name
  integer, intent(IN) :: rndflag,unfflag

  integer lng

  qqqf7op=0
  lng = len(trim(name))
!  print *,'opening file ',name(1:lng),' as unit ',iun
!  print *,'lrec=',lrec,' flags = ',rndflag,unfflag,' lmult=',lmult
!  print *,'len=',lng
  if (rndflag.eq.1) then
    if (unfflag.eq.1) then
!     print *,'ACCESS=DIRECT,RECL=',lrec
      OPEN(iun,FILE=name(1:lng),ACCESS='DIRECT',RECL=lrec*lmult,ERR=77)
    else
!     print *,'ACCESS=DIRECT,RECL=',lrec*4
      OPEN(iun,FILE=name(1:lng),ACCESS='DIRECT',FORM='FORMATTED',RECL=lrec*4,ERR=77)
    endif
  else
    if ((name(1:lng).EQ.'input') .OR. (name(1:lng).EQ.'$input')            &
       &  .OR. (name(1:lng).EQ.'output') .OR. (name(1:lng).EQ.'$output')   &
       &  .OR. (name(1:lng).EQ.'$in') .OR. (name(1:lng).EQ.'$out')) then
!            print *,' STDIN or STDOUT'
      return
    else
      if (unfflag.eq.1) then
!       print *,'UNFORMATTED open'
        OPEN(iun,FILE=name(1:lng),FORM='UNFORMATTED',ERR=77)
      else
!              print *,'FORMATTED open'
#if !defined (HP)
        OPEN(iun,FILE=name(1:lng),FORM='FORMATTED',DELIM='QUOTE',ERR=77)        
#else
        OPEN(iun,FILE=name(1:lng),FORM='FORMATTED',ERR=77)        
#endif
      endif
    endif
  endif
  return
77 continue
  qqqf7op = -1
  return
end
! close a Fortran file (normally used as a callback by c_fnom)
integer FUNCTION ftnclos(iun)
  implicit none
  integer iun

  ftnclos = 0
  CLOSE(iun)
  return
end
integer FUNCTION ftnclos_c(iun) bind(C,name='F90clos_for_c') ! for C callback
  implicit none
  integer iun

  ftnclos_c = 0
  CLOSE(iun)
  return
end
! close a file opened by fnom
integer function fclos(iun)
  use ISO_C_BINDING
  implicit none
  integer, intent(IN) :: iun
  interface
    function cfclos(iun) result(status) bind(C,name='c_fclos')
      use ISO_C_BINDING
      integer(C_INT), value :: iun
      integer(C_INT) :: status
    end function cfclos
  end interface
  fclos = cfclos(iun)
  return
end

integer function fretour(iun)
! ARGUMENTS: in iun   unit number, ignored
! RETURNS: zero.
! Kept only for backward compatibility. NO-OP
  fretour = 0
  return
end

INTEGER FUNCTION LONGUEUR(NOM)
  implicit none
  CHARACTER (len=*) NOM

  LONGUEUR = len(trim(NOM))
#if defined(USE_DEPRECATED_CODE)
  INTEGER LNG,I
!
  LNG = LEN(NOM)
  DO 10 I = LNG,1,-1
      IF (NOM(I:I) .EQ. ' ') THEN
        LNG = LNG - 1
      ELSE
        GOTO 20
      ENDIF
  10   CONTINUE
  20   CONTINUE
  LONGUEUR = LNG
#endif
  RETURN
END
! ====================================================
!     openda/closda readda/writda/checda
!     "asynchronous" random access by block routines
!     (the current implementation is SYNCHRONOUS)
! IUN(IN)     : fortran unit number
! BUF(IN/OUT) : array to write from or read into
! NS          : number of "sectors" (sector = 512 bytes)
! IS          : address of first "sector" for transfer
!               file starts at sector #1
! ====================================================
subroutine openda(iun)
  implicit none
  integer, intent(IN) :: iun

  call waopen(iun)
end subroutine openda

subroutine readda(iun,buf,ns,is)
  implicit none
  integer, intent(IN) :: iun, ns, is
  integer, intent(OUT), dimension(512,ns) :: buf
  call waread(iun,buf,(is-1)*512+1,ns*512)
end subroutine readda

subroutine writda(iun,buf,ns,is)
  implicit none
  integer, intent(IN) :: iun, ns, is
  integer, intent(IN), dimension(512,ns) :: buf

  call wawrit(iun,buf,(is-1)*512+1,ns*512)
end subroutine writda

subroutine checda(iun)
  use ISO_C_BINDING
  implicit none
  interface
    subroutine cchecda(iun) bind(C,name='c_checda')
      import
      integer(C_INT), intent(IN), value :: iun
    end subroutine cchecda
  end interface
  integer, intent(IN) :: iun

  call cchecda(iun)
end subroutine checda

subroutine closda(iun)
  implicit none
  integer, intent(IN) :: iun

  call waclos(iun)
end subroutine closda
! ====================================================
!  waopen/waclos waread/waread64/wawrit/wawrit64
!   random access by word (4 bytes) routines
!   these routines take care of endian conversion
!   the file contents are always BIG-ENDIAN (4 bytes)
! IUN(IN)     : fortran unit number
! BUF(IN/OUT) : array to write from or read into
! NMOTS(IN)   : number of "words" to read (word = 4 bytes)
! ADR(IN)     : address of first word for transfer
!               file starts at word #1
! ====================================================
subroutine waopen(iun)
  use ISO_C_BINDING
  implicit none
  interface
    subroutine cwaopen(iun) bind(C,name='c_waopen')
      import
      integer(C_INT), intent(IN), value :: iun
    end subroutine cwaopen
  end interface
  integer, intent(IN) :: iun

  call cwaopen(iun)
end subroutine waopen

subroutine waclos(iun)
  use ISO_C_BINDING
  implicit none
  interface
    subroutine cwaclos(iun) bind(C,name='c_waclos')
      import
      integer(C_INT), intent(IN), value :: iun
    end subroutine cwaclos
  end interface
  integer, intent(IN) :: iun

  call cwaclos(iun)
end subroutine waclos

subroutine waread(iun,buf,adr,nmots)
  use ISO_C_BINDING
  implicit none
  interface
    subroutine cwaread(iun,buf,adr,nmots) bind(C,name='c_waread')
      import
      integer(C_INT), intent(IN), value :: iun, nmots
      integer(C_INT), intent(IN), value :: adr
      integer(C_INT), intent(OUT), dimension(nmots) :: buf
    end subroutine cwaread
  end interface
  integer, intent(IN) :: iun, nmots
  integer, intent(IN) :: adr
  integer, intent(OUT), dimension(nmots) :: buf
print *,'waread, adr, nmots',adr,nmots
  call cwaread(iun,buf,adr,nmots)
end subroutine waread

function waread64(iun,buf,adr,nmots,partition) result(n)
  use ISO_C_BINDING
  implicit none
  interface
    function cwaread64(iun,buf,adr,nmots,partition) result(n) bind(C,name='c_waread64')
      import
      integer(C_INT), intent(IN), value :: iun, nmots, partition
      integer(C_LONG_LONG), intent(IN), value :: adr
      integer(C_INT), intent(OUT), dimension(nmots) :: buf
      integer(C_INT) :: n
    end function cwaread64
  end interface
  integer, intent(IN) :: iun, nmots, partition
  integer*8, intent(IN) :: adr
  integer, intent(OUT), dimension(nmots) :: buf
  integer :: n

  n = cwaread64(iun,buf,adr,nmots,partition)
end function waread64

subroutine wawrit(iun,buf,adr,nmots)
  use ISO_C_BINDING
  implicit none
  interface
    subroutine cwawrit(iun,buf,adr,nmots) bind(C,name='c_wawrit')
      import
      integer(C_INT), intent(IN), value :: iun, nmots
      integer(C_INT), intent(IN), value :: adr
      integer(C_INT), intent(IN), dimension(nmots) :: buf
    end subroutine cwawrit
  end interface
  integer, intent(IN) :: iun, nmots
  integer, intent(IN) :: adr
  integer, intent(IN), dimension(nmots) :: buf

  call cwawrit(iun,buf,adr,nmots)
end subroutine wawrit

function wawrit64(iun,buf,adr,nmots,partition) result(n)
  use ISO_C_BINDING
  implicit none
  interface
    function cwawrit64(iun,buf,adr,nmots,partition) result(n) bind(C,name='c_wawrit64')
      import
      integer(C_INT), intent(IN), value :: iun, nmots, partition
      integer(C_LONG_LONG), intent(IN), value :: adr
      integer(C_INT), intent(IN), dimension(nmots) :: buf
      integer(C_INT) :: n
    end function cwawrit64
  end interface
  integer, intent(IN) :: iun, nmots, partition
  integer*8, intent(IN) :: adr
  integer, intent(IN), dimension(nmots) :: buf
  integer :: n
  

  n = cwawrit64(iun,buf,adr,nmots,partition)
end function wawrit64

function existe(name) result(status)
  use ISO_C_BINDING
  implicit none
  interface
    function cexiste(name) result(status) bind(C,name='C_existe')
      import
      type(C_PTR), intent(IN) :: name
      integer :: status
    end function cexiste
  end interface
  character(len=*), intent(IN) :: name
  integer :: status
  character(C_CHAR), dimension(len(trim(name))+1), target :: name1

  name1 = transfer(trim(name)//achar(0),name1)
  status = cexiste(C_LOC(name1(1)))
  return
end function existe
!
! TODO
! qqqfnom, existe, waopen2, waclos2, waread2, wawrit2, wasize, numblks, 
! TODO sqgets, sqputs
subroutine sqopen(iun)
  use ISO_C_BINDING
  implicit none
  interface
    subroutine csqopen(iun) bind(C,name='c_sqopen')
      import
      integer(C_INT), intent(IN), value :: iun
    end subroutine csqopen
  end interface
  integer, intent(IN) :: iun

  call csqopen(iun)
end subroutine sqopen
subroutine sqclos(iun)
  use ISO_C_BINDING
  implicit none
  interface
    subroutine csqclos(iun) bind(C,name='c_sqclos')
      import
      integer(C_INT), intent(IN), value :: iun
    end subroutine csqclos
  end interface
  integer, intent(IN) :: iun

  call csqclos(iun)
end subroutine sqclos
subroutine sqrew(iun)
  use ISO_C_BINDING
  implicit none
  interface
    subroutine csqrew(iun) bind(C,name='c_sqrew')
      import
      integer(C_INT), intent(IN), value :: iun
    end subroutine csqrew
  end interface
  integer, intent(IN) :: iun

  call csqrew(iun)
end subroutine sqrew
subroutine sqeoi(iun)
  use ISO_C_BINDING
  implicit none
  interface
    subroutine csqeoi(iun) bind(C,name='c_sqeoi')
      import
      integer(C_INT), intent(IN), value :: iun
    end subroutine csqeoi
  end interface
  integer, intent(IN) :: iun

  call csqeoi(iun)
end subroutine sqeoi
subroutine sqgetw(iun,buf,nmots)
  use ISO_C_BINDING
  implicit none
  interface
    subroutine csqgetw(iun,buf,nmots) bind(C,name='c_sqgetw')
      import
      integer(C_INT), intent(IN), value :: iun,nmots
      integer(C_INT), intent(OUT) :: buf
    end subroutine csqgetw
  end interface
  integer, intent(IN) :: iun, nmots
  integer, intent(OUT) :: buf

  call csqgetw(iun,buf,nmots)
end subroutine sqgetw
subroutine sqputw(iun,buf,nmots)
  use ISO_C_BINDING
  implicit none
  interface
    subroutine csqputw(iun,buf,nmots) bind(C,name='c_sqputw')
      import
      integer(C_INT), intent(IN), value :: iun,nmots
      integer(C_INT), intent(IN) :: buf
    end subroutine csqputw
  end interface
  integer, intent(IN) :: iun, nmots
  integer, intent(IN) :: buf

  call csqputw(iun,buf,nmots)
end subroutine sqputw
function getfdsc(iun) result(i)
  use ISO_C_BINDING
  implicit none
  interface
    function cgetfdsc(iun) result(i) bind(C,name='c_getfdsc')
      import
      integer(C_INT), intent(IN), value :: iun
      integer(C_INT) :: i
    end function cgetfdsc
  end interface
  integer, intent(IN) :: iun
  integer :: i

  i = cgetfdsc(iun)
end function getfdsc
function numblks(iun) result(i)
  use ISO_C_BINDING
  implicit none
  interface
    function cnumblks(iun) result(i) bind(C,name='c_numblks')
      import
      integer(C_INT), intent(IN), value :: iun
      integer(C_INT) :: i
    end function cnumblks
  end interface
  integer, intent(IN) :: iun
  integer :: i

  i = cnumblks(iun)
end function numblks
function wasize(iun) result(i)
  use ISO_C_BINDING
  implicit none
  interface
    function cwasize(iun) result(i) bind(C,name='c_wasize')
      import
      integer(C_INT), intent(IN), value :: iun
      integer(C_INT) :: i
    end function cwasize
  end interface
  integer, intent(IN) :: iun
  integer :: i

  i = cwasize(iun)
end function wasize
!
! ftnword f77name(qqqfnom)(ftnword *iun,char *nom,char *type,ftnword *flrec,F2Cl l1,F2Cl l2)
! ftnword f77name(sqgets)(ftnword *iun, char  *bufptr, ftnword *nchar, F2Cl llbuf)
! ftnword f77name(sqputs)(ftnword *iun, char  *bufptr, ftnword *nchar, F2Cl llbuf)
! unsigned INT_32 f77name(check_host_id)()
#if defined(SELF_TEST)
program test
  integer, external :: fnom, fclos, wawrit64, waread64
  integer :: iun, status, i, r64, w64, iund77
  integer*8 :: ladr
  integer, dimension(1024) :: array0, array1, array2
  integer, dimension(100) :: darray

  do i=1,size(array1)
    array0(i) = i
  enddo
  print *,'========== base IO test, c_baseio + f_baseio =========='
  call test_c_fnom()
  iun = 0
  status = fnom(iun,'/tmp/Scrap','RND+WA',0)
  print *,'(fnom) iun,status =',iun,status
  call waopen(iun)
  array1 = array0
  call wawrit(iun,array1,1,size(array0))
  call waread(iun,array2,1,size(array0))
  if(any(array1 .ne. array2)) then
    print *,'did not read what was written'
  else
    print *,'read what was written',array2(1),array2(size(array0))
  endif
  array1 = array0 + 512
  ladr = 513
  w64 = wawrit64(iun,array1,ladr,size(array0),1)
  print *,'error expected, w64=',w64
  w64 = wawrit64(iun,array1,ladr,size(array0),0)
  print *,'OK expected, w64=',w64
  ladr = 129
  r64 = waread64(iun,array2,ladr,size(array0),1)
  print *,'error expected, r64=',r64
  r64 = waread64(iun,array2,ladr,size(array0),0)
  print *,'OK expected, r64=',r64
  if(any(array0+128 .ne. array2)) then
    print *,'did not read what was written'
  else
    print *,'read what was written',array2(1),array2(size(array0))
  endif
  call waclos(iun)
  call openda(iun)
  array2 = -1
  array1 = array0 + 1024 + 512
  call writda(iun,array1,2,4)
  call checda(iun)
  call readda(iun,array2,2,3)
  call checda(iun)
  if(any(array0+1024 .ne. array2)) then
    print *,'did not read what was written'
  else
    print *,'read what was written',array2(1),array2(size(array0))
  endif
  call closda(iun)
  call waopen(iun)
  ladr = 1385
  r64 = waread64(iun,array2,ladr,size(array0),0)
  print *,'r64=',r64
  if(any(array0+1384 .ne. array2)) then
    print *,'did not read what was written'
  else
    print *,'read what was written',array2(1),array2(size(array0))
  endif
  call waclos(iun)
  status = fclos(iun)
  print *,'(fclos) iun,status =',iun,status
  iund77 = 0
  status = fnom(iund77,'/tmp/Scrap','D77+UNF',10)
  print *,'(fnom) iun,D77 status =',iund77,status
  darray = -1
  read(iund77,rec=2)darray(2:11)
  print *,'expecting -1, 11 to 20, -1'
  print *,darray(1:12)
  status = fclos(iund77)
  print *,'(fclos) iun,D77 status =',iund77,status
  iun = 0
  status = fnom(iun,'/tmp/Scrap','FTN+FMT',0)
  print *,'(fnom) iun SEQ,status =',iun,status
  write(iun,100)"0123456789"
  write(iun,100)"abcdefghij"
100 format(A10)
  status = fclos(iun)
  print *,'(fclos) iun,SEQ status =',iund77,status
  status = fnom(iun,'/tmp/Scrap2','FTN+UNF',0)
  print *,'(fnom) iun SEQ UNF,status =',iun,status
  write(iun)"01234567"
  write(iun)"abcdefgh"
  status = fclos(iun)
  print *,'(fclos) iun,SEQ UNF status =',iund77,status
end program
#endif
