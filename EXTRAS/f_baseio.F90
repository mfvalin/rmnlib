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
    function cfnom(iun,name,opti,reclen,qqqfopen,qqqfclos) result (status) bind(C,name='c_fnom_callback')
      import
      integer(C_INT), intent(INOUT) :: iun
      integer(C_INT), intent(IN), value :: reclen
      type(C_PTR), intent(IN) :: name,opti
      type(C_FUNPTR), intent(IN), value :: qqqfopen
      type(C_FUNPTR), intent(IN), value :: qqqfclos
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

  end interface
  interface
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

! int c_fnom(int *iun,char *nom,      char *type,     int lrec, int (*open)()      , int (*clos)()
  status = cfnom(iun, c_loc(name1(1)), c_loc(opti1(1)), reclen, C_FUNLOC(qqqf7op_c), C_FUNLOC(ftnclos))
end function fnom

!int c_fnom(int *iun,char *nom,char *type,int lrec) 
function fnom_for_c(iun,name,opti,reclen) result(status) bind(C,name='c_fnom')
  use ISO_C_BINDING
  use fnom_helpers
  implicit none
  integer(C_INT), intent(INOUT) :: iun
  integer, intent(IN) :: reclen
  type(C_PTR), intent(IN) :: name,opti
  integer :: status

  status = cfnom(iun,name,opti,reclen, C_FUNLOC(qqqf7op_c), C_FUNLOC(ftnclos))
  return
end function fnom_for_c

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

  lname = len(trim(name))
  lftyp = len(trim(ftyp))
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
  integer iun

  ftnclos = 0
  CLOSE(iun)
  return
end
integer FUNCTION ftnclos_c(iun) bind(C,name='F90clos_for_c') ! for C callback
  integer iun

  ftnclos = 0
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
! Kept only for backward compatibility. NO-OP
  fretour = 0
  return
end

INTEGER FUNCTION LONGUEUR(NOM)
  implicit none
  CHARACTER * (*) NOM
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
  RETURN
END
! ====================================================
!     openda/closda readda/writda/checda
!     "asynchronous" random access by block routines
!     (the current implementation is SYNCHRONOUS)
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

  call waread(iun,buf,is*512,ns*512)
end subroutine readda

subroutine writda(iun,buf,ns,is)
  implicit none
  integer, intent(IN) :: iun, ns, is
  integer, intent(IN), dimension(512,ns) :: buf

  call wawrit(iun,buf,is*512,ns*512)
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

  call cwaread(iun,buf,adr,nmots)
end subroutine waread

subroutine waread64(iun,buf,adr,nmots,partition)
  use ISO_C_BINDING
  implicit none
  interface
    subroutine cwaread64(iun,buf,adr,nmots,partition) bind(C,name='c_waread64')
      import
      integer(C_INT), intent(IN), value :: iun, nmots, partition
      integer(C_LONG_LONG), intent(IN), value :: adr
      integer(C_INT), intent(OUT), dimension(nmots) :: buf
    end subroutine cwaread64
  end interface
  integer, intent(IN) :: iun, nmots, partition
  integer*8, intent(IN) :: adr
  integer, intent(OUT), dimension(nmots) :: buf

  call cwaread64(iun,buf,adr,nmots,partition)
end subroutine waread64

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

subroutine wawrit64(iun,buf,adr,nmots,partition)
  use ISO_C_BINDING
  implicit none
  interface
    subroutine cwawrit64(iun,buf,adr,nmots,partition) bind(C,name='c_wawrit64')
      import
      integer(C_INT), intent(IN), value :: iun, nmots, partition
      integer(C_LONG_LONG), intent(IN), value :: adr
      integer(C_INT), intent(IN), dimension(nmots) :: buf
    end subroutine cwawrit64
  end interface
  integer, intent(IN) :: iun, nmots, partition
  integer*8, intent(IN) :: adr
  integer, intent(IN), dimension(nmots) :: buf

  call cwawrit64(iun,buf,adr,nmots,partition)
end subroutine wawrit64

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
! getfdsc, sqclos, sqrew, sqeoi, sqgetw, sqputw, sqgets, sqputs, tracebck
! hrjust, hljust, check_host_id
!
! ftnword f77name(qqqfnom)(ftnword *iun,char *nom,char *type,ftnword *flrec,F2Cl l1,F2Cl l2)
! int c_existe(char *nom) 
! ftnword f77name(wasize)(ftnword *fiun) 
! ftnword f77name(numblks)(ftnword *fiun)
! ftnword f77name(getfdsc)( ftnword *iun)
! void f77name(sqclos)(ftnword *iun)
! void f77name(sqrew)
! void f77name(sqeoi)(ftnword *iun)
! ftnword f77name(sqgetw)(ftnword *iun, ftnword *bufptr, ftnword *nmots)
! ftnword f77name(sqputw)(ftnword *iun, ftnword *bufptr, ftnword *nmots)
! ftnword f77name(sqgets)(ftnword *iun, char  *bufptr, ftnword *nchar, F2Cl llbuf)
! ftnword f77name(sqputs)(ftnword *iun, char  *bufptr, ftnword *nchar, F2Cl llbuf)
! f77name(tracebck)()
! unsigned ftnword f77name(hrjust) (unsigned ftnword *moth, ftnword *ncar)
! unsigned ftnword f77name(hljust) (unsigned ftnword *moth, ftnword *ncar)
! unsigned INT_32 f77name(check_host_id)()
