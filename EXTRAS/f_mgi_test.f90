program f_mgi_test
  implicit none
  integer, parameter :: MAXVAL=100
  integer status
  integer, external :: mgi_open, mgi_init, mgi_clos, mgi_term, mgi_erad, mgi_write
  character(len=1) :: what
  character(len=1024) :: string
  real :: start, end, delta
  integer, dimension(MAXVAL) :: is
  real, dimension(MAXVAL) :: fs
  real*8, dimension(MAXVAL) :: ds
  integer :: iostat, nval, i

  open(unit=10,file='mgi_test.txt',form='FORMATTED')
  read(10,*,iostat=iostat)what
  do while(iostat == 0)
    backspace(10)
    if(what .ne. 'C') then
      read(10,*)what,start,end,delta
      print *,'"'//what//'",',start,end,delta
      nval = min(MAXVAL,nint(1.0+(end-start)/delta))
      select case(what)
      case('I')
        do i=1,nval
          is(i)=nint(start+(i-1)*delta)
        enddo
        print *,is(1:nval)
      case('R')
        do i=1,nval
          fs(i)=start+(i-1)*delta
        enddo
        print *,fs(1:nval)
      case('D')
        do i=1,nval
          ds(i)=start+(i-1)*delta
        enddo
        print *,ds(1:nval)
      end select
    else
      read(10,*)what,string
      print *,'"'//what//'",','"'//trim(string)//'"'
    endif
    read(10,*,iostat=iostat)what
  enddo
  close(unit=10)
  stop
end