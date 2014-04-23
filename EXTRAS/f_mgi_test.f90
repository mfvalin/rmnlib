program f_mgi_test
! usage: f_mgi_test shared_memory_id R|W scenario_file
  use ISO_C_BINDING
  implicit none
  interface
    subroutine sleep_a_bit(duration) bind(C,name='sleep')
    use ISO_C_BINDING
    integer(C_INT), intent(IN), value :: duration
    integer(C_INT) :: status
    end subroutine sleep_a_bit
  end interface

  integer, parameter :: MAXVAL=100
  integer status
  integer, external :: mgi_open, mgi_init, mgi_clos, mgi_term, mgi_erad, mgi_write
  integer, external :: iargc
  character(len=1) :: what, testmode
  character(len=1024) :: string, testfile
  real :: start, end, delta
  integer, dimension(MAXVAL) :: is
  real, dimension(MAXVAL) :: fs
  real*8, dimension(MAXVAL) :: ds
  integer :: iostat, nval, i, shmemid, nargs, channel

  nargs = iargc()
  if(nargs >= 1) then
    call getarg(1,string)
    read(string,*)shmemid   ! memory segment id
    print *,' memory segment id=',shmemid
  endif
  if(nargs >= 2) then
    call getarg(2,string)
    testmode=string(1:1)
    print *,' test mode = ','"'//testmode//'"'
  endif
  testfile='mgi_test.txt'
  if(nargs >= 3) then
    call getarg(3,testfile)
  endif
  print *,' scenario file: '//'"'//trim(testfile)//'"'
  if(nargs >= 4) then
  endif

  channel = mgi_init("test")
  print *,'channel=',channel
  call sleep_a_bit(1)
  if(testmode=='R') status = mgi_open(channel,'R')
  if(testmode=='W') status = mgi_open(channel,'W')
  print *,'status=',status
  call sleep_a_bit(1)

  open(unit=10,file=trim(testfile),form='FORMATTED')
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
  call sleep_a_bit(1)
  status = mgi_clos(channel)
  call sleep_a_bit(1)
  status = mgi_term()
  stop
end