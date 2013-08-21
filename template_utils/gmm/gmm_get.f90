!!===================== gmm_get (interface) =====================
!
  integer function gmm_get184(iname,p,m)
  use gmm_internals
  use pointer_table_data_184
  implicit none
   integer :: i, array_rank
  character(len=*), intent(in) :: iname               
  integer*8, pointer  :: p(:,:,:,:)
  type(gmm_metadata), optional, intent(out) :: m               
!  integer,intent(inout) :: reqid
  include 'gmm_directory_interface.inc'
  type(gmm_metadata) :: m2
  integer*8 :: key
      integer *8 get_address_from
      external get_address_from
  key = 0
  call check_directory_entry(iname,key)
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then  
    call find_directory_entry(iname,key)
  endif
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then   
    if (present(m)) then
      m%a = GMM_NULL_ATTRIB 
      m%l = GMM_NULL_LAYOUT 
    endif
    nullify(p)
    key= GMM_KEY_NOT_FOUND
    gmm_get184 = GMM_VAR_NOT_FOUND
  else
    m2%l=directory(cur_page)%entry(cur_entry)%l
    m2%a=directory(cur_page)%entry(cur_entry)%a
    if (present(m)) m=m2                           
    p=>gmm_ptrs184(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
    do i=1,4
!      print *,'DEBUG gmm_get m%l(',i,')%n=',m2%l(i)%n
      if (m2%l(i)%n /= 0) array_rank=i
    enddo
!    write(6,'(a,a,a,i2,a,i2)') 'DEBUG gmm_get iname=',iname,' DIM=',DIM,' array_rank=',array_rank
    if (array_rank /= 4) then
       nullify(p)
       if (present(m)) m = GMM_NULL_METADATA
       gmm_get184 = GMM_INCONSISTENT_DIMS
!       print *,'DEBUG gmm_get *** GMM_INCONSISTENT_DIMS ***'
    else
       gmm_get184 = GMM_OK
    endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_get name=',iname,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
   endif
  end function gmm_get184
!
  integer function gmm_get144(iname,p,m)
  use gmm_internals
  use pointer_table_data_144
  implicit none
   integer :: i, array_rank
  character(len=*), intent(in) :: iname               
  integer*4, pointer  :: p(:,:,:,:)
  type(gmm_metadata), optional, intent(out) :: m               
!  integer,intent(inout) :: reqid
  include 'gmm_directory_interface.inc'
  type(gmm_metadata) :: m2
  integer*8 :: key
      integer *8 get_address_from
      external get_address_from
  key = 0
  call check_directory_entry(iname,key)
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then  
    call find_directory_entry(iname,key)
  endif
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then   
    if (present(m)) then
      m%a = GMM_NULL_ATTRIB 
      m%l = GMM_NULL_LAYOUT 
    endif
    nullify(p)
    key= GMM_KEY_NOT_FOUND
    gmm_get144 = GMM_VAR_NOT_FOUND
  else
    m2%l=directory(cur_page)%entry(cur_entry)%l
    m2%a=directory(cur_page)%entry(cur_entry)%a
    if (present(m)) m=m2                           
    p=>gmm_ptrs144(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
    do i=1,4
!      print *,'DEBUG gmm_get m%l(',i,')%n=',m2%l(i)%n
      if (m2%l(i)%n /= 0) array_rank=i
    enddo
!    write(6,'(a,a,a,i2,a,i2)') 'DEBUG gmm_get iname=',iname,' DIM=',DIM,' array_rank=',array_rank
    if (array_rank /= 4) then
       nullify(p)
       if (present(m)) m = GMM_NULL_METADATA
       gmm_get144 = GMM_INCONSISTENT_DIMS
!       print *,'DEBUG gmm_get *** GMM_INCONSISTENT_DIMS ***'
    else
       gmm_get144 = GMM_OK
    endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_get name=',iname,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
   endif
  end function gmm_get144
!
  integer function gmm_get284(iname,p,m)
  use gmm_internals
  use pointer_table_data_284
  implicit none
   integer :: i, array_rank
  character(len=*), intent(in) :: iname               
  real*8, pointer  :: p(:,:,:,:)
  type(gmm_metadata), optional, intent(out) :: m               
!  integer,intent(inout) :: reqid
  include 'gmm_directory_interface.inc'
  type(gmm_metadata) :: m2
  integer*8 :: key
      integer *8 get_address_from
      external get_address_from
  key = 0
  call check_directory_entry(iname,key)
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then  
    call find_directory_entry(iname,key)
  endif
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then   
    if (present(m)) then
      m%a = GMM_NULL_ATTRIB 
      m%l = GMM_NULL_LAYOUT 
    endif
    nullify(p)
    key= GMM_KEY_NOT_FOUND
    gmm_get284 = GMM_VAR_NOT_FOUND
  else
    m2%l=directory(cur_page)%entry(cur_entry)%l
    m2%a=directory(cur_page)%entry(cur_entry)%a
    if (present(m)) m=m2                           
    p=>gmm_ptrs284(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
    do i=1,4
!      print *,'DEBUG gmm_get m%l(',i,')%n=',m2%l(i)%n
      if (m2%l(i)%n /= 0) array_rank=i
    enddo
!    write(6,'(a,a,a,i2,a,i2)') 'DEBUG gmm_get iname=',iname,' DIM=',DIM,' array_rank=',array_rank
    if (array_rank /= 4) then
       nullify(p)
       if (present(m)) m = GMM_NULL_METADATA
       gmm_get284 = GMM_INCONSISTENT_DIMS
!       print *,'DEBUG gmm_get *** GMM_INCONSISTENT_DIMS ***'
    else
       gmm_get284 = GMM_OK
    endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_get name=',iname,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
   endif
  end function gmm_get284
!
  integer function gmm_get244(iname,p,m)
  use gmm_internals
  use pointer_table_data_244
  implicit none
   integer :: i, array_rank
  character(len=*), intent(in) :: iname               
  real*4, pointer  :: p(:,:,:,:)
  type(gmm_metadata), optional, intent(out) :: m               
!  integer,intent(inout) :: reqid
  include 'gmm_directory_interface.inc'
  type(gmm_metadata) :: m2
  integer*8 :: key
      integer *8 get_address_from
      external get_address_from
  key = 0
  call check_directory_entry(iname,key)
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then  
    call find_directory_entry(iname,key)
  endif
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then   
    if (present(m)) then
      m%a = GMM_NULL_ATTRIB 
      m%l = GMM_NULL_LAYOUT 
    endif
    nullify(p)
    key= GMM_KEY_NOT_FOUND
    gmm_get244 = GMM_VAR_NOT_FOUND
  else
    m2%l=directory(cur_page)%entry(cur_entry)%l
    m2%a=directory(cur_page)%entry(cur_entry)%a
    if (present(m)) m=m2                           
    p=>gmm_ptrs244(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
    do i=1,4
!      print *,'DEBUG gmm_get m%l(',i,')%n=',m2%l(i)%n
      if (m2%l(i)%n /= 0) array_rank=i
    enddo
!    write(6,'(a,a,a,i2,a,i2)') 'DEBUG gmm_get iname=',iname,' DIM=',DIM,' array_rank=',array_rank
    if (array_rank /= 4) then
       nullify(p)
       if (present(m)) m = GMM_NULL_METADATA
       gmm_get244 = GMM_INCONSISTENT_DIMS
!       print *,'DEBUG gmm_get *** GMM_INCONSISTENT_DIMS ***'
    else
       gmm_get244 = GMM_OK
    endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_get name=',iname,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
   endif
  end function gmm_get244
!
  integer function gmm_get384(iname,p,m)
  use gmm_internals
  use pointer_table_data_384
  implicit none
   integer :: i, array_rank
  character(len=*), intent(in) :: iname               
  complex*8, pointer  :: p(:,:,:,:)
  type(gmm_metadata), optional, intent(out) :: m               
!  integer,intent(inout) :: reqid
  include 'gmm_directory_interface.inc'
  type(gmm_metadata) :: m2
  integer*8 :: key
      integer *8 get_address_from
      external get_address_from
  key = 0
  call check_directory_entry(iname,key)
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then  
    call find_directory_entry(iname,key)
  endif
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then   
    if (present(m)) then
      m%a = GMM_NULL_ATTRIB 
      m%l = GMM_NULL_LAYOUT 
    endif
    nullify(p)
    key= GMM_KEY_NOT_FOUND
    gmm_get384 = GMM_VAR_NOT_FOUND
  else
    m2%l=directory(cur_page)%entry(cur_entry)%l
    m2%a=directory(cur_page)%entry(cur_entry)%a
    if (present(m)) m=m2                           
    p=>gmm_ptrs384(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
    do i=1,4
!      print *,'DEBUG gmm_get m%l(',i,')%n=',m2%l(i)%n
      if (m2%l(i)%n /= 0) array_rank=i
    enddo
!    write(6,'(a,a,a,i2,a,i2)') 'DEBUG gmm_get iname=',iname,' DIM=',DIM,' array_rank=',array_rank
    if (array_rank /= 4) then
       nullify(p)
       if (present(m)) m = GMM_NULL_METADATA
       gmm_get384 = GMM_INCONSISTENT_DIMS
!       print *,'DEBUG gmm_get *** GMM_INCONSISTENT_DIMS ***'
    else
       gmm_get384 = GMM_OK
    endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_get name=',iname,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
   endif
  end function gmm_get384
!
  integer function gmm_get183(iname,p,m)
  use gmm_internals
  use pointer_table_data_183
  implicit none
   integer :: i, array_rank
  character(len=*), intent(in) :: iname               
  integer*8, pointer  :: p(:,:,:)
  type(gmm_metadata), optional, intent(out) :: m               
!  integer,intent(inout) :: reqid
  include 'gmm_directory_interface.inc'
  type(gmm_metadata) :: m2
  integer*8 :: key
      integer *8 get_address_from
      external get_address_from
  key = 0
  call check_directory_entry(iname,key)
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then  
    call find_directory_entry(iname,key)
  endif
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then   
    if (present(m)) then
      m%a = GMM_NULL_ATTRIB 
      m%l = GMM_NULL_LAYOUT 
    endif
    nullify(p)
    key= GMM_KEY_NOT_FOUND
    gmm_get183 = GMM_VAR_NOT_FOUND
  else
    m2%l=directory(cur_page)%entry(cur_entry)%l
    m2%a=directory(cur_page)%entry(cur_entry)%a
    if (present(m)) m=m2                           
    p=>gmm_ptrs183(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
    do i=1,4
!      print *,'DEBUG gmm_get m%l(',i,')%n=',m2%l(i)%n
      if (m2%l(i)%n /= 0) array_rank=i
    enddo
!    write(6,'(a,a,a,i2,a,i2)') 'DEBUG gmm_get iname=',iname,' DIM=',DIM,' array_rank=',array_rank
    if (array_rank /= 3) then
       nullify(p)
       if (present(m)) m = GMM_NULL_METADATA
       gmm_get183 = GMM_INCONSISTENT_DIMS
!       print *,'DEBUG gmm_get *** GMM_INCONSISTENT_DIMS ***'
    else
       gmm_get183 = GMM_OK
    endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_get name=',iname,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
   endif
  end function gmm_get183
!
  integer function gmm_get143(iname,p,m)
  use gmm_internals
  use pointer_table_data_143
  implicit none
   integer :: i, array_rank
  character(len=*), intent(in) :: iname               
  integer*4, pointer  :: p(:,:,:)
  type(gmm_metadata), optional, intent(out) :: m               
!  integer,intent(inout) :: reqid
  include 'gmm_directory_interface.inc'
  type(gmm_metadata) :: m2
  integer*8 :: key
      integer *8 get_address_from
      external get_address_from
  key = 0
  call check_directory_entry(iname,key)
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then  
    call find_directory_entry(iname,key)
  endif
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then   
    if (present(m)) then
      m%a = GMM_NULL_ATTRIB 
      m%l = GMM_NULL_LAYOUT 
    endif
    nullify(p)
    key= GMM_KEY_NOT_FOUND
    gmm_get143 = GMM_VAR_NOT_FOUND
  else
    m2%l=directory(cur_page)%entry(cur_entry)%l
    m2%a=directory(cur_page)%entry(cur_entry)%a
    if (present(m)) m=m2                           
    p=>gmm_ptrs143(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
    do i=1,4
!      print *,'DEBUG gmm_get m%l(',i,')%n=',m2%l(i)%n
      if (m2%l(i)%n /= 0) array_rank=i
    enddo
!    write(6,'(a,a,a,i2,a,i2)') 'DEBUG gmm_get iname=',iname,' DIM=',DIM,' array_rank=',array_rank
    if (array_rank /= 3) then
       nullify(p)
       if (present(m)) m = GMM_NULL_METADATA
       gmm_get143 = GMM_INCONSISTENT_DIMS
!       print *,'DEBUG gmm_get *** GMM_INCONSISTENT_DIMS ***'
    else
       gmm_get143 = GMM_OK
    endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_get name=',iname,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
   endif
  end function gmm_get143
!
  integer function gmm_get283(iname,p,m)
  use gmm_internals
  use pointer_table_data_283
  implicit none
   integer :: i, array_rank
  character(len=*), intent(in) :: iname               
  real*8, pointer  :: p(:,:,:)
  type(gmm_metadata), optional, intent(out) :: m               
!  integer,intent(inout) :: reqid
  include 'gmm_directory_interface.inc'
  type(gmm_metadata) :: m2
  integer*8 :: key
      integer *8 get_address_from
      external get_address_from
  key = 0
  call check_directory_entry(iname,key)
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then  
    call find_directory_entry(iname,key)
  endif
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then   
    if (present(m)) then
      m%a = GMM_NULL_ATTRIB 
      m%l = GMM_NULL_LAYOUT 
    endif
    nullify(p)
    key= GMM_KEY_NOT_FOUND
    gmm_get283 = GMM_VAR_NOT_FOUND
  else
    m2%l=directory(cur_page)%entry(cur_entry)%l
    m2%a=directory(cur_page)%entry(cur_entry)%a
    if (present(m)) m=m2                           
    p=>gmm_ptrs283(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
    do i=1,4
!      print *,'DEBUG gmm_get m%l(',i,')%n=',m2%l(i)%n
      if (m2%l(i)%n /= 0) array_rank=i
    enddo
!    write(6,'(a,a,a,i2,a,i2)') 'DEBUG gmm_get iname=',iname,' DIM=',DIM,' array_rank=',array_rank
    if (array_rank /= 3) then
       nullify(p)
       if (present(m)) m = GMM_NULL_METADATA
       gmm_get283 = GMM_INCONSISTENT_DIMS
!       print *,'DEBUG gmm_get *** GMM_INCONSISTENT_DIMS ***'
    else
       gmm_get283 = GMM_OK
    endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_get name=',iname,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
   endif
  end function gmm_get283
!
  integer function gmm_get243(iname,p,m)
  use gmm_internals
  use pointer_table_data_243
  implicit none
   integer :: i, array_rank
  character(len=*), intent(in) :: iname               
  real*4, pointer  :: p(:,:,:)
  type(gmm_metadata), optional, intent(out) :: m               
!  integer,intent(inout) :: reqid
  include 'gmm_directory_interface.inc'
  type(gmm_metadata) :: m2
  integer*8 :: key
      integer *8 get_address_from
      external get_address_from
  key = 0
  call check_directory_entry(iname,key)
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then  
    call find_directory_entry(iname,key)
  endif
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then   
    if (present(m)) then
      m%a = GMM_NULL_ATTRIB 
      m%l = GMM_NULL_LAYOUT 
    endif
    nullify(p)
    key= GMM_KEY_NOT_FOUND
    gmm_get243 = GMM_VAR_NOT_FOUND
  else
    m2%l=directory(cur_page)%entry(cur_entry)%l
    m2%a=directory(cur_page)%entry(cur_entry)%a
    if (present(m)) m=m2                           
    p=>gmm_ptrs243(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
    do i=1,4
!      print *,'DEBUG gmm_get m%l(',i,')%n=',m2%l(i)%n
      if (m2%l(i)%n /= 0) array_rank=i
    enddo
!    write(6,'(a,a,a,i2,a,i2)') 'DEBUG gmm_get iname=',iname,' DIM=',DIM,' array_rank=',array_rank
    if (array_rank /= 3) then
       nullify(p)
       if (present(m)) m = GMM_NULL_METADATA
       gmm_get243 = GMM_INCONSISTENT_DIMS
!       print *,'DEBUG gmm_get *** GMM_INCONSISTENT_DIMS ***'
    else
       gmm_get243 = GMM_OK
    endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_get name=',iname,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
   endif
  end function gmm_get243
!
  integer function gmm_get383(iname,p,m)
  use gmm_internals
  use pointer_table_data_383
  implicit none
   integer :: i, array_rank
  character(len=*), intent(in) :: iname               
  complex*8, pointer  :: p(:,:,:)
  type(gmm_metadata), optional, intent(out) :: m               
!  integer,intent(inout) :: reqid
  include 'gmm_directory_interface.inc'
  type(gmm_metadata) :: m2
  integer*8 :: key
      integer *8 get_address_from
      external get_address_from
  key = 0
  call check_directory_entry(iname,key)
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then  
    call find_directory_entry(iname,key)
  endif
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then   
    if (present(m)) then
      m%a = GMM_NULL_ATTRIB 
      m%l = GMM_NULL_LAYOUT 
    endif
    nullify(p)
    key= GMM_KEY_NOT_FOUND
    gmm_get383 = GMM_VAR_NOT_FOUND
  else
    m2%l=directory(cur_page)%entry(cur_entry)%l
    m2%a=directory(cur_page)%entry(cur_entry)%a
    if (present(m)) m=m2                           
    p=>gmm_ptrs383(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
    do i=1,4
!      print *,'DEBUG gmm_get m%l(',i,')%n=',m2%l(i)%n
      if (m2%l(i)%n /= 0) array_rank=i
    enddo
!    write(6,'(a,a,a,i2,a,i2)') 'DEBUG gmm_get iname=',iname,' DIM=',DIM,' array_rank=',array_rank
    if (array_rank /= 3) then
       nullify(p)
       if (present(m)) m = GMM_NULL_METADATA
       gmm_get383 = GMM_INCONSISTENT_DIMS
!       print *,'DEBUG gmm_get *** GMM_INCONSISTENT_DIMS ***'
    else
       gmm_get383 = GMM_OK
    endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_get name=',iname,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
   endif
  end function gmm_get383
!
  integer function gmm_get182(iname,p,m)
  use gmm_internals
  use pointer_table_data_182
  implicit none
   integer :: i, array_rank
  character(len=*), intent(in) :: iname               
  integer*8, pointer  :: p(:,:)
  type(gmm_metadata), optional, intent(out) :: m               
!  integer,intent(inout) :: reqid
  include 'gmm_directory_interface.inc'
  type(gmm_metadata) :: m2
  integer*8 :: key
      integer *8 get_address_from
      external get_address_from
  key = 0
  call check_directory_entry(iname,key)
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then  
    call find_directory_entry(iname,key)
  endif
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then   
    if (present(m)) then
      m%a = GMM_NULL_ATTRIB 
      m%l = GMM_NULL_LAYOUT 
    endif
    nullify(p)
    key= GMM_KEY_NOT_FOUND
    gmm_get182 = GMM_VAR_NOT_FOUND
  else
    m2%l=directory(cur_page)%entry(cur_entry)%l
    m2%a=directory(cur_page)%entry(cur_entry)%a
    if (present(m)) m=m2                           
    p=>gmm_ptrs182(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
    do i=1,4
!      print *,'DEBUG gmm_get m%l(',i,')%n=',m2%l(i)%n
      if (m2%l(i)%n /= 0) array_rank=i
    enddo
!    write(6,'(a,a,a,i2,a,i2)') 'DEBUG gmm_get iname=',iname,' DIM=',DIM,' array_rank=',array_rank
    if (array_rank /= 2) then
       nullify(p)
       if (present(m)) m = GMM_NULL_METADATA
       gmm_get182 = GMM_INCONSISTENT_DIMS
!       print *,'DEBUG gmm_get *** GMM_INCONSISTENT_DIMS ***'
    else
       gmm_get182 = GMM_OK
    endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_get name=',iname,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
   endif
  end function gmm_get182
!
  integer function gmm_get142(iname,p,m)
  use gmm_internals
  use pointer_table_data_142
  implicit none
   integer :: i, array_rank
  character(len=*), intent(in) :: iname               
  integer*4, pointer  :: p(:,:)
  type(gmm_metadata), optional, intent(out) :: m               
!  integer,intent(inout) :: reqid
  include 'gmm_directory_interface.inc'
  type(gmm_metadata) :: m2
  integer*8 :: key
      integer *8 get_address_from
      external get_address_from
  key = 0
  call check_directory_entry(iname,key)
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then  
    call find_directory_entry(iname,key)
  endif
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then   
    if (present(m)) then
      m%a = GMM_NULL_ATTRIB 
      m%l = GMM_NULL_LAYOUT 
    endif
    nullify(p)
    key= GMM_KEY_NOT_FOUND
    gmm_get142 = GMM_VAR_NOT_FOUND
  else
    m2%l=directory(cur_page)%entry(cur_entry)%l
    m2%a=directory(cur_page)%entry(cur_entry)%a
    if (present(m)) m=m2                           
    p=>gmm_ptrs142(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
    do i=1,4
!      print *,'DEBUG gmm_get m%l(',i,')%n=',m2%l(i)%n
      if (m2%l(i)%n /= 0) array_rank=i
    enddo
!    write(6,'(a,a,a,i2,a,i2)') 'DEBUG gmm_get iname=',iname,' DIM=',DIM,' array_rank=',array_rank
    if (array_rank /= 2) then
       nullify(p)
       if (present(m)) m = GMM_NULL_METADATA
       gmm_get142 = GMM_INCONSISTENT_DIMS
!       print *,'DEBUG gmm_get *** GMM_INCONSISTENT_DIMS ***'
    else
       gmm_get142 = GMM_OK
    endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_get name=',iname,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
   endif
  end function gmm_get142
!
  integer function gmm_get282(iname,p,m)
  use gmm_internals
  use pointer_table_data_282
  implicit none
   integer :: i, array_rank
  character(len=*), intent(in) :: iname               
  real*8, pointer  :: p(:,:)
  type(gmm_metadata), optional, intent(out) :: m               
!  integer,intent(inout) :: reqid
  include 'gmm_directory_interface.inc'
  type(gmm_metadata) :: m2
  integer*8 :: key
      integer *8 get_address_from
      external get_address_from
  key = 0
  call check_directory_entry(iname,key)
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then  
    call find_directory_entry(iname,key)
  endif
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then   
    if (present(m)) then
      m%a = GMM_NULL_ATTRIB 
      m%l = GMM_NULL_LAYOUT 
    endif
    nullify(p)
    key= GMM_KEY_NOT_FOUND
    gmm_get282 = GMM_VAR_NOT_FOUND
  else
    m2%l=directory(cur_page)%entry(cur_entry)%l
    m2%a=directory(cur_page)%entry(cur_entry)%a
    if (present(m)) m=m2                           
    p=>gmm_ptrs282(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
    do i=1,4
!      print *,'DEBUG gmm_get m%l(',i,')%n=',m2%l(i)%n
      if (m2%l(i)%n /= 0) array_rank=i
    enddo
!    write(6,'(a,a,a,i2,a,i2)') 'DEBUG gmm_get iname=',iname,' DIM=',DIM,' array_rank=',array_rank
    if (array_rank /= 2) then
       nullify(p)
       if (present(m)) m = GMM_NULL_METADATA
       gmm_get282 = GMM_INCONSISTENT_DIMS
!       print *,'DEBUG gmm_get *** GMM_INCONSISTENT_DIMS ***'
    else
       gmm_get282 = GMM_OK
    endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_get name=',iname,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
   endif
  end function gmm_get282
!
  integer function gmm_get242(iname,p,m)
  use gmm_internals
  use pointer_table_data_242
  implicit none
   integer :: i, array_rank
  character(len=*), intent(in) :: iname               
  real*4, pointer  :: p(:,:)
  type(gmm_metadata), optional, intent(out) :: m               
!  integer,intent(inout) :: reqid
  include 'gmm_directory_interface.inc'
  type(gmm_metadata) :: m2
  integer*8 :: key
      integer *8 get_address_from
      external get_address_from
  key = 0
  call check_directory_entry(iname,key)
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then  
    call find_directory_entry(iname,key)
  endif
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then   
    if (present(m)) then
      m%a = GMM_NULL_ATTRIB 
      m%l = GMM_NULL_LAYOUT 
    endif
    nullify(p)
    key= GMM_KEY_NOT_FOUND
    gmm_get242 = GMM_VAR_NOT_FOUND
  else
    m2%l=directory(cur_page)%entry(cur_entry)%l
    m2%a=directory(cur_page)%entry(cur_entry)%a
    if (present(m)) m=m2                           
    p=>gmm_ptrs242(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
    do i=1,4
!      print *,'DEBUG gmm_get m%l(',i,')%n=',m2%l(i)%n
      if (m2%l(i)%n /= 0) array_rank=i
    enddo
!    write(6,'(a,a,a,i2,a,i2)') 'DEBUG gmm_get iname=',iname,' DIM=',DIM,' array_rank=',array_rank
    if (array_rank /= 2) then
       nullify(p)
       if (present(m)) m = GMM_NULL_METADATA
       gmm_get242 = GMM_INCONSISTENT_DIMS
!       print *,'DEBUG gmm_get *** GMM_INCONSISTENT_DIMS ***'
    else
       gmm_get242 = GMM_OK
    endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_get name=',iname,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
   endif
  end function gmm_get242
!
  integer function gmm_get382(iname,p,m)
  use gmm_internals
  use pointer_table_data_382
  implicit none
   integer :: i, array_rank
  character(len=*), intent(in) :: iname               
  complex*8, pointer  :: p(:,:)
  type(gmm_metadata), optional, intent(out) :: m               
!  integer,intent(inout) :: reqid
  include 'gmm_directory_interface.inc'
  type(gmm_metadata) :: m2
  integer*8 :: key
      integer *8 get_address_from
      external get_address_from
  key = 0
  call check_directory_entry(iname,key)
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then  
    call find_directory_entry(iname,key)
  endif
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then   
    if (present(m)) then
      m%a = GMM_NULL_ATTRIB 
      m%l = GMM_NULL_LAYOUT 
    endif
    nullify(p)
    key= GMM_KEY_NOT_FOUND
    gmm_get382 = GMM_VAR_NOT_FOUND
  else
    m2%l=directory(cur_page)%entry(cur_entry)%l
    m2%a=directory(cur_page)%entry(cur_entry)%a
    if (present(m)) m=m2                           
    p=>gmm_ptrs382(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
    do i=1,4
!      print *,'DEBUG gmm_get m%l(',i,')%n=',m2%l(i)%n
      if (m2%l(i)%n /= 0) array_rank=i
    enddo
!    write(6,'(a,a,a,i2,a,i2)') 'DEBUG gmm_get iname=',iname,' DIM=',DIM,' array_rank=',array_rank
    if (array_rank /= 2) then
       nullify(p)
       if (present(m)) m = GMM_NULL_METADATA
       gmm_get382 = GMM_INCONSISTENT_DIMS
!       print *,'DEBUG gmm_get *** GMM_INCONSISTENT_DIMS ***'
    else
       gmm_get382 = GMM_OK
    endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_get name=',iname,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
   endif
  end function gmm_get382
!
  integer function gmm_get181(iname,p,m)
  use gmm_internals
  use pointer_table_data_181
  implicit none
   integer :: i, array_rank
  character(len=*), intent(in) :: iname               
  integer*8, pointer  :: p(:)
  type(gmm_metadata), optional, intent(out) :: m               
!  integer,intent(inout) :: reqid
  include 'gmm_directory_interface.inc'
  type(gmm_metadata) :: m2
  integer*8 :: key
      integer *8 get_address_from
      external get_address_from
  key = 0
  call check_directory_entry(iname,key)
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then  
    call find_directory_entry(iname,key)
  endif
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then   
    if (present(m)) then
      m%a = GMM_NULL_ATTRIB 
      m%l = GMM_NULL_LAYOUT 
    endif
    nullify(p)
    key= GMM_KEY_NOT_FOUND
    gmm_get181 = GMM_VAR_NOT_FOUND
  else
    m2%l=directory(cur_page)%entry(cur_entry)%l
    m2%a=directory(cur_page)%entry(cur_entry)%a
    if (present(m)) m=m2                           
    p=>gmm_ptrs181(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
    do i=1,4
!      print *,'DEBUG gmm_get m%l(',i,')%n=',m2%l(i)%n
      if (m2%l(i)%n /= 0) array_rank=i
    enddo
!    write(6,'(a,a,a,i2,a,i2)') 'DEBUG gmm_get iname=',iname,' DIM=',DIM,' array_rank=',array_rank
    if (array_rank /= 1) then
       nullify(p)
       if (present(m)) m = GMM_NULL_METADATA
       gmm_get181 = GMM_INCONSISTENT_DIMS
!       print *,'DEBUG gmm_get *** GMM_INCONSISTENT_DIMS ***'
    else
       gmm_get181 = GMM_OK
    endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_get name=',iname,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
   endif
  end function gmm_get181
!
  integer function gmm_get141(iname,p,m)
  use gmm_internals
  use pointer_table_data_141
  implicit none
   integer :: i, array_rank
  character(len=*), intent(in) :: iname               
  integer*4, pointer  :: p(:)
  type(gmm_metadata), optional, intent(out) :: m               
!  integer,intent(inout) :: reqid
  include 'gmm_directory_interface.inc'
  type(gmm_metadata) :: m2
  integer*8 :: key
      integer *8 get_address_from
      external get_address_from
  key = 0
  call check_directory_entry(iname,key)
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then  
    call find_directory_entry(iname,key)
  endif
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then   
    if (present(m)) then
      m%a = GMM_NULL_ATTRIB 
      m%l = GMM_NULL_LAYOUT 
    endif
    nullify(p)
    key= GMM_KEY_NOT_FOUND
    gmm_get141 = GMM_VAR_NOT_FOUND
  else
    m2%l=directory(cur_page)%entry(cur_entry)%l
    m2%a=directory(cur_page)%entry(cur_entry)%a
    if (present(m)) m=m2                           
    p=>gmm_ptrs141(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
    do i=1,4
!      print *,'DEBUG gmm_get m%l(',i,')%n=',m2%l(i)%n
      if (m2%l(i)%n /= 0) array_rank=i
    enddo
!    write(6,'(a,a,a,i2,a,i2)') 'DEBUG gmm_get iname=',iname,' DIM=',DIM,' array_rank=',array_rank
    if (array_rank /= 1) then
       nullify(p)
       if (present(m)) m = GMM_NULL_METADATA
       gmm_get141 = GMM_INCONSISTENT_DIMS
!       print *,'DEBUG gmm_get *** GMM_INCONSISTENT_DIMS ***'
    else
       gmm_get141 = GMM_OK
    endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_get name=',iname,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
   endif
  end function gmm_get141
!
  integer function gmm_get281(iname,p,m)
  use gmm_internals
  use pointer_table_data_281
  implicit none
   integer :: i, array_rank
  character(len=*), intent(in) :: iname               
  real*8, pointer  :: p(:)
  type(gmm_metadata), optional, intent(out) :: m               
!  integer,intent(inout) :: reqid
  include 'gmm_directory_interface.inc'
  type(gmm_metadata) :: m2
  integer*8 :: key
      integer *8 get_address_from
      external get_address_from
  key = 0
  call check_directory_entry(iname,key)
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then  
    call find_directory_entry(iname,key)
  endif
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then   
    if (present(m)) then
      m%a = GMM_NULL_ATTRIB 
      m%l = GMM_NULL_LAYOUT 
    endif
    nullify(p)
    key= GMM_KEY_NOT_FOUND
    gmm_get281 = GMM_VAR_NOT_FOUND
  else
    m2%l=directory(cur_page)%entry(cur_entry)%l
    m2%a=directory(cur_page)%entry(cur_entry)%a
    if (present(m)) m=m2                           
    p=>gmm_ptrs281(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
    do i=1,4
!      print *,'DEBUG gmm_get m%l(',i,')%n=',m2%l(i)%n
      if (m2%l(i)%n /= 0) array_rank=i
    enddo
!    write(6,'(a,a,a,i2,a,i2)') 'DEBUG gmm_get iname=',iname,' DIM=',DIM,' array_rank=',array_rank
    if (array_rank /= 1) then
       nullify(p)
       if (present(m)) m = GMM_NULL_METADATA
       gmm_get281 = GMM_INCONSISTENT_DIMS
!       print *,'DEBUG gmm_get *** GMM_INCONSISTENT_DIMS ***'
    else
       gmm_get281 = GMM_OK
    endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_get name=',iname,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
   endif
  end function gmm_get281
!
  integer function gmm_get241(iname,p,m)
  use gmm_internals
  use pointer_table_data_241
  implicit none
   integer :: i, array_rank
  character(len=*), intent(in) :: iname               
  real*4, pointer  :: p(:)
  type(gmm_metadata), optional, intent(out) :: m               
!  integer,intent(inout) :: reqid
  include 'gmm_directory_interface.inc'
  type(gmm_metadata) :: m2
  integer*8 :: key
      integer *8 get_address_from
      external get_address_from
  key = 0
  call check_directory_entry(iname,key)
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then  
    call find_directory_entry(iname,key)
  endif
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then   
    if (present(m)) then
      m%a = GMM_NULL_ATTRIB 
      m%l = GMM_NULL_LAYOUT 
    endif
    nullify(p)
    key= GMM_KEY_NOT_FOUND
    gmm_get241 = GMM_VAR_NOT_FOUND
  else
    m2%l=directory(cur_page)%entry(cur_entry)%l
    m2%a=directory(cur_page)%entry(cur_entry)%a
    if (present(m)) m=m2                           
    p=>gmm_ptrs241(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
    do i=1,4
!      print *,'DEBUG gmm_get m%l(',i,')%n=',m2%l(i)%n
      if (m2%l(i)%n /= 0) array_rank=i
    enddo
!    write(6,'(a,a,a,i2,a,i2)') 'DEBUG gmm_get iname=',iname,' DIM=',DIM,' array_rank=',array_rank
    if (array_rank /= 1) then
       nullify(p)
       if (present(m)) m = GMM_NULL_METADATA
       gmm_get241 = GMM_INCONSISTENT_DIMS
!       print *,'DEBUG gmm_get *** GMM_INCONSISTENT_DIMS ***'
    else
       gmm_get241 = GMM_OK
    endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_get name=',iname,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
   endif
  end function gmm_get241
!
  integer function gmm_get381(iname,p,m)
  use gmm_internals
  use pointer_table_data_381
  implicit none
   integer :: i, array_rank
  character(len=*), intent(in) :: iname               
  complex*8, pointer  :: p(:)
  type(gmm_metadata), optional, intent(out) :: m               
!  integer,intent(inout) :: reqid
  include 'gmm_directory_interface.inc'
  type(gmm_metadata) :: m2
  integer*8 :: key
      integer *8 get_address_from
      external get_address_from
  key = 0
  call check_directory_entry(iname,key)
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then  
    call find_directory_entry(iname,key)
  endif
  if(cur_page .eq. 0 .or. cur_entry .eq. 0) then   
    if (present(m)) then
      m%a = GMM_NULL_ATTRIB 
      m%l = GMM_NULL_LAYOUT 
    endif
    nullify(p)
    key= GMM_KEY_NOT_FOUND
    gmm_get381 = GMM_VAR_NOT_FOUND
  else
    m2%l=directory(cur_page)%entry(cur_entry)%l
    m2%a=directory(cur_page)%entry(cur_entry)%a
    if (present(m)) m=m2                           
    p=>gmm_ptrs381(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
    do i=1,4
!      print *,'DEBUG gmm_get m%l(',i,')%n=',m2%l(i)%n
      if (m2%l(i)%n /= 0) array_rank=i
    enddo
!    write(6,'(a,a,a,i2,a,i2)') 'DEBUG gmm_get iname=',iname,' DIM=',DIM,' array_rank=',array_rank
    if (array_rank /= 1) then
       nullify(p)
       if (present(m)) m = GMM_NULL_METADATA
       gmm_get381 = GMM_INCONSISTENT_DIMS
!       print *,'DEBUG gmm_get *** GMM_INCONSISTENT_DIMS ***'
    else
       gmm_get381 = GMM_OK
    endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_get name=',iname,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
   endif
  end function gmm_get381
!
  integer function gmm_getmeta2(iname,m)
      type gmm_layout                              
         SEQUENCE
         integer :: low,high,halo,halomax,n      
      end type
      type gmm_attributes
        SEQUENCE
        integer*8 :: key          
        integer*8 :: uuid1, uuid2 
        integer   :: initmode                   
        integer   :: flags                      
      end type
      type gmm_metadata
        SEQUENCE
        type(gmm_layout), dimension(4) :: l
        type(gmm_attributes) :: a
      end type
    character(len=*), intent(in) :: iname               
    type(gmm_metadata), intent(out) :: m               
   integer gmm_getmeta
   external gmm_getmeta
   gmm_getmeta2 = gmm_getmeta(iname, m)
  end function gmm_getmeta2
