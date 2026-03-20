  module pointer_table_data_184
  use gmm_internals
  implicit none
  save
    type gmm_p_184
    integer*8, pointer :: p(:,:,:,:)
    integer*8 key
    end type
  type (gmm_p_184) , dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs184
   integer :: gmm_p_used=0                
   integer :: gmm_p_table_size=0          
   integer :: gmm_p_cur_page=0            
   integer :: gmm_p_cur_entry=0           
   integer :: gmm_p_last_entry=MAX_PAGES * PAGE_SIZE  
   integer :: gmm_p_file_unit=0
   logical :: gmm_p_restart_mode=.false.
   integer :: gmm_p_ordinal=0             
  contains
  integer function add_table_entry(p, key)
    integer*8, pointer :: p(:,:,:,:)
    integer*8, intent(in) :: key
    gmm_ptrs184(gmm_p_table_size)%p => p
    gmm_ptrs184(gmm_p_table_size)%key = key
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
    print *, 'add_table_entry', ' of', ' gmm_ptrs184 ' , gmm_p_table_size
    endif
    add_table_entry = 0
    return
  end function add_table_entry
  integer function lgmm_get_nxt_avail_ptr()
    lgmm_get_nxt_avail_ptr = gmm_p_table_size + 1
    gmm_p_table_size = gmm_p_table_size + 1
    return
  end function lgmm_get_nxt_avail_ptr
  integer function update_table_entry(indx, key)
    integer*8, pointer :: p(:,:,:,:)
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
    if (indx > gmm_p_table_size) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry : wrong index', indx, gmm_p_table_size
         endif
      update_table_entry = GMM_POINTER_TABLE_OVERFLOW
    endif
!    Cat(gmm_ptrs, EXTENSION,)(indx)%p => p
    gmm_ptrs184(indx)%key = key
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry', 'of', indx
        endif
    update_table_entry = 0
    return
  end function update_table_entry
  end module pointer_table_data_184
  module pointer_table_data_144
  use gmm_internals
  implicit none
  save
    type gmm_p_144
    integer*4, pointer :: p(:,:,:,:)
    integer*8 key
    end type
  type (gmm_p_144) , dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs144
   integer :: gmm_p_used=0                
   integer :: gmm_p_table_size=0          
   integer :: gmm_p_cur_page=0            
   integer :: gmm_p_cur_entry=0           
   integer :: gmm_p_last_entry=MAX_PAGES * PAGE_SIZE  
   integer :: gmm_p_file_unit=0
   logical :: gmm_p_restart_mode=.false.
   integer :: gmm_p_ordinal=0             
  contains
  integer function add_table_entry(p, key)
    integer*4, pointer :: p(:,:,:,:)
    integer*8, intent(in) :: key
    gmm_ptrs144(gmm_p_table_size)%p => p
    gmm_ptrs144(gmm_p_table_size)%key = key
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
    print *, 'add_table_entry', ' of', ' gmm_ptrs144 ' , gmm_p_table_size
    endif
    add_table_entry = 0
    return
  end function add_table_entry
  integer function lgmm_get_nxt_avail_ptr()
    lgmm_get_nxt_avail_ptr = gmm_p_table_size + 1
    gmm_p_table_size = gmm_p_table_size + 1
    return
  end function lgmm_get_nxt_avail_ptr
  integer function update_table_entry(indx, key)
    integer*4, pointer :: p(:,:,:,:)
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
    if (indx > gmm_p_table_size) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry : wrong index', indx, gmm_p_table_size
         endif
      update_table_entry = GMM_POINTER_TABLE_OVERFLOW
    endif
!    Cat(gmm_ptrs, EXTENSION,)(indx)%p => p
    gmm_ptrs144(indx)%key = key
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry', 'of', indx
        endif
    update_table_entry = 0
    return
  end function update_table_entry
  end module pointer_table_data_144
  module pointer_table_data_284
  use gmm_internals
  implicit none
  save
    type gmm_p_284
    real*8, pointer :: p(:,:,:,:)
    integer*8 key
    end type
  type (gmm_p_284) , dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs284
   integer :: gmm_p_used=0                
   integer :: gmm_p_table_size=0          
   integer :: gmm_p_cur_page=0            
   integer :: gmm_p_cur_entry=0           
   integer :: gmm_p_last_entry=MAX_PAGES * PAGE_SIZE  
   integer :: gmm_p_file_unit=0
   logical :: gmm_p_restart_mode=.false.
   integer :: gmm_p_ordinal=0             
  contains
  integer function add_table_entry(p, key)
    real*8, pointer :: p(:,:,:,:)
    integer*8, intent(in) :: key
    gmm_ptrs284(gmm_p_table_size)%p => p
    gmm_ptrs284(gmm_p_table_size)%key = key
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
    print *, 'add_table_entry', ' of', ' gmm_ptrs284 ' , gmm_p_table_size
    endif
    add_table_entry = 0
    return
  end function add_table_entry
  integer function lgmm_get_nxt_avail_ptr()
    lgmm_get_nxt_avail_ptr = gmm_p_table_size + 1
    gmm_p_table_size = gmm_p_table_size + 1
    return
  end function lgmm_get_nxt_avail_ptr
  integer function update_table_entry(indx, key)
    real*8, pointer :: p(:,:,:,:)
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
    if (indx > gmm_p_table_size) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry : wrong index', indx, gmm_p_table_size
         endif
      update_table_entry = GMM_POINTER_TABLE_OVERFLOW
    endif
!    Cat(gmm_ptrs, EXTENSION,)(indx)%p => p
    gmm_ptrs284(indx)%key = key
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry', 'of', indx
        endif
    update_table_entry = 0
    return
  end function update_table_entry
  end module pointer_table_data_284
  module pointer_table_data_244
  use gmm_internals
  implicit none
  save
    type gmm_p_244
    real*4, pointer :: p(:,:,:,:)
    integer*8 key
    end type
  type (gmm_p_244) , dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs244
   integer :: gmm_p_used=0                
   integer :: gmm_p_table_size=0          
   integer :: gmm_p_cur_page=0            
   integer :: gmm_p_cur_entry=0           
   integer :: gmm_p_last_entry=MAX_PAGES * PAGE_SIZE  
   integer :: gmm_p_file_unit=0
   logical :: gmm_p_restart_mode=.false.
   integer :: gmm_p_ordinal=0             
  contains
  integer function add_table_entry(p, key)
    real*4, pointer :: p(:,:,:,:)
    integer*8, intent(in) :: key
    gmm_ptrs244(gmm_p_table_size)%p => p
    gmm_ptrs244(gmm_p_table_size)%key = key
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
    print *, 'add_table_entry', ' of', ' gmm_ptrs244 ' , gmm_p_table_size
    endif
    add_table_entry = 0
    return
  end function add_table_entry
  integer function lgmm_get_nxt_avail_ptr()
    lgmm_get_nxt_avail_ptr = gmm_p_table_size + 1
    gmm_p_table_size = gmm_p_table_size + 1
    return
  end function lgmm_get_nxt_avail_ptr
  integer function update_table_entry(indx, key)
    real*4, pointer :: p(:,:,:,:)
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
    if (indx > gmm_p_table_size) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry : wrong index', indx, gmm_p_table_size
         endif
      update_table_entry = GMM_POINTER_TABLE_OVERFLOW
    endif
!    Cat(gmm_ptrs, EXTENSION,)(indx)%p => p
    gmm_ptrs244(indx)%key = key
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry', 'of', indx
        endif
    update_table_entry = 0
    return
  end function update_table_entry
  end module pointer_table_data_244
  module pointer_table_data_384
  use gmm_internals
  implicit none
  save
    type gmm_p_384
    complex*8, pointer :: p(:,:,:,:)
    integer*8 key
    end type
  type (gmm_p_384) , dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs384
   integer :: gmm_p_used=0                
   integer :: gmm_p_table_size=0          
   integer :: gmm_p_cur_page=0            
   integer :: gmm_p_cur_entry=0           
   integer :: gmm_p_last_entry=MAX_PAGES * PAGE_SIZE  
   integer :: gmm_p_file_unit=0
   logical :: gmm_p_restart_mode=.false.
   integer :: gmm_p_ordinal=0             
  contains
  integer function add_table_entry(p, key)
    complex*8, pointer :: p(:,:,:,:)
    integer*8, intent(in) :: key
    gmm_ptrs384(gmm_p_table_size)%p => p
    gmm_ptrs384(gmm_p_table_size)%key = key
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
    print *, 'add_table_entry', ' of', ' gmm_ptrs384 ' , gmm_p_table_size
    endif
    add_table_entry = 0
    return
  end function add_table_entry
  integer function lgmm_get_nxt_avail_ptr()
    lgmm_get_nxt_avail_ptr = gmm_p_table_size + 1
    gmm_p_table_size = gmm_p_table_size + 1
    return
  end function lgmm_get_nxt_avail_ptr
  integer function update_table_entry(indx, key)
    complex*8, pointer :: p(:,:,:,:)
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
    if (indx > gmm_p_table_size) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry : wrong index', indx, gmm_p_table_size
         endif
      update_table_entry = GMM_POINTER_TABLE_OVERFLOW
    endif
!    Cat(gmm_ptrs, EXTENSION,)(indx)%p => p
    gmm_ptrs384(indx)%key = key
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry', 'of', indx
        endif
    update_table_entry = 0
    return
  end function update_table_entry
  end module pointer_table_data_384
  module pointer_table_data_183
  use gmm_internals
  implicit none
  save
    type gmm_p_183
    integer*8, pointer :: p(:,:,:)
    integer*8 key
    end type
  type (gmm_p_183) , dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs183
   integer :: gmm_p_used=0                
   integer :: gmm_p_table_size=0          
   integer :: gmm_p_cur_page=0            
   integer :: gmm_p_cur_entry=0           
   integer :: gmm_p_last_entry=MAX_PAGES * PAGE_SIZE  
   integer :: gmm_p_file_unit=0
   logical :: gmm_p_restart_mode=.false.
   integer :: gmm_p_ordinal=0             
  contains
  integer function add_table_entry(p, key)
    integer*8, pointer :: p(:,:,:)
    integer*8, intent(in) :: key
    gmm_ptrs183(gmm_p_table_size)%p => p
    gmm_ptrs183(gmm_p_table_size)%key = key
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
    print *, 'add_table_entry', ' of', ' gmm_ptrs183 ' , gmm_p_table_size
    endif
    add_table_entry = 0
    return
  end function add_table_entry
  integer function lgmm_get_nxt_avail_ptr()
    lgmm_get_nxt_avail_ptr = gmm_p_table_size + 1
    gmm_p_table_size = gmm_p_table_size + 1
    return
  end function lgmm_get_nxt_avail_ptr
  integer function update_table_entry(indx, key)
    integer*8, pointer :: p(:,:,:)
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
    if (indx > gmm_p_table_size) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry : wrong index', indx, gmm_p_table_size
         endif
      update_table_entry = GMM_POINTER_TABLE_OVERFLOW
    endif
!    Cat(gmm_ptrs, EXTENSION,)(indx)%p => p
    gmm_ptrs183(indx)%key = key
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry', 'of', indx
        endif
    update_table_entry = 0
    return
  end function update_table_entry
  end module pointer_table_data_183
  module pointer_table_data_143
  use gmm_internals
  implicit none
  save
    type gmm_p_143
    integer*4, pointer :: p(:,:,:)
    integer*8 key
    end type
  type (gmm_p_143) , dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs143
   integer :: gmm_p_used=0                
   integer :: gmm_p_table_size=0          
   integer :: gmm_p_cur_page=0            
   integer :: gmm_p_cur_entry=0           
   integer :: gmm_p_last_entry=MAX_PAGES * PAGE_SIZE  
   integer :: gmm_p_file_unit=0
   logical :: gmm_p_restart_mode=.false.
   integer :: gmm_p_ordinal=0             
  contains
  integer function add_table_entry(p, key)
    integer*4, pointer :: p(:,:,:)
    integer*8, intent(in) :: key
    gmm_ptrs143(gmm_p_table_size)%p => p
    gmm_ptrs143(gmm_p_table_size)%key = key
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
    print *, 'add_table_entry', ' of', ' gmm_ptrs143 ' , gmm_p_table_size
    endif
    add_table_entry = 0
    return
  end function add_table_entry
  integer function lgmm_get_nxt_avail_ptr()
    lgmm_get_nxt_avail_ptr = gmm_p_table_size + 1
    gmm_p_table_size = gmm_p_table_size + 1
    return
  end function lgmm_get_nxt_avail_ptr
  integer function update_table_entry(indx, key)
    integer*4, pointer :: p(:,:,:)
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
    if (indx > gmm_p_table_size) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry : wrong index', indx, gmm_p_table_size
         endif
      update_table_entry = GMM_POINTER_TABLE_OVERFLOW
    endif
!    Cat(gmm_ptrs, EXTENSION,)(indx)%p => p
    gmm_ptrs143(indx)%key = key
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry', 'of', indx
        endif
    update_table_entry = 0
    return
  end function update_table_entry
  end module pointer_table_data_143
  module pointer_table_data_283
  use gmm_internals
  implicit none
  save
    type gmm_p_283
    real*8, pointer :: p(:,:,:)
    integer*8 key
    end type
  type (gmm_p_283) , dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs283
   integer :: gmm_p_used=0                
   integer :: gmm_p_table_size=0          
   integer :: gmm_p_cur_page=0            
   integer :: gmm_p_cur_entry=0           
   integer :: gmm_p_last_entry=MAX_PAGES * PAGE_SIZE  
   integer :: gmm_p_file_unit=0
   logical :: gmm_p_restart_mode=.false.
   integer :: gmm_p_ordinal=0             
  contains
  integer function add_table_entry(p, key)
    real*8, pointer :: p(:,:,:)
    integer*8, intent(in) :: key
    gmm_ptrs283(gmm_p_table_size)%p => p
    gmm_ptrs283(gmm_p_table_size)%key = key
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
    print *, 'add_table_entry', ' of', ' gmm_ptrs283 ' , gmm_p_table_size
    endif
    add_table_entry = 0
    return
  end function add_table_entry
  integer function lgmm_get_nxt_avail_ptr()
    lgmm_get_nxt_avail_ptr = gmm_p_table_size + 1
    gmm_p_table_size = gmm_p_table_size + 1
    return
  end function lgmm_get_nxt_avail_ptr
  integer function update_table_entry(indx, key)
    real*8, pointer :: p(:,:,:)
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
    if (indx > gmm_p_table_size) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry : wrong index', indx, gmm_p_table_size
         endif
      update_table_entry = GMM_POINTER_TABLE_OVERFLOW
    endif
!    Cat(gmm_ptrs, EXTENSION,)(indx)%p => p
    gmm_ptrs283(indx)%key = key
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry', 'of', indx
        endif
    update_table_entry = 0
    return
  end function update_table_entry
  end module pointer_table_data_283
  module pointer_table_data_243
  use gmm_internals
  implicit none
  save
    type gmm_p_243
    real*4, pointer :: p(:,:,:)
    integer*8 key
    end type
  type (gmm_p_243) , dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs243
   integer :: gmm_p_used=0                
   integer :: gmm_p_table_size=0          
   integer :: gmm_p_cur_page=0            
   integer :: gmm_p_cur_entry=0           
   integer :: gmm_p_last_entry=MAX_PAGES * PAGE_SIZE  
   integer :: gmm_p_file_unit=0
   logical :: gmm_p_restart_mode=.false.
   integer :: gmm_p_ordinal=0             
  contains
  integer function add_table_entry(p, key)
    real*4, pointer :: p(:,:,:)
    integer*8, intent(in) :: key
    gmm_ptrs243(gmm_p_table_size)%p => p
    gmm_ptrs243(gmm_p_table_size)%key = key
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
    print *, 'add_table_entry', ' of', ' gmm_ptrs243 ' , gmm_p_table_size
    endif
    add_table_entry = 0
    return
  end function add_table_entry
  integer function lgmm_get_nxt_avail_ptr()
    lgmm_get_nxt_avail_ptr = gmm_p_table_size + 1
    gmm_p_table_size = gmm_p_table_size + 1
    return
  end function lgmm_get_nxt_avail_ptr
  integer function update_table_entry(indx, key)
    real*4, pointer :: p(:,:,:)
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
    if (indx > gmm_p_table_size) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry : wrong index', indx, gmm_p_table_size
         endif
      update_table_entry = GMM_POINTER_TABLE_OVERFLOW
    endif
!    Cat(gmm_ptrs, EXTENSION,)(indx)%p => p
    gmm_ptrs243(indx)%key = key
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry', 'of', indx
        endif
    update_table_entry = 0
    return
  end function update_table_entry
  end module pointer_table_data_243
  module pointer_table_data_383
  use gmm_internals
  implicit none
  save
    type gmm_p_383
    complex*8, pointer :: p(:,:,:)
    integer*8 key
    end type
  type (gmm_p_383) , dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs383
   integer :: gmm_p_used=0                
   integer :: gmm_p_table_size=0          
   integer :: gmm_p_cur_page=0            
   integer :: gmm_p_cur_entry=0           
   integer :: gmm_p_last_entry=MAX_PAGES * PAGE_SIZE  
   integer :: gmm_p_file_unit=0
   logical :: gmm_p_restart_mode=.false.
   integer :: gmm_p_ordinal=0             
  contains
  integer function add_table_entry(p, key)
    complex*8, pointer :: p(:,:,:)
    integer*8, intent(in) :: key
    gmm_ptrs383(gmm_p_table_size)%p => p
    gmm_ptrs383(gmm_p_table_size)%key = key
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
    print *, 'add_table_entry', ' of', ' gmm_ptrs383 ' , gmm_p_table_size
    endif
    add_table_entry = 0
    return
  end function add_table_entry
  integer function lgmm_get_nxt_avail_ptr()
    lgmm_get_nxt_avail_ptr = gmm_p_table_size + 1
    gmm_p_table_size = gmm_p_table_size + 1
    return
  end function lgmm_get_nxt_avail_ptr
  integer function update_table_entry(indx, key)
    complex*8, pointer :: p(:,:,:)
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
    if (indx > gmm_p_table_size) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry : wrong index', indx, gmm_p_table_size
         endif
      update_table_entry = GMM_POINTER_TABLE_OVERFLOW
    endif
!    Cat(gmm_ptrs, EXTENSION,)(indx)%p => p
    gmm_ptrs383(indx)%key = key
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry', 'of', indx
        endif
    update_table_entry = 0
    return
  end function update_table_entry
  end module pointer_table_data_383
  module pointer_table_data_182
  use gmm_internals
  implicit none
  save
    type gmm_p_182
    integer*8, pointer :: p(:,:)
    integer*8 key
    end type
  type (gmm_p_182) , dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs182
   integer :: gmm_p_used=0                
   integer :: gmm_p_table_size=0          
   integer :: gmm_p_cur_page=0            
   integer :: gmm_p_cur_entry=0           
   integer :: gmm_p_last_entry=MAX_PAGES * PAGE_SIZE  
   integer :: gmm_p_file_unit=0
   logical :: gmm_p_restart_mode=.false.
   integer :: gmm_p_ordinal=0             
  contains
  integer function add_table_entry(p, key)
    integer*8, pointer :: p(:,:)
    integer*8, intent(in) :: key
    gmm_ptrs182(gmm_p_table_size)%p => p
    gmm_ptrs182(gmm_p_table_size)%key = key
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
    print *, 'add_table_entry', ' of', ' gmm_ptrs182 ' , gmm_p_table_size
    endif
    add_table_entry = 0
    return
  end function add_table_entry
  integer function lgmm_get_nxt_avail_ptr()
    lgmm_get_nxt_avail_ptr = gmm_p_table_size + 1
    gmm_p_table_size = gmm_p_table_size + 1
    return
  end function lgmm_get_nxt_avail_ptr
  integer function update_table_entry(indx, key)
    integer*8, pointer :: p(:,:)
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
    if (indx > gmm_p_table_size) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry : wrong index', indx, gmm_p_table_size
         endif
      update_table_entry = GMM_POINTER_TABLE_OVERFLOW
    endif
!    Cat(gmm_ptrs, EXTENSION,)(indx)%p => p
    gmm_ptrs182(indx)%key = key
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry', 'of', indx
        endif
    update_table_entry = 0
    return
  end function update_table_entry
  end module pointer_table_data_182
  module pointer_table_data_142
  use gmm_internals
  implicit none
  save
    type gmm_p_142
    integer*4, pointer :: p(:,:)
    integer*8 key
    end type
  type (gmm_p_142) , dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs142
   integer :: gmm_p_used=0                
   integer :: gmm_p_table_size=0          
   integer :: gmm_p_cur_page=0            
   integer :: gmm_p_cur_entry=0           
   integer :: gmm_p_last_entry=MAX_PAGES * PAGE_SIZE  
   integer :: gmm_p_file_unit=0
   logical :: gmm_p_restart_mode=.false.
   integer :: gmm_p_ordinal=0             
  contains
  integer function add_table_entry(p, key)
    integer*4, pointer :: p(:,:)
    integer*8, intent(in) :: key
    gmm_ptrs142(gmm_p_table_size)%p => p
    gmm_ptrs142(gmm_p_table_size)%key = key
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
    print *, 'add_table_entry', ' of', ' gmm_ptrs142 ' , gmm_p_table_size
    endif
    add_table_entry = 0
    return
  end function add_table_entry
  integer function lgmm_get_nxt_avail_ptr()
    lgmm_get_nxt_avail_ptr = gmm_p_table_size + 1
    gmm_p_table_size = gmm_p_table_size + 1
    return
  end function lgmm_get_nxt_avail_ptr
  integer function update_table_entry(indx, key)
    integer*4, pointer :: p(:,:)
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
    if (indx > gmm_p_table_size) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry : wrong index', indx, gmm_p_table_size
         endif
      update_table_entry = GMM_POINTER_TABLE_OVERFLOW
    endif
!    Cat(gmm_ptrs, EXTENSION,)(indx)%p => p
    gmm_ptrs142(indx)%key = key
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry', 'of', indx
        endif
    update_table_entry = 0
    return
  end function update_table_entry
  end module pointer_table_data_142
  module pointer_table_data_282
  use gmm_internals
  implicit none
  save
    type gmm_p_282
    real*8, pointer :: p(:,:)
    integer*8 key
    end type
  type (gmm_p_282) , dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs282
   integer :: gmm_p_used=0                
   integer :: gmm_p_table_size=0          
   integer :: gmm_p_cur_page=0            
   integer :: gmm_p_cur_entry=0           
   integer :: gmm_p_last_entry=MAX_PAGES * PAGE_SIZE  
   integer :: gmm_p_file_unit=0
   logical :: gmm_p_restart_mode=.false.
   integer :: gmm_p_ordinal=0             
  contains
  integer function add_table_entry(p, key)
    real*8, pointer :: p(:,:)
    integer*8, intent(in) :: key
    gmm_ptrs282(gmm_p_table_size)%p => p
    gmm_ptrs282(gmm_p_table_size)%key = key
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
    print *, 'add_table_entry', ' of', ' gmm_ptrs282 ' , gmm_p_table_size
    endif
    add_table_entry = 0
    return
  end function add_table_entry
  integer function lgmm_get_nxt_avail_ptr()
    lgmm_get_nxt_avail_ptr = gmm_p_table_size + 1
    gmm_p_table_size = gmm_p_table_size + 1
    return
  end function lgmm_get_nxt_avail_ptr
  integer function update_table_entry(indx, key)
    real*8, pointer :: p(:,:)
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
    if (indx > gmm_p_table_size) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry : wrong index', indx, gmm_p_table_size
         endif
      update_table_entry = GMM_POINTER_TABLE_OVERFLOW
    endif
!    Cat(gmm_ptrs, EXTENSION,)(indx)%p => p
    gmm_ptrs282(indx)%key = key
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry', 'of', indx
        endif
    update_table_entry = 0
    return
  end function update_table_entry
  end module pointer_table_data_282
  module pointer_table_data_242
  use gmm_internals
  implicit none
  save
    type gmm_p_242
    real*4, pointer :: p(:,:)
    integer*8 key
    end type
  type (gmm_p_242) , dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs242
   integer :: gmm_p_used=0                
   integer :: gmm_p_table_size=0          
   integer :: gmm_p_cur_page=0            
   integer :: gmm_p_cur_entry=0           
   integer :: gmm_p_last_entry=MAX_PAGES * PAGE_SIZE  
   integer :: gmm_p_file_unit=0
   logical :: gmm_p_restart_mode=.false.
   integer :: gmm_p_ordinal=0             
  contains
  integer function add_table_entry(p, key)
    real*4, pointer :: p(:,:)
    integer*8, intent(in) :: key
    gmm_ptrs242(gmm_p_table_size)%p => p
    gmm_ptrs242(gmm_p_table_size)%key = key
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
    print *, 'add_table_entry', ' of', ' gmm_ptrs242 ' , gmm_p_table_size
    endif
    add_table_entry = 0
    return
  end function add_table_entry
  integer function lgmm_get_nxt_avail_ptr()
    lgmm_get_nxt_avail_ptr = gmm_p_table_size + 1
    gmm_p_table_size = gmm_p_table_size + 1
    return
  end function lgmm_get_nxt_avail_ptr
  integer function update_table_entry(indx, key)
    real*4, pointer :: p(:,:)
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
    if (indx > gmm_p_table_size) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry : wrong index', indx, gmm_p_table_size
         endif
      update_table_entry = GMM_POINTER_TABLE_OVERFLOW
    endif
!    Cat(gmm_ptrs, EXTENSION,)(indx)%p => p
    gmm_ptrs242(indx)%key = key
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry', 'of', indx
        endif
    update_table_entry = 0
    return
  end function update_table_entry
  end module pointer_table_data_242
  module pointer_table_data_382
  use gmm_internals
  implicit none
  save
    type gmm_p_382
    complex*8, pointer :: p(:,:)
    integer*8 key
    end type
  type (gmm_p_382) , dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs382
   integer :: gmm_p_used=0                
   integer :: gmm_p_table_size=0          
   integer :: gmm_p_cur_page=0            
   integer :: gmm_p_cur_entry=0           
   integer :: gmm_p_last_entry=MAX_PAGES * PAGE_SIZE  
   integer :: gmm_p_file_unit=0
   logical :: gmm_p_restart_mode=.false.
   integer :: gmm_p_ordinal=0             
  contains
  integer function add_table_entry(p, key)
    complex*8, pointer :: p(:,:)
    integer*8, intent(in) :: key
    gmm_ptrs382(gmm_p_table_size)%p => p
    gmm_ptrs382(gmm_p_table_size)%key = key
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
    print *, 'add_table_entry', ' of', ' gmm_ptrs382 ' , gmm_p_table_size
    endif
    add_table_entry = 0
    return
  end function add_table_entry
  integer function lgmm_get_nxt_avail_ptr()
    lgmm_get_nxt_avail_ptr = gmm_p_table_size + 1
    gmm_p_table_size = gmm_p_table_size + 1
    return
  end function lgmm_get_nxt_avail_ptr
  integer function update_table_entry(indx, key)
    complex*8, pointer :: p(:,:)
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
    if (indx > gmm_p_table_size) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry : wrong index', indx, gmm_p_table_size
         endif
      update_table_entry = GMM_POINTER_TABLE_OVERFLOW
    endif
!    Cat(gmm_ptrs, EXTENSION,)(indx)%p => p
    gmm_ptrs382(indx)%key = key
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry', 'of', indx
        endif
    update_table_entry = 0
    return
  end function update_table_entry
  end module pointer_table_data_382
  module pointer_table_data_181
  use gmm_internals
  implicit none
  save
    type gmm_p_181
    integer*8, pointer :: p(:)
    integer*8 key
    end type
  type (gmm_p_181) , dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs181
   integer :: gmm_p_used=0                
   integer :: gmm_p_table_size=0          
   integer :: gmm_p_cur_page=0            
   integer :: gmm_p_cur_entry=0           
   integer :: gmm_p_last_entry=MAX_PAGES * PAGE_SIZE  
   integer :: gmm_p_file_unit=0
   logical :: gmm_p_restart_mode=.false.
   integer :: gmm_p_ordinal=0             
  contains
  integer function add_table_entry(p, key)
    integer*8, pointer :: p(:)
    integer*8, intent(in) :: key
    gmm_ptrs181(gmm_p_table_size)%p => p
    gmm_ptrs181(gmm_p_table_size)%key = key
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
    print *, 'add_table_entry', ' of', ' gmm_ptrs181 ' , gmm_p_table_size
    endif
    add_table_entry = 0
    return
  end function add_table_entry
  integer function lgmm_get_nxt_avail_ptr()
    lgmm_get_nxt_avail_ptr = gmm_p_table_size + 1
    gmm_p_table_size = gmm_p_table_size + 1
    return
  end function lgmm_get_nxt_avail_ptr
  integer function update_table_entry(indx, key)
    integer*8, pointer :: p(:)
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
    if (indx > gmm_p_table_size) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry : wrong index', indx, gmm_p_table_size
         endif
      update_table_entry = GMM_POINTER_TABLE_OVERFLOW
    endif
!    Cat(gmm_ptrs, EXTENSION,)(indx)%p => p
    gmm_ptrs181(indx)%key = key
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry', 'of', indx
        endif
    update_table_entry = 0
    return
  end function update_table_entry
  end module pointer_table_data_181
  module pointer_table_data_141
  use gmm_internals
  implicit none
  save
    type gmm_p_141
    integer*4, pointer :: p(:)
    integer*8 key
    end type
  type (gmm_p_141) , dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs141
   integer :: gmm_p_used=0                
   integer :: gmm_p_table_size=0          
   integer :: gmm_p_cur_page=0            
   integer :: gmm_p_cur_entry=0           
   integer :: gmm_p_last_entry=MAX_PAGES * PAGE_SIZE  
   integer :: gmm_p_file_unit=0
   logical :: gmm_p_restart_mode=.false.
   integer :: gmm_p_ordinal=0             
  contains
  integer function add_table_entry(p, key)
    integer*4, pointer :: p(:)
    integer*8, intent(in) :: key
    gmm_ptrs141(gmm_p_table_size)%p => p
    gmm_ptrs141(gmm_p_table_size)%key = key
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
    print *, 'add_table_entry', ' of', ' gmm_ptrs141 ' , gmm_p_table_size
    endif
    add_table_entry = 0
    return
  end function add_table_entry
  integer function lgmm_get_nxt_avail_ptr()
    lgmm_get_nxt_avail_ptr = gmm_p_table_size + 1
    gmm_p_table_size = gmm_p_table_size + 1
    return
  end function lgmm_get_nxt_avail_ptr
  integer function update_table_entry(indx, key)
    integer*4, pointer :: p(:)
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
    if (indx > gmm_p_table_size) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry : wrong index', indx, gmm_p_table_size
         endif
      update_table_entry = GMM_POINTER_TABLE_OVERFLOW
    endif
!    Cat(gmm_ptrs, EXTENSION,)(indx)%p => p
    gmm_ptrs141(indx)%key = key
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry', 'of', indx
        endif
    update_table_entry = 0
    return
  end function update_table_entry
  end module pointer_table_data_141
  module pointer_table_data_281
  use gmm_internals
  implicit none
  save
    type gmm_p_281
    real*8, pointer :: p(:)
    integer*8 key
    end type
  type (gmm_p_281) , dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs281
   integer :: gmm_p_used=0                
   integer :: gmm_p_table_size=0          
   integer :: gmm_p_cur_page=0            
   integer :: gmm_p_cur_entry=0           
   integer :: gmm_p_last_entry=MAX_PAGES * PAGE_SIZE  
   integer :: gmm_p_file_unit=0
   logical :: gmm_p_restart_mode=.false.
   integer :: gmm_p_ordinal=0             
  contains
  integer function add_table_entry(p, key)
    real*8, pointer :: p(:)
    integer*8, intent(in) :: key
    gmm_ptrs281(gmm_p_table_size)%p => p
    gmm_ptrs281(gmm_p_table_size)%key = key
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
    print *, 'add_table_entry', ' of', ' gmm_ptrs281 ' , gmm_p_table_size
    endif
    add_table_entry = 0
    return
  end function add_table_entry
  integer function lgmm_get_nxt_avail_ptr()
    lgmm_get_nxt_avail_ptr = gmm_p_table_size + 1
    gmm_p_table_size = gmm_p_table_size + 1
    return
  end function lgmm_get_nxt_avail_ptr
  integer function update_table_entry(indx, key)
    real*8, pointer :: p(:)
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
    if (indx > gmm_p_table_size) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry : wrong index', indx, gmm_p_table_size
         endif
      update_table_entry = GMM_POINTER_TABLE_OVERFLOW
    endif
!    Cat(gmm_ptrs, EXTENSION,)(indx)%p => p
    gmm_ptrs281(indx)%key = key
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry', 'of', indx
        endif
    update_table_entry = 0
    return
  end function update_table_entry
  end module pointer_table_data_281
  module pointer_table_data_241
  use gmm_internals
  implicit none
  save
    type gmm_p_241
    real*4, pointer :: p(:)
    integer*8 key
    end type
  type (gmm_p_241) , dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs241
   integer :: gmm_p_used=0                
   integer :: gmm_p_table_size=0          
   integer :: gmm_p_cur_page=0            
   integer :: gmm_p_cur_entry=0           
   integer :: gmm_p_last_entry=MAX_PAGES * PAGE_SIZE  
   integer :: gmm_p_file_unit=0
   logical :: gmm_p_restart_mode=.false.
   integer :: gmm_p_ordinal=0             
  contains
  integer function add_table_entry(p, key)
    real*4, pointer :: p(:)
    integer*8, intent(in) :: key
    gmm_ptrs241(gmm_p_table_size)%p => p
    gmm_ptrs241(gmm_p_table_size)%key = key
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
    print *, 'add_table_entry', ' of', ' gmm_ptrs241 ' , gmm_p_table_size
    endif
    add_table_entry = 0
    return
  end function add_table_entry
  integer function lgmm_get_nxt_avail_ptr()
    lgmm_get_nxt_avail_ptr = gmm_p_table_size + 1
    gmm_p_table_size = gmm_p_table_size + 1
    return
  end function lgmm_get_nxt_avail_ptr
  integer function update_table_entry(indx, key)
    real*4, pointer :: p(:)
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
    if (indx > gmm_p_table_size) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry : wrong index', indx, gmm_p_table_size
         endif
      update_table_entry = GMM_POINTER_TABLE_OVERFLOW
    endif
!    Cat(gmm_ptrs, EXTENSION,)(indx)%p => p
    gmm_ptrs241(indx)%key = key
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry', 'of', indx
        endif
    update_table_entry = 0
    return
  end function update_table_entry
  end module pointer_table_data_241
  module pointer_table_data_381
  use gmm_internals
  implicit none
  save
    type gmm_p_381
    complex*8, pointer :: p(:)
    integer*8 key
    end type
  type (gmm_p_381) , dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs381
   integer :: gmm_p_used=0                
   integer :: gmm_p_table_size=0          
   integer :: gmm_p_cur_page=0            
   integer :: gmm_p_cur_entry=0           
   integer :: gmm_p_last_entry=MAX_PAGES * PAGE_SIZE  
   integer :: gmm_p_file_unit=0
   logical :: gmm_p_restart_mode=.false.
   integer :: gmm_p_ordinal=0             
  contains
  integer function add_table_entry(p, key)
    complex*8, pointer :: p(:)
    integer*8, intent(in) :: key
    gmm_ptrs381(gmm_p_table_size)%p => p
    gmm_ptrs381(gmm_p_table_size)%key = key
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
    print *, 'add_table_entry', ' of', ' gmm_ptrs381 ' , gmm_p_table_size
    endif
    add_table_entry = 0
    return
  end function add_table_entry
  integer function lgmm_get_nxt_avail_ptr()
    lgmm_get_nxt_avail_ptr = gmm_p_table_size + 1
    gmm_p_table_size = gmm_p_table_size + 1
    return
  end function lgmm_get_nxt_avail_ptr
  integer function update_table_entry(indx, key)
    complex*8, pointer :: p(:)
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
    if (indx > gmm_p_table_size) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry : wrong index', indx, gmm_p_table_size
         endif
      update_table_entry = GMM_POINTER_TABLE_OVERFLOW
    endif
!    Cat(gmm_ptrs, EXTENSION,)(indx)%p => p
    gmm_ptrs381(indx)%key = key
        if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *, 'update_table_entry', 'of', indx
        endif
    update_table_entry = 0
    return
  end function update_table_entry
  end module pointer_table_data_381
