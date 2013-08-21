!!===================== gmm_create (interface) =====================
!
!!===================== gmm_create (interface) =====================
!
  integer function gmm_update_tpi_key2(indx,datatype, key)
    implicit none
    integer, intent(in) :: indx, datatype
    integer*8, intent(in) :: key
    integer ier
      integer gmm_update_tpi_key184
      integer gmm_update_tpi_key144
      integer gmm_update_tpi_key284
      integer gmm_update_tpi_key244
      integer gmm_update_tpi_key384
      integer gmm_update_tpi_key183
      integer gmm_update_tpi_key143
      integer gmm_update_tpi_key283
      integer gmm_update_tpi_key243
      integer gmm_update_tpi_key383
      integer gmm_update_tpi_key182
      integer gmm_update_tpi_key142
      integer gmm_update_tpi_key282
      integer gmm_update_tpi_key242
      integer gmm_update_tpi_key382
      integer gmm_update_tpi_key181
      integer gmm_update_tpi_key141
      integer gmm_update_tpi_key281
      integer gmm_update_tpi_key241
      integer gmm_update_tpi_key381
    dtype: select case (datatype)
    case (184)
      gmm_update_tpi_key2 = gmm_update_tpi_key184(indx, key)
    case (144)
      gmm_update_tpi_key2 = gmm_update_tpi_key144(indx, key)
    case (284)
      gmm_update_tpi_key2 = gmm_update_tpi_key284(indx, key)
    case (244)
      gmm_update_tpi_key2 = gmm_update_tpi_key244(indx, key)
    case (384)
      gmm_update_tpi_key2 = gmm_update_tpi_key384(indx, key)
    case (183)
      gmm_update_tpi_key2 = gmm_update_tpi_key183(indx, key)
    case (143)
      gmm_update_tpi_key2 = gmm_update_tpi_key143(indx, key)
    case (283)
      gmm_update_tpi_key2 = gmm_update_tpi_key283(indx, key)
    case (243)
      gmm_update_tpi_key2 = gmm_update_tpi_key243(indx, key)
    case (383)
      gmm_update_tpi_key2 = gmm_update_tpi_key383(indx, key)
    case (182)
      gmm_update_tpi_key2 = gmm_update_tpi_key182(indx, key)
    case (142)
      gmm_update_tpi_key2 = gmm_update_tpi_key142(indx, key)
    case (282)
      gmm_update_tpi_key2 = gmm_update_tpi_key282(indx, key)
    case (242)
      gmm_update_tpi_key2 = gmm_update_tpi_key242(indx, key)
    case (382)
      gmm_update_tpi_key2 = gmm_update_tpi_key382(indx, key)
    case (181)
      gmm_update_tpi_key2 = gmm_update_tpi_key181(indx, key)
    case (141)
      gmm_update_tpi_key2 = gmm_update_tpi_key141(indx, key)
    case (281)
      gmm_update_tpi_key2 = gmm_update_tpi_key281(indx, key)
    case (241)
      gmm_update_tpi_key2 = gmm_update_tpi_key241(indx, key)
    case (381)
      gmm_update_tpi_key2 = gmm_update_tpi_key381(indx, key)
  end select dtype
  end function gmm_update_tpi_key2
!!===================== gmm_create (code) =====================
  integer function gmm_update_tpi_key184(indx, key)
  use gmm_internals
  use pointer_table_data_184
  implicit none
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
  type(gmm_attributes) :: localattr, attrs
  type (gmm_attributes) lcl_attr
  type(gmm_layout), dimension(4) :: lcl_layout, dims
  integer lcl_datatype
  logical consistent
  integer i, ier
  integer lcl_pti
    if (indx > gmm_p_table_size) then
      print *, 'update_table_entry : wrong index', indx,'in table type ', 184, 'of size', gmm_p_table_size
      gmm_update_tpi_key184 = GMM_POINTER_TABLE_OVERFLOW
    endif
    gmm_ptrs184(indx)%key = key
    print *, 'update_table_entry', 'of', indx, 'in table type ', 184, 'of size', gmm_p_table_size
  gmm_update_tpi_key184 = 0
  end function gmm_update_tpi_key184
!
  integer function gmm_update_tpi_key144(indx, key)
  use gmm_internals
  use pointer_table_data_144
  implicit none
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
  type(gmm_attributes) :: localattr, attrs
  type (gmm_attributes) lcl_attr
  type(gmm_layout), dimension(4) :: lcl_layout, dims
  integer lcl_datatype
  logical consistent
  integer i, ier
  integer lcl_pti
    if (indx > gmm_p_table_size) then
      print *, 'update_table_entry : wrong index', indx,'in table type ', 144, 'of size', gmm_p_table_size
      gmm_update_tpi_key144 = GMM_POINTER_TABLE_OVERFLOW
    endif
    gmm_ptrs144(indx)%key = key
    print *, 'update_table_entry', 'of', indx, 'in table type ', 144, 'of size', gmm_p_table_size
  gmm_update_tpi_key144 = 0
  end function gmm_update_tpi_key144
!
  integer function gmm_update_tpi_key284(indx, key)
  use gmm_internals
  use pointer_table_data_284
  implicit none
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
  type(gmm_attributes) :: localattr, attrs
  type (gmm_attributes) lcl_attr
  type(gmm_layout), dimension(4) :: lcl_layout, dims
  integer lcl_datatype
  logical consistent
  integer i, ier
  integer lcl_pti
    if (indx > gmm_p_table_size) then
      print *, 'update_table_entry : wrong index', indx,'in table type ', 284, 'of size', gmm_p_table_size
      gmm_update_tpi_key284 = GMM_POINTER_TABLE_OVERFLOW
    endif
    gmm_ptrs284(indx)%key = key
    print *, 'update_table_entry', 'of', indx, 'in table type ', 284, 'of size', gmm_p_table_size
  gmm_update_tpi_key284 = 0
  end function gmm_update_tpi_key284
!
  integer function gmm_update_tpi_key244(indx, key)
  use gmm_internals
  use pointer_table_data_244
  implicit none
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
  type(gmm_attributes) :: localattr, attrs
  type (gmm_attributes) lcl_attr
  type(gmm_layout), dimension(4) :: lcl_layout, dims
  integer lcl_datatype
  logical consistent
  integer i, ier
  integer lcl_pti
    if (indx > gmm_p_table_size) then
      print *, 'update_table_entry : wrong index', indx,'in table type ', 244, 'of size', gmm_p_table_size
      gmm_update_tpi_key244 = GMM_POINTER_TABLE_OVERFLOW
    endif
    gmm_ptrs244(indx)%key = key
    print *, 'update_table_entry', 'of', indx, 'in table type ', 244, 'of size', gmm_p_table_size
  gmm_update_tpi_key244 = 0
  end function gmm_update_tpi_key244
!
  integer function gmm_update_tpi_key384(indx, key)
  use gmm_internals
  use pointer_table_data_384
  implicit none
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
  type(gmm_attributes) :: localattr, attrs
  type (gmm_attributes) lcl_attr
  type(gmm_layout), dimension(4) :: lcl_layout, dims
  integer lcl_datatype
  logical consistent
  integer i, ier
  integer lcl_pti
    if (indx > gmm_p_table_size) then
      print *, 'update_table_entry : wrong index', indx,'in table type ', 384, 'of size', gmm_p_table_size
      gmm_update_tpi_key384 = GMM_POINTER_TABLE_OVERFLOW
    endif
    gmm_ptrs384(indx)%key = key
    print *, 'update_table_entry', 'of', indx, 'in table type ', 384, 'of size', gmm_p_table_size
  gmm_update_tpi_key384 = 0
  end function gmm_update_tpi_key384
!
  integer function gmm_update_tpi_key183(indx, key)
  use gmm_internals
  use pointer_table_data_183
  implicit none
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
  type(gmm_attributes) :: localattr, attrs
  type (gmm_attributes) lcl_attr
  type(gmm_layout), dimension(4) :: lcl_layout, dims
  integer lcl_datatype
  logical consistent
  integer i, ier
  integer lcl_pti
    if (indx > gmm_p_table_size) then
      print *, 'update_table_entry : wrong index', indx,'in table type ', 183, 'of size', gmm_p_table_size
      gmm_update_tpi_key183 = GMM_POINTER_TABLE_OVERFLOW
    endif
    gmm_ptrs183(indx)%key = key
    print *, 'update_table_entry', 'of', indx, 'in table type ', 183, 'of size', gmm_p_table_size
  gmm_update_tpi_key183 = 0
  end function gmm_update_tpi_key183
!
  integer function gmm_update_tpi_key143(indx, key)
  use gmm_internals
  use pointer_table_data_143
  implicit none
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
  type(gmm_attributes) :: localattr, attrs
  type (gmm_attributes) lcl_attr
  type(gmm_layout), dimension(4) :: lcl_layout, dims
  integer lcl_datatype
  logical consistent
  integer i, ier
  integer lcl_pti
    if (indx > gmm_p_table_size) then
      print *, 'update_table_entry : wrong index', indx,'in table type ', 143, 'of size', gmm_p_table_size
      gmm_update_tpi_key143 = GMM_POINTER_TABLE_OVERFLOW
    endif
    gmm_ptrs143(indx)%key = key
    print *, 'update_table_entry', 'of', indx, 'in table type ', 143, 'of size', gmm_p_table_size
  gmm_update_tpi_key143 = 0
  end function gmm_update_tpi_key143
!
  integer function gmm_update_tpi_key283(indx, key)
  use gmm_internals
  use pointer_table_data_283
  implicit none
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
  type(gmm_attributes) :: localattr, attrs
  type (gmm_attributes) lcl_attr
  type(gmm_layout), dimension(4) :: lcl_layout, dims
  integer lcl_datatype
  logical consistent
  integer i, ier
  integer lcl_pti
    if (indx > gmm_p_table_size) then
      print *, 'update_table_entry : wrong index', indx,'in table type ', 283, 'of size', gmm_p_table_size
      gmm_update_tpi_key283 = GMM_POINTER_TABLE_OVERFLOW
    endif
    gmm_ptrs283(indx)%key = key
    print *, 'update_table_entry', 'of', indx, 'in table type ', 283, 'of size', gmm_p_table_size
  gmm_update_tpi_key283 = 0
  end function gmm_update_tpi_key283
!
  integer function gmm_update_tpi_key243(indx, key)
  use gmm_internals
  use pointer_table_data_243
  implicit none
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
  type(gmm_attributes) :: localattr, attrs
  type (gmm_attributes) lcl_attr
  type(gmm_layout), dimension(4) :: lcl_layout, dims
  integer lcl_datatype
  logical consistent
  integer i, ier
  integer lcl_pti
    if (indx > gmm_p_table_size) then
      print *, 'update_table_entry : wrong index', indx,'in table type ', 243, 'of size', gmm_p_table_size
      gmm_update_tpi_key243 = GMM_POINTER_TABLE_OVERFLOW
    endif
    gmm_ptrs243(indx)%key = key
    print *, 'update_table_entry', 'of', indx, 'in table type ', 243, 'of size', gmm_p_table_size
  gmm_update_tpi_key243 = 0
  end function gmm_update_tpi_key243
!
  integer function gmm_update_tpi_key383(indx, key)
  use gmm_internals
  use pointer_table_data_383
  implicit none
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
  type(gmm_attributes) :: localattr, attrs
  type (gmm_attributes) lcl_attr
  type(gmm_layout), dimension(4) :: lcl_layout, dims
  integer lcl_datatype
  logical consistent
  integer i, ier
  integer lcl_pti
    if (indx > gmm_p_table_size) then
      print *, 'update_table_entry : wrong index', indx,'in table type ', 383, 'of size', gmm_p_table_size
      gmm_update_tpi_key383 = GMM_POINTER_TABLE_OVERFLOW
    endif
    gmm_ptrs383(indx)%key = key
    print *, 'update_table_entry', 'of', indx, 'in table type ', 383, 'of size', gmm_p_table_size
  gmm_update_tpi_key383 = 0
  end function gmm_update_tpi_key383
!
  integer function gmm_update_tpi_key182(indx, key)
  use gmm_internals
  use pointer_table_data_182
  implicit none
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
  type(gmm_attributes) :: localattr, attrs
  type (gmm_attributes) lcl_attr
  type(gmm_layout), dimension(4) :: lcl_layout, dims
  integer lcl_datatype
  logical consistent
  integer i, ier
  integer lcl_pti
    if (indx > gmm_p_table_size) then
      print *, 'update_table_entry : wrong index', indx,'in table type ', 182, 'of size', gmm_p_table_size
      gmm_update_tpi_key182 = GMM_POINTER_TABLE_OVERFLOW
    endif
    gmm_ptrs182(indx)%key = key
    print *, 'update_table_entry', 'of', indx, 'in table type ', 182, 'of size', gmm_p_table_size
  gmm_update_tpi_key182 = 0
  end function gmm_update_tpi_key182
!
  integer function gmm_update_tpi_key142(indx, key)
  use gmm_internals
  use pointer_table_data_142
  implicit none
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
  type(gmm_attributes) :: localattr, attrs
  type (gmm_attributes) lcl_attr
  type(gmm_layout), dimension(4) :: lcl_layout, dims
  integer lcl_datatype
  logical consistent
  integer i, ier
  integer lcl_pti
    if (indx > gmm_p_table_size) then
      print *, 'update_table_entry : wrong index', indx,'in table type ', 142, 'of size', gmm_p_table_size
      gmm_update_tpi_key142 = GMM_POINTER_TABLE_OVERFLOW
    endif
    gmm_ptrs142(indx)%key = key
    print *, 'update_table_entry', 'of', indx, 'in table type ', 142, 'of size', gmm_p_table_size
  gmm_update_tpi_key142 = 0
  end function gmm_update_tpi_key142
!
  integer function gmm_update_tpi_key282(indx, key)
  use gmm_internals
  use pointer_table_data_282
  implicit none
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
  type(gmm_attributes) :: localattr, attrs
  type (gmm_attributes) lcl_attr
  type(gmm_layout), dimension(4) :: lcl_layout, dims
  integer lcl_datatype
  logical consistent
  integer i, ier
  integer lcl_pti
    if (indx > gmm_p_table_size) then
      print *, 'update_table_entry : wrong index', indx,'in table type ', 282, 'of size', gmm_p_table_size
      gmm_update_tpi_key282 = GMM_POINTER_TABLE_OVERFLOW
    endif
    gmm_ptrs282(indx)%key = key
    print *, 'update_table_entry', 'of', indx, 'in table type ', 282, 'of size', gmm_p_table_size
  gmm_update_tpi_key282 = 0
  end function gmm_update_tpi_key282
!
  integer function gmm_update_tpi_key242(indx, key)
  use gmm_internals
  use pointer_table_data_242
  implicit none
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
  type(gmm_attributes) :: localattr, attrs
  type (gmm_attributes) lcl_attr
  type(gmm_layout), dimension(4) :: lcl_layout, dims
  integer lcl_datatype
  logical consistent
  integer i, ier
  integer lcl_pti
    if (indx > gmm_p_table_size) then
      print *, 'update_table_entry : wrong index', indx,'in table type ', 242, 'of size', gmm_p_table_size
      gmm_update_tpi_key242 = GMM_POINTER_TABLE_OVERFLOW
    endif
    gmm_ptrs242(indx)%key = key
    print *, 'update_table_entry', 'of', indx, 'in table type ', 242, 'of size', gmm_p_table_size
  gmm_update_tpi_key242 = 0
  end function gmm_update_tpi_key242
!
  integer function gmm_update_tpi_key382(indx, key)
  use gmm_internals
  use pointer_table_data_382
  implicit none
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
  type(gmm_attributes) :: localattr, attrs
  type (gmm_attributes) lcl_attr
  type(gmm_layout), dimension(4) :: lcl_layout, dims
  integer lcl_datatype
  logical consistent
  integer i, ier
  integer lcl_pti
    if (indx > gmm_p_table_size) then
      print *, 'update_table_entry : wrong index', indx,'in table type ', 382, 'of size', gmm_p_table_size
      gmm_update_tpi_key382 = GMM_POINTER_TABLE_OVERFLOW
    endif
    gmm_ptrs382(indx)%key = key
    print *, 'update_table_entry', 'of', indx, 'in table type ', 382, 'of size', gmm_p_table_size
  gmm_update_tpi_key382 = 0
  end function gmm_update_tpi_key382
!
  integer function gmm_update_tpi_key181(indx, key)
  use gmm_internals
  use pointer_table_data_181
  implicit none
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
  type(gmm_attributes) :: localattr, attrs
  type (gmm_attributes) lcl_attr
  type(gmm_layout), dimension(4) :: lcl_layout, dims
  integer lcl_datatype
  logical consistent
  integer i, ier
  integer lcl_pti
    if (indx > gmm_p_table_size) then
      print *, 'update_table_entry : wrong index', indx,'in table type ', 181, 'of size', gmm_p_table_size
      gmm_update_tpi_key181 = GMM_POINTER_TABLE_OVERFLOW
    endif
    gmm_ptrs181(indx)%key = key
    print *, 'update_table_entry', 'of', indx, 'in table type ', 181, 'of size', gmm_p_table_size
  gmm_update_tpi_key181 = 0
  end function gmm_update_tpi_key181
!
  integer function gmm_update_tpi_key141(indx, key)
  use gmm_internals
  use pointer_table_data_141
  implicit none
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
  type(gmm_attributes) :: localattr, attrs
  type (gmm_attributes) lcl_attr
  type(gmm_layout), dimension(4) :: lcl_layout, dims
  integer lcl_datatype
  logical consistent
  integer i, ier
  integer lcl_pti
    if (indx > gmm_p_table_size) then
      print *, 'update_table_entry : wrong index', indx,'in table type ', 141, 'of size', gmm_p_table_size
      gmm_update_tpi_key141 = GMM_POINTER_TABLE_OVERFLOW
    endif
    gmm_ptrs141(indx)%key = key
    print *, 'update_table_entry', 'of', indx, 'in table type ', 141, 'of size', gmm_p_table_size
  gmm_update_tpi_key141 = 0
  end function gmm_update_tpi_key141
!
  integer function gmm_update_tpi_key281(indx, key)
  use gmm_internals
  use pointer_table_data_281
  implicit none
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
  type(gmm_attributes) :: localattr, attrs
  type (gmm_attributes) lcl_attr
  type(gmm_layout), dimension(4) :: lcl_layout, dims
  integer lcl_datatype
  logical consistent
  integer i, ier
  integer lcl_pti
    if (indx > gmm_p_table_size) then
      print *, 'update_table_entry : wrong index', indx,'in table type ', 281, 'of size', gmm_p_table_size
      gmm_update_tpi_key281 = GMM_POINTER_TABLE_OVERFLOW
    endif
    gmm_ptrs281(indx)%key = key
    print *, 'update_table_entry', 'of', indx, 'in table type ', 281, 'of size', gmm_p_table_size
  gmm_update_tpi_key281 = 0
  end function gmm_update_tpi_key281
!
  integer function gmm_update_tpi_key241(indx, key)
  use gmm_internals
  use pointer_table_data_241
  implicit none
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
  type(gmm_attributes) :: localattr, attrs
  type (gmm_attributes) lcl_attr
  type(gmm_layout), dimension(4) :: lcl_layout, dims
  integer lcl_datatype
  logical consistent
  integer i, ier
  integer lcl_pti
    if (indx > gmm_p_table_size) then
      print *, 'update_table_entry : wrong index', indx,'in table type ', 241, 'of size', gmm_p_table_size
      gmm_update_tpi_key241 = GMM_POINTER_TABLE_OVERFLOW
    endif
    gmm_ptrs241(indx)%key = key
    print *, 'update_table_entry', 'of', indx, 'in table type ', 241, 'of size', gmm_p_table_size
  gmm_update_tpi_key241 = 0
  end function gmm_update_tpi_key241
!
  integer function gmm_update_tpi_key381(indx, key)
  use gmm_internals
  use pointer_table_data_381
  implicit none
    integer, intent(in) :: indx
    integer*8, intent(in) :: key
  type(gmm_attributes) :: localattr, attrs
  type (gmm_attributes) lcl_attr
  type(gmm_layout), dimension(4) :: lcl_layout, dims
  integer lcl_datatype
  logical consistent
  integer i, ier
  integer lcl_pti
    if (indx > gmm_p_table_size) then
      print *, 'update_table_entry : wrong index', indx,'in table type ', 381, 'of size', gmm_p_table_size
      gmm_update_tpi_key381 = GMM_POINTER_TABLE_OVERFLOW
    endif
    gmm_ptrs381(indx)%key = key
    print *, 'update_table_entry', 'of', indx, 'in table type ', 381, 'of size', gmm_p_table_size
  gmm_update_tpi_key381 = 0
  end function gmm_update_tpi_key381
!
