!!===================== gmm_checkpoint_all =====================
  integer function gmm_checkpoint_all(read_or_write)
!
!        checkpoint read or write for all known types
!
  use gmm_internals
  implicit none
  logical read_or_write
  integer code,istat,fnom
  external fnom
!
  if (read_or_write) then  
    if (restart_mode) then
      if (gmm_verbose_level <= GMM_MSG_WARN) then
        print *,'(GMM_CHECKPOINT_ALL) Warning: restart file already read'
      endif
      gmm_checkpoint_all = GMM_OK
      return
    endif
    if (file_unit .eq. 0) then
      istat=fnom(file_unit,'gmm_restart','SEQ+UNF+FTN+OLD',0)  
      if (gmm_verbose_level == GMM_MSG_DEBUG) then
        print *,'open restart file, status=',istat
      endif
      if (istat .lt. 0) then
        file_unit = 0
        gmm_checkpoint_all = GMM_ERROR
        return
      endif
    endif
    do while(.true.)
      read(file_unit,end=999)code
      restart_mode=.true.   
      if (-1 .eq. code) then                              
        print *,'ERROR: gmm_checkpoint_all this cannot happen'
      else if (code .eq. 184) then
        call gmm_checkpoint_184(.true.)
      else if (code .eq. 144) then
        call gmm_checkpoint_144(.true.)
      else if (code .eq. 284) then
        call gmm_checkpoint_284(.true.)
      else if (code .eq. 244) then
        call gmm_checkpoint_244(.true.)
      else if (code .eq. 384) then
        call gmm_checkpoint_384(.true.)
      else if (code .eq. 183) then
        call gmm_checkpoint_183(.true.)
      else if (code .eq. 143) then
        call gmm_checkpoint_143(.true.)
      else if (code .eq. 283) then
        call gmm_checkpoint_283(.true.)
      else if (code .eq. 243) then
        call gmm_checkpoint_243(.true.)
      else if (code .eq. 383) then
        call gmm_checkpoint_383(.true.)
      else if (code .eq. 182) then
        call gmm_checkpoint_182(.true.)
      else if (code .eq. 142) then
        call gmm_checkpoint_142(.true.)
      else if (code .eq. 282) then
        call gmm_checkpoint_282(.true.)
      else if (code .eq. 242) then
        call gmm_checkpoint_242(.true.)
      else if (code .eq. 382) then
        call gmm_checkpoint_382(.true.)
      else if (code .eq. 181) then
        call gmm_checkpoint_181(.true.)
      else if (code .eq. 141) then
        call gmm_checkpoint_141(.true.)
      else if (code .eq. 281) then
        call gmm_checkpoint_281(.true.)
      else if (code .eq. 241) then
        call gmm_checkpoint_241(.true.)
      else if (code .eq. 381) then
        call gmm_checkpoint_381(.true.)
      else
        print *,'ERROR: gmm_checkpoint_all unrecognized type=',code,' in restart file'
        call qqexit(1)
      endif
    end do
  else       
    if (file_unit .eq. 0) then
      istat=fnom(file_unit,'gmm_restart','SEQ+UNF+FTN',0)  
      if (gmm_verbose_level == GMM_MSG_DEBUG) then
        print *,'open restart file, status=',istat
      endif
      if (istat .lt. 0) then
        file_unit = 0
        gmm_checkpoint_all = GMM_ERROR
        return
      endif
    endif
    call gmm_checkpoint_184(.false.)
    call gmm_checkpoint_144(.false.)
    call gmm_checkpoint_284(.false.)
    call gmm_checkpoint_244(.false.)
    call gmm_checkpoint_384(.false.)
    call gmm_checkpoint_183(.false.)
    call gmm_checkpoint_143(.false.)
    call gmm_checkpoint_283(.false.)
    call gmm_checkpoint_243(.false.)
    call gmm_checkpoint_383(.false.)
    call gmm_checkpoint_182(.false.)
    call gmm_checkpoint_142(.false.)
    call gmm_checkpoint_282(.false.)
    call gmm_checkpoint_242(.false.)
    call gmm_checkpoint_382(.false.)
    call gmm_checkpoint_181(.false.)
    call gmm_checkpoint_141(.false.)
    call gmm_checkpoint_281(.false.)
    call gmm_checkpoint_241(.false.)
    call gmm_checkpoint_381(.false.)
  endif
999 call fclos(file_unit)
    file_unit=0
  gmm_checkpoint_all = GMM_OK
  end function gmm_checkpoint_all
!!===================== gmm_checkpoint =====================
!        if  read_or_write is READ_CKPT (.true.) , read one checkpoint group of records
!        if  read_or_write is WRIT_CKPT (.false.) , write all groups of records to checkpoint file
!
  subroutine gmm_checkpoint_184(read_or_write)
  use gmm_internals
  use pointer_table_data_184
  implicit none
  logical read_or_write
  integer istat, fnom, i, j, ier, lcl_pti
  type(gmm_layout), dimension(1:4) :: siz
  type(gmm_attributes) :: attrib
  integer *8 :: key
  external fnom
      integer *8 get_address_from
      external get_address_from
  if (read_or_write) then      
!
    call add_directory_entry  
!    read(file_unit)directory(cur_page)%entry(cur_entry)%m%a%name  ! read name of variable
    read(file_unit)directory(cur_page)%entry(cur_entry)%name
    read(file_unit)siz(1:4)                                    
    directory(cur_page)%entry(cur_entry)%l(1:4) = siz(1:4) 
    read(file_unit)attrib                                        
!    print *,'name=',directory(cur_page)%entry(cur_entry)%name,' dims=',siz(1:DIM)
    attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
    directory(cur_page)%entry(cur_entry)%a = attrib            
    read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
    lcl_pti = lgmm_get_nxt_avail_ptr()
    directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
    ordinal = ordinal + 1
    key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
    key = key + ishft(184,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
    directory(cur_page)%entry(cur_entry)%a%key = key            
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
    allocate(gmm_ptrs184(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(siz(1)%low:siz(1)%high,&
                                                        &siz(2)%low:siz(2)%high,&
                                                        &siz(3)%low:siz(3)%high,&
                                                        &siz(4)%low:siz(4)%high  ))
    read(file_unit)gmm_ptrs184(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p         
    directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
          &gmm_ptrs184(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',&
     &cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs184(directory(&
     &cur_page)%entry(cur_entry)%pointer_table_index)%p)
    endif
    ier=add_table_entry(gmm_ptrs184(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
!
  else                    
!
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      print *,'checkpointing type ',184
    endif
    do i=1,table_size
      do j=1,PAGE_SIZE
        if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0.and.directory(i)%entry(j)%data_type == 184) then
          if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'writing field ',directory(i)%entry(j)%name
          endif
          write(file_unit)184
          write(file_unit)directory(i)%entry(j)%name
          write(file_unit)directory(i)%entry(j)%l(1:4)
          attrib = directory(i)%entry(j)%a
          attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
          write(file_unit)attrib
          write(file_unit)directory(i)%entry(j)%data_type
          write(file_unit)gmm_ptrs184(directory(i)%entry(j)%pointer_table_index)%p
        endif
      enddo
    enddo
!
  endif
  end subroutine gmm_checkpoint_184
  subroutine gmm_checkpoint_144(read_or_write)
  use gmm_internals
  use pointer_table_data_144
  implicit none
  logical read_or_write
  integer istat, fnom, i, j, ier, lcl_pti
  type(gmm_layout), dimension(1:4) :: siz
  type(gmm_attributes) :: attrib
  integer *8 :: key
  external fnom
      integer *8 get_address_from
      external get_address_from
  if (read_or_write) then      
!
    call add_directory_entry  
!    read(file_unit)directory(cur_page)%entry(cur_entry)%m%a%name  ! read name of variable
    read(file_unit)directory(cur_page)%entry(cur_entry)%name
    read(file_unit)siz(1:4)                                    
    directory(cur_page)%entry(cur_entry)%l(1:4) = siz(1:4) 
    read(file_unit)attrib                                        
!    print *,'name=',directory(cur_page)%entry(cur_entry)%name,' dims=',siz(1:DIM)
    attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
    directory(cur_page)%entry(cur_entry)%a = attrib            
    read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
    lcl_pti = lgmm_get_nxt_avail_ptr()
    directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
    ordinal = ordinal + 1
    key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
    key = key + ishft(144,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
    directory(cur_page)%entry(cur_entry)%a%key = key            
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
    allocate(gmm_ptrs144(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(siz(1)%low:siz(1)%high,&
                                                        &siz(2)%low:siz(2)%high,&
                                                        &siz(3)%low:siz(3)%high,&
                                                        &siz(4)%low:siz(4)%high  ))
    read(file_unit)gmm_ptrs144(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p         
    directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
          &gmm_ptrs144(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',&
     &cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs144(directory(&
     &cur_page)%entry(cur_entry)%pointer_table_index)%p)
    endif
    ier=add_table_entry(gmm_ptrs144(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
!
  else                    
!
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      print *,'checkpointing type ',144
    endif
    do i=1,table_size
      do j=1,PAGE_SIZE
        if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0.and.directory(i)%entry(j)%data_type == 144) then
          if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'writing field ',directory(i)%entry(j)%name
          endif
          write(file_unit)144
          write(file_unit)directory(i)%entry(j)%name
          write(file_unit)directory(i)%entry(j)%l(1:4)
          attrib = directory(i)%entry(j)%a
          attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
          write(file_unit)attrib
          write(file_unit)directory(i)%entry(j)%data_type
          write(file_unit)gmm_ptrs144(directory(i)%entry(j)%pointer_table_index)%p
        endif
      enddo
    enddo
!
  endif
  end subroutine gmm_checkpoint_144
  subroutine gmm_checkpoint_284(read_or_write)
  use gmm_internals
  use pointer_table_data_284
  implicit none
  logical read_or_write
  integer istat, fnom, i, j, ier, lcl_pti
  type(gmm_layout), dimension(1:4) :: siz
  type(gmm_attributes) :: attrib
  integer *8 :: key
  external fnom
      integer *8 get_address_from
      external get_address_from
  if (read_or_write) then      
!
    call add_directory_entry  
!    read(file_unit)directory(cur_page)%entry(cur_entry)%m%a%name  ! read name of variable
    read(file_unit)directory(cur_page)%entry(cur_entry)%name
    read(file_unit)siz(1:4)                                    
    directory(cur_page)%entry(cur_entry)%l(1:4) = siz(1:4) 
    read(file_unit)attrib                                        
!    print *,'name=',directory(cur_page)%entry(cur_entry)%name,' dims=',siz(1:DIM)
    attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
    directory(cur_page)%entry(cur_entry)%a = attrib            
    read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
    lcl_pti = lgmm_get_nxt_avail_ptr()
    directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
    ordinal = ordinal + 1
    key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
    key = key + ishft(284,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
    directory(cur_page)%entry(cur_entry)%a%key = key            
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
    allocate(gmm_ptrs284(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(siz(1)%low:siz(1)%high,&
                                                        &siz(2)%low:siz(2)%high,&
                                                        &siz(3)%low:siz(3)%high,&
                                                        &siz(4)%low:siz(4)%high  ))
    read(file_unit)gmm_ptrs284(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p         
    directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
          &gmm_ptrs284(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',&
     &cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs284(directory(&
     &cur_page)%entry(cur_entry)%pointer_table_index)%p)
    endif
    ier=add_table_entry(gmm_ptrs284(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
!
  else                    
!
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      print *,'checkpointing type ',284
    endif
    do i=1,table_size
      do j=1,PAGE_SIZE
        if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0.and.directory(i)%entry(j)%data_type == 284) then
          if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'writing field ',directory(i)%entry(j)%name
          endif
          write(file_unit)284
          write(file_unit)directory(i)%entry(j)%name
          write(file_unit)directory(i)%entry(j)%l(1:4)
          attrib = directory(i)%entry(j)%a
          attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
          write(file_unit)attrib
          write(file_unit)directory(i)%entry(j)%data_type
          write(file_unit)gmm_ptrs284(directory(i)%entry(j)%pointer_table_index)%p
        endif
      enddo
    enddo
!
  endif
  end subroutine gmm_checkpoint_284
  subroutine gmm_checkpoint_244(read_or_write)
  use gmm_internals
  use pointer_table_data_244
  implicit none
  logical read_or_write
  integer istat, fnom, i, j, ier, lcl_pti
  type(gmm_layout), dimension(1:4) :: siz
  type(gmm_attributes) :: attrib
  integer *8 :: key
  external fnom
      integer *8 get_address_from
      external get_address_from
  if (read_or_write) then      
!
    call add_directory_entry  
!    read(file_unit)directory(cur_page)%entry(cur_entry)%m%a%name  ! read name of variable
    read(file_unit)directory(cur_page)%entry(cur_entry)%name
    read(file_unit)siz(1:4)                                    
    directory(cur_page)%entry(cur_entry)%l(1:4) = siz(1:4) 
    read(file_unit)attrib                                        
!    print *,'name=',directory(cur_page)%entry(cur_entry)%name,' dims=',siz(1:DIM)
    attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
    directory(cur_page)%entry(cur_entry)%a = attrib            
    read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
    lcl_pti = lgmm_get_nxt_avail_ptr()
    directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
    ordinal = ordinal + 1
    key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
    key = key + ishft(244,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
    directory(cur_page)%entry(cur_entry)%a%key = key            
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
    allocate(gmm_ptrs244(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(siz(1)%low:siz(1)%high,&
                                                        &siz(2)%low:siz(2)%high,&
                                                        &siz(3)%low:siz(3)%high,&
                                                        &siz(4)%low:siz(4)%high  ))
    read(file_unit)gmm_ptrs244(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p         
    directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
          &gmm_ptrs244(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',&
     &cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs244(directory(&
     &cur_page)%entry(cur_entry)%pointer_table_index)%p)
    endif
    ier=add_table_entry(gmm_ptrs244(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
!
  else                    
!
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      print *,'checkpointing type ',244
    endif
    do i=1,table_size
      do j=1,PAGE_SIZE
        if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0.and.directory(i)%entry(j)%data_type == 244) then
          if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'writing field ',directory(i)%entry(j)%name
          endif
          write(file_unit)244
          write(file_unit)directory(i)%entry(j)%name
          write(file_unit)directory(i)%entry(j)%l(1:4)
          attrib = directory(i)%entry(j)%a
          attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
          write(file_unit)attrib
          write(file_unit)directory(i)%entry(j)%data_type
          write(file_unit)gmm_ptrs244(directory(i)%entry(j)%pointer_table_index)%p
        endif
      enddo
    enddo
!
  endif
  end subroutine gmm_checkpoint_244
  subroutine gmm_checkpoint_384(read_or_write)
  use gmm_internals
  use pointer_table_data_384
  implicit none
  logical read_or_write
  integer istat, fnom, i, j, ier, lcl_pti
  type(gmm_layout), dimension(1:4) :: siz
  type(gmm_attributes) :: attrib
  integer *8 :: key
  external fnom
      integer *8 get_address_from
      external get_address_from
  if (read_or_write) then      
!
    call add_directory_entry  
!    read(file_unit)directory(cur_page)%entry(cur_entry)%m%a%name  ! read name of variable
    read(file_unit)directory(cur_page)%entry(cur_entry)%name
    read(file_unit)siz(1:4)                                    
    directory(cur_page)%entry(cur_entry)%l(1:4) = siz(1:4) 
    read(file_unit)attrib                                        
!    print *,'name=',directory(cur_page)%entry(cur_entry)%name,' dims=',siz(1:DIM)
    attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
    directory(cur_page)%entry(cur_entry)%a = attrib            
    read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
    lcl_pti = lgmm_get_nxt_avail_ptr()
    directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
    ordinal = ordinal + 1
    key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
    key = key + ishft(384,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
    directory(cur_page)%entry(cur_entry)%a%key = key            
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
    allocate(gmm_ptrs384(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(siz(1)%low:siz(1)%high,&
                                                        &siz(2)%low:siz(2)%high,&
                                                        &siz(3)%low:siz(3)%high,&
                                                        &siz(4)%low:siz(4)%high  ))
    read(file_unit)gmm_ptrs384(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p         
    directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
          &gmm_ptrs384(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',&
     &cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs384(directory(&
     &cur_page)%entry(cur_entry)%pointer_table_index)%p)
    endif
    ier=add_table_entry(gmm_ptrs384(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
!
  else                    
!
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      print *,'checkpointing type ',384
    endif
    do i=1,table_size
      do j=1,PAGE_SIZE
        if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0.and.directory(i)%entry(j)%data_type == 384) then
          if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'writing field ',directory(i)%entry(j)%name
          endif
          write(file_unit)384
          write(file_unit)directory(i)%entry(j)%name
          write(file_unit)directory(i)%entry(j)%l(1:4)
          attrib = directory(i)%entry(j)%a
          attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
          write(file_unit)attrib
          write(file_unit)directory(i)%entry(j)%data_type
          write(file_unit)gmm_ptrs384(directory(i)%entry(j)%pointer_table_index)%p
        endif
      enddo
    enddo
!
  endif
  end subroutine gmm_checkpoint_384
  subroutine gmm_checkpoint_183(read_or_write)
  use gmm_internals
  use pointer_table_data_183
  implicit none
  logical read_or_write
  integer istat, fnom, i, j, ier, lcl_pti
  type(gmm_layout), dimension(1:3) :: siz
  type(gmm_attributes) :: attrib
  integer *8 :: key
  external fnom
      integer *8 get_address_from
      external get_address_from
  if (read_or_write) then      
!
    call add_directory_entry  
!    read(file_unit)directory(cur_page)%entry(cur_entry)%m%a%name  ! read name of variable
    read(file_unit)directory(cur_page)%entry(cur_entry)%name
    read(file_unit)siz(1:3)                                    
    directory(cur_page)%entry(cur_entry)%l(1:3) = siz(1:3) 
    read(file_unit)attrib                                        
!    print *,'name=',directory(cur_page)%entry(cur_entry)%name,' dims=',siz(1:DIM)
    attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
    directory(cur_page)%entry(cur_entry)%a = attrib            
    read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
    lcl_pti = lgmm_get_nxt_avail_ptr()
    directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
    ordinal = ordinal + 1
    key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
    key = key + ishft(183,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
    directory(cur_page)%entry(cur_entry)%a%key = key            
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
    allocate(gmm_ptrs183(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(siz(1)%low:siz(1)%high,&
                                                        &siz(2)%low:siz(2)%high,&
                                                        &siz(3)%low:siz(3)%high  ))
    read(file_unit)gmm_ptrs183(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p         
    directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
          &gmm_ptrs183(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',&
     &cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs183(directory(&
     &cur_page)%entry(cur_entry)%pointer_table_index)%p)
    endif
    ier=add_table_entry(gmm_ptrs183(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
!
  else                    
!
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      print *,'checkpointing type ',183
    endif
    do i=1,table_size
      do j=1,PAGE_SIZE
        if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0.and.directory(i)%entry(j)%data_type == 183) then
          if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'writing field ',directory(i)%entry(j)%name
          endif
          write(file_unit)183
          write(file_unit)directory(i)%entry(j)%name
          write(file_unit)directory(i)%entry(j)%l(1:3)
          attrib = directory(i)%entry(j)%a
          attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
          write(file_unit)attrib
          write(file_unit)directory(i)%entry(j)%data_type
          write(file_unit)gmm_ptrs183(directory(i)%entry(j)%pointer_table_index)%p
        endif
      enddo
    enddo
!
  endif
  end subroutine gmm_checkpoint_183
  subroutine gmm_checkpoint_143(read_or_write)
  use gmm_internals
  use pointer_table_data_143
  implicit none
  logical read_or_write
  integer istat, fnom, i, j, ier, lcl_pti
  type(gmm_layout), dimension(1:3) :: siz
  type(gmm_attributes) :: attrib
  integer *8 :: key
  external fnom
      integer *8 get_address_from
      external get_address_from
  if (read_or_write) then      
!
    call add_directory_entry  
!    read(file_unit)directory(cur_page)%entry(cur_entry)%m%a%name  ! read name of variable
    read(file_unit)directory(cur_page)%entry(cur_entry)%name
    read(file_unit)siz(1:3)                                    
    directory(cur_page)%entry(cur_entry)%l(1:3) = siz(1:3) 
    read(file_unit)attrib                                        
!    print *,'name=',directory(cur_page)%entry(cur_entry)%name,' dims=',siz(1:DIM)
    attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
    directory(cur_page)%entry(cur_entry)%a = attrib            
    read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
    lcl_pti = lgmm_get_nxt_avail_ptr()
    directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
    ordinal = ordinal + 1
    key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
    key = key + ishft(143,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
    directory(cur_page)%entry(cur_entry)%a%key = key            
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
    allocate(gmm_ptrs143(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(siz(1)%low:siz(1)%high,&
                                                        &siz(2)%low:siz(2)%high,&
                                                        &siz(3)%low:siz(3)%high  ))
    read(file_unit)gmm_ptrs143(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p         
    directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
          &gmm_ptrs143(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',&
     &cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs143(directory(&
     &cur_page)%entry(cur_entry)%pointer_table_index)%p)
    endif
    ier=add_table_entry(gmm_ptrs143(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
!
  else                    
!
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      print *,'checkpointing type ',143
    endif
    do i=1,table_size
      do j=1,PAGE_SIZE
        if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0.and.directory(i)%entry(j)%data_type == 143) then
          if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'writing field ',directory(i)%entry(j)%name
          endif
          write(file_unit)143
          write(file_unit)directory(i)%entry(j)%name
          write(file_unit)directory(i)%entry(j)%l(1:3)
          attrib = directory(i)%entry(j)%a
          attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
          write(file_unit)attrib
          write(file_unit)directory(i)%entry(j)%data_type
          write(file_unit)gmm_ptrs143(directory(i)%entry(j)%pointer_table_index)%p
        endif
      enddo
    enddo
!
  endif
  end subroutine gmm_checkpoint_143
  subroutine gmm_checkpoint_283(read_or_write)
  use gmm_internals
  use pointer_table_data_283
  implicit none
  logical read_or_write
  integer istat, fnom, i, j, ier, lcl_pti
  type(gmm_layout), dimension(1:3) :: siz
  type(gmm_attributes) :: attrib
  integer *8 :: key
  external fnom
      integer *8 get_address_from
      external get_address_from
  if (read_or_write) then      
!
    call add_directory_entry  
!    read(file_unit)directory(cur_page)%entry(cur_entry)%m%a%name  ! read name of variable
    read(file_unit)directory(cur_page)%entry(cur_entry)%name
    read(file_unit)siz(1:3)                                    
    directory(cur_page)%entry(cur_entry)%l(1:3) = siz(1:3) 
    read(file_unit)attrib                                        
!    print *,'name=',directory(cur_page)%entry(cur_entry)%name,' dims=',siz(1:DIM)
    attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
    directory(cur_page)%entry(cur_entry)%a = attrib            
    read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
    lcl_pti = lgmm_get_nxt_avail_ptr()
    directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
    ordinal = ordinal + 1
    key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
    key = key + ishft(283,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
    directory(cur_page)%entry(cur_entry)%a%key = key            
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
    allocate(gmm_ptrs283(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(siz(1)%low:siz(1)%high,&
                                                        &siz(2)%low:siz(2)%high,&
                                                        &siz(3)%low:siz(3)%high  ))
    read(file_unit)gmm_ptrs283(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p         
    directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
          &gmm_ptrs283(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',&
     &cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs283(directory(&
     &cur_page)%entry(cur_entry)%pointer_table_index)%p)
    endif
    ier=add_table_entry(gmm_ptrs283(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
!
  else                    
!
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      print *,'checkpointing type ',283
    endif
    do i=1,table_size
      do j=1,PAGE_SIZE
        if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0.and.directory(i)%entry(j)%data_type == 283) then
          if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'writing field ',directory(i)%entry(j)%name
          endif
          write(file_unit)283
          write(file_unit)directory(i)%entry(j)%name
          write(file_unit)directory(i)%entry(j)%l(1:3)
          attrib = directory(i)%entry(j)%a
          attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
          write(file_unit)attrib
          write(file_unit)directory(i)%entry(j)%data_type
          write(file_unit)gmm_ptrs283(directory(i)%entry(j)%pointer_table_index)%p
        endif
      enddo
    enddo
!
  endif
  end subroutine gmm_checkpoint_283
  subroutine gmm_checkpoint_243(read_or_write)
  use gmm_internals
  use pointer_table_data_243
  implicit none
  logical read_or_write
  integer istat, fnom, i, j, ier, lcl_pti
  type(gmm_layout), dimension(1:3) :: siz
  type(gmm_attributes) :: attrib
  integer *8 :: key
  external fnom
      integer *8 get_address_from
      external get_address_from
  if (read_or_write) then      
!
    call add_directory_entry  
!    read(file_unit)directory(cur_page)%entry(cur_entry)%m%a%name  ! read name of variable
    read(file_unit)directory(cur_page)%entry(cur_entry)%name
    read(file_unit)siz(1:3)                                    
    directory(cur_page)%entry(cur_entry)%l(1:3) = siz(1:3) 
    read(file_unit)attrib                                        
!    print *,'name=',directory(cur_page)%entry(cur_entry)%name,' dims=',siz(1:DIM)
    attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
    directory(cur_page)%entry(cur_entry)%a = attrib            
    read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
    lcl_pti = lgmm_get_nxt_avail_ptr()
    directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
    ordinal = ordinal + 1
    key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
    key = key + ishft(243,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
    directory(cur_page)%entry(cur_entry)%a%key = key            
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
    allocate(gmm_ptrs243(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(siz(1)%low:siz(1)%high,&
                                                        &siz(2)%low:siz(2)%high,&
                                                        &siz(3)%low:siz(3)%high  ))
    read(file_unit)gmm_ptrs243(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p         
    directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
          &gmm_ptrs243(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',&
     &cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs243(directory(&
     &cur_page)%entry(cur_entry)%pointer_table_index)%p)
    endif
    ier=add_table_entry(gmm_ptrs243(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
!
  else                    
!
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      print *,'checkpointing type ',243
    endif
    do i=1,table_size
      do j=1,PAGE_SIZE
        if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0.and.directory(i)%entry(j)%data_type == 243) then
          if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'writing field ',directory(i)%entry(j)%name
          endif
          write(file_unit)243
          write(file_unit)directory(i)%entry(j)%name
          write(file_unit)directory(i)%entry(j)%l(1:3)
          attrib = directory(i)%entry(j)%a
          attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
          write(file_unit)attrib
          write(file_unit)directory(i)%entry(j)%data_type
          write(file_unit)gmm_ptrs243(directory(i)%entry(j)%pointer_table_index)%p
        endif
      enddo
    enddo
!
  endif
  end subroutine gmm_checkpoint_243
  subroutine gmm_checkpoint_383(read_or_write)
  use gmm_internals
  use pointer_table_data_383
  implicit none
  logical read_or_write
  integer istat, fnom, i, j, ier, lcl_pti
  type(gmm_layout), dimension(1:3) :: siz
  type(gmm_attributes) :: attrib
  integer *8 :: key
  external fnom
      integer *8 get_address_from
      external get_address_from
  if (read_or_write) then      
!
    call add_directory_entry  
!    read(file_unit)directory(cur_page)%entry(cur_entry)%m%a%name  ! read name of variable
    read(file_unit)directory(cur_page)%entry(cur_entry)%name
    read(file_unit)siz(1:3)                                    
    directory(cur_page)%entry(cur_entry)%l(1:3) = siz(1:3) 
    read(file_unit)attrib                                        
!    print *,'name=',directory(cur_page)%entry(cur_entry)%name,' dims=',siz(1:DIM)
    attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
    directory(cur_page)%entry(cur_entry)%a = attrib            
    read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
    lcl_pti = lgmm_get_nxt_avail_ptr()
    directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
    ordinal = ordinal + 1
    key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
    key = key + ishft(383,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
    directory(cur_page)%entry(cur_entry)%a%key = key            
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
    allocate(gmm_ptrs383(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(siz(1)%low:siz(1)%high,&
                                                        &siz(2)%low:siz(2)%high,&
                                                        &siz(3)%low:siz(3)%high  ))
    read(file_unit)gmm_ptrs383(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p         
    directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
          &gmm_ptrs383(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',&
     &cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs383(directory(&
     &cur_page)%entry(cur_entry)%pointer_table_index)%p)
    endif
    ier=add_table_entry(gmm_ptrs383(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
!
  else                    
!
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      print *,'checkpointing type ',383
    endif
    do i=1,table_size
      do j=1,PAGE_SIZE
        if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0.and.directory(i)%entry(j)%data_type == 383) then
          if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'writing field ',directory(i)%entry(j)%name
          endif
          write(file_unit)383
          write(file_unit)directory(i)%entry(j)%name
          write(file_unit)directory(i)%entry(j)%l(1:3)
          attrib = directory(i)%entry(j)%a
          attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
          write(file_unit)attrib
          write(file_unit)directory(i)%entry(j)%data_type
          write(file_unit)gmm_ptrs383(directory(i)%entry(j)%pointer_table_index)%p
        endif
      enddo
    enddo
!
  endif
  end subroutine gmm_checkpoint_383
  subroutine gmm_checkpoint_182(read_or_write)
  use gmm_internals
  use pointer_table_data_182
  implicit none
  logical read_or_write
  integer istat, fnom, i, j, ier, lcl_pti
  type(gmm_layout), dimension(1:2) :: siz
  type(gmm_attributes) :: attrib
  integer *8 :: key
  external fnom
      integer *8 get_address_from
      external get_address_from
  if (read_or_write) then      
!
    call add_directory_entry  
!    read(file_unit)directory(cur_page)%entry(cur_entry)%m%a%name  ! read name of variable
    read(file_unit)directory(cur_page)%entry(cur_entry)%name
    read(file_unit)siz(1:2)                                    
    directory(cur_page)%entry(cur_entry)%l(1:2) = siz(1:2) 
    read(file_unit)attrib                                        
!    print *,'name=',directory(cur_page)%entry(cur_entry)%name,' dims=',siz(1:DIM)
    attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
    directory(cur_page)%entry(cur_entry)%a = attrib            
    read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
    lcl_pti = lgmm_get_nxt_avail_ptr()
    directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
    ordinal = ordinal + 1
    key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
    key = key + ishft(182,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
    directory(cur_page)%entry(cur_entry)%a%key = key            
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
    allocate(gmm_ptrs182(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(siz(1)%low:siz(1)%high,&
                                                        &siz(2)%low:siz(2)%high  ))
    read(file_unit)gmm_ptrs182(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p         
    directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
          &gmm_ptrs182(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',&
     &cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs182(directory(&
     &cur_page)%entry(cur_entry)%pointer_table_index)%p)
    endif
    ier=add_table_entry(gmm_ptrs182(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
!
  else                    
!
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      print *,'checkpointing type ',182
    endif
    do i=1,table_size
      do j=1,PAGE_SIZE
        if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0.and.directory(i)%entry(j)%data_type == 182) then
          if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'writing field ',directory(i)%entry(j)%name
          endif
          write(file_unit)182
          write(file_unit)directory(i)%entry(j)%name
          write(file_unit)directory(i)%entry(j)%l(1:2)
          attrib = directory(i)%entry(j)%a
          attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
          write(file_unit)attrib
          write(file_unit)directory(i)%entry(j)%data_type
          write(file_unit)gmm_ptrs182(directory(i)%entry(j)%pointer_table_index)%p
        endif
      enddo
    enddo
!
  endif
  end subroutine gmm_checkpoint_182
  subroutine gmm_checkpoint_142(read_or_write)
  use gmm_internals
  use pointer_table_data_142
  implicit none
  logical read_or_write
  integer istat, fnom, i, j, ier, lcl_pti
  type(gmm_layout), dimension(1:2) :: siz
  type(gmm_attributes) :: attrib
  integer *8 :: key
  external fnom
      integer *8 get_address_from
      external get_address_from
  if (read_or_write) then      
!
    call add_directory_entry  
!    read(file_unit)directory(cur_page)%entry(cur_entry)%m%a%name  ! read name of variable
    read(file_unit)directory(cur_page)%entry(cur_entry)%name
    read(file_unit)siz(1:2)                                    
    directory(cur_page)%entry(cur_entry)%l(1:2) = siz(1:2) 
    read(file_unit)attrib                                        
!    print *,'name=',directory(cur_page)%entry(cur_entry)%name,' dims=',siz(1:DIM)
    attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
    directory(cur_page)%entry(cur_entry)%a = attrib            
    read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
    lcl_pti = lgmm_get_nxt_avail_ptr()
    directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
    ordinal = ordinal + 1
    key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
    key = key + ishft(142,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
    directory(cur_page)%entry(cur_entry)%a%key = key            
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
    allocate(gmm_ptrs142(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(siz(1)%low:siz(1)%high,&
                                                        &siz(2)%low:siz(2)%high  ))
    read(file_unit)gmm_ptrs142(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p         
    directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
          &gmm_ptrs142(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',&
     &cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs142(directory(&
     &cur_page)%entry(cur_entry)%pointer_table_index)%p)
    endif
    ier=add_table_entry(gmm_ptrs142(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
!
  else                    
!
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      print *,'checkpointing type ',142
    endif
    do i=1,table_size
      do j=1,PAGE_SIZE
        if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0.and.directory(i)%entry(j)%data_type == 142) then
          if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'writing field ',directory(i)%entry(j)%name
          endif
          write(file_unit)142
          write(file_unit)directory(i)%entry(j)%name
          write(file_unit)directory(i)%entry(j)%l(1:2)
          attrib = directory(i)%entry(j)%a
          attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
          write(file_unit)attrib
          write(file_unit)directory(i)%entry(j)%data_type
          write(file_unit)gmm_ptrs142(directory(i)%entry(j)%pointer_table_index)%p
        endif
      enddo
    enddo
!
  endif
  end subroutine gmm_checkpoint_142
  subroutine gmm_checkpoint_282(read_or_write)
  use gmm_internals
  use pointer_table_data_282
  implicit none
  logical read_or_write
  integer istat, fnom, i, j, ier, lcl_pti
  type(gmm_layout), dimension(1:2) :: siz
  type(gmm_attributes) :: attrib
  integer *8 :: key
  external fnom
      integer *8 get_address_from
      external get_address_from
  if (read_or_write) then      
!
    call add_directory_entry  
!    read(file_unit)directory(cur_page)%entry(cur_entry)%m%a%name  ! read name of variable
    read(file_unit)directory(cur_page)%entry(cur_entry)%name
    read(file_unit)siz(1:2)                                    
    directory(cur_page)%entry(cur_entry)%l(1:2) = siz(1:2) 
    read(file_unit)attrib                                        
!    print *,'name=',directory(cur_page)%entry(cur_entry)%name,' dims=',siz(1:DIM)
    attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
    directory(cur_page)%entry(cur_entry)%a = attrib            
    read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
    lcl_pti = lgmm_get_nxt_avail_ptr()
    directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
    ordinal = ordinal + 1
    key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
    key = key + ishft(282,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
    directory(cur_page)%entry(cur_entry)%a%key = key            
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
    allocate(gmm_ptrs282(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(siz(1)%low:siz(1)%high,&
                                                        &siz(2)%low:siz(2)%high  ))
    read(file_unit)gmm_ptrs282(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p         
    directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
          &gmm_ptrs282(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',&
     &cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs282(directory(&
     &cur_page)%entry(cur_entry)%pointer_table_index)%p)
    endif
    ier=add_table_entry(gmm_ptrs282(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
!
  else                    
!
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      print *,'checkpointing type ',282
    endif
    do i=1,table_size
      do j=1,PAGE_SIZE
        if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0.and.directory(i)%entry(j)%data_type == 282) then
          if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'writing field ',directory(i)%entry(j)%name
          endif
          write(file_unit)282
          write(file_unit)directory(i)%entry(j)%name
          write(file_unit)directory(i)%entry(j)%l(1:2)
          attrib = directory(i)%entry(j)%a
          attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
          write(file_unit)attrib
          write(file_unit)directory(i)%entry(j)%data_type
          write(file_unit)gmm_ptrs282(directory(i)%entry(j)%pointer_table_index)%p
        endif
      enddo
    enddo
!
  endif
  end subroutine gmm_checkpoint_282
  subroutine gmm_checkpoint_242(read_or_write)
  use gmm_internals
  use pointer_table_data_242
  implicit none
  logical read_or_write
  integer istat, fnom, i, j, ier, lcl_pti
  type(gmm_layout), dimension(1:2) :: siz
  type(gmm_attributes) :: attrib
  integer *8 :: key
  external fnom
      integer *8 get_address_from
      external get_address_from
  if (read_or_write) then      
!
    call add_directory_entry  
!    read(file_unit)directory(cur_page)%entry(cur_entry)%m%a%name  ! read name of variable
    read(file_unit)directory(cur_page)%entry(cur_entry)%name
    read(file_unit)siz(1:2)                                    
    directory(cur_page)%entry(cur_entry)%l(1:2) = siz(1:2) 
    read(file_unit)attrib                                        
!    print *,'name=',directory(cur_page)%entry(cur_entry)%name,' dims=',siz(1:DIM)
    attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
    directory(cur_page)%entry(cur_entry)%a = attrib            
    read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
    lcl_pti = lgmm_get_nxt_avail_ptr()
    directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
    ordinal = ordinal + 1
    key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
    key = key + ishft(242,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
    directory(cur_page)%entry(cur_entry)%a%key = key            
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
    allocate(gmm_ptrs242(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(siz(1)%low:siz(1)%high,&
                                                        &siz(2)%low:siz(2)%high  ))
    read(file_unit)gmm_ptrs242(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p         
    directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
          &gmm_ptrs242(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',&
     &cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs242(directory(&
     &cur_page)%entry(cur_entry)%pointer_table_index)%p)
    endif
    ier=add_table_entry(gmm_ptrs242(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
!
  else                    
!
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      print *,'checkpointing type ',242
    endif
    do i=1,table_size
      do j=1,PAGE_SIZE
        if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0.and.directory(i)%entry(j)%data_type == 242) then
          if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'writing field ',directory(i)%entry(j)%name
          endif
          write(file_unit)242
          write(file_unit)directory(i)%entry(j)%name
          write(file_unit)directory(i)%entry(j)%l(1:2)
          attrib = directory(i)%entry(j)%a
          attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
          write(file_unit)attrib
          write(file_unit)directory(i)%entry(j)%data_type
          write(file_unit)gmm_ptrs242(directory(i)%entry(j)%pointer_table_index)%p
        endif
      enddo
    enddo
!
  endif
  end subroutine gmm_checkpoint_242
  subroutine gmm_checkpoint_382(read_or_write)
  use gmm_internals
  use pointer_table_data_382
  implicit none
  logical read_or_write
  integer istat, fnom, i, j, ier, lcl_pti
  type(gmm_layout), dimension(1:2) :: siz
  type(gmm_attributes) :: attrib
  integer *8 :: key
  external fnom
      integer *8 get_address_from
      external get_address_from
  if (read_or_write) then      
!
    call add_directory_entry  
!    read(file_unit)directory(cur_page)%entry(cur_entry)%m%a%name  ! read name of variable
    read(file_unit)directory(cur_page)%entry(cur_entry)%name
    read(file_unit)siz(1:2)                                    
    directory(cur_page)%entry(cur_entry)%l(1:2) = siz(1:2) 
    read(file_unit)attrib                                        
!    print *,'name=',directory(cur_page)%entry(cur_entry)%name,' dims=',siz(1:DIM)
    attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
    directory(cur_page)%entry(cur_entry)%a = attrib            
    read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
    lcl_pti = lgmm_get_nxt_avail_ptr()
    directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
    ordinal = ordinal + 1
    key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
    key = key + ishft(382,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
    directory(cur_page)%entry(cur_entry)%a%key = key            
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
    allocate(gmm_ptrs382(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(siz(1)%low:siz(1)%high,&
                                                        &siz(2)%low:siz(2)%high  ))
    read(file_unit)gmm_ptrs382(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p         
    directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
          &gmm_ptrs382(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',&
     &cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs382(directory(&
     &cur_page)%entry(cur_entry)%pointer_table_index)%p)
    endif
    ier=add_table_entry(gmm_ptrs382(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
!
  else                    
!
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      print *,'checkpointing type ',382
    endif
    do i=1,table_size
      do j=1,PAGE_SIZE
        if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0.and.directory(i)%entry(j)%data_type == 382) then
          if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'writing field ',directory(i)%entry(j)%name
          endif
          write(file_unit)382
          write(file_unit)directory(i)%entry(j)%name
          write(file_unit)directory(i)%entry(j)%l(1:2)
          attrib = directory(i)%entry(j)%a
          attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
          write(file_unit)attrib
          write(file_unit)directory(i)%entry(j)%data_type
          write(file_unit)gmm_ptrs382(directory(i)%entry(j)%pointer_table_index)%p
        endif
      enddo
    enddo
!
  endif
  end subroutine gmm_checkpoint_382
  subroutine gmm_checkpoint_181(read_or_write)
  use gmm_internals
  use pointer_table_data_181
  implicit none
  logical read_or_write
  integer istat, fnom, i, j, ier, lcl_pti
  type(gmm_layout), dimension(1:1) :: siz
  type(gmm_attributes) :: attrib
  integer *8 :: key
  external fnom
      integer *8 get_address_from
      external get_address_from
  if (read_or_write) then      
!
    call add_directory_entry  
!    read(file_unit)directory(cur_page)%entry(cur_entry)%m%a%name  ! read name of variable
    read(file_unit)directory(cur_page)%entry(cur_entry)%name
    read(file_unit)siz(1:1)                                    
    directory(cur_page)%entry(cur_entry)%l(1:1) = siz(1:1) 
    read(file_unit)attrib                                        
!    print *,'name=',directory(cur_page)%entry(cur_entry)%name,' dims=',siz(1:DIM)
    attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
    directory(cur_page)%entry(cur_entry)%a = attrib            
    read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
    lcl_pti = lgmm_get_nxt_avail_ptr()
    directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
    ordinal = ordinal + 1
    key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
    key = key + ishft(181,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
    directory(cur_page)%entry(cur_entry)%a%key = key            
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
    allocate(gmm_ptrs181(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(siz(1)%low:siz(1)%high  ))
    read(file_unit)gmm_ptrs181(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p         
    directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
          &gmm_ptrs181(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',&
     &cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs181(directory(&
     &cur_page)%entry(cur_entry)%pointer_table_index)%p)
    endif
    ier=add_table_entry(gmm_ptrs181(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
!
  else                    
!
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      print *,'checkpointing type ',181
    endif
    do i=1,table_size
      do j=1,PAGE_SIZE
        if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0.and.directory(i)%entry(j)%data_type == 181) then
          if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'writing field ',directory(i)%entry(j)%name
          endif
          write(file_unit)181
          write(file_unit)directory(i)%entry(j)%name
          write(file_unit)directory(i)%entry(j)%l(1:1)
          attrib = directory(i)%entry(j)%a
          attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
          write(file_unit)attrib
          write(file_unit)directory(i)%entry(j)%data_type
          write(file_unit)gmm_ptrs181(directory(i)%entry(j)%pointer_table_index)%p
        endif
      enddo
    enddo
!
  endif
  end subroutine gmm_checkpoint_181
  subroutine gmm_checkpoint_141(read_or_write)
  use gmm_internals
  use pointer_table_data_141
  implicit none
  logical read_or_write
  integer istat, fnom, i, j, ier, lcl_pti
  type(gmm_layout), dimension(1:1) :: siz
  type(gmm_attributes) :: attrib
  integer *8 :: key
  external fnom
      integer *8 get_address_from
      external get_address_from
  if (read_or_write) then      
!
    call add_directory_entry  
!    read(file_unit)directory(cur_page)%entry(cur_entry)%m%a%name  ! read name of variable
    read(file_unit)directory(cur_page)%entry(cur_entry)%name
    read(file_unit)siz(1:1)                                    
    directory(cur_page)%entry(cur_entry)%l(1:1) = siz(1:1) 
    read(file_unit)attrib                                        
!    print *,'name=',directory(cur_page)%entry(cur_entry)%name,' dims=',siz(1:DIM)
    attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
    directory(cur_page)%entry(cur_entry)%a = attrib            
    read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
    lcl_pti = lgmm_get_nxt_avail_ptr()
    directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
    ordinal = ordinal + 1
    key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
    key = key + ishft(141,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
    directory(cur_page)%entry(cur_entry)%a%key = key            
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
    allocate(gmm_ptrs141(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(siz(1)%low:siz(1)%high  ))
    read(file_unit)gmm_ptrs141(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p         
    directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
          &gmm_ptrs141(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',&
     &cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs141(directory(&
     &cur_page)%entry(cur_entry)%pointer_table_index)%p)
    endif
    ier=add_table_entry(gmm_ptrs141(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
!
  else                    
!
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      print *,'checkpointing type ',141
    endif
    do i=1,table_size
      do j=1,PAGE_SIZE
        if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0.and.directory(i)%entry(j)%data_type == 141) then
          if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'writing field ',directory(i)%entry(j)%name
          endif
          write(file_unit)141
          write(file_unit)directory(i)%entry(j)%name
          write(file_unit)directory(i)%entry(j)%l(1:1)
          attrib = directory(i)%entry(j)%a
          attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
          write(file_unit)attrib
          write(file_unit)directory(i)%entry(j)%data_type
          write(file_unit)gmm_ptrs141(directory(i)%entry(j)%pointer_table_index)%p
        endif
      enddo
    enddo
!
  endif
  end subroutine gmm_checkpoint_141
  subroutine gmm_checkpoint_281(read_or_write)
  use gmm_internals
  use pointer_table_data_281
  implicit none
  logical read_or_write
  integer istat, fnom, i, j, ier, lcl_pti
  type(gmm_layout), dimension(1:1) :: siz
  type(gmm_attributes) :: attrib
  integer *8 :: key
  external fnom
      integer *8 get_address_from
      external get_address_from
  if (read_or_write) then      
!
    call add_directory_entry  
!    read(file_unit)directory(cur_page)%entry(cur_entry)%m%a%name  ! read name of variable
    read(file_unit)directory(cur_page)%entry(cur_entry)%name
    read(file_unit)siz(1:1)                                    
    directory(cur_page)%entry(cur_entry)%l(1:1) = siz(1:1) 
    read(file_unit)attrib                                        
!    print *,'name=',directory(cur_page)%entry(cur_entry)%name,' dims=',siz(1:DIM)
    attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
    directory(cur_page)%entry(cur_entry)%a = attrib            
    read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
    lcl_pti = lgmm_get_nxt_avail_ptr()
    directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
    ordinal = ordinal + 1
    key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
    key = key + ishft(281,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
    directory(cur_page)%entry(cur_entry)%a%key = key            
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
    allocate(gmm_ptrs281(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(siz(1)%low:siz(1)%high  ))
    read(file_unit)gmm_ptrs281(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p         
    directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
          &gmm_ptrs281(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',&
     &cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs281(directory(&
     &cur_page)%entry(cur_entry)%pointer_table_index)%p)
    endif
    ier=add_table_entry(gmm_ptrs281(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
!
  else                    
!
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      print *,'checkpointing type ',281
    endif
    do i=1,table_size
      do j=1,PAGE_SIZE
        if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0.and.directory(i)%entry(j)%data_type == 281) then
          if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'writing field ',directory(i)%entry(j)%name
          endif
          write(file_unit)281
          write(file_unit)directory(i)%entry(j)%name
          write(file_unit)directory(i)%entry(j)%l(1:1)
          attrib = directory(i)%entry(j)%a
          attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
          write(file_unit)attrib
          write(file_unit)directory(i)%entry(j)%data_type
          write(file_unit)gmm_ptrs281(directory(i)%entry(j)%pointer_table_index)%p
        endif
      enddo
    enddo
!
  endif
  end subroutine gmm_checkpoint_281
  subroutine gmm_checkpoint_241(read_or_write)
  use gmm_internals
  use pointer_table_data_241
  implicit none
  logical read_or_write
  integer istat, fnom, i, j, ier, lcl_pti
  type(gmm_layout), dimension(1:1) :: siz
  type(gmm_attributes) :: attrib
  integer *8 :: key
  external fnom
      integer *8 get_address_from
      external get_address_from
  if (read_or_write) then      
!
    call add_directory_entry  
!    read(file_unit)directory(cur_page)%entry(cur_entry)%m%a%name  ! read name of variable
    read(file_unit)directory(cur_page)%entry(cur_entry)%name
    read(file_unit)siz(1:1)                                    
    directory(cur_page)%entry(cur_entry)%l(1:1) = siz(1:1) 
    read(file_unit)attrib                                        
!    print *,'name=',directory(cur_page)%entry(cur_entry)%name,' dims=',siz(1:DIM)
    attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
    directory(cur_page)%entry(cur_entry)%a = attrib            
    read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
    lcl_pti = lgmm_get_nxt_avail_ptr()
    directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
    ordinal = ordinal + 1
    key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
    key = key + ishft(241,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
    directory(cur_page)%entry(cur_entry)%a%key = key            
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
    allocate(gmm_ptrs241(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(siz(1)%low:siz(1)%high  ))
    read(file_unit)gmm_ptrs241(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p         
    directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
          &gmm_ptrs241(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',&
     &cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs241(directory(&
     &cur_page)%entry(cur_entry)%pointer_table_index)%p)
    endif
    ier=add_table_entry(gmm_ptrs241(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
!
  else                    
!
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      print *,'checkpointing type ',241
    endif
    do i=1,table_size
      do j=1,PAGE_SIZE
        if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0.and.directory(i)%entry(j)%data_type == 241) then
          if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'writing field ',directory(i)%entry(j)%name
          endif
          write(file_unit)241
          write(file_unit)directory(i)%entry(j)%name
          write(file_unit)directory(i)%entry(j)%l(1:1)
          attrib = directory(i)%entry(j)%a
          attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
          write(file_unit)attrib
          write(file_unit)directory(i)%entry(j)%data_type
          write(file_unit)gmm_ptrs241(directory(i)%entry(j)%pointer_table_index)%p
        endif
      enddo
    enddo
!
  endif
  end subroutine gmm_checkpoint_241
  subroutine gmm_checkpoint_381(read_or_write)
  use gmm_internals
  use pointer_table_data_381
  implicit none
  logical read_or_write
  integer istat, fnom, i, j, ier, lcl_pti
  type(gmm_layout), dimension(1:1) :: siz
  type(gmm_attributes) :: attrib
  integer *8 :: key
  external fnom
      integer *8 get_address_from
      external get_address_from
  if (read_or_write) then      
!
    call add_directory_entry  
!    read(file_unit)directory(cur_page)%entry(cur_entry)%m%a%name  ! read name of variable
    read(file_unit)directory(cur_page)%entry(cur_entry)%name
    read(file_unit)siz(1:1)                                    
    directory(cur_page)%entry(cur_entry)%l(1:1) = siz(1:1) 
    read(file_unit)attrib                                        
!    print *,'name=',directory(cur_page)%entry(cur_entry)%name,' dims=',siz(1:DIM)
    attrib%flags = ior(attrib%flags,GMM_FLAG_READ)
    directory(cur_page)%entry(cur_entry)%a = attrib            
    read(file_unit)directory(cur_page)%entry(cur_entry)%data_type
    lcl_pti = lgmm_get_nxt_avail_ptr()
    directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
    ordinal = ordinal + 1
    key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
    key = key + ishft(381,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
    directory(cur_page)%entry(cur_entry)%a%key = key            
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
    allocate(gmm_ptrs381(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p(siz(1)%low:siz(1)%high  ))
    read(file_unit)gmm_ptrs381(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p         
    directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(&
          &gmm_ptrs381(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p)
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'name=',directory(cur_page)%entry(cur_entry)%name,' cur_page=',cur_page,' cur_entry=',&
     &cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(gmm_ptrs381(directory(&
     &cur_page)%entry(cur_entry)%pointer_table_index)%p)
    endif
    ier=add_table_entry(gmm_ptrs381(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p,key)
!
  else                    
!
    if (gmm_verbose_level == GMM_MSG_DEBUG) then
      print *,'checkpointing type ',381
    endif
    do i=1,table_size
      do j=1,PAGE_SIZE
        if (iand(GMM_FLAG_RSTR,directory(i)%entry(j)%a%flags) .ne. 0.and.directory(i)%entry(j)%data_type == 381) then
          if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'writing field ',directory(i)%entry(j)%name
          endif
          write(file_unit)381
          write(file_unit)directory(i)%entry(j)%name
          write(file_unit)directory(i)%entry(j)%l(1:1)
          attrib = directory(i)%entry(j)%a
          attrib%flags = iand(attrib%flags,FLAGS_KEPT_IN_RESTART)
          write(file_unit)attrib
          write(file_unit)directory(i)%entry(j)%data_type
          write(file_unit)gmm_ptrs381(directory(i)%entry(j)%pointer_table_index)%p
        endif
      enddo
    enddo
!
  endif
  end subroutine gmm_checkpoint_381
