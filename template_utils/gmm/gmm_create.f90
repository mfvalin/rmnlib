!!===================== gmm_create (interface) =====================
!
!!===================== gmm_create (code) =====================
   integer function gmm_create184(iname,p,field_meta,flags_arg)
   use gmm_internals
   use pointer_table_data_184
   implicit none
   character(len=*), intent(in) :: iname               
   integer*8, pointer :: p(:,:,:,:)
   type(gmm_metadata), intent(inout) :: field_meta               
   integer, intent(in), optional :: flags_arg
   integer *8 get_address_from
   external get_address_from
   external fool_optimizer
       interface
         subroutine check_directory_entry(name,key)
         character(len=*) :: name
         integer*8, intent(in) :: key
         end subroutine check_directory_entry
         subroutine find_directory_entry(name, key)
         implicit none
         character(len=*) :: name
         integer*8, optional :: key
         end subroutine find_directory_entry
         subroutine add_directory_entry
         end subroutine add_directory_entry
       end interface
   type(gmm_attributes) :: localattr, attrs
   type (gmm_attributes) lcl_attr
   type(gmm_layout), dimension(4) :: lcl_layout, dims
   integer lcl_datatype
   integer*8 :: key                      
   logical consistent
   integer i, ier
   integer lcl_pti
   character(len=GMM_MAXNAMELENGTH) :: lcl_name               
   integer u_bound, l_bound
   integer*8, pointer :: pp(:,:,:,:)
   if (present(flags_arg)) then
    field_meta%a%flags = flags_arg
   endif
   lcl_layout = field_meta%l
   dims = lcl_layout
   lcl_attr   = field_meta%a
   attrs = lcl_attr
   lcl_datatype = 184
   
   
   if (associated(p)) then
      consistent=.true.
      do i=1,4   
         consistent = consistent .and. size(p,i).eq.(dims(i)%high-dims(i)%low+1)
      enddo
      if (.not. consistent ) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'ERROR: gmm_create, p has dimensions that are not consistent with dims'
         endif
         key=0
         gmm_create184 = GMM_INCONSISTENT_DIMS
         return 
      endif
   endif  
   
   localattr = attrs    
   lcl_name = trim(iname)
   localattr%flags = iand(localattr%flags,FLAGS_KEPT_ON_CREATE)  
!   call find_directory_entry(localattr%name,key)     ! is there a field with this name that exists ?
   call find_directory_entry(lcl_name,key)     
   
   
   if (cur_page .ne. 0 .and. cur_entry .ne. 0) then    
      if (associated(p)) then
         print *,'ERROR: gmm_create called with existing p and array has already been created'
         key=0
         gmm_create184 = GMM_ARRAY_ALREADY_EXISTS
         return 
      endif
      pp=>gmm_ptrs184(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
      consistent=.true.
      do i=1,4   
         call fool_optimizer(size(pp,i))  
         consistent = consistent .and. (size(pp,i).eq.(dims(i)%high-dims(i)%low+1))
         if (.not. consistent ) print *,'size(pp,',i,')=',size(pp,i),' high=',dims(i)%high,' low=',dims(i)%low
      enddo
      if (.not. consistent ) then
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=1 =',lbound(pp,1),ubound(pp,1)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=2 =',lbound(pp,2),ubound(pp,2)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=3 =',lbound(pp,3),ubound(pp,3)
         print *,'ERROR: gmm_create, requested dimensions differ from previous specification (restart/create)'
         print *,'ERROR: gmm_create, variable name ="',lcl_name,'"'
         key=0
         nullify(p)
         gmm_create184 = GMM_INCONSISTENT_DIMS
         return 
      else
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
           print *,'INFO: gmm_create, variable name =',lcl_name,' exists and is consistent'
         endif
      endif
   
      if (iand(GMM_FLAG_CRTD,directory(cur_page)%entry(cur_entry)%a%flags) .ne. 0) then 
         print *,'ERROR: gmm_create, field ',lcl_name,' has already been created'
         key = 0
         nullify(p)
         gmm_create184 = GMM_VARIABLE_ALREADY_CREATED
         return
!
      else   
!         print *,'Debug+ ', \' Cat(gmm_create, EXTENSION,) \','array ',lcl_name,'must then have been read from a restart file' 
         localattr%flags = ior(localattr%flags,directory(cur_page)%entry(cur_entry)%a%flags) 
         key=directory(cur_page)%entry(cur_entry)%a%key      
         directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
         p=>gmm_ptrs184(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p   
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i20)') 'Debug+++ gmm_create name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p)
         directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
         gmm_create184 = 0
         return   
      endif
   else        
      if (iand(GMM_FLAG_RSTR,localattr%flags).ne.0 .and. restart_mode) then  
         print *,'ERROR: gmm_create field ',lcl_name, 'should have been read from restart file but was not'
!  ==========  HOW SERIOUS AN ERROR IS THIS ? ===============
      endif
      call add_directory_entry
   endif
   
   
   ordinal = ordinal + 1   
   key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
   key = key + ishft(184,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
   directory(cur_page)%entry(cur_entry)%a = localattr  
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
   directory(cur_page)%entry(cur_entry)%name = lcl_name
   directory(cur_page)%entry(cur_entry)%a%key = key
   directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
   directory(cur_page)%entry(cur_entry)%l(1:4) = dims(1:4)  
   directory(cur_page)%entry(cur_entry)%data_type = 184 
   if (associated(p)) then  
      print *,'GMM_CREATE: using user supplied array'
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      ier = add_table_entry(p, key)
! ======= must check that certain attributes are not requested (e.g. FLAG_RSTR) and that size is consistent
   else
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      allocate(p(dims(1)%low:dims(1)%high,&
                &dims(2)%low:dims(2)%high,&
                &dims(3)%low:dims(3)%high,&
                &dims(4)%low:dims(4)%high),stat=ier)
      if (ier /= 0) then
         gmm_create184 = GMM_ERROR
         return
      endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_create creation name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
      directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
      ier = add_table_entry(p, key)
   endif
   field_meta%l = directory(cur_page)%entry(cur_entry)%l
   field_meta%a = directory(cur_page)%entry(cur_entry)%a
!    Cat(gmm_ptrs, EXTENSION,)(directory(cur_page)%entry(cur_entry)%f => p
!         if (iand(directory(cur_page)%entry(cur_entry)%a%flags,FLAG_IZER) .ne. 0) then
!           directory(cur_page)%entry(cur_entry)%f = 0        ! ZERO fill requested
!         endif
! CODE MISSING HERE FOR INITIALIZATION
   if (iand(field_meta%a%flags, GMM_FLAG_IZER) /= 0) THEN
!     print *,'Debug+ gmm_create init to zero for variable ',lcl_name
     p = 0
   endif
   gmm_create184 = 0
   end function gmm_create184
!
   integer function gmm_create144(iname,p,field_meta,flags_arg)
   use gmm_internals
   use pointer_table_data_144
   implicit none
   character(len=*), intent(in) :: iname               
   integer*4, pointer :: p(:,:,:,:)
   type(gmm_metadata), intent(inout) :: field_meta               
   integer, intent(in), optional :: flags_arg
   integer *8 get_address_from
   external get_address_from
   external fool_optimizer
       interface
         subroutine check_directory_entry(name,key)
         character(len=*) :: name
         integer*8, intent(in) :: key
         end subroutine check_directory_entry
         subroutine find_directory_entry(name, key)
         implicit none
         character(len=*) :: name
         integer*8, optional :: key
         end subroutine find_directory_entry
         subroutine add_directory_entry
         end subroutine add_directory_entry
       end interface
   type(gmm_attributes) :: localattr, attrs
   type (gmm_attributes) lcl_attr
   type(gmm_layout), dimension(4) :: lcl_layout, dims
   integer lcl_datatype
   integer*8 :: key                      
   logical consistent
   integer i, ier
   integer lcl_pti
   character(len=GMM_MAXNAMELENGTH) :: lcl_name               
   integer u_bound, l_bound
   integer*4, pointer :: pp(:,:,:,:)
   if (present(flags_arg)) then
    field_meta%a%flags = flags_arg
   endif
   lcl_layout = field_meta%l
   dims = lcl_layout
   lcl_attr   = field_meta%a
   attrs = lcl_attr
   lcl_datatype = 144
   
   
   if (associated(p)) then
      consistent=.true.
      do i=1,4   
         consistent = consistent .and. size(p,i).eq.(dims(i)%high-dims(i)%low+1)
      enddo
      if (.not. consistent ) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'ERROR: gmm_create, p has dimensions that are not consistent with dims'
         endif
         key=0
         gmm_create144 = GMM_INCONSISTENT_DIMS
         return 
      endif
   endif  
   
   localattr = attrs    
   lcl_name = trim(iname)
   localattr%flags = iand(localattr%flags,FLAGS_KEPT_ON_CREATE)  
!   call find_directory_entry(localattr%name,key)     ! is there a field with this name that exists ?
   call find_directory_entry(lcl_name,key)     
   
   
   if (cur_page .ne. 0 .and. cur_entry .ne. 0) then    
      if (associated(p)) then
         print *,'ERROR: gmm_create called with existing p and array has already been created'
         key=0
         gmm_create144 = GMM_ARRAY_ALREADY_EXISTS
         return 
      endif
      pp=>gmm_ptrs144(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
      consistent=.true.
      do i=1,4   
         call fool_optimizer(size(pp,i))  
         consistent = consistent .and. (size(pp,i).eq.(dims(i)%high-dims(i)%low+1))
         if (.not. consistent ) print *,'size(pp,',i,')=',size(pp,i),' high=',dims(i)%high,' low=',dims(i)%low
      enddo
      if (.not. consistent ) then
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=1 =',lbound(pp,1),ubound(pp,1)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=2 =',lbound(pp,2),ubound(pp,2)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=3 =',lbound(pp,3),ubound(pp,3)
         print *,'ERROR: gmm_create, requested dimensions differ from previous specification (restart/create)'
         print *,'ERROR: gmm_create, variable name ="',lcl_name,'"'
         key=0
         nullify(p)
         gmm_create144 = GMM_INCONSISTENT_DIMS
         return 
      else
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
           print *,'INFO: gmm_create, variable name =',lcl_name,' exists and is consistent'
         endif
      endif
   
      if (iand(GMM_FLAG_CRTD,directory(cur_page)%entry(cur_entry)%a%flags) .ne. 0) then 
         print *,'ERROR: gmm_create, field ',lcl_name,' has already been created'
         key = 0
         nullify(p)
         gmm_create144 = GMM_VARIABLE_ALREADY_CREATED
         return
!
      else   
!         print *,'Debug+ ', \' Cat(gmm_create, EXTENSION,) \','array ',lcl_name,'must then have been read from a restart file' 
         localattr%flags = ior(localattr%flags,directory(cur_page)%entry(cur_entry)%a%flags) 
         key=directory(cur_page)%entry(cur_entry)%a%key      
         directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
         p=>gmm_ptrs144(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p   
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i20)') 'Debug+++ gmm_create name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p)
         directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
         gmm_create144 = 0
         return   
      endif
   else        
      if (iand(GMM_FLAG_RSTR,localattr%flags).ne.0 .and. restart_mode) then  
         print *,'ERROR: gmm_create field ',lcl_name, 'should have been read from restart file but was not'
!  ==========  HOW SERIOUS AN ERROR IS THIS ? ===============
      endif
      call add_directory_entry
   endif
   
   
   ordinal = ordinal + 1   
   key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
   key = key + ishft(144,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
   directory(cur_page)%entry(cur_entry)%a = localattr  
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
   directory(cur_page)%entry(cur_entry)%name = lcl_name
   directory(cur_page)%entry(cur_entry)%a%key = key
   directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
   directory(cur_page)%entry(cur_entry)%l(1:4) = dims(1:4)  
   directory(cur_page)%entry(cur_entry)%data_type = 144 
   if (associated(p)) then  
      print *,'GMM_CREATE: using user supplied array'
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      ier = add_table_entry(p, key)
! ======= must check that certain attributes are not requested (e.g. FLAG_RSTR) and that size is consistent
   else
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      allocate(p(dims(1)%low:dims(1)%high,&
                &dims(2)%low:dims(2)%high,&
                &dims(3)%low:dims(3)%high,&
                &dims(4)%low:dims(4)%high),stat=ier)
      if (ier /= 0) then
         gmm_create144 = GMM_ERROR
         return
      endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_create creation name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
      directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
      ier = add_table_entry(p, key)
   endif
   field_meta%l = directory(cur_page)%entry(cur_entry)%l
   field_meta%a = directory(cur_page)%entry(cur_entry)%a
!    Cat(gmm_ptrs, EXTENSION,)(directory(cur_page)%entry(cur_entry)%f => p
!         if (iand(directory(cur_page)%entry(cur_entry)%a%flags,FLAG_IZER) .ne. 0) then
!           directory(cur_page)%entry(cur_entry)%f = 0        ! ZERO fill requested
!         endif
! CODE MISSING HERE FOR INITIALIZATION
   if (iand(field_meta%a%flags, GMM_FLAG_IZER) /= 0) THEN
!     print *,'Debug+ gmm_create init to zero for variable ',lcl_name
     p = 0
   endif
   gmm_create144 = 0
   end function gmm_create144
!
   integer function gmm_create284(iname,p,field_meta,flags_arg)
   use gmm_internals
   use pointer_table_data_284
   implicit none
   character(len=*), intent(in) :: iname               
   real*8, pointer :: p(:,:,:,:)
   type(gmm_metadata), intent(inout) :: field_meta               
   integer, intent(in), optional :: flags_arg
   integer *8 get_address_from
   external get_address_from
   external fool_optimizer
       interface
         subroutine check_directory_entry(name,key)
         character(len=*) :: name
         integer*8, intent(in) :: key
         end subroutine check_directory_entry
         subroutine find_directory_entry(name, key)
         implicit none
         character(len=*) :: name
         integer*8, optional :: key
         end subroutine find_directory_entry
         subroutine add_directory_entry
         end subroutine add_directory_entry
       end interface
   type(gmm_attributes) :: localattr, attrs
   type (gmm_attributes) lcl_attr
   type(gmm_layout), dimension(4) :: lcl_layout, dims
   integer lcl_datatype
   integer*8 :: key                      
   logical consistent
   integer i, ier
   integer lcl_pti
   character(len=GMM_MAXNAMELENGTH) :: lcl_name               
   integer u_bound, l_bound
   real*8, pointer :: pp(:,:,:,:)
   if (present(flags_arg)) then
    field_meta%a%flags = flags_arg
   endif
   lcl_layout = field_meta%l
   dims = lcl_layout
   lcl_attr   = field_meta%a
   attrs = lcl_attr
   lcl_datatype = 284
   
   
   if (associated(p)) then
      consistent=.true.
      do i=1,4   
         consistent = consistent .and. size(p,i).eq.(dims(i)%high-dims(i)%low+1)
      enddo
      if (.not. consistent ) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'ERROR: gmm_create, p has dimensions that are not consistent with dims'
         endif
         key=0
         gmm_create284 = GMM_INCONSISTENT_DIMS
         return 
      endif
   endif  
   
   localattr = attrs    
   lcl_name = trim(iname)
   localattr%flags = iand(localattr%flags,FLAGS_KEPT_ON_CREATE)  
!   call find_directory_entry(localattr%name,key)     ! is there a field with this name that exists ?
   call find_directory_entry(lcl_name,key)     
   
   
   if (cur_page .ne. 0 .and. cur_entry .ne. 0) then    
      if (associated(p)) then
         print *,'ERROR: gmm_create called with existing p and array has already been created'
         key=0
         gmm_create284 = GMM_ARRAY_ALREADY_EXISTS
         return 
      endif
      pp=>gmm_ptrs284(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
      consistent=.true.
      do i=1,4   
         call fool_optimizer(size(pp,i))  
         consistent = consistent .and. (size(pp,i).eq.(dims(i)%high-dims(i)%low+1))
         if (.not. consistent ) print *,'size(pp,',i,')=',size(pp,i),' high=',dims(i)%high,' low=',dims(i)%low
      enddo
      if (.not. consistent ) then
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=1 =',lbound(pp,1),ubound(pp,1)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=2 =',lbound(pp,2),ubound(pp,2)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=3 =',lbound(pp,3),ubound(pp,3)
         print *,'ERROR: gmm_create, requested dimensions differ from previous specification (restart/create)'
         print *,'ERROR: gmm_create, variable name ="',lcl_name,'"'
         key=0
         nullify(p)
         gmm_create284 = GMM_INCONSISTENT_DIMS
         return 
      else
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
           print *,'INFO: gmm_create, variable name =',lcl_name,' exists and is consistent'
         endif
      endif
   
      if (iand(GMM_FLAG_CRTD,directory(cur_page)%entry(cur_entry)%a%flags) .ne. 0) then 
         print *,'ERROR: gmm_create, field ',lcl_name,' has already been created'
         key = 0
         nullify(p)
         gmm_create284 = GMM_VARIABLE_ALREADY_CREATED
         return
!
      else   
!         print *,'Debug+ ', \' Cat(gmm_create, EXTENSION,) \','array ',lcl_name,'must then have been read from a restart file' 
         localattr%flags = ior(localattr%flags,directory(cur_page)%entry(cur_entry)%a%flags) 
         key=directory(cur_page)%entry(cur_entry)%a%key      
         directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
         p=>gmm_ptrs284(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p   
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i20)') 'Debug+++ gmm_create name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p)
         directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
         gmm_create284 = 0
         return   
      endif
   else        
      if (iand(GMM_FLAG_RSTR,localattr%flags).ne.0 .and. restart_mode) then  
         print *,'ERROR: gmm_create field ',lcl_name, 'should have been read from restart file but was not'
!  ==========  HOW SERIOUS AN ERROR IS THIS ? ===============
      endif
      call add_directory_entry
   endif
   
   
   ordinal = ordinal + 1   
   key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
   key = key + ishft(284,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
   directory(cur_page)%entry(cur_entry)%a = localattr  
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
   directory(cur_page)%entry(cur_entry)%name = lcl_name
   directory(cur_page)%entry(cur_entry)%a%key = key
   directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
   directory(cur_page)%entry(cur_entry)%l(1:4) = dims(1:4)  
   directory(cur_page)%entry(cur_entry)%data_type = 284 
   if (associated(p)) then  
      print *,'GMM_CREATE: using user supplied array'
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      ier = add_table_entry(p, key)
! ======= must check that certain attributes are not requested (e.g. FLAG_RSTR) and that size is consistent
   else
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      allocate(p(dims(1)%low:dims(1)%high,&
                &dims(2)%low:dims(2)%high,&
                &dims(3)%low:dims(3)%high,&
                &dims(4)%low:dims(4)%high),stat=ier)
      if (ier /= 0) then
         gmm_create284 = GMM_ERROR
         return
      endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_create creation name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
      directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
      ier = add_table_entry(p, key)
   endif
   field_meta%l = directory(cur_page)%entry(cur_entry)%l
   field_meta%a = directory(cur_page)%entry(cur_entry)%a
!    Cat(gmm_ptrs, EXTENSION,)(directory(cur_page)%entry(cur_entry)%f => p
!         if (iand(directory(cur_page)%entry(cur_entry)%a%flags,FLAG_IZER) .ne. 0) then
!           directory(cur_page)%entry(cur_entry)%f = 0        ! ZERO fill requested
!         endif
! CODE MISSING HERE FOR INITIALIZATION
   if (iand(field_meta%a%flags, GMM_FLAG_IZER) /= 0) THEN
!     print *,'Debug+ gmm_create init to zero for variable ',lcl_name
     p = 0
   endif
   gmm_create284 = 0
   end function gmm_create284
!
   integer function gmm_create244(iname,p,field_meta,flags_arg)
   use gmm_internals
   use pointer_table_data_244
   implicit none
   character(len=*), intent(in) :: iname               
   real*4, pointer :: p(:,:,:,:)
   type(gmm_metadata), intent(inout) :: field_meta               
   integer, intent(in), optional :: flags_arg
   integer *8 get_address_from
   external get_address_from
   external fool_optimizer
       interface
         subroutine check_directory_entry(name,key)
         character(len=*) :: name
         integer*8, intent(in) :: key
         end subroutine check_directory_entry
         subroutine find_directory_entry(name, key)
         implicit none
         character(len=*) :: name
         integer*8, optional :: key
         end subroutine find_directory_entry
         subroutine add_directory_entry
         end subroutine add_directory_entry
       end interface
   type(gmm_attributes) :: localattr, attrs
   type (gmm_attributes) lcl_attr
   type(gmm_layout), dimension(4) :: lcl_layout, dims
   integer lcl_datatype
   integer*8 :: key                      
   logical consistent
   integer i, ier
   integer lcl_pti
   character(len=GMM_MAXNAMELENGTH) :: lcl_name               
   integer u_bound, l_bound
   real*4, pointer :: pp(:,:,:,:)
   if (present(flags_arg)) then
    field_meta%a%flags = flags_arg
   endif
   lcl_layout = field_meta%l
   dims = lcl_layout
   lcl_attr   = field_meta%a
   attrs = lcl_attr
   lcl_datatype = 244
   
   
   if (associated(p)) then
      consistent=.true.
      do i=1,4   
         consistent = consistent .and. size(p,i).eq.(dims(i)%high-dims(i)%low+1)
      enddo
      if (.not. consistent ) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'ERROR: gmm_create, p has dimensions that are not consistent with dims'
         endif
         key=0
         gmm_create244 = GMM_INCONSISTENT_DIMS
         return 
      endif
   endif  
   
   localattr = attrs    
   lcl_name = trim(iname)
   localattr%flags = iand(localattr%flags,FLAGS_KEPT_ON_CREATE)  
!   call find_directory_entry(localattr%name,key)     ! is there a field with this name that exists ?
   call find_directory_entry(lcl_name,key)     
   
   
   if (cur_page .ne. 0 .and. cur_entry .ne. 0) then    
      if (associated(p)) then
         print *,'ERROR: gmm_create called with existing p and array has already been created'
         key=0
         gmm_create244 = GMM_ARRAY_ALREADY_EXISTS
         return 
      endif
      pp=>gmm_ptrs244(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
      consistent=.true.
      do i=1,4   
         call fool_optimizer(size(pp,i))  
         consistent = consistent .and. (size(pp,i).eq.(dims(i)%high-dims(i)%low+1))
         if (.not. consistent ) print *,'size(pp,',i,')=',size(pp,i),' high=',dims(i)%high,' low=',dims(i)%low
      enddo
      if (.not. consistent ) then
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=1 =',lbound(pp,1),ubound(pp,1)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=2 =',lbound(pp,2),ubound(pp,2)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=3 =',lbound(pp,3),ubound(pp,3)
         print *,'ERROR: gmm_create, requested dimensions differ from previous specification (restart/create)'
         print *,'ERROR: gmm_create, variable name ="',lcl_name,'"'
         key=0
         nullify(p)
         gmm_create244 = GMM_INCONSISTENT_DIMS
         return 
      else
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
           print *,'INFO: gmm_create, variable name =',lcl_name,' exists and is consistent'
         endif
      endif
   
      if (iand(GMM_FLAG_CRTD,directory(cur_page)%entry(cur_entry)%a%flags) .ne. 0) then 
         print *,'ERROR: gmm_create, field ',lcl_name,' has already been created'
         key = 0
         nullify(p)
         gmm_create244 = GMM_VARIABLE_ALREADY_CREATED
         return
!
      else   
!         print *,'Debug+ ', \' Cat(gmm_create, EXTENSION,) \','array ',lcl_name,'must then have been read from a restart file' 
         localattr%flags = ior(localattr%flags,directory(cur_page)%entry(cur_entry)%a%flags) 
         key=directory(cur_page)%entry(cur_entry)%a%key      
         directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
         p=>gmm_ptrs244(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p   
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i20)') 'Debug+++ gmm_create name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p)
         directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
         gmm_create244 = 0
         return   
      endif
   else        
      if (iand(GMM_FLAG_RSTR,localattr%flags).ne.0 .and. restart_mode) then  
         print *,'ERROR: gmm_create field ',lcl_name, 'should have been read from restart file but was not'
!  ==========  HOW SERIOUS AN ERROR IS THIS ? ===============
      endif
      call add_directory_entry
   endif
   
   
   ordinal = ordinal + 1   
   key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
   key = key + ishft(244,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
   directory(cur_page)%entry(cur_entry)%a = localattr  
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
   directory(cur_page)%entry(cur_entry)%name = lcl_name
   directory(cur_page)%entry(cur_entry)%a%key = key
   directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
   directory(cur_page)%entry(cur_entry)%l(1:4) = dims(1:4)  
   directory(cur_page)%entry(cur_entry)%data_type = 244 
   if (associated(p)) then  
      print *,'GMM_CREATE: using user supplied array'
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      ier = add_table_entry(p, key)
! ======= must check that certain attributes are not requested (e.g. FLAG_RSTR) and that size is consistent
   else
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      allocate(p(dims(1)%low:dims(1)%high,&
                &dims(2)%low:dims(2)%high,&
                &dims(3)%low:dims(3)%high,&
                &dims(4)%low:dims(4)%high),stat=ier)
      if (ier /= 0) then
         gmm_create244 = GMM_ERROR
         return
      endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_create creation name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
      directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
      ier = add_table_entry(p, key)
   endif
   field_meta%l = directory(cur_page)%entry(cur_entry)%l
   field_meta%a = directory(cur_page)%entry(cur_entry)%a
!    Cat(gmm_ptrs, EXTENSION,)(directory(cur_page)%entry(cur_entry)%f => p
!         if (iand(directory(cur_page)%entry(cur_entry)%a%flags,FLAG_IZER) .ne. 0) then
!           directory(cur_page)%entry(cur_entry)%f = 0        ! ZERO fill requested
!         endif
! CODE MISSING HERE FOR INITIALIZATION
   if (iand(field_meta%a%flags, GMM_FLAG_IZER) /= 0) THEN
!     print *,'Debug+ gmm_create init to zero for variable ',lcl_name
     p = 0
   endif
   gmm_create244 = 0
   end function gmm_create244
!
   integer function gmm_create384(iname,p,field_meta,flags_arg)
   use gmm_internals
   use pointer_table_data_384
   implicit none
   character(len=*), intent(in) :: iname               
   complex*8, pointer :: p(:,:,:,:)
   type(gmm_metadata), intent(inout) :: field_meta               
   integer, intent(in), optional :: flags_arg
   integer *8 get_address_from
   external get_address_from
   external fool_optimizer
       interface
         subroutine check_directory_entry(name,key)
         character(len=*) :: name
         integer*8, intent(in) :: key
         end subroutine check_directory_entry
         subroutine find_directory_entry(name, key)
         implicit none
         character(len=*) :: name
         integer*8, optional :: key
         end subroutine find_directory_entry
         subroutine add_directory_entry
         end subroutine add_directory_entry
       end interface
   type(gmm_attributes) :: localattr, attrs
   type (gmm_attributes) lcl_attr
   type(gmm_layout), dimension(4) :: lcl_layout, dims
   integer lcl_datatype
   integer*8 :: key                      
   logical consistent
   integer i, ier
   integer lcl_pti
   character(len=GMM_MAXNAMELENGTH) :: lcl_name               
   integer u_bound, l_bound
   complex*8, pointer :: pp(:,:,:,:)
   if (present(flags_arg)) then
    field_meta%a%flags = flags_arg
   endif
   lcl_layout = field_meta%l
   dims = lcl_layout
   lcl_attr   = field_meta%a
   attrs = lcl_attr
   lcl_datatype = 384
   
   
   if (associated(p)) then
      consistent=.true.
      do i=1,4   
         consistent = consistent .and. size(p,i).eq.(dims(i)%high-dims(i)%low+1)
      enddo
      if (.not. consistent ) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'ERROR: gmm_create, p has dimensions that are not consistent with dims'
         endif
         key=0
         gmm_create384 = GMM_INCONSISTENT_DIMS
         return 
      endif
   endif  
   
   localattr = attrs    
   lcl_name = trim(iname)
   localattr%flags = iand(localattr%flags,FLAGS_KEPT_ON_CREATE)  
!   call find_directory_entry(localattr%name,key)     ! is there a field with this name that exists ?
   call find_directory_entry(lcl_name,key)     
   
   
   if (cur_page .ne. 0 .and. cur_entry .ne. 0) then    
      if (associated(p)) then
         print *,'ERROR: gmm_create called with existing p and array has already been created'
         key=0
         gmm_create384 = GMM_ARRAY_ALREADY_EXISTS
         return 
      endif
      pp=>gmm_ptrs384(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
      consistent=.true.
      do i=1,4   
         call fool_optimizer(size(pp,i))  
         consistent = consistent .and. (size(pp,i).eq.(dims(i)%high-dims(i)%low+1))
         if (.not. consistent ) print *,'size(pp,',i,')=',size(pp,i),' high=',dims(i)%high,' low=',dims(i)%low
      enddo
      if (.not. consistent ) then
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=1 =',lbound(pp,1),ubound(pp,1)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=2 =',lbound(pp,2),ubound(pp,2)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=3 =',lbound(pp,3),ubound(pp,3)
         print *,'ERROR: gmm_create, requested dimensions differ from previous specification (restart/create)'
         print *,'ERROR: gmm_create, variable name ="',lcl_name,'"'
         key=0
         nullify(p)
         gmm_create384 = GMM_INCONSISTENT_DIMS
         return 
      else
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
           print *,'INFO: gmm_create, variable name =',lcl_name,' exists and is consistent'
         endif
      endif
   
      if (iand(GMM_FLAG_CRTD,directory(cur_page)%entry(cur_entry)%a%flags) .ne. 0) then 
         print *,'ERROR: gmm_create, field ',lcl_name,' has already been created'
         key = 0
         nullify(p)
         gmm_create384 = GMM_VARIABLE_ALREADY_CREATED
         return
!
      else   
!         print *,'Debug+ ', \' Cat(gmm_create, EXTENSION,) \','array ',lcl_name,'must then have been read from a restart file' 
         localattr%flags = ior(localattr%flags,directory(cur_page)%entry(cur_entry)%a%flags) 
         key=directory(cur_page)%entry(cur_entry)%a%key      
         directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
         p=>gmm_ptrs384(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p   
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i20)') 'Debug+++ gmm_create name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p)
         directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
         gmm_create384 = 0
         return   
      endif
   else        
      if (iand(GMM_FLAG_RSTR,localattr%flags).ne.0 .and. restart_mode) then  
         print *,'ERROR: gmm_create field ',lcl_name, 'should have been read from restart file but was not'
!  ==========  HOW SERIOUS AN ERROR IS THIS ? ===============
      endif
      call add_directory_entry
   endif
   
   
   ordinal = ordinal + 1   
   key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
   key = key + ishft(384,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
   directory(cur_page)%entry(cur_entry)%a = localattr  
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
   directory(cur_page)%entry(cur_entry)%name = lcl_name
   directory(cur_page)%entry(cur_entry)%a%key = key
   directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
   directory(cur_page)%entry(cur_entry)%l(1:4) = dims(1:4)  
   directory(cur_page)%entry(cur_entry)%data_type = 384 
   if (associated(p)) then  
      print *,'GMM_CREATE: using user supplied array'
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      ier = add_table_entry(p, key)
! ======= must check that certain attributes are not requested (e.g. FLAG_RSTR) and that size is consistent
   else
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      allocate(p(dims(1)%low:dims(1)%high,&
                &dims(2)%low:dims(2)%high,&
                &dims(3)%low:dims(3)%high,&
                &dims(4)%low:dims(4)%high),stat=ier)
      if (ier /= 0) then
         gmm_create384 = GMM_ERROR
         return
      endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_create creation name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
      directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
      ier = add_table_entry(p, key)
   endif
   field_meta%l = directory(cur_page)%entry(cur_entry)%l
   field_meta%a = directory(cur_page)%entry(cur_entry)%a
!    Cat(gmm_ptrs, EXTENSION,)(directory(cur_page)%entry(cur_entry)%f => p
!         if (iand(directory(cur_page)%entry(cur_entry)%a%flags,FLAG_IZER) .ne. 0) then
!           directory(cur_page)%entry(cur_entry)%f = 0        ! ZERO fill requested
!         endif
! CODE MISSING HERE FOR INITIALIZATION
   if (iand(field_meta%a%flags, GMM_FLAG_IZER) /= 0) THEN
!     print *,'Debug+ gmm_create init to zero for variable ',lcl_name
     p = 0
   endif
   gmm_create384 = 0
   end function gmm_create384
!
   integer function gmm_create183(iname,p,field_meta,flags_arg)
   use gmm_internals
   use pointer_table_data_183
   implicit none
   character(len=*), intent(in) :: iname               
   integer*8, pointer :: p(:,:,:)
   type(gmm_metadata), intent(inout) :: field_meta               
   integer, intent(in), optional :: flags_arg
   integer *8 get_address_from
   external get_address_from
   external fool_optimizer
       interface
         subroutine check_directory_entry(name,key)
         character(len=*) :: name
         integer*8, intent(in) :: key
         end subroutine check_directory_entry
         subroutine find_directory_entry(name, key)
         implicit none
         character(len=*) :: name
         integer*8, optional :: key
         end subroutine find_directory_entry
         subroutine add_directory_entry
         end subroutine add_directory_entry
       end interface
   type(gmm_attributes) :: localattr, attrs
   type (gmm_attributes) lcl_attr
   type(gmm_layout), dimension(4) :: lcl_layout, dims
   integer lcl_datatype
   integer*8 :: key                      
   logical consistent
   integer i, ier
   integer lcl_pti
   character(len=GMM_MAXNAMELENGTH) :: lcl_name               
   integer u_bound, l_bound
   integer*8, pointer :: pp(:,:,:)
   if (present(flags_arg)) then
    field_meta%a%flags = flags_arg
   endif
   lcl_layout = field_meta%l
   dims = lcl_layout
   lcl_attr   = field_meta%a
   attrs = lcl_attr
   lcl_datatype = 183
   
   
   if (associated(p)) then
      consistent=.true.
      do i=1,3   
         consistent = consistent .and. size(p,i).eq.(dims(i)%high-dims(i)%low+1)
      enddo
      if (.not. consistent ) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'ERROR: gmm_create, p has dimensions that are not consistent with dims'
         endif
         key=0
         gmm_create183 = GMM_INCONSISTENT_DIMS
         return 
      endif
   endif  
   
   localattr = attrs    
   lcl_name = trim(iname)
   localattr%flags = iand(localattr%flags,FLAGS_KEPT_ON_CREATE)  
!   call find_directory_entry(localattr%name,key)     ! is there a field with this name that exists ?
   call find_directory_entry(lcl_name,key)     
   
   
   if (cur_page .ne. 0 .and. cur_entry .ne. 0) then    
      if (associated(p)) then
         print *,'ERROR: gmm_create called with existing p and array has already been created'
         key=0
         gmm_create183 = GMM_ARRAY_ALREADY_EXISTS
         return 
      endif
      pp=>gmm_ptrs183(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
      consistent=.true.
      do i=1,3   
         call fool_optimizer(size(pp,i))  
         consistent = consistent .and. (size(pp,i).eq.(dims(i)%high-dims(i)%low+1))
         if (.not. consistent ) print *,'size(pp,',i,')=',size(pp,i),' high=',dims(i)%high,' low=',dims(i)%low
      enddo
      if (.not. consistent ) then
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=1 =',lbound(pp,1),ubound(pp,1)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=2 =',lbound(pp,2),ubound(pp,2)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=3 =',lbound(pp,3),ubound(pp,3)
         print *,'ERROR: gmm_create, requested dimensions differ from previous specification (restart/create)'
         print *,'ERROR: gmm_create, variable name ="',lcl_name,'"'
         key=0
         nullify(p)
         gmm_create183 = GMM_INCONSISTENT_DIMS
         return 
      else
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
           print *,'INFO: gmm_create, variable name =',lcl_name,' exists and is consistent'
         endif
      endif
   
      if (iand(GMM_FLAG_CRTD,directory(cur_page)%entry(cur_entry)%a%flags) .ne. 0) then 
         print *,'ERROR: gmm_create, field ',lcl_name,' has already been created'
         key = 0
         nullify(p)
         gmm_create183 = GMM_VARIABLE_ALREADY_CREATED
         return
!
      else   
!         print *,'Debug+ ', \' Cat(gmm_create, EXTENSION,) \','array ',lcl_name,'must then have been read from a restart file' 
         localattr%flags = ior(localattr%flags,directory(cur_page)%entry(cur_entry)%a%flags) 
         key=directory(cur_page)%entry(cur_entry)%a%key      
         directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
         p=>gmm_ptrs183(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p   
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i20)') 'Debug+++ gmm_create name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p)
         directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
         gmm_create183 = 0
         return   
      endif
   else        
      if (iand(GMM_FLAG_RSTR,localattr%flags).ne.0 .and. restart_mode) then  
         print *,'ERROR: gmm_create field ',lcl_name, 'should have been read from restart file but was not'
!  ==========  HOW SERIOUS AN ERROR IS THIS ? ===============
      endif
      call add_directory_entry
   endif
   
   
   ordinal = ordinal + 1   
   key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
   key = key + ishft(183,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
   directory(cur_page)%entry(cur_entry)%a = localattr  
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
   directory(cur_page)%entry(cur_entry)%name = lcl_name
   directory(cur_page)%entry(cur_entry)%a%key = key
   directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
   directory(cur_page)%entry(cur_entry)%l(1:3) = dims(1:3)  
   directory(cur_page)%entry(cur_entry)%data_type = 183 
   if (associated(p)) then  
      print *,'GMM_CREATE: using user supplied array'
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      ier = add_table_entry(p, key)
! ======= must check that certain attributes are not requested (e.g. FLAG_RSTR) and that size is consistent
   else
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      allocate(p(dims(1)%low:dims(1)%high,&
                &dims(2)%low:dims(2)%high,&
                &dims(3)%low:dims(3)%high),stat=ier)
      if (ier /= 0) then
         gmm_create183 = GMM_ERROR
         return
      endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_create creation name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
      directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
      ier = add_table_entry(p, key)
   endif
   field_meta%l = directory(cur_page)%entry(cur_entry)%l
   field_meta%a = directory(cur_page)%entry(cur_entry)%a
!    Cat(gmm_ptrs, EXTENSION,)(directory(cur_page)%entry(cur_entry)%f => p
!         if (iand(directory(cur_page)%entry(cur_entry)%a%flags,FLAG_IZER) .ne. 0) then
!           directory(cur_page)%entry(cur_entry)%f = 0        ! ZERO fill requested
!         endif
! CODE MISSING HERE FOR INITIALIZATION
   if (iand(field_meta%a%flags, GMM_FLAG_IZER) /= 0) THEN
!     print *,'Debug+ gmm_create init to zero for variable ',lcl_name
     p = 0
   endif
   gmm_create183 = 0
   end function gmm_create183
!
   integer function gmm_create143(iname,p,field_meta,flags_arg)
   use gmm_internals
   use pointer_table_data_143
   implicit none
   character(len=*), intent(in) :: iname               
   integer*4, pointer :: p(:,:,:)
   type(gmm_metadata), intent(inout) :: field_meta               
   integer, intent(in), optional :: flags_arg
   integer *8 get_address_from
   external get_address_from
   external fool_optimizer
       interface
         subroutine check_directory_entry(name,key)
         character(len=*) :: name
         integer*8, intent(in) :: key
         end subroutine check_directory_entry
         subroutine find_directory_entry(name, key)
         implicit none
         character(len=*) :: name
         integer*8, optional :: key
         end subroutine find_directory_entry
         subroutine add_directory_entry
         end subroutine add_directory_entry
       end interface
   type(gmm_attributes) :: localattr, attrs
   type (gmm_attributes) lcl_attr
   type(gmm_layout), dimension(4) :: lcl_layout, dims
   integer lcl_datatype
   integer*8 :: key                      
   logical consistent
   integer i, ier
   integer lcl_pti
   character(len=GMM_MAXNAMELENGTH) :: lcl_name               
   integer u_bound, l_bound
   integer*4, pointer :: pp(:,:,:)
   if (present(flags_arg)) then
    field_meta%a%flags = flags_arg
   endif
   lcl_layout = field_meta%l
   dims = lcl_layout
   lcl_attr   = field_meta%a
   attrs = lcl_attr
   lcl_datatype = 143
   
   
   if (associated(p)) then
      consistent=.true.
      do i=1,3   
         consistent = consistent .and. size(p,i).eq.(dims(i)%high-dims(i)%low+1)
      enddo
      if (.not. consistent ) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'ERROR: gmm_create, p has dimensions that are not consistent with dims'
         endif
         key=0
         gmm_create143 = GMM_INCONSISTENT_DIMS
         return 
      endif
   endif  
   
   localattr = attrs    
   lcl_name = trim(iname)
   localattr%flags = iand(localattr%flags,FLAGS_KEPT_ON_CREATE)  
!   call find_directory_entry(localattr%name,key)     ! is there a field with this name that exists ?
   call find_directory_entry(lcl_name,key)     
   
   
   if (cur_page .ne. 0 .and. cur_entry .ne. 0) then    
      if (associated(p)) then
         print *,'ERROR: gmm_create called with existing p and array has already been created'
         key=0
         gmm_create143 = GMM_ARRAY_ALREADY_EXISTS
         return 
      endif
      pp=>gmm_ptrs143(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
      consistent=.true.
      do i=1,3   
         call fool_optimizer(size(pp,i))  
         consistent = consistent .and. (size(pp,i).eq.(dims(i)%high-dims(i)%low+1))
         if (.not. consistent ) print *,'size(pp,',i,')=',size(pp,i),' high=',dims(i)%high,' low=',dims(i)%low
      enddo
      if (.not. consistent ) then
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=1 =',lbound(pp,1),ubound(pp,1)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=2 =',lbound(pp,2),ubound(pp,2)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=3 =',lbound(pp,3),ubound(pp,3)
         print *,'ERROR: gmm_create, requested dimensions differ from previous specification (restart/create)'
         print *,'ERROR: gmm_create, variable name ="',lcl_name,'"'
         key=0
         nullify(p)
         gmm_create143 = GMM_INCONSISTENT_DIMS
         return 
      else
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
           print *,'INFO: gmm_create, variable name =',lcl_name,' exists and is consistent'
         endif
      endif
   
      if (iand(GMM_FLAG_CRTD,directory(cur_page)%entry(cur_entry)%a%flags) .ne. 0) then 
         print *,'ERROR: gmm_create, field ',lcl_name,' has already been created'
         key = 0
         nullify(p)
         gmm_create143 = GMM_VARIABLE_ALREADY_CREATED
         return
!
      else   
!         print *,'Debug+ ', \' Cat(gmm_create, EXTENSION,) \','array ',lcl_name,'must then have been read from a restart file' 
         localattr%flags = ior(localattr%flags,directory(cur_page)%entry(cur_entry)%a%flags) 
         key=directory(cur_page)%entry(cur_entry)%a%key      
         directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
         p=>gmm_ptrs143(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p   
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i20)') 'Debug+++ gmm_create name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p)
         directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
         gmm_create143 = 0
         return   
      endif
   else        
      if (iand(GMM_FLAG_RSTR,localattr%flags).ne.0 .and. restart_mode) then  
         print *,'ERROR: gmm_create field ',lcl_name, 'should have been read from restart file but was not'
!  ==========  HOW SERIOUS AN ERROR IS THIS ? ===============
      endif
      call add_directory_entry
   endif
   
   
   ordinal = ordinal + 1   
   key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
   key = key + ishft(143,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
   directory(cur_page)%entry(cur_entry)%a = localattr  
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
   directory(cur_page)%entry(cur_entry)%name = lcl_name
   directory(cur_page)%entry(cur_entry)%a%key = key
   directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
   directory(cur_page)%entry(cur_entry)%l(1:3) = dims(1:3)  
   directory(cur_page)%entry(cur_entry)%data_type = 143 
   if (associated(p)) then  
      print *,'GMM_CREATE: using user supplied array'
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      ier = add_table_entry(p, key)
! ======= must check that certain attributes are not requested (e.g. FLAG_RSTR) and that size is consistent
   else
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      allocate(p(dims(1)%low:dims(1)%high,&
                &dims(2)%low:dims(2)%high,&
                &dims(3)%low:dims(3)%high),stat=ier)
      if (ier /= 0) then
         gmm_create143 = GMM_ERROR
         return
      endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_create creation name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
      directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
      ier = add_table_entry(p, key)
   endif
   field_meta%l = directory(cur_page)%entry(cur_entry)%l
   field_meta%a = directory(cur_page)%entry(cur_entry)%a
!    Cat(gmm_ptrs, EXTENSION,)(directory(cur_page)%entry(cur_entry)%f => p
!         if (iand(directory(cur_page)%entry(cur_entry)%a%flags,FLAG_IZER) .ne. 0) then
!           directory(cur_page)%entry(cur_entry)%f = 0        ! ZERO fill requested
!         endif
! CODE MISSING HERE FOR INITIALIZATION
   if (iand(field_meta%a%flags, GMM_FLAG_IZER) /= 0) THEN
!     print *,'Debug+ gmm_create init to zero for variable ',lcl_name
     p = 0
   endif
   gmm_create143 = 0
   end function gmm_create143
!
   integer function gmm_create283(iname,p,field_meta,flags_arg)
   use gmm_internals
   use pointer_table_data_283
   implicit none
   character(len=*), intent(in) :: iname               
   real*8, pointer :: p(:,:,:)
   type(gmm_metadata), intent(inout) :: field_meta               
   integer, intent(in), optional :: flags_arg
   integer *8 get_address_from
   external get_address_from
   external fool_optimizer
       interface
         subroutine check_directory_entry(name,key)
         character(len=*) :: name
         integer*8, intent(in) :: key
         end subroutine check_directory_entry
         subroutine find_directory_entry(name, key)
         implicit none
         character(len=*) :: name
         integer*8, optional :: key
         end subroutine find_directory_entry
         subroutine add_directory_entry
         end subroutine add_directory_entry
       end interface
   type(gmm_attributes) :: localattr, attrs
   type (gmm_attributes) lcl_attr
   type(gmm_layout), dimension(4) :: lcl_layout, dims
   integer lcl_datatype
   integer*8 :: key                      
   logical consistent
   integer i, ier
   integer lcl_pti
   character(len=GMM_MAXNAMELENGTH) :: lcl_name               
   integer u_bound, l_bound
   real*8, pointer :: pp(:,:,:)
   if (present(flags_arg)) then
    field_meta%a%flags = flags_arg
   endif
   lcl_layout = field_meta%l
   dims = lcl_layout
   lcl_attr   = field_meta%a
   attrs = lcl_attr
   lcl_datatype = 283
   
   
   if (associated(p)) then
      consistent=.true.
      do i=1,3   
         consistent = consistent .and. size(p,i).eq.(dims(i)%high-dims(i)%low+1)
      enddo
      if (.not. consistent ) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'ERROR: gmm_create, p has dimensions that are not consistent with dims'
         endif
         key=0
         gmm_create283 = GMM_INCONSISTENT_DIMS
         return 
      endif
   endif  
   
   localattr = attrs    
   lcl_name = trim(iname)
   localattr%flags = iand(localattr%flags,FLAGS_KEPT_ON_CREATE)  
!   call find_directory_entry(localattr%name,key)     ! is there a field with this name that exists ?
   call find_directory_entry(lcl_name,key)     
   
   
   if (cur_page .ne. 0 .and. cur_entry .ne. 0) then    
      if (associated(p)) then
         print *,'ERROR: gmm_create called with existing p and array has already been created'
         key=0
         gmm_create283 = GMM_ARRAY_ALREADY_EXISTS
         return 
      endif
      pp=>gmm_ptrs283(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
      consistent=.true.
      do i=1,3   
         call fool_optimizer(size(pp,i))  
         consistent = consistent .and. (size(pp,i).eq.(dims(i)%high-dims(i)%low+1))
         if (.not. consistent ) print *,'size(pp,',i,')=',size(pp,i),' high=',dims(i)%high,' low=',dims(i)%low
      enddo
      if (.not. consistent ) then
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=1 =',lbound(pp,1),ubound(pp,1)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=2 =',lbound(pp,2),ubound(pp,2)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=3 =',lbound(pp,3),ubound(pp,3)
         print *,'ERROR: gmm_create, requested dimensions differ from previous specification (restart/create)'
         print *,'ERROR: gmm_create, variable name ="',lcl_name,'"'
         key=0
         nullify(p)
         gmm_create283 = GMM_INCONSISTENT_DIMS
         return 
      else
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
           print *,'INFO: gmm_create, variable name =',lcl_name,' exists and is consistent'
         endif
      endif
   
      if (iand(GMM_FLAG_CRTD,directory(cur_page)%entry(cur_entry)%a%flags) .ne. 0) then 
         print *,'ERROR: gmm_create, field ',lcl_name,' has already been created'
         key = 0
         nullify(p)
         gmm_create283 = GMM_VARIABLE_ALREADY_CREATED
         return
!
      else   
!         print *,'Debug+ ', \' Cat(gmm_create, EXTENSION,) \','array ',lcl_name,'must then have been read from a restart file' 
         localattr%flags = ior(localattr%flags,directory(cur_page)%entry(cur_entry)%a%flags) 
         key=directory(cur_page)%entry(cur_entry)%a%key      
         directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
         p=>gmm_ptrs283(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p   
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i20)') 'Debug+++ gmm_create name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p)
         directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
         gmm_create283 = 0
         return   
      endif
   else        
      if (iand(GMM_FLAG_RSTR,localattr%flags).ne.0 .and. restart_mode) then  
         print *,'ERROR: gmm_create field ',lcl_name, 'should have been read from restart file but was not'
!  ==========  HOW SERIOUS AN ERROR IS THIS ? ===============
      endif
      call add_directory_entry
   endif
   
   
   ordinal = ordinal + 1   
   key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
   key = key + ishft(283,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
   directory(cur_page)%entry(cur_entry)%a = localattr  
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
   directory(cur_page)%entry(cur_entry)%name = lcl_name
   directory(cur_page)%entry(cur_entry)%a%key = key
   directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
   directory(cur_page)%entry(cur_entry)%l(1:3) = dims(1:3)  
   directory(cur_page)%entry(cur_entry)%data_type = 283 
   if (associated(p)) then  
      print *,'GMM_CREATE: using user supplied array'
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      ier = add_table_entry(p, key)
! ======= must check that certain attributes are not requested (e.g. FLAG_RSTR) and that size is consistent
   else
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      allocate(p(dims(1)%low:dims(1)%high,&
                &dims(2)%low:dims(2)%high,&
                &dims(3)%low:dims(3)%high),stat=ier)
      if (ier /= 0) then
         gmm_create283 = GMM_ERROR
         return
      endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_create creation name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
      directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
      ier = add_table_entry(p, key)
   endif
   field_meta%l = directory(cur_page)%entry(cur_entry)%l
   field_meta%a = directory(cur_page)%entry(cur_entry)%a
!    Cat(gmm_ptrs, EXTENSION,)(directory(cur_page)%entry(cur_entry)%f => p
!         if (iand(directory(cur_page)%entry(cur_entry)%a%flags,FLAG_IZER) .ne. 0) then
!           directory(cur_page)%entry(cur_entry)%f = 0        ! ZERO fill requested
!         endif
! CODE MISSING HERE FOR INITIALIZATION
   if (iand(field_meta%a%flags, GMM_FLAG_IZER) /= 0) THEN
!     print *,'Debug+ gmm_create init to zero for variable ',lcl_name
     p = 0
   endif
   gmm_create283 = 0
   end function gmm_create283
!
   integer function gmm_create243(iname,p,field_meta,flags_arg)
   use gmm_internals
   use pointer_table_data_243
   implicit none
   character(len=*), intent(in) :: iname               
   real*4, pointer :: p(:,:,:)
   type(gmm_metadata), intent(inout) :: field_meta               
   integer, intent(in), optional :: flags_arg
   integer *8 get_address_from
   external get_address_from
   external fool_optimizer
       interface
         subroutine check_directory_entry(name,key)
         character(len=*) :: name
         integer*8, intent(in) :: key
         end subroutine check_directory_entry
         subroutine find_directory_entry(name, key)
         implicit none
         character(len=*) :: name
         integer*8, optional :: key
         end subroutine find_directory_entry
         subroutine add_directory_entry
         end subroutine add_directory_entry
       end interface
   type(gmm_attributes) :: localattr, attrs
   type (gmm_attributes) lcl_attr
   type(gmm_layout), dimension(4) :: lcl_layout, dims
   integer lcl_datatype
   integer*8 :: key                      
   logical consistent
   integer i, ier
   integer lcl_pti
   character(len=GMM_MAXNAMELENGTH) :: lcl_name               
   integer u_bound, l_bound
   real*4, pointer :: pp(:,:,:)
   if (present(flags_arg)) then
    field_meta%a%flags = flags_arg
   endif
   lcl_layout = field_meta%l
   dims = lcl_layout
   lcl_attr   = field_meta%a
   attrs = lcl_attr
   lcl_datatype = 243
   
   
   if (associated(p)) then
      consistent=.true.
      do i=1,3   
         consistent = consistent .and. size(p,i).eq.(dims(i)%high-dims(i)%low+1)
      enddo
      if (.not. consistent ) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'ERROR: gmm_create, p has dimensions that are not consistent with dims'
         endif
         key=0
         gmm_create243 = GMM_INCONSISTENT_DIMS
         return 
      endif
   endif  
   
   localattr = attrs    
   lcl_name = trim(iname)
   localattr%flags = iand(localattr%flags,FLAGS_KEPT_ON_CREATE)  
!   call find_directory_entry(localattr%name,key)     ! is there a field with this name that exists ?
   call find_directory_entry(lcl_name,key)     
   
   
   if (cur_page .ne. 0 .and. cur_entry .ne. 0) then    
      if (associated(p)) then
         print *,'ERROR: gmm_create called with existing p and array has already been created'
         key=0
         gmm_create243 = GMM_ARRAY_ALREADY_EXISTS
         return 
      endif
      pp=>gmm_ptrs243(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
      consistent=.true.
      do i=1,3   
         call fool_optimizer(size(pp,i))  
         consistent = consistent .and. (size(pp,i).eq.(dims(i)%high-dims(i)%low+1))
         if (.not. consistent ) print *,'size(pp,',i,')=',size(pp,i),' high=',dims(i)%high,' low=',dims(i)%low
      enddo
      if (.not. consistent ) then
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=1 =',lbound(pp,1),ubound(pp,1)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=2 =',lbound(pp,2),ubound(pp,2)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=3 =',lbound(pp,3),ubound(pp,3)
         print *,'ERROR: gmm_create, requested dimensions differ from previous specification (restart/create)'
         print *,'ERROR: gmm_create, variable name ="',lcl_name,'"'
         key=0
         nullify(p)
         gmm_create243 = GMM_INCONSISTENT_DIMS
         return 
      else
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
           print *,'INFO: gmm_create, variable name =',lcl_name,' exists and is consistent'
         endif
      endif
   
      if (iand(GMM_FLAG_CRTD,directory(cur_page)%entry(cur_entry)%a%flags) .ne. 0) then 
         print *,'ERROR: gmm_create, field ',lcl_name,' has already been created'
         key = 0
         nullify(p)
         gmm_create243 = GMM_VARIABLE_ALREADY_CREATED
         return
!
      else   
!         print *,'Debug+ ', \' Cat(gmm_create, EXTENSION,) \','array ',lcl_name,'must then have been read from a restart file' 
         localattr%flags = ior(localattr%flags,directory(cur_page)%entry(cur_entry)%a%flags) 
         key=directory(cur_page)%entry(cur_entry)%a%key      
         directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
         p=>gmm_ptrs243(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p   
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i20)') 'Debug+++ gmm_create name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p)
         directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
         gmm_create243 = 0
         return   
      endif
   else        
      if (iand(GMM_FLAG_RSTR,localattr%flags).ne.0 .and. restart_mode) then  
         print *,'ERROR: gmm_create field ',lcl_name, 'should have been read from restart file but was not'
!  ==========  HOW SERIOUS AN ERROR IS THIS ? ===============
      endif
      call add_directory_entry
   endif
   
   
   ordinal = ordinal + 1   
   key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
   key = key + ishft(243,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
   directory(cur_page)%entry(cur_entry)%a = localattr  
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
   directory(cur_page)%entry(cur_entry)%name = lcl_name
   directory(cur_page)%entry(cur_entry)%a%key = key
   directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
   directory(cur_page)%entry(cur_entry)%l(1:3) = dims(1:3)  
   directory(cur_page)%entry(cur_entry)%data_type = 243 
   if (associated(p)) then  
      print *,'GMM_CREATE: using user supplied array'
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      ier = add_table_entry(p, key)
! ======= must check that certain attributes are not requested (e.g. FLAG_RSTR) and that size is consistent
   else
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      allocate(p(dims(1)%low:dims(1)%high,&
                &dims(2)%low:dims(2)%high,&
                &dims(3)%low:dims(3)%high),stat=ier)
      if (ier /= 0) then
         gmm_create243 = GMM_ERROR
         return
      endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_create creation name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
      directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
      ier = add_table_entry(p, key)
   endif
   field_meta%l = directory(cur_page)%entry(cur_entry)%l
   field_meta%a = directory(cur_page)%entry(cur_entry)%a
!    Cat(gmm_ptrs, EXTENSION,)(directory(cur_page)%entry(cur_entry)%f => p
!         if (iand(directory(cur_page)%entry(cur_entry)%a%flags,FLAG_IZER) .ne. 0) then
!           directory(cur_page)%entry(cur_entry)%f = 0        ! ZERO fill requested
!         endif
! CODE MISSING HERE FOR INITIALIZATION
   if (iand(field_meta%a%flags, GMM_FLAG_IZER) /= 0) THEN
!     print *,'Debug+ gmm_create init to zero for variable ',lcl_name
     p = 0
   endif
   gmm_create243 = 0
   end function gmm_create243
!
   integer function gmm_create383(iname,p,field_meta,flags_arg)
   use gmm_internals
   use pointer_table_data_383
   implicit none
   character(len=*), intent(in) :: iname               
   complex*8, pointer :: p(:,:,:)
   type(gmm_metadata), intent(inout) :: field_meta               
   integer, intent(in), optional :: flags_arg
   integer *8 get_address_from
   external get_address_from
   external fool_optimizer
       interface
         subroutine check_directory_entry(name,key)
         character(len=*) :: name
         integer*8, intent(in) :: key
         end subroutine check_directory_entry
         subroutine find_directory_entry(name, key)
         implicit none
         character(len=*) :: name
         integer*8, optional :: key
         end subroutine find_directory_entry
         subroutine add_directory_entry
         end subroutine add_directory_entry
       end interface
   type(gmm_attributes) :: localattr, attrs
   type (gmm_attributes) lcl_attr
   type(gmm_layout), dimension(4) :: lcl_layout, dims
   integer lcl_datatype
   integer*8 :: key                      
   logical consistent
   integer i, ier
   integer lcl_pti
   character(len=GMM_MAXNAMELENGTH) :: lcl_name               
   integer u_bound, l_bound
   complex*8, pointer :: pp(:,:,:)
   if (present(flags_arg)) then
    field_meta%a%flags = flags_arg
   endif
   lcl_layout = field_meta%l
   dims = lcl_layout
   lcl_attr   = field_meta%a
   attrs = lcl_attr
   lcl_datatype = 383
   
   
   if (associated(p)) then
      consistent=.true.
      do i=1,3   
         consistent = consistent .and. size(p,i).eq.(dims(i)%high-dims(i)%low+1)
      enddo
      if (.not. consistent ) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'ERROR: gmm_create, p has dimensions that are not consistent with dims'
         endif
         key=0
         gmm_create383 = GMM_INCONSISTENT_DIMS
         return 
      endif
   endif  
   
   localattr = attrs    
   lcl_name = trim(iname)
   localattr%flags = iand(localattr%flags,FLAGS_KEPT_ON_CREATE)  
!   call find_directory_entry(localattr%name,key)     ! is there a field with this name that exists ?
   call find_directory_entry(lcl_name,key)     
   
   
   if (cur_page .ne. 0 .and. cur_entry .ne. 0) then    
      if (associated(p)) then
         print *,'ERROR: gmm_create called with existing p and array has already been created'
         key=0
         gmm_create383 = GMM_ARRAY_ALREADY_EXISTS
         return 
      endif
      pp=>gmm_ptrs383(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
      consistent=.true.
      do i=1,3   
         call fool_optimizer(size(pp,i))  
         consistent = consistent .and. (size(pp,i).eq.(dims(i)%high-dims(i)%low+1))
         if (.not. consistent ) print *,'size(pp,',i,')=',size(pp,i),' high=',dims(i)%high,' low=',dims(i)%low
      enddo
      if (.not. consistent ) then
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=1 =',lbound(pp,1),ubound(pp,1)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=2 =',lbound(pp,2),ubound(pp,2)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=3 =',lbound(pp,3),ubound(pp,3)
         print *,'ERROR: gmm_create, requested dimensions differ from previous specification (restart/create)'
         print *,'ERROR: gmm_create, variable name ="',lcl_name,'"'
         key=0
         nullify(p)
         gmm_create383 = GMM_INCONSISTENT_DIMS
         return 
      else
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
           print *,'INFO: gmm_create, variable name =',lcl_name,' exists and is consistent'
         endif
      endif
   
      if (iand(GMM_FLAG_CRTD,directory(cur_page)%entry(cur_entry)%a%flags) .ne. 0) then 
         print *,'ERROR: gmm_create, field ',lcl_name,' has already been created'
         key = 0
         nullify(p)
         gmm_create383 = GMM_VARIABLE_ALREADY_CREATED
         return
!
      else   
!         print *,'Debug+ ', \' Cat(gmm_create, EXTENSION,) \','array ',lcl_name,'must then have been read from a restart file' 
         localattr%flags = ior(localattr%flags,directory(cur_page)%entry(cur_entry)%a%flags) 
         key=directory(cur_page)%entry(cur_entry)%a%key      
         directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
         p=>gmm_ptrs383(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p   
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i20)') 'Debug+++ gmm_create name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p)
         directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
         gmm_create383 = 0
         return   
      endif
   else        
      if (iand(GMM_FLAG_RSTR,localattr%flags).ne.0 .and. restart_mode) then  
         print *,'ERROR: gmm_create field ',lcl_name, 'should have been read from restart file but was not'
!  ==========  HOW SERIOUS AN ERROR IS THIS ? ===============
      endif
      call add_directory_entry
   endif
   
   
   ordinal = ordinal + 1   
   key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
   key = key + ishft(383,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
   directory(cur_page)%entry(cur_entry)%a = localattr  
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
   directory(cur_page)%entry(cur_entry)%name = lcl_name
   directory(cur_page)%entry(cur_entry)%a%key = key
   directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
   directory(cur_page)%entry(cur_entry)%l(1:3) = dims(1:3)  
   directory(cur_page)%entry(cur_entry)%data_type = 383 
   if (associated(p)) then  
      print *,'GMM_CREATE: using user supplied array'
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      ier = add_table_entry(p, key)
! ======= must check that certain attributes are not requested (e.g. FLAG_RSTR) and that size is consistent
   else
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      allocate(p(dims(1)%low:dims(1)%high,&
                &dims(2)%low:dims(2)%high,&
                &dims(3)%low:dims(3)%high),stat=ier)
      if (ier /= 0) then
         gmm_create383 = GMM_ERROR
         return
      endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_create creation name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
      directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
      ier = add_table_entry(p, key)
   endif
   field_meta%l = directory(cur_page)%entry(cur_entry)%l
   field_meta%a = directory(cur_page)%entry(cur_entry)%a
!    Cat(gmm_ptrs, EXTENSION,)(directory(cur_page)%entry(cur_entry)%f => p
!         if (iand(directory(cur_page)%entry(cur_entry)%a%flags,FLAG_IZER) .ne. 0) then
!           directory(cur_page)%entry(cur_entry)%f = 0        ! ZERO fill requested
!         endif
! CODE MISSING HERE FOR INITIALIZATION
   if (iand(field_meta%a%flags, GMM_FLAG_IZER) /= 0) THEN
!     print *,'Debug+ gmm_create init to zero for variable ',lcl_name
     p = 0
   endif
   gmm_create383 = 0
   end function gmm_create383
!
   integer function gmm_create182(iname,p,field_meta,flags_arg)
   use gmm_internals
   use pointer_table_data_182
   implicit none
   character(len=*), intent(in) :: iname               
   integer*8, pointer :: p(:,:)
   type(gmm_metadata), intent(inout) :: field_meta               
   integer, intent(in), optional :: flags_arg
   integer *8 get_address_from
   external get_address_from
   external fool_optimizer
       interface
         subroutine check_directory_entry(name,key)
         character(len=*) :: name
         integer*8, intent(in) :: key
         end subroutine check_directory_entry
         subroutine find_directory_entry(name, key)
         implicit none
         character(len=*) :: name
         integer*8, optional :: key
         end subroutine find_directory_entry
         subroutine add_directory_entry
         end subroutine add_directory_entry
       end interface
   type(gmm_attributes) :: localattr, attrs
   type (gmm_attributes) lcl_attr
   type(gmm_layout), dimension(4) :: lcl_layout, dims
   integer lcl_datatype
   integer*8 :: key                      
   logical consistent
   integer i, ier
   integer lcl_pti
   character(len=GMM_MAXNAMELENGTH) :: lcl_name               
   integer u_bound, l_bound
   integer*8, pointer :: pp(:,:)
   if (present(flags_arg)) then
    field_meta%a%flags = flags_arg
   endif
   lcl_layout = field_meta%l
   dims = lcl_layout
   lcl_attr   = field_meta%a
   attrs = lcl_attr
   lcl_datatype = 182
   
   
   if (associated(p)) then
      consistent=.true.
      do i=1,2   
         consistent = consistent .and. size(p,i).eq.(dims(i)%high-dims(i)%low+1)
      enddo
      if (.not. consistent ) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'ERROR: gmm_create, p has dimensions that are not consistent with dims'
         endif
         key=0
         gmm_create182 = GMM_INCONSISTENT_DIMS
         return 
      endif
   endif  
   
   localattr = attrs    
   lcl_name = trim(iname)
   localattr%flags = iand(localattr%flags,FLAGS_KEPT_ON_CREATE)  
!   call find_directory_entry(localattr%name,key)     ! is there a field with this name that exists ?
   call find_directory_entry(lcl_name,key)     
   
   
   if (cur_page .ne. 0 .and. cur_entry .ne. 0) then    
      if (associated(p)) then
         print *,'ERROR: gmm_create called with existing p and array has already been created'
         key=0
         gmm_create182 = GMM_ARRAY_ALREADY_EXISTS
         return 
      endif
      pp=>gmm_ptrs182(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
      consistent=.true.
      do i=1,2   
         call fool_optimizer(size(pp,i))  
         consistent = consistent .and. (size(pp,i).eq.(dims(i)%high-dims(i)%low+1))
         if (.not. consistent ) print *,'size(pp,',i,')=',size(pp,i),' high=',dims(i)%high,' low=',dims(i)%low
      enddo
      if (.not. consistent ) then
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=1 =',lbound(pp,1),ubound(pp,1)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=2 =',lbound(pp,2),ubound(pp,2)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=3 =',lbound(pp,3),ubound(pp,3)
         print *,'ERROR: gmm_create, requested dimensions differ from previous specification (restart/create)'
         print *,'ERROR: gmm_create, variable name ="',lcl_name,'"'
         key=0
         nullify(p)
         gmm_create182 = GMM_INCONSISTENT_DIMS
         return 
      else
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
           print *,'INFO: gmm_create, variable name =',lcl_name,' exists and is consistent'
         endif
      endif
   
      if (iand(GMM_FLAG_CRTD,directory(cur_page)%entry(cur_entry)%a%flags) .ne. 0) then 
         print *,'ERROR: gmm_create, field ',lcl_name,' has already been created'
         key = 0
         nullify(p)
         gmm_create182 = GMM_VARIABLE_ALREADY_CREATED
         return
!
      else   
!         print *,'Debug+ ', \' Cat(gmm_create, EXTENSION,) \','array ',lcl_name,'must then have been read from a restart file' 
         localattr%flags = ior(localattr%flags,directory(cur_page)%entry(cur_entry)%a%flags) 
         key=directory(cur_page)%entry(cur_entry)%a%key      
         directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
         p=>gmm_ptrs182(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p   
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i20)') 'Debug+++ gmm_create name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p)
         directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
         gmm_create182 = 0
         return   
      endif
   else        
      if (iand(GMM_FLAG_RSTR,localattr%flags).ne.0 .and. restart_mode) then  
         print *,'ERROR: gmm_create field ',lcl_name, 'should have been read from restart file but was not'
!  ==========  HOW SERIOUS AN ERROR IS THIS ? ===============
      endif
      call add_directory_entry
   endif
   
   
   ordinal = ordinal + 1   
   key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
   key = key + ishft(182,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
   directory(cur_page)%entry(cur_entry)%a = localattr  
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
   directory(cur_page)%entry(cur_entry)%name = lcl_name
   directory(cur_page)%entry(cur_entry)%a%key = key
   directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
   directory(cur_page)%entry(cur_entry)%l(1:2) = dims(1:2)  
   directory(cur_page)%entry(cur_entry)%data_type = 182 
   if (associated(p)) then  
      print *,'GMM_CREATE: using user supplied array'
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      ier = add_table_entry(p, key)
! ======= must check that certain attributes are not requested (e.g. FLAG_RSTR) and that size is consistent
   else
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      allocate(p(dims(1)%low:dims(1)%high,&
                &dims(2)%low:dims(2)%high),stat=ier)
      if (ier /= 0) then
         gmm_create182 = GMM_ERROR
         return
      endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_create creation name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
      directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
      ier = add_table_entry(p, key)
   endif
   field_meta%l = directory(cur_page)%entry(cur_entry)%l
   field_meta%a = directory(cur_page)%entry(cur_entry)%a
!    Cat(gmm_ptrs, EXTENSION,)(directory(cur_page)%entry(cur_entry)%f => p
!         if (iand(directory(cur_page)%entry(cur_entry)%a%flags,FLAG_IZER) .ne. 0) then
!           directory(cur_page)%entry(cur_entry)%f = 0        ! ZERO fill requested
!         endif
! CODE MISSING HERE FOR INITIALIZATION
   if (iand(field_meta%a%flags, GMM_FLAG_IZER) /= 0) THEN
!     print *,'Debug+ gmm_create init to zero for variable ',lcl_name
     p = 0
   endif
   gmm_create182 = 0
   end function gmm_create182
!
   integer function gmm_create142(iname,p,field_meta,flags_arg)
   use gmm_internals
   use pointer_table_data_142
   implicit none
   character(len=*), intent(in) :: iname               
   integer*4, pointer :: p(:,:)
   type(gmm_metadata), intent(inout) :: field_meta               
   integer, intent(in), optional :: flags_arg
   integer *8 get_address_from
   external get_address_from
   external fool_optimizer
       interface
         subroutine check_directory_entry(name,key)
         character(len=*) :: name
         integer*8, intent(in) :: key
         end subroutine check_directory_entry
         subroutine find_directory_entry(name, key)
         implicit none
         character(len=*) :: name
         integer*8, optional :: key
         end subroutine find_directory_entry
         subroutine add_directory_entry
         end subroutine add_directory_entry
       end interface
   type(gmm_attributes) :: localattr, attrs
   type (gmm_attributes) lcl_attr
   type(gmm_layout), dimension(4) :: lcl_layout, dims
   integer lcl_datatype
   integer*8 :: key                      
   logical consistent
   integer i, ier
   integer lcl_pti
   character(len=GMM_MAXNAMELENGTH) :: lcl_name               
   integer u_bound, l_bound
   integer*4, pointer :: pp(:,:)
   if (present(flags_arg)) then
    field_meta%a%flags = flags_arg
   endif
   lcl_layout = field_meta%l
   dims = lcl_layout
   lcl_attr   = field_meta%a
   attrs = lcl_attr
   lcl_datatype = 142
   
   
   if (associated(p)) then
      consistent=.true.
      do i=1,2   
         consistent = consistent .and. size(p,i).eq.(dims(i)%high-dims(i)%low+1)
      enddo
      if (.not. consistent ) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'ERROR: gmm_create, p has dimensions that are not consistent with dims'
         endif
         key=0
         gmm_create142 = GMM_INCONSISTENT_DIMS
         return 
      endif
   endif  
   
   localattr = attrs    
   lcl_name = trim(iname)
   localattr%flags = iand(localattr%flags,FLAGS_KEPT_ON_CREATE)  
!   call find_directory_entry(localattr%name,key)     ! is there a field with this name that exists ?
   call find_directory_entry(lcl_name,key)     
   
   
   if (cur_page .ne. 0 .and. cur_entry .ne. 0) then    
      if (associated(p)) then
         print *,'ERROR: gmm_create called with existing p and array has already been created'
         key=0
         gmm_create142 = GMM_ARRAY_ALREADY_EXISTS
         return 
      endif
      pp=>gmm_ptrs142(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
      consistent=.true.
      do i=1,2   
         call fool_optimizer(size(pp,i))  
         consistent = consistent .and. (size(pp,i).eq.(dims(i)%high-dims(i)%low+1))
         if (.not. consistent ) print *,'size(pp,',i,')=',size(pp,i),' high=',dims(i)%high,' low=',dims(i)%low
      enddo
      if (.not. consistent ) then
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=1 =',lbound(pp,1),ubound(pp,1)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=2 =',lbound(pp,2),ubound(pp,2)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=3 =',lbound(pp,3),ubound(pp,3)
         print *,'ERROR: gmm_create, requested dimensions differ from previous specification (restart/create)'
         print *,'ERROR: gmm_create, variable name ="',lcl_name,'"'
         key=0
         nullify(p)
         gmm_create142 = GMM_INCONSISTENT_DIMS
         return 
      else
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
           print *,'INFO: gmm_create, variable name =',lcl_name,' exists and is consistent'
         endif
      endif
   
      if (iand(GMM_FLAG_CRTD,directory(cur_page)%entry(cur_entry)%a%flags) .ne. 0) then 
         print *,'ERROR: gmm_create, field ',lcl_name,' has already been created'
         key = 0
         nullify(p)
         gmm_create142 = GMM_VARIABLE_ALREADY_CREATED
         return
!
      else   
!         print *,'Debug+ ', \' Cat(gmm_create, EXTENSION,) \','array ',lcl_name,'must then have been read from a restart file' 
         localattr%flags = ior(localattr%flags,directory(cur_page)%entry(cur_entry)%a%flags) 
         key=directory(cur_page)%entry(cur_entry)%a%key      
         directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
         p=>gmm_ptrs142(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p   
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i20)') 'Debug+++ gmm_create name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p)
         directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
         gmm_create142 = 0
         return   
      endif
   else        
      if (iand(GMM_FLAG_RSTR,localattr%flags).ne.0 .and. restart_mode) then  
         print *,'ERROR: gmm_create field ',lcl_name, 'should have been read from restart file but was not'
!  ==========  HOW SERIOUS AN ERROR IS THIS ? ===============
      endif
      call add_directory_entry
   endif
   
   
   ordinal = ordinal + 1   
   key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
   key = key + ishft(142,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
   directory(cur_page)%entry(cur_entry)%a = localattr  
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
   directory(cur_page)%entry(cur_entry)%name = lcl_name
   directory(cur_page)%entry(cur_entry)%a%key = key
   directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
   directory(cur_page)%entry(cur_entry)%l(1:2) = dims(1:2)  
   directory(cur_page)%entry(cur_entry)%data_type = 142 
   if (associated(p)) then  
      print *,'GMM_CREATE: using user supplied array'
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      ier = add_table_entry(p, key)
! ======= must check that certain attributes are not requested (e.g. FLAG_RSTR) and that size is consistent
   else
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      allocate(p(dims(1)%low:dims(1)%high,&
                &dims(2)%low:dims(2)%high),stat=ier)
      if (ier /= 0) then
         gmm_create142 = GMM_ERROR
         return
      endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_create creation name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
      directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
      ier = add_table_entry(p, key)
   endif
   field_meta%l = directory(cur_page)%entry(cur_entry)%l
   field_meta%a = directory(cur_page)%entry(cur_entry)%a
!    Cat(gmm_ptrs, EXTENSION,)(directory(cur_page)%entry(cur_entry)%f => p
!         if (iand(directory(cur_page)%entry(cur_entry)%a%flags,FLAG_IZER) .ne. 0) then
!           directory(cur_page)%entry(cur_entry)%f = 0        ! ZERO fill requested
!         endif
! CODE MISSING HERE FOR INITIALIZATION
   if (iand(field_meta%a%flags, GMM_FLAG_IZER) /= 0) THEN
!     print *,'Debug+ gmm_create init to zero for variable ',lcl_name
     p = 0
   endif
   gmm_create142 = 0
   end function gmm_create142
!
   integer function gmm_create282(iname,p,field_meta,flags_arg)
   use gmm_internals
   use pointer_table_data_282
   implicit none
   character(len=*), intent(in) :: iname               
   real*8, pointer :: p(:,:)
   type(gmm_metadata), intent(inout) :: field_meta               
   integer, intent(in), optional :: flags_arg
   integer *8 get_address_from
   external get_address_from
   external fool_optimizer
       interface
         subroutine check_directory_entry(name,key)
         character(len=*) :: name
         integer*8, intent(in) :: key
         end subroutine check_directory_entry
         subroutine find_directory_entry(name, key)
         implicit none
         character(len=*) :: name
         integer*8, optional :: key
         end subroutine find_directory_entry
         subroutine add_directory_entry
         end subroutine add_directory_entry
       end interface
   type(gmm_attributes) :: localattr, attrs
   type (gmm_attributes) lcl_attr
   type(gmm_layout), dimension(4) :: lcl_layout, dims
   integer lcl_datatype
   integer*8 :: key                      
   logical consistent
   integer i, ier
   integer lcl_pti
   character(len=GMM_MAXNAMELENGTH) :: lcl_name               
   integer u_bound, l_bound
   real*8, pointer :: pp(:,:)
   if (present(flags_arg)) then
    field_meta%a%flags = flags_arg
   endif
   lcl_layout = field_meta%l
   dims = lcl_layout
   lcl_attr   = field_meta%a
   attrs = lcl_attr
   lcl_datatype = 282
   
   
   if (associated(p)) then
      consistent=.true.
      do i=1,2   
         consistent = consistent .and. size(p,i).eq.(dims(i)%high-dims(i)%low+1)
      enddo
      if (.not. consistent ) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'ERROR: gmm_create, p has dimensions that are not consistent with dims'
         endif
         key=0
         gmm_create282 = GMM_INCONSISTENT_DIMS
         return 
      endif
   endif  
   
   localattr = attrs    
   lcl_name = trim(iname)
   localattr%flags = iand(localattr%flags,FLAGS_KEPT_ON_CREATE)  
!   call find_directory_entry(localattr%name,key)     ! is there a field with this name that exists ?
   call find_directory_entry(lcl_name,key)     
   
   
   if (cur_page .ne. 0 .and. cur_entry .ne. 0) then    
      if (associated(p)) then
         print *,'ERROR: gmm_create called with existing p and array has already been created'
         key=0
         gmm_create282 = GMM_ARRAY_ALREADY_EXISTS
         return 
      endif
      pp=>gmm_ptrs282(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
      consistent=.true.
      do i=1,2   
         call fool_optimizer(size(pp,i))  
         consistent = consistent .and. (size(pp,i).eq.(dims(i)%high-dims(i)%low+1))
         if (.not. consistent ) print *,'size(pp,',i,')=',size(pp,i),' high=',dims(i)%high,' low=',dims(i)%low
      enddo
      if (.not. consistent ) then
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=1 =',lbound(pp,1),ubound(pp,1)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=2 =',lbound(pp,2),ubound(pp,2)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=3 =',lbound(pp,3),ubound(pp,3)
         print *,'ERROR: gmm_create, requested dimensions differ from previous specification (restart/create)'
         print *,'ERROR: gmm_create, variable name ="',lcl_name,'"'
         key=0
         nullify(p)
         gmm_create282 = GMM_INCONSISTENT_DIMS
         return 
      else
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
           print *,'INFO: gmm_create, variable name =',lcl_name,' exists and is consistent'
         endif
      endif
   
      if (iand(GMM_FLAG_CRTD,directory(cur_page)%entry(cur_entry)%a%flags) .ne. 0) then 
         print *,'ERROR: gmm_create, field ',lcl_name,' has already been created'
         key = 0
         nullify(p)
         gmm_create282 = GMM_VARIABLE_ALREADY_CREATED
         return
!
      else   
!         print *,'Debug+ ', \' Cat(gmm_create, EXTENSION,) \','array ',lcl_name,'must then have been read from a restart file' 
         localattr%flags = ior(localattr%flags,directory(cur_page)%entry(cur_entry)%a%flags) 
         key=directory(cur_page)%entry(cur_entry)%a%key      
         directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
         p=>gmm_ptrs282(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p   
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i20)') 'Debug+++ gmm_create name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p)
         directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
         gmm_create282 = 0
         return   
      endif
   else        
      if (iand(GMM_FLAG_RSTR,localattr%flags).ne.0 .and. restart_mode) then  
         print *,'ERROR: gmm_create field ',lcl_name, 'should have been read from restart file but was not'
!  ==========  HOW SERIOUS AN ERROR IS THIS ? ===============
      endif
      call add_directory_entry
   endif
   
   
   ordinal = ordinal + 1   
   key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
   key = key + ishft(282,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
   directory(cur_page)%entry(cur_entry)%a = localattr  
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
   directory(cur_page)%entry(cur_entry)%name = lcl_name
   directory(cur_page)%entry(cur_entry)%a%key = key
   directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
   directory(cur_page)%entry(cur_entry)%l(1:2) = dims(1:2)  
   directory(cur_page)%entry(cur_entry)%data_type = 282 
   if (associated(p)) then  
      print *,'GMM_CREATE: using user supplied array'
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      ier = add_table_entry(p, key)
! ======= must check that certain attributes are not requested (e.g. FLAG_RSTR) and that size is consistent
   else
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      allocate(p(dims(1)%low:dims(1)%high,&
                &dims(2)%low:dims(2)%high),stat=ier)
      if (ier /= 0) then
         gmm_create282 = GMM_ERROR
         return
      endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_create creation name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
      directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
      ier = add_table_entry(p, key)
   endif
   field_meta%l = directory(cur_page)%entry(cur_entry)%l
   field_meta%a = directory(cur_page)%entry(cur_entry)%a
!    Cat(gmm_ptrs, EXTENSION,)(directory(cur_page)%entry(cur_entry)%f => p
!         if (iand(directory(cur_page)%entry(cur_entry)%a%flags,FLAG_IZER) .ne. 0) then
!           directory(cur_page)%entry(cur_entry)%f = 0        ! ZERO fill requested
!         endif
! CODE MISSING HERE FOR INITIALIZATION
   if (iand(field_meta%a%flags, GMM_FLAG_IZER) /= 0) THEN
!     print *,'Debug+ gmm_create init to zero for variable ',lcl_name
     p = 0
   endif
   gmm_create282 = 0
   end function gmm_create282
!
   integer function gmm_create242(iname,p,field_meta,flags_arg)
   use gmm_internals
   use pointer_table_data_242
   implicit none
   character(len=*), intent(in) :: iname               
   real*4, pointer :: p(:,:)
   type(gmm_metadata), intent(inout) :: field_meta               
   integer, intent(in), optional :: flags_arg
   integer *8 get_address_from
   external get_address_from
   external fool_optimizer
       interface
         subroutine check_directory_entry(name,key)
         character(len=*) :: name
         integer*8, intent(in) :: key
         end subroutine check_directory_entry
         subroutine find_directory_entry(name, key)
         implicit none
         character(len=*) :: name
         integer*8, optional :: key
         end subroutine find_directory_entry
         subroutine add_directory_entry
         end subroutine add_directory_entry
       end interface
   type(gmm_attributes) :: localattr, attrs
   type (gmm_attributes) lcl_attr
   type(gmm_layout), dimension(4) :: lcl_layout, dims
   integer lcl_datatype
   integer*8 :: key                      
   logical consistent
   integer i, ier
   integer lcl_pti
   character(len=GMM_MAXNAMELENGTH) :: lcl_name               
   integer u_bound, l_bound
   real*4, pointer :: pp(:,:)
   if (present(flags_arg)) then
    field_meta%a%flags = flags_arg
   endif
   lcl_layout = field_meta%l
   dims = lcl_layout
   lcl_attr   = field_meta%a
   attrs = lcl_attr
   lcl_datatype = 242
   
   
   if (associated(p)) then
      consistent=.true.
      do i=1,2   
         consistent = consistent .and. size(p,i).eq.(dims(i)%high-dims(i)%low+1)
      enddo
      if (.not. consistent ) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'ERROR: gmm_create, p has dimensions that are not consistent with dims'
         endif
         key=0
         gmm_create242 = GMM_INCONSISTENT_DIMS
         return 
      endif
   endif  
   
   localattr = attrs    
   lcl_name = trim(iname)
   localattr%flags = iand(localattr%flags,FLAGS_KEPT_ON_CREATE)  
!   call find_directory_entry(localattr%name,key)     ! is there a field with this name that exists ?
   call find_directory_entry(lcl_name,key)     
   
   
   if (cur_page .ne. 0 .and. cur_entry .ne. 0) then    
      if (associated(p)) then
         print *,'ERROR: gmm_create called with existing p and array has already been created'
         key=0
         gmm_create242 = GMM_ARRAY_ALREADY_EXISTS
         return 
      endif
      pp=>gmm_ptrs242(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
      consistent=.true.
      do i=1,2   
         call fool_optimizer(size(pp,i))  
         consistent = consistent .and. (size(pp,i).eq.(dims(i)%high-dims(i)%low+1))
         if (.not. consistent ) print *,'size(pp,',i,')=',size(pp,i),' high=',dims(i)%high,' low=',dims(i)%low
      enddo
      if (.not. consistent ) then
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=1 =',lbound(pp,1),ubound(pp,1)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=2 =',lbound(pp,2),ubound(pp,2)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=3 =',lbound(pp,3),ubound(pp,3)
         print *,'ERROR: gmm_create, requested dimensions differ from previous specification (restart/create)'
         print *,'ERROR: gmm_create, variable name ="',lcl_name,'"'
         key=0
         nullify(p)
         gmm_create242 = GMM_INCONSISTENT_DIMS
         return 
      else
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
           print *,'INFO: gmm_create, variable name =',lcl_name,' exists and is consistent'
         endif
      endif
   
      if (iand(GMM_FLAG_CRTD,directory(cur_page)%entry(cur_entry)%a%flags) .ne. 0) then 
         print *,'ERROR: gmm_create, field ',lcl_name,' has already been created'
         key = 0
         nullify(p)
         gmm_create242 = GMM_VARIABLE_ALREADY_CREATED
         return
!
      else   
!         print *,'Debug+ ', \' Cat(gmm_create, EXTENSION,) \','array ',lcl_name,'must then have been read from a restart file' 
         localattr%flags = ior(localattr%flags,directory(cur_page)%entry(cur_entry)%a%flags) 
         key=directory(cur_page)%entry(cur_entry)%a%key      
         directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
         p=>gmm_ptrs242(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p   
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i20)') 'Debug+++ gmm_create name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p)
         directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
         gmm_create242 = 0
         return   
      endif
   else        
      if (iand(GMM_FLAG_RSTR,localattr%flags).ne.0 .and. restart_mode) then  
         print *,'ERROR: gmm_create field ',lcl_name, 'should have been read from restart file but was not'
!  ==========  HOW SERIOUS AN ERROR IS THIS ? ===============
      endif
      call add_directory_entry
   endif
   
   
   ordinal = ordinal + 1   
   key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
   key = key + ishft(242,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
   directory(cur_page)%entry(cur_entry)%a = localattr  
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
   directory(cur_page)%entry(cur_entry)%name = lcl_name
   directory(cur_page)%entry(cur_entry)%a%key = key
   directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
   directory(cur_page)%entry(cur_entry)%l(1:2) = dims(1:2)  
   directory(cur_page)%entry(cur_entry)%data_type = 242 
   if (associated(p)) then  
      print *,'GMM_CREATE: using user supplied array'
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      ier = add_table_entry(p, key)
! ======= must check that certain attributes are not requested (e.g. FLAG_RSTR) and that size is consistent
   else
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      allocate(p(dims(1)%low:dims(1)%high,&
                &dims(2)%low:dims(2)%high),stat=ier)
      if (ier /= 0) then
         gmm_create242 = GMM_ERROR
         return
      endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_create creation name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
      directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
      ier = add_table_entry(p, key)
   endif
   field_meta%l = directory(cur_page)%entry(cur_entry)%l
   field_meta%a = directory(cur_page)%entry(cur_entry)%a
!    Cat(gmm_ptrs, EXTENSION,)(directory(cur_page)%entry(cur_entry)%f => p
!         if (iand(directory(cur_page)%entry(cur_entry)%a%flags,FLAG_IZER) .ne. 0) then
!           directory(cur_page)%entry(cur_entry)%f = 0        ! ZERO fill requested
!         endif
! CODE MISSING HERE FOR INITIALIZATION
   if (iand(field_meta%a%flags, GMM_FLAG_IZER) /= 0) THEN
!     print *,'Debug+ gmm_create init to zero for variable ',lcl_name
     p = 0
   endif
   gmm_create242 = 0
   end function gmm_create242
!
   integer function gmm_create382(iname,p,field_meta,flags_arg)
   use gmm_internals
   use pointer_table_data_382
   implicit none
   character(len=*), intent(in) :: iname               
   complex*8, pointer :: p(:,:)
   type(gmm_metadata), intent(inout) :: field_meta               
   integer, intent(in), optional :: flags_arg
   integer *8 get_address_from
   external get_address_from
   external fool_optimizer
       interface
         subroutine check_directory_entry(name,key)
         character(len=*) :: name
         integer*8, intent(in) :: key
         end subroutine check_directory_entry
         subroutine find_directory_entry(name, key)
         implicit none
         character(len=*) :: name
         integer*8, optional :: key
         end subroutine find_directory_entry
         subroutine add_directory_entry
         end subroutine add_directory_entry
       end interface
   type(gmm_attributes) :: localattr, attrs
   type (gmm_attributes) lcl_attr
   type(gmm_layout), dimension(4) :: lcl_layout, dims
   integer lcl_datatype
   integer*8 :: key                      
   logical consistent
   integer i, ier
   integer lcl_pti
   character(len=GMM_MAXNAMELENGTH) :: lcl_name               
   integer u_bound, l_bound
   complex*8, pointer :: pp(:,:)
   if (present(flags_arg)) then
    field_meta%a%flags = flags_arg
   endif
   lcl_layout = field_meta%l
   dims = lcl_layout
   lcl_attr   = field_meta%a
   attrs = lcl_attr
   lcl_datatype = 382
   
   
   if (associated(p)) then
      consistent=.true.
      do i=1,2   
         consistent = consistent .and. size(p,i).eq.(dims(i)%high-dims(i)%low+1)
      enddo
      if (.not. consistent ) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'ERROR: gmm_create, p has dimensions that are not consistent with dims'
         endif
         key=0
         gmm_create382 = GMM_INCONSISTENT_DIMS
         return 
      endif
   endif  
   
   localattr = attrs    
   lcl_name = trim(iname)
   localattr%flags = iand(localattr%flags,FLAGS_KEPT_ON_CREATE)  
!   call find_directory_entry(localattr%name,key)     ! is there a field with this name that exists ?
   call find_directory_entry(lcl_name,key)     
   
   
   if (cur_page .ne. 0 .and. cur_entry .ne. 0) then    
      if (associated(p)) then
         print *,'ERROR: gmm_create called with existing p and array has already been created'
         key=0
         gmm_create382 = GMM_ARRAY_ALREADY_EXISTS
         return 
      endif
      pp=>gmm_ptrs382(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
      consistent=.true.
      do i=1,2   
         call fool_optimizer(size(pp,i))  
         consistent = consistent .and. (size(pp,i).eq.(dims(i)%high-dims(i)%low+1))
         if (.not. consistent ) print *,'size(pp,',i,')=',size(pp,i),' high=',dims(i)%high,' low=',dims(i)%low
      enddo
      if (.not. consistent ) then
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=1 =',lbound(pp,1),ubound(pp,1)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=2 =',lbound(pp,2),ubound(pp,2)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=3 =',lbound(pp,3),ubound(pp,3)
         print *,'ERROR: gmm_create, requested dimensions differ from previous specification (restart/create)'
         print *,'ERROR: gmm_create, variable name ="',lcl_name,'"'
         key=0
         nullify(p)
         gmm_create382 = GMM_INCONSISTENT_DIMS
         return 
      else
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
           print *,'INFO: gmm_create, variable name =',lcl_name,' exists and is consistent'
         endif
      endif
   
      if (iand(GMM_FLAG_CRTD,directory(cur_page)%entry(cur_entry)%a%flags) .ne. 0) then 
         print *,'ERROR: gmm_create, field ',lcl_name,' has already been created'
         key = 0
         nullify(p)
         gmm_create382 = GMM_VARIABLE_ALREADY_CREATED
         return
!
      else   
!         print *,'Debug+ ', \' Cat(gmm_create, EXTENSION,) \','array ',lcl_name,'must then have been read from a restart file' 
         localattr%flags = ior(localattr%flags,directory(cur_page)%entry(cur_entry)%a%flags) 
         key=directory(cur_page)%entry(cur_entry)%a%key      
         directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
         p=>gmm_ptrs382(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p   
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i20)') 'Debug+++ gmm_create name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p)
         directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
         gmm_create382 = 0
         return   
      endif
   else        
      if (iand(GMM_FLAG_RSTR,localattr%flags).ne.0 .and. restart_mode) then  
         print *,'ERROR: gmm_create field ',lcl_name, 'should have been read from restart file but was not'
!  ==========  HOW SERIOUS AN ERROR IS THIS ? ===============
      endif
      call add_directory_entry
   endif
   
   
   ordinal = ordinal + 1   
   key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
   key = key + ishft(382,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
   directory(cur_page)%entry(cur_entry)%a = localattr  
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
   directory(cur_page)%entry(cur_entry)%name = lcl_name
   directory(cur_page)%entry(cur_entry)%a%key = key
   directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
   directory(cur_page)%entry(cur_entry)%l(1:2) = dims(1:2)  
   directory(cur_page)%entry(cur_entry)%data_type = 382 
   if (associated(p)) then  
      print *,'GMM_CREATE: using user supplied array'
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      ier = add_table_entry(p, key)
! ======= must check that certain attributes are not requested (e.g. FLAG_RSTR) and that size is consistent
   else
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      allocate(p(dims(1)%low:dims(1)%high,&
                &dims(2)%low:dims(2)%high),stat=ier)
      if (ier /= 0) then
         gmm_create382 = GMM_ERROR
         return
      endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_create creation name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
      directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
      ier = add_table_entry(p, key)
   endif
   field_meta%l = directory(cur_page)%entry(cur_entry)%l
   field_meta%a = directory(cur_page)%entry(cur_entry)%a
!    Cat(gmm_ptrs, EXTENSION,)(directory(cur_page)%entry(cur_entry)%f => p
!         if (iand(directory(cur_page)%entry(cur_entry)%a%flags,FLAG_IZER) .ne. 0) then
!           directory(cur_page)%entry(cur_entry)%f = 0        ! ZERO fill requested
!         endif
! CODE MISSING HERE FOR INITIALIZATION
   if (iand(field_meta%a%flags, GMM_FLAG_IZER) /= 0) THEN
!     print *,'Debug+ gmm_create init to zero for variable ',lcl_name
     p = 0
   endif
   gmm_create382 = 0
   end function gmm_create382
!
   integer function gmm_create181(iname,p,field_meta,flags_arg)
   use gmm_internals
   use pointer_table_data_181
   implicit none
   character(len=*), intent(in) :: iname               
   integer*8, pointer :: p(:)
   type(gmm_metadata), intent(inout) :: field_meta               
   integer, intent(in), optional :: flags_arg
   integer *8 get_address_from
   external get_address_from
   external fool_optimizer
       interface
         subroutine check_directory_entry(name,key)
         character(len=*) :: name
         integer*8, intent(in) :: key
         end subroutine check_directory_entry
         subroutine find_directory_entry(name, key)
         implicit none
         character(len=*) :: name
         integer*8, optional :: key
         end subroutine find_directory_entry
         subroutine add_directory_entry
         end subroutine add_directory_entry
       end interface
   type(gmm_attributes) :: localattr, attrs
   type (gmm_attributes) lcl_attr
   type(gmm_layout), dimension(4) :: lcl_layout, dims
   integer lcl_datatype
   integer*8 :: key                      
   logical consistent
   integer i, ier
   integer lcl_pti
   character(len=GMM_MAXNAMELENGTH) :: lcl_name               
   integer u_bound, l_bound
   integer*8, pointer :: pp(:)
   if (present(flags_arg)) then
    field_meta%a%flags = flags_arg
   endif
   lcl_layout = field_meta%l
   dims = lcl_layout
   lcl_attr   = field_meta%a
   attrs = lcl_attr
   lcl_datatype = 181
   
   
   if (associated(p)) then
      consistent=.true.
      do i=1,1   
         consistent = consistent .and. size(p,i).eq.(dims(i)%high-dims(i)%low+1)
      enddo
      if (.not. consistent ) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'ERROR: gmm_create, p has dimensions that are not consistent with dims'
         endif
         key=0
         gmm_create181 = GMM_INCONSISTENT_DIMS
         return 
      endif
   endif  
   
   localattr = attrs    
   lcl_name = trim(iname)
   localattr%flags = iand(localattr%flags,FLAGS_KEPT_ON_CREATE)  
!   call find_directory_entry(localattr%name,key)     ! is there a field with this name that exists ?
   call find_directory_entry(lcl_name,key)     
   
   
   if (cur_page .ne. 0 .and. cur_entry .ne. 0) then    
      if (associated(p)) then
         print *,'ERROR: gmm_create called with existing p and array has already been created'
         key=0
         gmm_create181 = GMM_ARRAY_ALREADY_EXISTS
         return 
      endif
      pp=>gmm_ptrs181(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
      consistent=.true.
      do i=1,1   
         call fool_optimizer(size(pp,i))  
         consistent = consistent .and. (size(pp,i).eq.(dims(i)%high-dims(i)%low+1))
         if (.not. consistent ) print *,'size(pp,',i,')=',size(pp,i),' high=',dims(i)%high,' low=',dims(i)%low
      enddo
      if (.not. consistent ) then
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=1 =',lbound(pp,1),ubound(pp,1)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=2 =',lbound(pp,2),ubound(pp,2)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=3 =',lbound(pp,3),ubound(pp,3)
         print *,'ERROR: gmm_create, requested dimensions differ from previous specification (restart/create)'
         print *,'ERROR: gmm_create, variable name ="',lcl_name,'"'
         key=0
         nullify(p)
         gmm_create181 = GMM_INCONSISTENT_DIMS
         return 
      else
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
           print *,'INFO: gmm_create, variable name =',lcl_name,' exists and is consistent'
         endif
      endif
   
      if (iand(GMM_FLAG_CRTD,directory(cur_page)%entry(cur_entry)%a%flags) .ne. 0) then 
         print *,'ERROR: gmm_create, field ',lcl_name,' has already been created'
         key = 0
         nullify(p)
         gmm_create181 = GMM_VARIABLE_ALREADY_CREATED
         return
!
      else   
!         print *,'Debug+ ', \' Cat(gmm_create, EXTENSION,) \','array ',lcl_name,'must then have been read from a restart file' 
         localattr%flags = ior(localattr%flags,directory(cur_page)%entry(cur_entry)%a%flags) 
         key=directory(cur_page)%entry(cur_entry)%a%key      
         directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
         p=>gmm_ptrs181(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p   
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i20)') 'Debug+++ gmm_create name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p)
         directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
         gmm_create181 = 0
         return   
      endif
   else        
      if (iand(GMM_FLAG_RSTR,localattr%flags).ne.0 .and. restart_mode) then  
         print *,'ERROR: gmm_create field ',lcl_name, 'should have been read from restart file but was not'
!  ==========  HOW SERIOUS AN ERROR IS THIS ? ===============
      endif
      call add_directory_entry
   endif
   
   
   ordinal = ordinal + 1   
   key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
   key = key + ishft(181,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
   directory(cur_page)%entry(cur_entry)%a = localattr  
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
   directory(cur_page)%entry(cur_entry)%name = lcl_name
   directory(cur_page)%entry(cur_entry)%a%key = key
   directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
   directory(cur_page)%entry(cur_entry)%l(1:1) = dims(1:1)  
   directory(cur_page)%entry(cur_entry)%data_type = 181 
   if (associated(p)) then  
      print *,'GMM_CREATE: using user supplied array'
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      ier = add_table_entry(p, key)
! ======= must check that certain attributes are not requested (e.g. FLAG_RSTR) and that size is consistent
   else
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      allocate(p(dims(1)%low:dims(1)%high),stat=ier)
      if (ier /= 0) then
         gmm_create181 = GMM_ERROR
         return
      endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_create creation name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
      directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
      ier = add_table_entry(p, key)
   endif
   field_meta%l = directory(cur_page)%entry(cur_entry)%l
   field_meta%a = directory(cur_page)%entry(cur_entry)%a
!    Cat(gmm_ptrs, EXTENSION,)(directory(cur_page)%entry(cur_entry)%f => p
!         if (iand(directory(cur_page)%entry(cur_entry)%a%flags,FLAG_IZER) .ne. 0) then
!           directory(cur_page)%entry(cur_entry)%f = 0        ! ZERO fill requested
!         endif
! CODE MISSING HERE FOR INITIALIZATION
   if (iand(field_meta%a%flags, GMM_FLAG_IZER) /= 0) THEN
!     print *,'Debug+ gmm_create init to zero for variable ',lcl_name
     p = 0
   endif
   gmm_create181 = 0
   end function gmm_create181
!
   integer function gmm_create141(iname,p,field_meta,flags_arg)
   use gmm_internals
   use pointer_table_data_141
   implicit none
   character(len=*), intent(in) :: iname               
   integer*4, pointer :: p(:)
   type(gmm_metadata), intent(inout) :: field_meta               
   integer, intent(in), optional :: flags_arg
   integer *8 get_address_from
   external get_address_from
   external fool_optimizer
       interface
         subroutine check_directory_entry(name,key)
         character(len=*) :: name
         integer*8, intent(in) :: key
         end subroutine check_directory_entry
         subroutine find_directory_entry(name, key)
         implicit none
         character(len=*) :: name
         integer*8, optional :: key
         end subroutine find_directory_entry
         subroutine add_directory_entry
         end subroutine add_directory_entry
       end interface
   type(gmm_attributes) :: localattr, attrs
   type (gmm_attributes) lcl_attr
   type(gmm_layout), dimension(4) :: lcl_layout, dims
   integer lcl_datatype
   integer*8 :: key                      
   logical consistent
   integer i, ier
   integer lcl_pti
   character(len=GMM_MAXNAMELENGTH) :: lcl_name               
   integer u_bound, l_bound
   integer*4, pointer :: pp(:)
   if (present(flags_arg)) then
    field_meta%a%flags = flags_arg
   endif
   lcl_layout = field_meta%l
   dims = lcl_layout
   lcl_attr   = field_meta%a
   attrs = lcl_attr
   lcl_datatype = 141
   
   
   if (associated(p)) then
      consistent=.true.
      do i=1,1   
         consistent = consistent .and. size(p,i).eq.(dims(i)%high-dims(i)%low+1)
      enddo
      if (.not. consistent ) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'ERROR: gmm_create, p has dimensions that are not consistent with dims'
         endif
         key=0
         gmm_create141 = GMM_INCONSISTENT_DIMS
         return 
      endif
   endif  
   
   localattr = attrs    
   lcl_name = trim(iname)
   localattr%flags = iand(localattr%flags,FLAGS_KEPT_ON_CREATE)  
!   call find_directory_entry(localattr%name,key)     ! is there a field with this name that exists ?
   call find_directory_entry(lcl_name,key)     
   
   
   if (cur_page .ne. 0 .and. cur_entry .ne. 0) then    
      if (associated(p)) then
         print *,'ERROR: gmm_create called with existing p and array has already been created'
         key=0
         gmm_create141 = GMM_ARRAY_ALREADY_EXISTS
         return 
      endif
      pp=>gmm_ptrs141(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
      consistent=.true.
      do i=1,1   
         call fool_optimizer(size(pp,i))  
         consistent = consistent .and. (size(pp,i).eq.(dims(i)%high-dims(i)%low+1))
         if (.not. consistent ) print *,'size(pp,',i,')=',size(pp,i),' high=',dims(i)%high,' low=',dims(i)%low
      enddo
      if (.not. consistent ) then
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=1 =',lbound(pp,1),ubound(pp,1)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=2 =',lbound(pp,2),ubound(pp,2)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=3 =',lbound(pp,3),ubound(pp,3)
         print *,'ERROR: gmm_create, requested dimensions differ from previous specification (restart/create)'
         print *,'ERROR: gmm_create, variable name ="',lcl_name,'"'
         key=0
         nullify(p)
         gmm_create141 = GMM_INCONSISTENT_DIMS
         return 
      else
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
           print *,'INFO: gmm_create, variable name =',lcl_name,' exists and is consistent'
         endif
      endif
   
      if (iand(GMM_FLAG_CRTD,directory(cur_page)%entry(cur_entry)%a%flags) .ne. 0) then 
         print *,'ERROR: gmm_create, field ',lcl_name,' has already been created'
         key = 0
         nullify(p)
         gmm_create141 = GMM_VARIABLE_ALREADY_CREATED
         return
!
      else   
!         print *,'Debug+ ', \' Cat(gmm_create, EXTENSION,) \','array ',lcl_name,'must then have been read from a restart file' 
         localattr%flags = ior(localattr%flags,directory(cur_page)%entry(cur_entry)%a%flags) 
         key=directory(cur_page)%entry(cur_entry)%a%key      
         directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
         p=>gmm_ptrs141(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p   
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i20)') 'Debug+++ gmm_create name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p)
         directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
         gmm_create141 = 0
         return   
      endif
   else        
      if (iand(GMM_FLAG_RSTR,localattr%flags).ne.0 .and. restart_mode) then  
         print *,'ERROR: gmm_create field ',lcl_name, 'should have been read from restart file but was not'
!  ==========  HOW SERIOUS AN ERROR IS THIS ? ===============
      endif
      call add_directory_entry
   endif
   
   
   ordinal = ordinal + 1   
   key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
   key = key + ishft(141,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
   directory(cur_page)%entry(cur_entry)%a = localattr  
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
   directory(cur_page)%entry(cur_entry)%name = lcl_name
   directory(cur_page)%entry(cur_entry)%a%key = key
   directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
   directory(cur_page)%entry(cur_entry)%l(1:1) = dims(1:1)  
   directory(cur_page)%entry(cur_entry)%data_type = 141 
   if (associated(p)) then  
      print *,'GMM_CREATE: using user supplied array'
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      ier = add_table_entry(p, key)
! ======= must check that certain attributes are not requested (e.g. FLAG_RSTR) and that size is consistent
   else
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      allocate(p(dims(1)%low:dims(1)%high),stat=ier)
      if (ier /= 0) then
         gmm_create141 = GMM_ERROR
         return
      endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_create creation name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
      directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
      ier = add_table_entry(p, key)
   endif
   field_meta%l = directory(cur_page)%entry(cur_entry)%l
   field_meta%a = directory(cur_page)%entry(cur_entry)%a
!    Cat(gmm_ptrs, EXTENSION,)(directory(cur_page)%entry(cur_entry)%f => p
!         if (iand(directory(cur_page)%entry(cur_entry)%a%flags,FLAG_IZER) .ne. 0) then
!           directory(cur_page)%entry(cur_entry)%f = 0        ! ZERO fill requested
!         endif
! CODE MISSING HERE FOR INITIALIZATION
   if (iand(field_meta%a%flags, GMM_FLAG_IZER) /= 0) THEN
!     print *,'Debug+ gmm_create init to zero for variable ',lcl_name
     p = 0
   endif
   gmm_create141 = 0
   end function gmm_create141
!
   integer function gmm_create281(iname,p,field_meta,flags_arg)
   use gmm_internals
   use pointer_table_data_281
   implicit none
   character(len=*), intent(in) :: iname               
   real*8, pointer :: p(:)
   type(gmm_metadata), intent(inout) :: field_meta               
   integer, intent(in), optional :: flags_arg
   integer *8 get_address_from
   external get_address_from
   external fool_optimizer
       interface
         subroutine check_directory_entry(name,key)
         character(len=*) :: name
         integer*8, intent(in) :: key
         end subroutine check_directory_entry
         subroutine find_directory_entry(name, key)
         implicit none
         character(len=*) :: name
         integer*8, optional :: key
         end subroutine find_directory_entry
         subroutine add_directory_entry
         end subroutine add_directory_entry
       end interface
   type(gmm_attributes) :: localattr, attrs
   type (gmm_attributes) lcl_attr
   type(gmm_layout), dimension(4) :: lcl_layout, dims
   integer lcl_datatype
   integer*8 :: key                      
   logical consistent
   integer i, ier
   integer lcl_pti
   character(len=GMM_MAXNAMELENGTH) :: lcl_name               
   integer u_bound, l_bound
   real*8, pointer :: pp(:)
   if (present(flags_arg)) then
    field_meta%a%flags = flags_arg
   endif
   lcl_layout = field_meta%l
   dims = lcl_layout
   lcl_attr   = field_meta%a
   attrs = lcl_attr
   lcl_datatype = 281
   
   
   if (associated(p)) then
      consistent=.true.
      do i=1,1   
         consistent = consistent .and. size(p,i).eq.(dims(i)%high-dims(i)%low+1)
      enddo
      if (.not. consistent ) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'ERROR: gmm_create, p has dimensions that are not consistent with dims'
         endif
         key=0
         gmm_create281 = GMM_INCONSISTENT_DIMS
         return 
      endif
   endif  
   
   localattr = attrs    
   lcl_name = trim(iname)
   localattr%flags = iand(localattr%flags,FLAGS_KEPT_ON_CREATE)  
!   call find_directory_entry(localattr%name,key)     ! is there a field with this name that exists ?
   call find_directory_entry(lcl_name,key)     
   
   
   if (cur_page .ne. 0 .and. cur_entry .ne. 0) then    
      if (associated(p)) then
         print *,'ERROR: gmm_create called with existing p and array has already been created'
         key=0
         gmm_create281 = GMM_ARRAY_ALREADY_EXISTS
         return 
      endif
      pp=>gmm_ptrs281(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
      consistent=.true.
      do i=1,1   
         call fool_optimizer(size(pp,i))  
         consistent = consistent .and. (size(pp,i).eq.(dims(i)%high-dims(i)%low+1))
         if (.not. consistent ) print *,'size(pp,',i,')=',size(pp,i),' high=',dims(i)%high,' low=',dims(i)%low
      enddo
      if (.not. consistent ) then
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=1 =',lbound(pp,1),ubound(pp,1)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=2 =',lbound(pp,2),ubound(pp,2)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=3 =',lbound(pp,3),ubound(pp,3)
         print *,'ERROR: gmm_create, requested dimensions differ from previous specification (restart/create)'
         print *,'ERROR: gmm_create, variable name ="',lcl_name,'"'
         key=0
         nullify(p)
         gmm_create281 = GMM_INCONSISTENT_DIMS
         return 
      else
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
           print *,'INFO: gmm_create, variable name =',lcl_name,' exists and is consistent'
         endif
      endif
   
      if (iand(GMM_FLAG_CRTD,directory(cur_page)%entry(cur_entry)%a%flags) .ne. 0) then 
         print *,'ERROR: gmm_create, field ',lcl_name,' has already been created'
         key = 0
         nullify(p)
         gmm_create281 = GMM_VARIABLE_ALREADY_CREATED
         return
!
      else   
!         print *,'Debug+ ', \' Cat(gmm_create, EXTENSION,) \','array ',lcl_name,'must then have been read from a restart file' 
         localattr%flags = ior(localattr%flags,directory(cur_page)%entry(cur_entry)%a%flags) 
         key=directory(cur_page)%entry(cur_entry)%a%key      
         directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
         p=>gmm_ptrs281(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p   
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i20)') 'Debug+++ gmm_create name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p)
         directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
         gmm_create281 = 0
         return   
      endif
   else        
      if (iand(GMM_FLAG_RSTR,localattr%flags).ne.0 .and. restart_mode) then  
         print *,'ERROR: gmm_create field ',lcl_name, 'should have been read from restart file but was not'
!  ==========  HOW SERIOUS AN ERROR IS THIS ? ===============
      endif
      call add_directory_entry
   endif
   
   
   ordinal = ordinal + 1   
   key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
   key = key + ishft(281,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
   directory(cur_page)%entry(cur_entry)%a = localattr  
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
   directory(cur_page)%entry(cur_entry)%name = lcl_name
   directory(cur_page)%entry(cur_entry)%a%key = key
   directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
   directory(cur_page)%entry(cur_entry)%l(1:1) = dims(1:1)  
   directory(cur_page)%entry(cur_entry)%data_type = 281 
   if (associated(p)) then  
      print *,'GMM_CREATE: using user supplied array'
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      ier = add_table_entry(p, key)
! ======= must check that certain attributes are not requested (e.g. FLAG_RSTR) and that size is consistent
   else
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      allocate(p(dims(1)%low:dims(1)%high),stat=ier)
      if (ier /= 0) then
         gmm_create281 = GMM_ERROR
         return
      endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_create creation name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
      directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
      ier = add_table_entry(p, key)
   endif
   field_meta%l = directory(cur_page)%entry(cur_entry)%l
   field_meta%a = directory(cur_page)%entry(cur_entry)%a
!    Cat(gmm_ptrs, EXTENSION,)(directory(cur_page)%entry(cur_entry)%f => p
!         if (iand(directory(cur_page)%entry(cur_entry)%a%flags,FLAG_IZER) .ne. 0) then
!           directory(cur_page)%entry(cur_entry)%f = 0        ! ZERO fill requested
!         endif
! CODE MISSING HERE FOR INITIALIZATION
   if (iand(field_meta%a%flags, GMM_FLAG_IZER) /= 0) THEN
!     print *,'Debug+ gmm_create init to zero for variable ',lcl_name
     p = 0
   endif
   gmm_create281 = 0
   end function gmm_create281
!
   integer function gmm_create241(iname,p,field_meta,flags_arg)
   use gmm_internals
   use pointer_table_data_241
   implicit none
   character(len=*), intent(in) :: iname               
   real*4, pointer :: p(:)
   type(gmm_metadata), intent(inout) :: field_meta               
   integer, intent(in), optional :: flags_arg
   integer *8 get_address_from
   external get_address_from
   external fool_optimizer
       interface
         subroutine check_directory_entry(name,key)
         character(len=*) :: name
         integer*8, intent(in) :: key
         end subroutine check_directory_entry
         subroutine find_directory_entry(name, key)
         implicit none
         character(len=*) :: name
         integer*8, optional :: key
         end subroutine find_directory_entry
         subroutine add_directory_entry
         end subroutine add_directory_entry
       end interface
   type(gmm_attributes) :: localattr, attrs
   type (gmm_attributes) lcl_attr
   type(gmm_layout), dimension(4) :: lcl_layout, dims
   integer lcl_datatype
   integer*8 :: key                      
   logical consistent
   integer i, ier
   integer lcl_pti
   character(len=GMM_MAXNAMELENGTH) :: lcl_name               
   integer u_bound, l_bound
   real*4, pointer :: pp(:)
   if (present(flags_arg)) then
    field_meta%a%flags = flags_arg
   endif
   lcl_layout = field_meta%l
   dims = lcl_layout
   lcl_attr   = field_meta%a
   attrs = lcl_attr
   lcl_datatype = 241
   
   
   if (associated(p)) then
      consistent=.true.
      do i=1,1   
         consistent = consistent .and. size(p,i).eq.(dims(i)%high-dims(i)%low+1)
      enddo
      if (.not. consistent ) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'ERROR: gmm_create, p has dimensions that are not consistent with dims'
         endif
         key=0
         gmm_create241 = GMM_INCONSISTENT_DIMS
         return 
      endif
   endif  
   
   localattr = attrs    
   lcl_name = trim(iname)
   localattr%flags = iand(localattr%flags,FLAGS_KEPT_ON_CREATE)  
!   call find_directory_entry(localattr%name,key)     ! is there a field with this name that exists ?
   call find_directory_entry(lcl_name,key)     
   
   
   if (cur_page .ne. 0 .and. cur_entry .ne. 0) then    
      if (associated(p)) then
         print *,'ERROR: gmm_create called with existing p and array has already been created'
         key=0
         gmm_create241 = GMM_ARRAY_ALREADY_EXISTS
         return 
      endif
      pp=>gmm_ptrs241(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
      consistent=.true.
      do i=1,1   
         call fool_optimizer(size(pp,i))  
         consistent = consistent .and. (size(pp,i).eq.(dims(i)%high-dims(i)%low+1))
         if (.not. consistent ) print *,'size(pp,',i,')=',size(pp,i),' high=',dims(i)%high,' low=',dims(i)%low
      enddo
      if (.not. consistent ) then
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=1 =',lbound(pp,1),ubound(pp,1)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=2 =',lbound(pp,2),ubound(pp,2)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=3 =',lbound(pp,3),ubound(pp,3)
         print *,'ERROR: gmm_create, requested dimensions differ from previous specification (restart/create)'
         print *,'ERROR: gmm_create, variable name ="',lcl_name,'"'
         key=0
         nullify(p)
         gmm_create241 = GMM_INCONSISTENT_DIMS
         return 
      else
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
           print *,'INFO: gmm_create, variable name =',lcl_name,' exists and is consistent'
         endif
      endif
   
      if (iand(GMM_FLAG_CRTD,directory(cur_page)%entry(cur_entry)%a%flags) .ne. 0) then 
         print *,'ERROR: gmm_create, field ',lcl_name,' has already been created'
         key = 0
         nullify(p)
         gmm_create241 = GMM_VARIABLE_ALREADY_CREATED
         return
!
      else   
!         print *,'Debug+ ', \' Cat(gmm_create, EXTENSION,) \','array ',lcl_name,'must then have been read from a restart file' 
         localattr%flags = ior(localattr%flags,directory(cur_page)%entry(cur_entry)%a%flags) 
         key=directory(cur_page)%entry(cur_entry)%a%key      
         directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
         p=>gmm_ptrs241(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p   
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i20)') 'Debug+++ gmm_create name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p)
         directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
         gmm_create241 = 0
         return   
      endif
   else        
      if (iand(GMM_FLAG_RSTR,localattr%flags).ne.0 .and. restart_mode) then  
         print *,'ERROR: gmm_create field ',lcl_name, 'should have been read from restart file but was not'
!  ==========  HOW SERIOUS AN ERROR IS THIS ? ===============
      endif
      call add_directory_entry
   endif
   
   
   ordinal = ordinal + 1   
   key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
   key = key + ishft(241,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
   directory(cur_page)%entry(cur_entry)%a = localattr  
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
   directory(cur_page)%entry(cur_entry)%name = lcl_name
   directory(cur_page)%entry(cur_entry)%a%key = key
   directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
   directory(cur_page)%entry(cur_entry)%l(1:1) = dims(1:1)  
   directory(cur_page)%entry(cur_entry)%data_type = 241 
   if (associated(p)) then  
      print *,'GMM_CREATE: using user supplied array'
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      ier = add_table_entry(p, key)
! ======= must check that certain attributes are not requested (e.g. FLAG_RSTR) and that size is consistent
   else
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      allocate(p(dims(1)%low:dims(1)%high),stat=ier)
      if (ier /= 0) then
         gmm_create241 = GMM_ERROR
         return
      endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_create creation name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
      directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
      ier = add_table_entry(p, key)
   endif
   field_meta%l = directory(cur_page)%entry(cur_entry)%l
   field_meta%a = directory(cur_page)%entry(cur_entry)%a
!    Cat(gmm_ptrs, EXTENSION,)(directory(cur_page)%entry(cur_entry)%f => p
!         if (iand(directory(cur_page)%entry(cur_entry)%a%flags,FLAG_IZER) .ne. 0) then
!           directory(cur_page)%entry(cur_entry)%f = 0        ! ZERO fill requested
!         endif
! CODE MISSING HERE FOR INITIALIZATION
   if (iand(field_meta%a%flags, GMM_FLAG_IZER) /= 0) THEN
!     print *,'Debug+ gmm_create init to zero for variable ',lcl_name
     p = 0
   endif
   gmm_create241 = 0
   end function gmm_create241
!
   integer function gmm_create381(iname,p,field_meta,flags_arg)
   use gmm_internals
   use pointer_table_data_381
   implicit none
   character(len=*), intent(in) :: iname               
   complex*8, pointer :: p(:)
   type(gmm_metadata), intent(inout) :: field_meta               
   integer, intent(in), optional :: flags_arg
   integer *8 get_address_from
   external get_address_from
   external fool_optimizer
       interface
         subroutine check_directory_entry(name,key)
         character(len=*) :: name
         integer*8, intent(in) :: key
         end subroutine check_directory_entry
         subroutine find_directory_entry(name, key)
         implicit none
         character(len=*) :: name
         integer*8, optional :: key
         end subroutine find_directory_entry
         subroutine add_directory_entry
         end subroutine add_directory_entry
       end interface
   type(gmm_attributes) :: localattr, attrs
   type (gmm_attributes) lcl_attr
   type(gmm_layout), dimension(4) :: lcl_layout, dims
   integer lcl_datatype
   integer*8 :: key                      
   logical consistent
   integer i, ier
   integer lcl_pti
   character(len=GMM_MAXNAMELENGTH) :: lcl_name               
   integer u_bound, l_bound
   complex*8, pointer :: pp(:)
   if (present(flags_arg)) then
    field_meta%a%flags = flags_arg
   endif
   lcl_layout = field_meta%l
   dims = lcl_layout
   lcl_attr   = field_meta%a
   attrs = lcl_attr
   lcl_datatype = 381
   
   
   if (associated(p)) then
      consistent=.true.
      do i=1,1   
         consistent = consistent .and. size(p,i).eq.(dims(i)%high-dims(i)%low+1)
      enddo
      if (.not. consistent ) then
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
            print *,'ERROR: gmm_create, p has dimensions that are not consistent with dims'
         endif
         key=0
         gmm_create381 = GMM_INCONSISTENT_DIMS
         return 
      endif
   endif  
   
   localattr = attrs    
   lcl_name = trim(iname)
   localattr%flags = iand(localattr%flags,FLAGS_KEPT_ON_CREATE)  
!   call find_directory_entry(localattr%name,key)     ! is there a field with this name that exists ?
   call find_directory_entry(lcl_name,key)     
   
   
   if (cur_page .ne. 0 .and. cur_entry .ne. 0) then    
      if (associated(p)) then
         print *,'ERROR: gmm_create called with existing p and array has already been created'
         key=0
         gmm_create381 = GMM_ARRAY_ALREADY_EXISTS
         return 
      endif
      pp=>gmm_ptrs381(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p
      consistent=.true.
      do i=1,1   
         call fool_optimizer(size(pp,i))  
         consistent = consistent .and. (size(pp,i).eq.(dims(i)%high-dims(i)%low+1))
         if (.not. consistent ) print *,'size(pp,',i,')=',size(pp,i),' high=',dims(i)%high,' low=',dims(i)%low
      enddo
      if (.not. consistent ) then
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=1 =',lbound(pp,1),ubound(pp,1)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=2 =',lbound(pp,2),ubound(pp,2)
!         write(6,'(a,2x,a8,2x,a,2x,i3,2x,i3)') 'Debug+++ ',lcl_name,' bounds dim=3 =',lbound(pp,3),ubound(pp,3)
         print *,'ERROR: gmm_create, requested dimensions differ from previous specification (restart/create)'
         print *,'ERROR: gmm_create, variable name ="',lcl_name,'"'
         key=0
         nullify(p)
         gmm_create381 = GMM_INCONSISTENT_DIMS
         return 
      else
         if (gmm_verbose_level == GMM_MSG_DEBUG) then
           print *,'INFO: gmm_create, variable name =',lcl_name,' exists and is consistent'
         endif
      endif
   
      if (iand(GMM_FLAG_CRTD,directory(cur_page)%entry(cur_entry)%a%flags) .ne. 0) then 
         print *,'ERROR: gmm_create, field ',lcl_name,' has already been created'
         key = 0
         nullify(p)
         gmm_create381 = GMM_VARIABLE_ALREADY_CREATED
         return
!
      else   
!         print *,'Debug+ ', \' Cat(gmm_create, EXTENSION,) \','array ',lcl_name,'must then have been read from a restart file' 
         localattr%flags = ior(localattr%flags,directory(cur_page)%entry(cur_entry)%a%flags) 
         key=directory(cur_page)%entry(cur_entry)%a%key      
         directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
         p=>gmm_ptrs381(directory(cur_page)%entry(cur_entry)%pointer_table_index)%p   
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i20)') 'Debug+++ gmm_create name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p)
         directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
         gmm_create381 = 0
         return   
      endif
   else        
      if (iand(GMM_FLAG_RSTR,localattr%flags).ne.0 .and. restart_mode) then  
         print *,'ERROR: gmm_create field ',lcl_name, 'should have been read from restart file but was not'
!  ==========  HOW SERIOUS AN ERROR IS THIS ? ===============
      endif
      call add_directory_entry
   endif
   
   
   ordinal = ordinal + 1   
   key = ishft((cur_page-1),PAGE_NB_SHFT) + ishft((cur_entry-1),NTRY_NB_SHFT)
   key = key + ishft(381,EXTN_NB_SHFT) + ishft(ordinal,MAGC_NB_SHFT)
   directory(cur_page)%entry(cur_entry)%a = localattr  
! CODE POSSIBLY MISSING HERE FOR FLAGS SETTINGS
   directory(cur_page)%entry(cur_entry)%name = lcl_name
   directory(cur_page)%entry(cur_entry)%a%key = key
   directory(cur_page)%entry(cur_entry)%a%flags = ior(localattr%flags,GMM_FLAG_CRTD) 
   directory(cur_page)%entry(cur_entry)%l(1:1) = dims(1:1)  
   directory(cur_page)%entry(cur_entry)%data_type = 381 
   if (associated(p)) then  
      print *,'GMM_CREATE: using user supplied array'
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      ier = add_table_entry(p, key)
! ======= must check that certain attributes are not requested (e.g. FLAG_RSTR) and that size is consistent
   else
      lcl_pti = lgmm_get_nxt_avail_ptr()
      directory(cur_page)%entry(cur_entry)%pointer_table_index = lcl_pti
      allocate(p(dims(1)%low:dims(1)%high),stat=ier)
      if (ier /= 0) then
         gmm_create381 = GMM_ERROR
         return
      endif
!    write(6,'(a,a8,a,i4,a,i4,a,i4,a,i10)') 'Debug+++ gmm_create creation name=',lcl_name,' cur_page=',cur_page,' cur_entry=',cur_entry,' index=',directory(cur_page)%entry(cur_entry)%pointer_table_index,' addr=',get_address_from(p) 
      directory(cur_page)%entry(cur_entry)%array_addr = get_address_from(p)
      ier = add_table_entry(p, key)
   endif
   field_meta%l = directory(cur_page)%entry(cur_entry)%l
   field_meta%a = directory(cur_page)%entry(cur_entry)%a
!    Cat(gmm_ptrs, EXTENSION,)(directory(cur_page)%entry(cur_entry)%f => p
!         if (iand(directory(cur_page)%entry(cur_entry)%a%flags,FLAG_IZER) .ne. 0) then
!           directory(cur_page)%entry(cur_entry)%f = 0        ! ZERO fill requested
!         endif
! CODE MISSING HERE FOR INITIALIZATION
   if (iand(field_meta%a%flags, GMM_FLAG_IZER) /= 0) THEN
!     print *,'Debug+ gmm_create init to zero for variable ',lcl_name
     p = 0
   endif
   gmm_create381 = 0
   end function gmm_create381
!
