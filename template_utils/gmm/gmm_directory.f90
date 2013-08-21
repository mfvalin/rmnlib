  subroutine check_directory_entry(name,key)
  use gmm_internals
  implicit none
  character(len=*) :: name
  integer*8, intent(in) :: key
!
  character(len=GMM_MAXNAMELENGTH) :: l_name
  integer temp
  logical found
!
  found = .false.
  if (cur_page == 0 .and. cur_entry == 0) then
    return
  endif
  l_name = trim(name)
  temp = ishft(key,-PAGE_NB_SHFT)
  cur_page = iand(PAGE_NB_MASK,temp)
  cur_page = min(cur_page+1,table_size)    
  temp = ishft(key,-NTRY_NB_SHFT)
  cur_entry = iand(NTRY_NB_MASK,temp)
  cur_entry = min(cur_entry+1,PAGE_SIZE)   
  found = key .eq. directory(cur_page)%entry(cur_entry)%a%key
  found = found .and. ( directory(cur_page)%entry(cur_entry)%name .eq. l_name )
  if (.not. found) then
    cur_page = 0      
    cur_entry = 0
  endif
  return
  end subroutine check_directory_entry
!!
! find entry called name in directory starting from beginning of directory (the hard way)
! upon exit cur_page and cur_entry are nonzero if desired entry found
! ==============================================================================================
   subroutine find_directory_entry(name, key)
   use gmm_internals
   implicit none
   character(len=*) :: name
   integer*8, optional :: key
   integer :: i
   character(len=GMM_MAXNAMELENGTH) :: l_name
!
   l_name = trim(name)
   cur_page = 1
   cur_entry = 1
   do i=1,used
     if (directory(cur_page)%entry(cur_entry)%name .eq. l_name) then
       if (present(key)) then
         key = directory(cur_page)%entry(cur_entry)%a%key
       endif
       return
     endif
     cur_entry = cur_entry + 1
     if (cur_entry .gt. PAGE_SIZE) then
       cur_page = cur_page + 1
       cur_entry = 1
     endif
   enddo
   cur_page = 0      
   cur_entry = 0
!   if (present(key)) then
    key = GMM_KEY_NOT_FOUND
!   endif
   return
   end subroutine find_directory_entry
!
! locate/create a new properly initialized entry in directory
! ==============================================================================================
   subroutine add_directory_entry
   use gmm_internals
   implicit none
   integer :: i
!
   if ( table_size .eq. 0 ) then  
     do i=1,MAX_PAGES
       nullify(directory(i)%entry)
     enddo
   endif
!
   used = used + 1
   last_entry = last_entry +1
   if ( last_entry .gt. PAGE_SIZE ) then  
     table_size = table_size + 1         
     last_entry = 1
     if (table_size .le. MAX_PAGES) then  
       allocate(directory(table_size)%entry(PAGE_SIZE))
     else                                
!               print *,'ERROR: too many entries in directory for type=',EXTENSION
       call qqexit(1)
     endif
     do i=1,PAGE_SIZE                    
!               nullify( directory(table_size)%entry(i)%f )      ! invalid array pointer
         directory(table_size)%entry(i)%l = GMM_NULL_LAYOUT 
         directory(table_size)%entry(i)%a = GMM_NULL_ATTRIB 
       enddo
       cur_entry = 1
     else
       cur_entry = last_entry
     endif
     cur_page = table_size
     return
     end subroutine add_directory_entry
!
