 
 
 function gmm_encodemeta(F_meta,F_code) result(F_istat)
    use gmm_internals
    implicit none
    
    
    type(gmm_metadata), intent(in) :: F_meta  
    integer,            intent(out):: F_code(:)
    
    integer ::  F_istat  
    
 
    integer :: i,j
    
    if (size(F_code) < GMM_META_SIZE) then
       F_istat = GMM_ERROR
       return
    endif
    F_istat = GMM_OK
    call movlev(F_meta, F_code, GMM_META_SIZE)
!     j=0
!     do i=1,4
!        j=j+1
!        F_code(j) = F_meta%l(i)%low
!        j=j+1
!        F_code(j) = F_meta%l(i)%high
!        j=j+1
!        F_code(j) = F_meta%l(i)%halo
!        j=j+1
!        F_code(j) = F_meta%l(i)%halomax
!        j=j+1
!        F_code(j) = F_meta%l(i)%n
!     enddo
!     j=j+1
!     call movlev(F_meta%a%key,F_code(j),2)
!     j=j+2
!     call movlev(F_meta%a%uuid1,F_code(j),2)
!     j=j+2
!     call movlev(F_meta%a%uuid2,F_code(j),2)
!     j=j+2
!     F_code(j) = F_meta%a%initmode
!     j=j+1
!     F_code(j) = F_meta%a%flags
    
    return
 end function gmm_encodemeta
  
 
 function gmm_decodemeta(F_meta,F_code) result(F_istat)
    use gmm_internals
    implicit none
    
    
    type(gmm_metadata), intent(out):: F_meta  
    integer,            intent(in) :: F_code(:)
    
    integer ::  F_istat  
    
 
    integer :: i,j
    
    if (size(F_code) < GMM_META_SIZE) then
       F_istat = GMM_ERROR
       return
    endif
    F_istat = GMM_OK
    call movlev(F_code, F_meta, GMM_META_SIZE)
!     j = 0
!     do i=1,4
!        j=j+1
!        F_meta%l(i)%low = F_code(j)
!        j=j+1
!        F_meta%l(i)%high = F_code(j)
!        j=j+1
!        F_meta%l(i)%halo = F_code(j)
!        j=j+1
!        F_meta%l(i)%halomax = F_code(j)
!        j=j+1
!        F_meta%l(i)%n = F_code(j)
!     enddo
!     j=j+1
!     call movlev(F_code(j),F_meta%a%key,2)
!     j=j+2
!     call movlev(F_code(j),F_meta%a%uuid1,2)
!     j=j+2
!     call movlev(F_code(j),F_meta%a%uuid2,2)
!     j=j+2
!     F_meta%a%initmode = F_code(j)
!     j=j+1
!     F_meta%a%flags = F_code(j)
    
    return
 end function gmm_decodemeta
