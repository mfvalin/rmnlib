      integer, parameter ::  MAX_PAGES    =  16      ! max number of directory pages
      integer, parameter ::  PAGE_SIZE    = 128      ! max number of entries in directory page
      integer, parameter ::  NTRY_NB_SHFT =   0      ! shift for entry number in page
      integer, parameter ::  NTRY_NB_MASK = 127      ! mask for entry number in page
      integer, parameter ::  PAGE_NB_SHFT =   7      ! shift for page number in directory
      integer, parameter ::  PAGE_NB_MASK =  15      ! mask for page number in directory
      integer, parameter ::  EXTN_NB_SHFT =  11      ! shift for extension code
      integer, parameter ::  EXTN_NB_MASK = 511      ! mask for extension code
      integer, parameter ::  MAGC_NB_SHFT =  32       ! shift for magic number
      integer, parameter ::  MAGC_NB_MASK =  -1       ! mask for magic number (all ones)

      integer, parameter :: FLAGS_KEPT_ON_CREATE=GMM_FLAG_IZER + GMM_FLAG_INAN + GMM_FLAG_RSTR ! flags kept from user specified flags upon creation
      integer, parameter :: FLAGS_KEPT_IN_RESTART=GMM_FLAG_IZER + GMM_FLAG_INAN + GMM_FLAG_RSTR + GMM_FLAG_IINV ! flags propagated to restart file
!
