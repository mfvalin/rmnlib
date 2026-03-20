      interface
         function gmm_encodemeta(F_meta,F_code) result(F_istat)
      type gmm_layout                              ! dimensioning elements
         SEQUENCE
         integer :: low,high,halo,halomax,n      ! dimension is low:high, useful contents 1..n
      end type
      type gmm_attributes
        SEQUENCE
        integer*8 :: key          ! unique key, used mainly for vmmget interface
        integer*8 :: uuid1, uuid2 ! extra keys used for lookup in external tables or additional flags
        integer   :: initmode                   ! how the field must be initialized (default=-1, no init)
        integer   :: flags                      ! flags (valid, restart, .... )
      end type
      type gmm_metadata
        SEQUENCE
        type(gmm_layout), dimension(4) :: l
        type(gmm_attributes) :: a
      end type
         type(gmm_metadata), intent(in) :: F_meta  !- GMM Metadata
         integer,            intent(out):: F_code(:)
         integer ::  F_istat  !status
         end function gmm_encodemeta
         function gmm_decodemeta(F_meta,F_code) result(F_istat)
      type gmm_layout                              ! dimensioning elements
         SEQUENCE
         integer :: low,high,halo,halomax,n      ! dimension is low:high, useful contents 1..n
      end type
      type gmm_attributes
        SEQUENCE
        integer*8 :: key          ! unique key, used mainly for vmmget interface
        integer*8 :: uuid1, uuid2 ! extra keys used for lookup in external tables or additional flags
        integer   :: initmode                   ! how the field must be initialized (default=-1, no init)
        integer   :: flags                      ! flags (valid, restart, .... )
      end type
      type gmm_metadata
        SEQUENCE
        type(gmm_layout), dimension(4) :: l
        type(gmm_attributes) :: a
      end type
         type(gmm_metadata), intent(out):: F_meta  !- GMM Metadata
         integer,            intent(in) :: F_code(:)
         integer ::  F_istat  !status
         end function gmm_decodemeta
       end interface
