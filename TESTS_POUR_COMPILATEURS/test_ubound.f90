program mymain
   implicit none
   integer :: src_ijDim, numExtArraysIn

   real, dimension(2, 4)  :: r_ExtArraysIn

   src_ijDim = 2
   numExtArraysIn = 4
   call passdim(src_ijDim, numExtArraysIn, r_ExtArraysIn)
end program mymain
    subroutine passdim(src_ijDim, numExtArraysIn, ExtArraysIn)
       implicit none


       integer, intent(in) :: src_ijDim
       integer, intent(in) :: numExtArraysIn
       real, dimension(src_ijDim, numExtArraysIn), intent(in)  :: ExtArraysIn

       real, dimension(ubound(ExtArraysIn,1)) :: z0
       real, dimension(src_ijDim) :: z1

       write(*,*)"JWB:  src_ijDim, ubound(ExtArraysIn,1), ubound(z0,1)=", &
                        src_ijDim, ubound(ExtArraysIn,1), ubound(z0,1)

       write(*,*)"size of z0=",size(z0)
       write(*,*)"ubound(z0,1)=",ubound(z0,1)
       write(*,*)"ubound(z1,1)=",ubound(z1,1)
       write(*,*)"size of z1=",size(z1)
    end subroutine passdim


