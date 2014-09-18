#!/bin/bash
[[ ${1:---help} == *-h* ]] && cat <<true && exit 0
usage: ${0} compiler compiler options

examples:
       ${0} gfortran -O3
       ${0} pgf95 -O2 -fast -fpic
true
cat <<EOT
expected output : 5 times 
 Set tplus= 240.0
 Set uplus= to 10.0
 Set vplus= to 5.0
 uplus0(1,1)=   10.000000      u0(1,1)=   10.000000    
 vplus0(1,1)=   5.0000000      v0(1,1)=   5.0000000    
 tplus0(1,1)=   240.00000      t0(1,1)=   240.00000    
 Set tplus(:,:)= 240.0
 Set uplus(:,:)= to 10.0
 Set vplus(:,:)= to 5.0
 uplus0(1,1)=   10.000000      u0(1,1)=   10.000000    
 vplus0(1,1)=   5.0000000      v0(1,1)=   5.0000000    
 tplus0(1,1)=   240.00000      t0(1,1)=   240.00000    
EOT
cat <<EOT >a_ptr_test.f90
    program toto

     implicit none
     integer ni,nj,nk,i,j,k,nelem,dsiz
     real, target, allocatable, dimension(:,:) :: dbus
     real, allocatable, dimension (:,:) :: u0,v0,t0
     real, pointer, dimension(:) :: pbus
     integer uplus,vplus,tplus
     common /phybus_i/uplus,vplus,tplus
     real, pointer, dimension (:,:) :: uplus0,vplus0,tplus0
     ni=10
     nj=5
     nk=10
     nelem=3
     uplus=1
     vplus=ni*nk+1
     tplus=ni*nk*2 + 1
     dsiz=ni*nk*nelem
     allocate(dbus(ni*nk*nelem,nj))
     allocate(u0(ni,nk))
     allocate(v0(ni,nk))
     allocate(t0(ni,nk))
     do j=1,nj
     pbus => dbus(:,j)
     uplus0(1:ni,1:nk) => pbus(uplus:)
     vplus0(1:ni,1:nk) => pbus(vplus:)
     tplus0(1:ni,1:nk) => pbus(tplus:)
     print *,'Set tplus= 240.0'
     print *,'Set uplus= to 10.0'
     print *,'Set vplus= to 5.0'
     tplus0 = 240.0
     uplus0 = 10.0
     vplus0 = 5.0
     u0 = uplus0
     v0 = vplus0
     t0 = tplus0
     print *,'uplus0(1,1)=',uplus0(1,1),' u0(1,1)=',u0(1,1)
     print *,'vplus0(1,1)=',vplus0(1,1),' v0(1,1)=',v0(1,1)
     print *,'tplus0(1,1)=',tplus0(1,1),' t0(1,1)=',t0(1,1)
     print *,'Set tplus(:,:)= 240.0'
     print *,'Set uplus(:,:)= to 10.0'
     print *,'Set vplus(:,:)= to 5.0'

     tplus0(:,:) = 240.0
     uplus0(:,:) = 10.0
     vplus0(:,:) = 5.0
     u0(:,:) = uplus0
     v0(:,:) = vplus0
     t0(:,:) = tplus0
     print *,'uplus0(1,1)=',uplus0(1,1),' u0(1,1)=',u0(1,1)
     print *,'vplus0(1,1)=',vplus0(1,1),' v0(1,1)=',v0(1,1)
     print *,'tplus0(1,1)=',tplus0(1,1),' t0(1,1)=',t0(1,1)
     enddo
     stop
     end
EOT
Compiler=${1:-gfortran}
shift
set -x
${Compiler} $* a_ptr_test.f90
set +x
./a.out
rm -f a_ptr_test.f90 a_ptr_test.o a.out
