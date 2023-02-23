#!/bin/bash
#module load aocc/4.0.0
module load ${1:-llvm}
#export FC=flang
set -x
rm -f *.o *mod
cat <<EOF >file1.F90
module fstd_98
  implicit none
  type :: fstd98
    integer     :: iun = -1                  ! Fortran unit number
  contains
    procedure, NOPASS     :: fst_version
  end type
interface
  module function fst_version() result(vers)          ! problem if no argumnent
!   module function fst_version(dummy) result(vers)   ! no problem if optional dummy argument
!     implicit none
!     integer, optional :: dummy
    integer :: vers
  end function fst_version
end interface
end module
EOF
cat <<EOF >file2.F90
submodule (fstd_98) fstd_98_sub
contains
  module procedure fst_version
    implicit none
    vers = 123
  end procedure
end
EOF
$FC --version
# module and submodule in separate files
$FC -c file1.F90 file2.F90
# all code in a single file
cat file1.F90 file2.F90 >file3.F90
$FC -c file3.F90

