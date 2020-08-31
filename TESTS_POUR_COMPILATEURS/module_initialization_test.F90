module demo
  integer, parameter :: const_zFORp_version_major = 0
  integer, parameter :: const_zFORp_version_minor = 5
  integer, parameter :: const_zFORp_version_patch = 5
  integer, protected, bind(c, name="zFORp_version_major") :: zFORp_version_major = const_zFORp_version_major
  integer, protected, bind(c, name="zFORp_version_minor") :: zFORp_version_minor = const_zFORp_version_minor
  integer, protected, bind(c, name="zFORp_version_patch") :: zFORp_version_patch = const_zFORp_version_patch

end module
