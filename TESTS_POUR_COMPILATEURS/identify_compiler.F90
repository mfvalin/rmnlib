function identify_compiler_f() result(code) BIND(C, name='IdentifyCompiler_f')
  implicit none
  integer :: code
  code = 0
#if defined(__x86_64__)
  code = code + 1
#endif
#if defined(__i386__)
  code = code + 2
#endif

#if defined(__GFORTRAN__)
  code = code + 8

#elif defined(__INTEL_COMPILER)
#if defined(__INTEL_LLVM_COMPILER)
  code = code + 16
#else
  code = code + 32
#endif

#elif defined(__INTEL_PRE_FFLAGS)
  code = code + 64

#elif defined(__PGI)
  code = code + 128

#elif defined(__PGIF90__)
  code = code + 256

#elif defined(__FLANG)
  code = code + 512

#elif defined(__flang__)
  code = code + 1024
#endif
  return
end
subroutine identify_compiler()
#if defined(__x86_64__)
  print *,'X86 64 bit mode detected'
#endif
#if defined(__i386__)
  print *,'X86 32 bit mode detected'
#endif

#if defined(__GFORTRAN__)
  print *,'GNU gfortran detected'

#elif defined(__INTEL_COMPILER)
#if defined(__INTEL_LLVM_COMPILER)
    print *,'Intel icx/ifx detected'
#else
    print *,'Intel icc/ifort detected'
#endif

#elif defined(__INTEL_PRE_FFLAGS)
  print *,'Intel Fortran compiler detected'

#elif defined(__PGI)
  print *,'Nvidia/PGI Fortran compiler detected'

#elif defined(__PGIF90__)
  print *,'__PGIF90__ Fortran compiler detected'

#elif defined(__FLANG)
  print *,'aocc flang mode detected'

#elif defined(__flang__)
  print *,'llvm flang mode detected'  ! llvm flang
#endif

end
program test
call identify_compiler
endif
