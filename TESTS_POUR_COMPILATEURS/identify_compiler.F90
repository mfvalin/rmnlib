program identify_compiler
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
