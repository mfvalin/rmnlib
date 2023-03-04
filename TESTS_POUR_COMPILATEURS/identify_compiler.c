#include <stdio.h>

int main(int argc, char **argv){

#if defined(__x86_64__)
  printf("X86 64 bit mode detected\n") ;
#endif
#if defined(__i386__)
  printf("X86 32 bit mode detected\n") ;
#endif

#if defined(__INTEL_COMPILER)
    printf("Intel icc/ifort detected\n") ;

#elif defined(__INTEL_LLVM_COMPILER)
    printf("Intel icx/ifx detected\n") ;

#elif defined(__PGI)
  printf("Nvidia/PGI compiler detected\n") ;

#elif defined(__clang__)
  printf("clang compiler detected\n") ;  // llvm/aocc flang

#elif defined(__GNUC__)
  printf("gcc detected\n") ;  // gcc or lookalike
#endif

#if defined(__GNUC_STDC_INLINE__)
  printf("gcc lookalike detected\n") ;  // gcc 
#endif

}
