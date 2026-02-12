#include <stdio.h>
#include <stdint.h>
// compile with -O2
// llvm clang, aocc clang, intel icx, gnu gcc do not exhibit the problem
// nvc (PGI/Nvidia) exhibits the problem at -O2 and above (-O -O0 -O1 are O.K.)
int main(int argc, char **argv){
  (void) (argc) ;
  (void) (argv) ;
  int64_t a64[4], b64[4], c64[4] ;
  int32_t c32[4] ;
// -DUSE_INT causes the problem to disappear
#if defined(USE_INT)
  int32_t i ;
#else
  uint32_t i ;
#endif
  for(i=0 ; i<4 ; i++){
    c32[i] = i - 2 ;
    a64[i] = c32[i] ;
    b64[i] = (int64_t)c32[i] ;
    c64[i] = ((int32_t)i - 2) ;
  }
  for(i=0 ; i<4 ; i++){
    fprintf(stderr, "i = %4d, a64[i] = %4ld, b64[i] = %4ld, c64[i] = %4ld, c32[i] = %4d\n",
            i, a64[i], b64[i], c64[i], c32[i]) ;
  }
}

