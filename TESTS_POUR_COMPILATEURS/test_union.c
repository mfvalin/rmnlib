#include <stdint.h>
#include <stdio.h>

typedef struct{
  int32_t  a ;
  uint32_t b ;
  uint32_t meta[] ;
} ma_struct1 ;

typedef struct{
  int dummy[0] ;
  int i[] ;
} ma_struct2 ;

// typedef union{
//   ma_struct2 i ;
//   ma_struct1 s ;
// } ma_struct_plus ;

typedef union{
  struct{
    uint32_t sztotal ;  // size of meta + fixed size
    uint32_t szmeta ;   // size of meta
    int i[] ;
  } i ;
  struct{
    uint32_t sztotal ;  // size of meta + fixed size
    uint32_t szmeta ;   // size of meta
    int32_t  m_1 ;      // fixed item 1
//     ...              // other fixed items
    uint32_t m_n ;      // fixed item n
    uint32_t meta[] ;
  } s ;
} rsf_meta ;

int main(int argc, char **argv){
  rsf_meta meta ;
  printf("size of meta.i = %ld\n", sizeof(meta.i)) ;
  printf("size of meta.s = %ld\n", sizeof(meta.s)) ;
  printf("size of meta.s.m_1 = %ld\n", sizeof(meta.s.m_1)) ;
  printf("size of ma_struct_plus = %ld\n", sizeof(rsf_meta)) ;
  return 0 ;
}
