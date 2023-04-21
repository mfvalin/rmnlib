#include <stdint.h>

typedef union{
  int32_t vi[8] ;
  uint32_t vu[8] ;
} __m256i ;

typedef union{
  float vf[8] ;
} __m256 ;

typedef union{
  double vd[8] ;
} __m256d ;

__m256i  _mm256_add_epi32 (__m256i va, __m256i vb){
  int i ; 
  __m256i vr ;
  for(i=0 ; i<8 ; i++) vr.vi[i] = va.vi[i] + vb.vi[i] ;
  return vr ;
}

__m256i _mm256_loadu_si256 (__m256i const * restrict mem){
  int i ;
  int32_t * restrict m = (int32_t *) mem ; 
  __m256i vr ;
  for(i=0 ; i<8 ; i++) vr.vi[i] = m[i] ;
  return vr ;
}

void _mm256_storeu_si256 (__m256i * restrict mem, __m256i va){
  int i ; 
  int32_t * restrict m = (int32_t *) mem ; 
  for(i=0 ; i<8 ; i++) m[i] = va.vi[i] ;
}
