#include <stdlib.h>
#include <stdio.h>
#ifdef __AVX2__
#include <immintrin.h>
#endif
#if ! defined(NPTS)
#define NPTS (64*32)
#endif
static unsigned char shuf8[40] = {0,1,4,5,8,9,12,13,128,128,128,128,128,128,128,128,
                                  0,1,4,5,8,9,12,13,128,128,128,128,128,128,128,128,
                                  0,1,4,5,8,9,12,13};

void avx_squash_32_16(short *dst,int *src,int n) {
  int i, i1;
  int n2 = n>>1;
  __m256i shuf, shuf2, s, s2;
  __m128i d, d2;
//  printf("avx_squash_32_16\n");
  shuf  = _mm256_lddqu_si256((__m256i const *)shuf8);
  shuf2 = _mm256_lddqu_si256((__m256i const *)&shuf8[8]);
  for (i=0 ; i<n ; i+=16){
    s  = _mm256_lddqu_si256((__m256i const *)&src[i ]);   // load 8 ints (256 bits)
    s2 = _mm256_lddqu_si256((__m256i const *)&src[i+8]);
    s  = _mm256_shuffle_epi8 (s, shuf);                   // compress upper and lower 128 bits into lower 64 bit part
    s2 = _mm256_shuffle_epi8 (s2, shuf2);                 // compress upper and lower 128 bits into upper 64 bit part
    s  = _mm256_or_si256(s,s2);                           // combine
    s  = _mm256_permute4x64_epi64(s, 216);                // 3 1 2 0 order for 64 bit tokens 
//    s  = _mm256_permute4x64_epi64(s, 8);                  // lower 128 bits = lower 64 bits of both 128 bits ,  upper 128 bits = don't care
//    s2 = _mm256_permute4x64_epi64(s2, 8);
//    d  = _mm256_extracti128_si256 (s, 0);                 // transfer lower 128 bits into xmm (dummy instruction in practice)
//    _mm_storeu_si128 ((__m128i*) &dst[i], d);             // store 8 shorts (128 bits)
//    d2 = _mm256_extracti128_si256 (s2, 0);
//    _mm_storeu_si128 ((__m128i*) &dst[i+8], d2);
    _mm256_storeu_si256 ((__m256i *) &dst[i], s);
  }
}

void c_squash_32_16(short *dst,int *src,int n) {
  int i;
//  printf("c_squash_32_16\n");
  for (i=0 ; i<n ; i++) dst[i] = src[i] & 0xFFFF ;
}
#include <sys/time.h>
#if ! defined(NREP)
#define NREP 1000
#endif
main() {
  struct timeval t1,t2;
  long long T1, T2;
  int duree;
  int src[NPTS];
  short dst[NPTS];
  short dst2[NPTS];
  int i;
  int errors = 0;
  int ok = 0;
#if defined(TEST)
#if defined(__AVX2__)
  printf("AVX2\n");
#endif
#if defined(__AVX__)
  printf("AVX\n");
#endif
#if defined(__BMI2__)
  printf("BMI2\n");
#endif
#if defined(__BMI__)
  printf("BMI\n");
#endif
#if defined(__FMA__)
  printf("FMA\n");
#endif
#if defined(__SSE4_2__)
  printf("SSE4_2\n");
#endif
#if defined(__SSE4_1__)
  printf("SSE4_1\n");
#endif
#if defined(__SSE3__)
  printf("SSE3\n");
#endif
#if defined(__SSE2__)
  printf("SSE2\n");
#endif
#if defined(__SSE__)
  printf("SSE\n");
#endif
#endif
  for (i=0 ; i<NPTS ; i++) { src[i] = i; dst[i] = 32767 ; }

  gettimeofday(&t1,NULL);
  for (i=0 ; i<NREP ; i++) c_squash_32_16(dst,src,NPTS);
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("duree c_squash_32_16 = %d, Mtokens = %d\n", duree,NPTS*NREP/1000000);

  gettimeofday(&t1,NULL);
  for (i=0 ; i<NREP ; i++) avx_squash_32_16(dst2,src,NPTS);
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("duree avx_squash_32_16 = %d, Mtokens = %d\n", duree,NPTS*NREP/1000000);
  for (i=0 ; i<NPTS ; i++){
    if(dst2[i] != dst[i]){
      errors++;
    }else{
      ok++;
    }
  }
  printf("ok = %d, errors = %d\n",ok,errors);
}
