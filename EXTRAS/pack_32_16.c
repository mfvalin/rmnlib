#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

#include <cpu_type.h>

#ifdef __AVX2__
#include <immintrin.h>
#endif
#if ! defined(NPTS)
#define NPTS (64*32)
#endif

int MinMax(float *z_in, int n, float *Max, float *Min)  // universal version, can take advantage of AVX2 if present
{
  float smin, smax ;
  int i, j;
  int i0, i1;
  int block = 8;
  int limit = 0;              //         = n - block + 1;
#ifdef __AVX2__
  float *z = z_in;
  float zmin[8], zmax[8];
  __m256 z0, z1, zzmin0, zzmax0, zzmin1, zzmax1;
  int nminus ;
#endif
  if(n >= block){   // and avx2 is available  Cpu_has_feature(AVX2), needs #include cpu_type.h
#ifdef __AVX2__
//    nminus = (n / (2*block)) * (2*block);  // multiple of block * 2
//    i1 = nminus/2;                         // multiple of block
//    limit = i1 - block + 1;
    nminus = ((n+1)>>1);                // ceiling(n/2)
    limit = ((nminus+7)>>3)<<3 ;         // first multiple of 8 >= nminus
//  printf("DEBUG: n = %d, nminus = %d, limit = %d\n",n,nminus,limit);

    zzmin0 = _mm256_loadu_ps(&z[0]);
    zzmax0 = zzmin0;
    zzmin1 = zzmin0;
    zzmax1 = zzmax0;

    for (i0 = 0, i1 = n - limit ; i0 < limit ; i0 += block, i1 += block){    //    i0 = 0, limit-1, 8  ; i1 = i1, n-1, 8

      z0 = _mm256_loadu_ps(&z[i0]);   // stream from beginning
      z1 = _mm256_loadu_ps(&z[i1]);   // stream from "half way" point

      zzmin0 = _mm256_min_ps(zzmin0,z0);       // min
      zzmax0 = _mm256_max_ps(zzmax0,z0);       // max
      zzmin1 = _mm256_min_ps(zzmin1,z1);
      zzmax1 = _mm256_max_ps(zzmax1,z1);
    }

    zzmin0 = _mm256_min_ps(zzmin0,zzmin1);
    zzmax0 = _mm256_max_ps(zzmax0,zzmax1);
    _mm256_storeu_ps(zmin,zzmin0);
    _mm256_storeu_ps(zmax,zzmax0);
    smax = zmax[0];
    smin = zmin[0];
    for (j=1 ; j<8 ; j++){
      smax = (smax > zmax[j]) ? smax : zmax[j] ;
      smin = (smin < zmin[j]) ? smin : zmin[j] ;
    }
#endif
  }else {   // n is less than 1 block or AVX2 is not available
    i1 = 1;
    smax = z_in[0] ;
    smin = z_in[0] ;
  }

  for (i=i1 ; i<n ; i++){   /* handle leftovers the old way */
    smax = (smax > z_in[i]) ? smax : z_in[i] ;
    smin = (smin < z_in[i]) ? smin : z_in[i] ;
  }
  *Max=smax;
  *Min=smin;
  return(limit) ;
}

void c_squash_32_16(short *dst,int *src,int n) {
  int i;
//  printf("c_squash_32_16\n");
  for (i=0 ; i<n ; i++) dst[i] = src[i] & 0xFFFF ;
}

#define VBLOCK32 8
#define VBLOCK64 4

static unsigned char shufp[32] = {0,1,4,5,8,9,12,13,128,128,128,128,128,128,128,128,
                                  0,1,4,5,8,9,12,13,128,128,128,128,128,128,128,128};

int32_t FastFloatPacker16(uint16_t *stream, float *source, int n, int32_t nbits, int32_t *Header)
{
  int32_t limit, i ;
  union{
    float   f;
    int32_t i;
  } fmin, fmax, temp, temp2;
  float decoded;
  int32_t MaxExp, Exp, Mask, Mantis, Shift, Minimum, Maximum, Src, Shift2, Round, Sgn, MaxMax, Range;
  int32_t *Source = (int32_t *) source;

// step 1 : get max and min
  limit = MinMax(source, n, &fmax.f, &fmin.f); // get max and min as well as the "half way" point

// step 2 : dissect max and min to determine compression parameters
  MaxExp = (fmax.i >> 23) & 0xFF;         /* extract IEEE float 32 exponent of max */
  Exp    = (fmin.i >> 23) & 0xFF;         /* extract IEEE float 32 exponent of min */
  MaxExp = MaxExp > Exp ? MaxExp : Exp;   /* MaxExp is largest of the two exponents, used for normalization */

  Src    = fmax.i;                           /* dissect Maximum value */
  Mantis = (1 << 23) | ( 0x7FFFFF & Src );   /* get IEEE mantissa, restore hidden 1 */
  Exp    = (fmax.i >> 23) & 0xFF;            /* extract IEEE float 32 exponent */
  Shift  = MaxExp - Exp;                     /* normalize mantissa to largest exponent */
  if (Shift > 31) Shift = 31;
  Mantis = Mantis >> Shift;
  if( Src >> 31 ) Mantis = - Mantis;
  Maximum= Mantis;
  if (Exp < 1) Maximum = 0;                  /* denormalized number */

  Src    = fmin.i;                           /* dissect Minimum value */
  Mantis = (1 << 23) | ( 0x7FFFFF & Src );
  Exp    = (fmin.i >> 23) & 0xFF;
  Shift  = MaxExp - Exp;
  if (Shift > 31) Shift = 31;
  Mantis = Mantis >> Shift;
  if( Src >> 31 ) Mantis = - Mantis;
  Minimum= Mantis;
  if (Exp < 1) Minimum = 0;                  /* denormalized number */
  
  Shift2 = 0;
  Round  = 1;                            /* rounding quantity */
  Mask   = ~( -1 << nbits);              /* right mask of nbits bits */
  Range = Maximum - Minimum;             /* largest integer left after subtracting minimum mantissa */
  while ( Range > Mask ) {               /* Maximum must fit within *nbits bits */
    Round = Round << 1;
    Shift2++;
    Minimum = (Minimum >> Shift2) << Shift2;   /* get rid of low order bits (rounded minimum) */
    Range   = (Maximum - Minimum) >> Shift2;
    }

  Round = Round >> 1;                       /* this bit ends up vis a vis last bit shifted out */
  Header[1] = Minimum;                      /* store minimum, maxexp, shift2, npts into header (64 bits) */
//  header[0] = header[0] | ((MaxExp & 0xFF) << 8) | (Shift2 & 0xFF);

// step 3 : encode then decode max value to see if decoded value > original max
  Src = fmax.i;                           /* encode maximum value */
  Mantis = (1 << 23) | ( 0x7FFFFF & Src );
  Exp    = (Src >> 23) & 0xFF;
  Shift  = MaxExp - Exp;
  if (Shift > 31) Shift = 31;
  Mantis = Mantis >> Shift;
  if( Src >> 31 ) Mantis = - Mantis;
  Mantis = Mantis - Minimum;              /* subtract minimum from mantissa */
  Mantis = Mantis + Round;                /* add rounding term */
  Mantis = Mantis >> Shift2;              /* force to fit within nbits bits */
  if (Mantis > Mask) Mantis = Mask;
  MaxMax=Mantis;

  Mantis = Mantis << Shift2;                         /* decode maximum value*/
  Mantis = Mantis + Minimum;                         /* regenerate mantissa, possibly not normalized */
  Sgn = (Mantis >> 31) & 1;
  if(Sgn) Mantis =- Mantis;                          /* need absolute value of Mantis */
  if (Mantis > 0xFFFFFF) Mantis = 0xFFFFFF;
  temp.i = (Mantis & (~(-1<<23))) | (MaxExp << 23);  /* eliminate bit 23 (hidden 1) and add exponent */
  temp.i = temp.i | (Sgn << 31);                     /* add sign in proper position */
  if(Mantis & (1<<23)) {
    decoded = temp.f;                                /* hidden 1 is genuine */
  }else{
    temp2.i= MaxExp << 23;                           /* subtract this bogus hidden 1 */
    temp2.i = temp2.i | (Sgn << 31);                 /* add sign in proper position */
    temp2.i = temp2.i & ( ~( (Mantis << 8) >> 31 ) );/* non zero only if hidden 1 is not present */
    decoded = temp.f - temp2.f;                      /* hidden 1 was not present, subtract it */
    }
  if(decoded > fmax.f) {       /* make sure decoded max will be <= actual max value */
    if(MaxMax > 127) Mask=MaxMax-1;   /* but refuse to damage max error by more than 1%  */
  }

// step 4 : perform actual compression (vectorized by VBLOCK32)
  __m256i mask, minimum, maxexp, m123, m31, shift2, shuf;
  int offset = n - limit;
  minimum = _mm256_set1_epi32(Round - Minimum) ;  // -minimum + round in one shot later on
  maxexp  = _mm256_set1_epi32(MaxExp) ;
  mask    = _mm256_set1_epi32(Mask) ;
  m31     = _mm256_set1_epi32(31) ;
  shift2  = _mm256_set1_epi32(Shift2) ;
  m123    = _mm256_set1_epi32(1) ;
  m123    = _mm256_slli_epi32(m123,23) ;      // hidden 1
  shuf    = _mm256_loadu_si256((const __m256i *)&shufp[0]);
  for (i = 0 ; i < limit ; i += VBLOCK32){
    __m256i mantis, exp, shift, sign;
    __m256i mantis1, exp1, shift1, sign1;
    __m128i t128;
    mantis = _mm256_loadu_si256((const __m256i *)&Source[i]);
    mantis1= _mm256_loadu_si256((const __m256i *)&Source[i+offset]);
    sign   = _mm256_srai_epi32(mantis,31) ;      // 0 if positive, -1 if negative
    exp    = _mm256_slli_epi32(mantis,1) ;       // get rid of upper 1 and lower 23 bits
    exp    = _mm256_srli_epi32(exp,24) ;
    sign1  = _mm256_srai_epi32(mantis1,31) ;      // 0 if positive, -1 if negative
    exp1   = _mm256_slli_epi32(mantis1,1) ;       // get rid of upper 1 and lower 23 bits
    exp1   = _mm256_srli_epi32(exp1,24) ;
    mantis = _mm256_slli_epi32(mantis,9) ;       // get rid of upper 9 bits
    mantis = _mm256_srli_epi32(mantis,9) ;
    mantis = _mm256_or_si256(mantis,m123) ;      // add hidden 1
    shift  = _mm256_sub_epi32(maxexp,exp) ;      // maxexp - exp
    shift  = _mm256_min_epi32(shift,m31) ;       // min(shift,31)
    mantis1= _mm256_slli_epi32(mantis1,9) ;       // get rid of upper 9 bits
    mantis1= _mm256_srli_epi32(mantis1,9) ;
    mantis1= _mm256_or_si256(mantis1,m123) ;      // add hidden 1
    shift1 = _mm256_sub_epi32(maxexp,exp1) ;      // maxexp - exp
    shift1 = _mm256_min_epi32(shift1,m31) ;       // min(shift,31)
    mantis = _mm256_srlv_epi32(mantis,shift) ;   // mantis >> shift
    mantis = _mm256_xor_si256(mantis,sign) ;     // if(sign == -1) mantis = -mantis (2's complement arithmetic)
    mantis = _mm256_sub_epi32(mantis,sign) ;     // (mantis XOR sign) + sign (mantis if sign 0, -mantis if sign -1)
    mantis = _mm256_add_epi32(mantis,minimum) ;  // - Minimum + Round
    mantis = _mm256_srlv_epi32(mantis,shift2) ;  // mantis >> Shift2
    mantis = _mm256_min_epi32(mantis,mask) ;     // min(mantis,mask)
    mantis = _mm256_shuffle_epi8 (mantis, shuf); // compress upper and lower 128 bits into their lower 64 bit part
    mantis = _mm256_permute4x64_epi64(mantis, 0x8);    //  00 00 10 00 binary 
    mantis1= _mm256_srlv_epi32(mantis1,shift1) ;   // mantis >> shift
    mantis1= _mm256_xor_si256(mantis1,sign1) ;     // if(sign == -1) mantis= -mantis (2's complement arithmetic)
    mantis1= _mm256_sub_epi32(mantis1,sign1) ;     // (mantis XOR sign) + sign (mantis if sign 0, -mantis if sign -1)
    mantis1= _mm256_add_epi32(mantis1,minimum) ;  // - Minimum + Round
    mantis1= _mm256_srlv_epi32(mantis1,shift2) ;  // mantis >> Shift2
    mantis1= _mm256_min_epi32(mantis1,mask) ;     // min(mantis,mask)
    mantis1= _mm256_shuffle_epi8 (mantis1, shuf); // compress upper and lower 128 bits into their lower 64 bit part
    mantis1= _mm256_permute4x64_epi64(mantis1, 0x8);    //  00 00 10 00 binary 

    t128   = _mm256_extracti128_si256 (mantis, 0);
    _mm_storeu_si128 ((__m128i*) &stream[i], t128);

    t128   = _mm256_extracti128_si256 (mantis1, 0);
    _mm_storeu_si128 ((__m128i*) &stream[i+offset], t128);
  }

  return(limit);
}

static unsigned char s_32_8[32]  = { 3, 2, 1, 0, 7, 6, 5, 4, 11,10, 9, 8,15,14,13,12,
                                     3, 2, 1, 0, 7, 6, 5, 4, 11,10, 9, 8,15,14,13,12};
static unsigned char s_32_16[32] = { 2, 3, 0, 1, 6, 7, 4, 5, 10,11, 8, 9,14,15,12,13,
                                     2, 3, 0, 1, 6, 7, 4, 5, 10,11, 8, 9,14,15,12,13};
static unsigned int  s_64_32[8]  = { 1, 0, 3, 2, 5, 4, 7, 6} ;

void avx_swap_64_32(uint64_t *src,int n) {  // endian shuffle, 8 bits within 32
  int i0;
  uint64_t t;
#ifdef __AVX2__
  __m256i shuf;
#endif

  i0=0 ;
#ifdef __AVX2__
  if(Cpu_has_feature(FLAG_AVX2)) {
    shuf  = _mm256_lddqu_si256((__m256i const *)s_64_32);
    for( ; i0<n-VBLOCK64+1 ; i0+=VBLOCK64){
      __m256i s;
      s  = _mm256_lddqu_si256((__m256i const *)&src[i0]) ;   // load 4 64 bit longs (256 bits)
      s  = _mm256_permutevar8x32_epi32(s,shuf);                // shuffle words within long long (10 11 00 01 b)
      _mm256_storeu_si256 ((__m256i *) &src[i0], s);          // store in place
    }
  }
#endif
  while(i0<n){
    t = src[i0];
    src[i0++] = (t >> 32) | (t << 32);
  }
}

void avx_swap_32_16(uint32_t *src,int n) {  // endian shuffle, 8 bits within 32
  register int i0, limit;
  register uint32_t t;
#ifdef __AVX2__
  __m256i shuf;
#endif

  limit = (n & (~(VBLOCK32-1))) ;         //  first multiple of VBLOCK32 < n
  i0=0 ;
#ifdef __AVX2__
  if(Cpu_has_feature(FLAG_AVX2)) {
    shuf  = _mm256_lddqu_si256((__m256i const *)s_32_16);
    for( ; i0<limit ; i0+=VBLOCK32){
      __m256i s;
      s  = _mm256_lddqu_si256((__m256i const *)&src[i0]) ;   // load 8 ints (256 bits)
      s  = _mm256_shuffle_epi8 (s, shuf) ;                   // shuffle shorts within 32 bit word
      _mm256_storeu_si256 ((__m256i *) &src[i0], s);          // store in place
    }
  }
#endif
  while(i0<n){
    t = src[i0];
    src[i0++] = (t >> 16) | (t << 16);
  }
}

void avx_swap_32_8(uint32_t *src,int n) {  // endian shuffle, 8 bits within 32
  int i0, limit;
  uint32_t t;
#ifdef __AVX2__
  __m256i shuf;
#endif

  limit = (n & (~(VBLOCK32-1))) ;         //  first multiple of VBLOCK32 < n
  i0=0 ;
#ifdef __AVX2__
  if(Cpu_has_feature(FLAG_AVX2)) {
    shuf  = _mm256_lddqu_si256((__m256i const *)s_32_8);
    for( ; i0<limit ; i0+=VBLOCK32){
      __m256i s;
      s  = _mm256_lddqu_si256((__m256i const *)&src[i0]) ;   // load 8 ints (256 bits)
      s  = _mm256_shuffle_epi8 (s, shuf) ;                   // shuffle bytes within 32 bit word
      _mm256_storeu_si256 ((__m256i *) &src[i0], s);          // store in place
    }
  }
#endif
  while(i0<n){
    t = src[i0];
    src[i0++] = (t >> 24) | (t << 24) | ((t & 0xFF00) << 8) | ((t >> 8) & 0xFF00) ;
  }
}

static unsigned char shuf8[40] = {0,1,4,5,8,9,12,13,128,128,128,128,128,128,128,128,
                                  0,1,4,5,8,9,12,13,128,128,128,128,128,128,128,128,
                                  0,1,4,5,8,9,12,13};

void avx_squash_32_16(short *dst,int *src,int n) {
  int i ;
#ifdef __AVX2__
  __m256i shuf, shuf2, s, s2;
#endif
//  __m128i d ;
//  __m128i d2 ;
//  printf("avx_squash_32_16\n");
  if(Cpu_has_feature(FLAG_AVX2)) {
#ifdef __AVX2__
    shuf  = _mm256_lddqu_si256((__m256i const *)shuf8);
    shuf2 = _mm256_lddqu_si256((__m256i const *)&shuf8[8]);
    for (i=0 ; i<n ; i+=16){
      s  = _mm256_lddqu_si256((__m256i const *)&src[i ]);   // load 8 ints (256 bits)
      s2 = _mm256_lddqu_si256((__m256i const *)&src[i+8]);
      s  = _mm256_shuffle_epi8 (s, shuf);                   // compress upper and lower 128 bits into lower 64 bit part
      s2 = _mm256_shuffle_epi8 (s2, shuf2);                 // compress upper and lower 128 bits into upper 64 bit part
      s  = _mm256_or_si256(s,s2);                           // combine
      s  = _mm256_permute4x64_epi64(s, 216);                // 3 1 2 0 order for 64 bit tokens 
  //    s  = _mm256_permute4x64_epi64(s, 8);           // lower 128 bits = lower 64 bits of both 128 bits ,  upper 128 bits = don't care
  //    s2 = _mm256_permute4x64_epi64(s2, 8);
  //    d  = _mm256_extracti128_si256 (s, 0);          // transfer lower 128 bits into xmm (dummy instruction in practice)
  //    _mm_storeu_si128 ((__m128i*) &dst[i], d);      // store 8 shorts (128 bits)
  //    d2 = _mm256_extracti128_si256 (s2, 0);
  //    _mm_storeu_si128 ((__m128i*) &dst[i+8], d2);
      _mm256_storeu_si256 ((__m256i *) &dst[i], s);
    }
#endif
  } else {
    c_squash_32_16(dst,src,n);    // call plain C routine if AVX2 is not an option
  }
}

#if defined(SELF_TEST)
#include <sys/time.h>
#if ! defined(NREP)
#define NREP 1000
#endif
int main() {
  struct timeval t1,t2;
  long long T1, T2;
  int duree;
  int src[NPTS];
  short dst[NPTS];
  short dst2[NPTS];
  int i;
  int errors = 0;
  int ok = 0;
  int n;
#if defined(FULL_TEST)
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
  if(Cpu_has_feature(FLAG_AVX2)) printf("USING AVX2 instruction set\n");

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
  for (i=0 ; i<NPTS ; i++) { src[i] = (i & 1) ? 0x01020304 : 0x0A0B0C0D ; }
  n = NPTS-2;
  printf("================== avx_swap_32_8 ==================\n");
  printf("AVANT  : %8.8x  %8.8x %8.8x %8.8x %8.8x %8.8x\n",src[0],src[1],src[2],src[3],src[n],src[n+1]);
  avx_swap_32_8((uint32_t *)src,n);
  printf("APRES 1: %8.8x  %8.8x %8.8x %8.8x %8.8x %8.8x\n",src[0],src[1],src[2],src[3],src[n],src[n+1]);
  gettimeofday(&t1,NULL);
  for (i=1 ; i<NREP ; i++) avx_swap_32_8((uint32_t *)src,n);
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("duree avx_swap_32_8 = %d, Mtokens = %d\n", duree,NPTS*NREP/1000000);
  printf("APRES 2: %8.8x  %8.8x %8.8x %8.8x %8.8x %8.8x\n",src[0],src[1],src[2],src[3],src[n],src[n+1]);
  printf("================== avx_swap_32_16 ==================\n");
  printf("AVANT  : %8.8x  %8.8x %8.8x %8.8x %8.8x %8.8x\n",src[0],src[1],src[2],src[3],src[n],src[n+1]);
  avx_swap_32_16((uint32_t *)src,n);
  printf("APRES 1: %8.8x  %8.8x %8.8x %8.8x %8.8x %8.8x\n",src[0],src[1],src[2],src[3],src[n],src[n+1]);
  gettimeofday(&t1,NULL);
  for (i=1 ; i<NREP ; i++) avx_swap_32_16((uint32_t *)src,n);
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("duree avx_swap_32_16 = %d, Mtokens = %d\n", duree,NPTS*NREP/1000000);
  printf("APRES 2: %8.8x  %8.8x %8.8x %8.8x %8.8x %8.8x\n",src[0],src[1],src[2],src[3],src[n],src[n+1]);
  printf("================== avx_swap_64_32 ==================\n");
  printf("AVANT  : %8.8x  %8.8x %8.8x %8.8x %8.8x %8.8x\n",src[0],src[1],src[2],src[3],src[n],src[n+1]);
  avx_swap_64_32((uint64_t *)src,n/2);
  printf("APRES 1: %8.8x  %8.8x %8.8x %8.8x %8.8x %8.8x\n",src[0],src[1],src[2],src[3],src[n],src[n+1]);
  gettimeofday(&t1,NULL);
  for (i=1 ; i<NREP ; i++) avx_swap_64_32((uint64_t *)src,n/2);
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("duree avx_swap_64_32 = %d, Mtokens = %d\n", duree,NPTS*NREP/1000000);
  printf("APRES 2: %8.8x  %8.8x %8.8x %8.8x %8.8x %8.8x\n",src[0],src[1],src[2],src[3],src[n],src[n+1]);
  return(0);
}
#endif
