#include <stdlib.h>
#include <stdio.h>
#include <rpnmacros.h>

#ifdef __SSE2__
#include <immintrin.h>
#define VLEN 4
#endif

typedef union {
 INT_32 i;
 float f;
} floatint;

static unsigned int LittleEndianInteger = 1;
static unsigned char *IsLittleEndian = (unsigned char *)&LittleEndianInteger;  /* *IsLittleEndian == 1 on little endian machines, 0 en big endian */


/* =====================================================================================================

   this set of routines will work only if the values use the 32 BIT IEEE floating point format.
   maximum number of floating values in a record =  ( 2 Giga values )

   packed record format :

   +-------------+-----------------+-----------------+-----------------------+
   | main header | header block    |   D A T A
   +-------------+-----------------+-----------------+-----------------------+

   32 bit main header format :

     12 bits   4 bits    8 bits  8 bits
   +---------+-------+-----------------+
   | 0xEFF   |nbits-1|  MaxExp | Shift |
   +---------+-------+-----------------+

   64 bit header block format :

         32 bits             32 bits
   +--------------------+------------------+
   | Minimum value      |  nb of values    |
   +--------------------+------------------+

   data : one 16 bit token for each float value

   ===================================================================================================== */
#if defined(__SSE2__)
#define VECTOR_STRIPE 8
#define SSE_VECTOR 4
// unpack only an integral number of vector stripes (VECTOR_STRIPE) tokens
// this routine will return the number of processed tokens from its input stream 
static int float_unpacker_sse2(float *dest, void *src, int n, const int Shift2, const int Minimum_, const int MaxExp_, const int stream_32bit)
{
  unsigned int   *stream32 = (unsigned int   *)src;
  unsigned short *stream16 = (unsigned short *)src;
  int   __attribute__ ((aligned(64))) Mantis_[VECTOR_STRIPE];
  float __attribute__ ((aligned(64))) Temp_[VECTOR_STRIPE];
  __m128i Mantis0, Mantis1;
  __m128i Temp0, Temp1, Temp02, Temp12, X0, X1, Mask0, Mask1;
  __m128i Minimum, M23, Shift1_31, Xffffff, X7fffff;
  int i, i0, j0, n0, in;

  if(n<VECTOR_STRIPE) return (n);
  n0 = n & (~(VECTOR_STRIPE-1)) ;             // n - modulo(n , VECTOR_STRIPE) where VECTOR_STRIPE is a power of 2
  M23 = _mm_set1_epi32(MaxExp_ << 23);        // IEEE exponent for 32 bit float
  Shift1_31 = _mm_set1_epi32(1<<31);
  Minimum = _mm_set1_epi32(Minimum_);
  Xffffff = _mm_set1_epi32(0xffffff);
  X7fffff = _mm_set1_epi32(0x7fffff);

  if(stream_32bit){                            /* input stream contains 32 bit tokens */
    for (i0 = 0, j0 = 0 ; i0 < n0 ; i0 += VECTOR_STRIPE, j0 += SSE_VECTOR){
      in = i0 + VECTOR_STRIPE;
      in = (in > n) ? n : in;

      Mantis0 = _mm_load_si128((const __m128i *)&stream32[j0]);
      Mantis1 = Mantis0;
      X0      = _mm_xor_si128(X0,X0);
      Mantis0 = _mm_unpacklo_epi16(Mantis0,X0);
      Mantis1 = _mm_unpackhi_epi16(Mantis1,X0);
      Mantis0 = _mm_shuffle_epi32(Mantis0,0xB1);
      Mantis1 = _mm_shuffle_epi32(Mantis1,0xB1);

      Mantis0 = _mm_slli_epi32(Mantis0,Shift2);     // mantis = mantis << shift2
      Mantis1 = _mm_slli_epi32(Mantis1,Shift2);
      Mantis0 = _mm_add_epi32(Mantis0,Minimum);     // mantis = mantis + minimum
      Mantis1 = _mm_add_epi32(Mantis1,Minimum);
      Temp02  = _mm_and_si128(Mantis0,Shift1_31);   // temp2 = mantis & (1 << 31)
      Temp12  = _mm_and_si128(Mantis1,Shift1_31);

      Mask0   = _mm_srai_epi32(Mantis0,31);         // abs(mantis)
      Mask1   = _mm_srai_epi32(Mantis1,31);
      Mantis0 = _mm_add_epi32(Mantis0,Mask0);
      Mantis1 = _mm_add_epi32(Mantis1,Mask1);
      Mantis0 = _mm_xor_si128(Mantis0,Mask0);
      Mantis1 = _mm_xor_si128(Mantis1,Mask1);

      X0      = Xffffff;
      X1      = Xffffff;
      X0      = _mm_cmplt_epi32(X0,Mantis0);        // all ones if X < mantis
      X1      = _mm_cmplt_epi32(X1,Mantis1);
      Mantis0 = _mm_or_si128(Mantis0,X0);           // set mantis to all ones if > ffffff
      Mantis1 = _mm_or_si128(Mantis1,X1);
      Mantis0 = _mm_and_si128(Mantis0,Xffffff);     // keep lower 24 bits
      Mantis1 = _mm_and_si128(Mantis1,Xffffff);     // we now have min(mantis,0xFFFFFF)

      Temp02  = _mm_or_si128(Temp02,M23);           // temp2 = temp2 | m23
      Temp12  = _mm_or_si128(Temp12,M23);
      Temp0   = Temp02;                             // temp = temp2
      Temp1   = Temp12;
      Mask0   = _mm_slli_epi32(Mantis0,8);          // mask = (mantis <<8) >> 31
      Mask1   = _mm_slli_epi32(Mantis1,8);
      Mask0   = _mm_srai_epi32(Mask0,31);
      Mask1   = _mm_srai_epi32(Mask1,31);
      Mask0   = _mm_andnot_si128(Mask0,Temp02);     // mask = (~mask) & temp2
      Mask1   = _mm_andnot_si128(Mask1,Temp12);
      Mantis0 = _mm_and_si128(Mantis0,X7fffff);     // mantis = mantis & 0x7FFFFF
      Mantis1 = _mm_and_si128(Mantis1,X7fffff);
      Temp0   = _mm_or_si128(Temp0,Mantis0);        // temp = temp | mantis
      Temp1   = _mm_or_si128(Temp1,Mantis1);
      Temp0   = (__m128i)_mm_sub_ps((__m128)Temp0,(__m128)Mask0);
      Temp1   = (__m128i)_mm_sub_ps((__m128)Temp1,(__m128)Mask1);

      _mm_storeu_ps(&dest[i0  ],(__m128)Temp0);                                // store result
      _mm_storeu_ps(&dest[i0+4],(__m128)Temp1);
    }
  }else{   // incoming stream contains shorts(16bit) therefore no shuffle is needed
    for (i0 = 0, j0 = 0 ; i0 < n0 ; i0 += VECTOR_STRIPE, j0 += SSE_VECTOR){
      in = i0 + VECTOR_STRIPE;
      in = (in > n) ? n : in;

      Mantis0 = _mm_load_si128((const __m128i *)&stream32[j0]);
      Mantis1 = Mantis0;
      X0      = _mm_xor_si128(X0,X0);
      Mantis0 = _mm_unpacklo_epi16(Mantis0,X0);
      Mantis1 = _mm_unpackhi_epi16(Mantis1,X0);
//      Mantis0 = _mm_shuffle_epi32(Mantis0,0xB1);  
//      Mantis1 = _mm_shuffle_epi32(Mantis1,0xB1);

      Mantis0 = _mm_slli_epi32(Mantis0,Shift2);     // mantis = mantis << shift2
      Mantis1 = _mm_slli_epi32(Mantis1,Shift2);
      Mantis0 = _mm_add_epi32(Mantis0,Minimum);     // mantis = mantis + minimum
      Mantis1 = _mm_add_epi32(Mantis1,Minimum);
      Temp02  = _mm_and_si128(Mantis0,Shift1_31);   // temp2 = mantis & (1 << 31)
      Temp12  = _mm_and_si128(Mantis1,Shift1_31);

      Mask0   = _mm_srai_epi32(Mantis0,31);         // abs(mantis)
      Mask1   = _mm_srai_epi32(Mantis1,31);
      Mantis0 = _mm_add_epi32(Mantis0,Mask0);
      Mantis1 = _mm_add_epi32(Mantis1,Mask1);
      Mantis0 = _mm_xor_si128(Mantis0,Mask0);
      Mantis1 = _mm_xor_si128(Mantis1,Mask1);

      X0      = Xffffff;
      X1      = Xffffff;
      X0      = _mm_cmplt_epi32(X0,Mantis0);        // all ones if X < mantis
      X1      = _mm_cmplt_epi32(X1,Mantis1);
      Mantis0 = _mm_or_si128(Mantis0,X0);           // set mantis to all ones if > ffffff
      Mantis1 = _mm_or_si128(Mantis1,X1);
      Mantis0 = _mm_and_si128(Mantis0,Xffffff);     // keep lower 24 bits
      Mantis1 = _mm_and_si128(Mantis1,Xffffff);     // we now have min(mantis,0xFFFFFF)

      Temp02  = _mm_or_si128(Temp02,M23);           // temp2 = temp2 | m23
      Temp12  = _mm_or_si128(Temp12,M23);
      Temp0   = Temp02;                             // temp = temp2
      Temp1   = Temp12;
      Mask0   = _mm_slli_epi32(Mantis0,8);          // mask = (mantis <<8) >> 31
      Mask1   = _mm_slli_epi32(Mantis1,8);
      Mask0   = _mm_srai_epi32(Mask0,31);
      Mask1   = _mm_srai_epi32(Mask1,31);
      Mask0   = _mm_andnot_si128(Mask0,Temp02);     // mask = (~mask) & temp2
      Mask1   = _mm_andnot_si128(Mask1,Temp12);
      Mantis0 = _mm_and_si128(Mantis0,X7fffff);     // mantis = mantis & 0x7FFFFF
      Mantis1 = _mm_and_si128(Mantis1,X7fffff);
      Temp0   = _mm_or_si128(Temp0,Mantis0);        // temp = temp | mantis
      Temp1   = _mm_or_si128(Temp1,Mantis1);
      Temp0   = (__m128i)_mm_sub_ps((__m128)Temp0,(__m128)Mask0);
      Temp1   = (__m128i)_mm_sub_ps((__m128)Temp1,(__m128)Mask1);

      _mm_storeu_ps(&dest[i0  ],(__m128)Temp0);                                // store result
      _mm_storeu_ps(&dest[i0+4],(__m128)Temp1);
    }
  }
  return (n0) ;
}
#endif
/*
    SINGLE BLOCK floating point unpacker
    dest    : pointer to output array of floating point numbers
    header  : pointer to 64 bit header for this block
    stream  : pointer to packed stream (16 bits per token, 32 bit aligned at start)
    npts    : number of values to unpack
            : npts < 0 indicates a stream made of 16 bit tokens of type short arther than int

    return value is 0 if there is no error, the number of point discrepancy otherwise

*/

float dummy[4];
/* stream is really INT32 but addressed as INT16, one MUST account for endianness of machines */

#ifdef ORIGINAL
static INT_32 float_unpacker_new(float *dest, INT_32 *header, INT_32 *stream, INT_32 npts)
#else
static INT_32 float_unpacker_1(float *dest, INT_32 *header, INT_32 *stream, INT_32 npts)
#endif
{
  floatint Temp0,Temp1,Temp2,Temp3,temp0,temp1,temp2,temp3;
  INT_32 n, shft1_23, shft1_31, m23, mask24;
  INT_32 MaxExp, Mantis, Mantis0, Mantis1, Mantis2, Mantis3, Sgn, Sgn0, Sgn1, Sgn2, Sgn3, Minimum, Shift2, i0;
  int Mask0, Mask1, Mask2, Mask3;
  INT_32 StReAm[3];
  int n0;
  int stream_32bit = 0;   /* true only if little endian and stream data is 32 bit int */

  if((*IsLittleEndian == 1) && (npts > 0)) stream_32bit = 1;  /* stream data is made of 32 bit tpkens (ints) */
  n=npts;
  if(npts<0) n = -npts;
  Minimum = header[1];                     /* get Minimum, MaxExp, Shift2 from header */
  MaxExp = (header[0] >> 8) & 0xFF;
  Shift2 = header[0] & 0xFF;
  if (n != header[2]) {     /* verify that the number of points is consistent with header */
    printf("float_unpacker_1: ERROR inconsistent number of points, expected %d, got %d\n",n,header[2]);
    return n - header[2];   /* return discrepancy */
    }

  shft1_23 = 1 << 23;
  shft1_31 = 1 << 31;
  m23 = MaxExp << 23 ;
  mask24 = 0xFFFFFF;
  if (MaxExp == 0) {
    while (n--) *dest++ = 0.0;
    return (0);
    }

#if defined(__SSE2__)
  n0 = float_unpacker_sse2(dest, stream, n, Shift2, Minimum, MaxExp, stream_32bit);
  if(n == n0) return(0);
  n = n - n0;
  stream += n0/2;
  dest += n0;
#endif
  while(n>3){
    Mantis0 = *stream++ ;
    Mantis2 = *stream++ ;

    if(stream_32bit) {   /* endian swap of mantis 0/1 and 2/3  */
      Mantis1 = Mantis0 & 0xFFFF ;
      Mantis3 = Mantis2 & 0xFFFF ;
      Mantis0 >>= 16;
      Mantis2 >>= 16;
    }else{
      Mantis1 = Mantis0 >> 16;
      Mantis3 = Mantis2 >> 16;
      Mantis1 = Mantis1 & 0xFFFF ;
      Mantis3 = Mantis3 & 0xFFFF ;
    }
    Mantis0 = Mantis0 & 0xFFFF ;
    Mantis2 = Mantis2 & 0xFFFF ;

    Mantis0 = Mantis0 << Shift2;
    Mantis1 = Mantis1 << Shift2;
    Mantis2 = Mantis2 << Shift2;
    Mantis3 = Mantis3 << Shift2;
    Mantis0 = Mantis0 + Minimum;                         /* regenerate mantissa, possibly not normalized */
    Mantis1 = Mantis1 + Minimum;                         /* regenerate mantissa, possibly not normalized */
    Mantis2 = Mantis2 + Minimum;                         /* regenerate mantissa, possibly not normalized */
    Mantis3 = Mantis3 + Minimum;                         /* regenerate mantissa, possibly not normalized */
    Sgn0 = (Mantis0 & shft1_31) | m23;
    Sgn1 = (Mantis1 & shft1_31) | m23;
    Sgn2 = (Mantis2 & shft1_31) | m23;
    Sgn3 = (Mantis3 & shft1_31) | m23;
    if(Mantis0 < 0) { Mantis0 =- Mantis0; }                         /* need absolute value of Mantis */
    if(Mantis1 < 0) { Mantis1 =- Mantis1; }                         /* need absolute value of Mantis */
    if(Mantis2 < 0) { Mantis2 =- Mantis2; }                         /* need absolute value of Mantis */
    if(Mantis3 < 0) { Mantis3 =- Mantis3; }                         /* need absolute value of Mantis */
    if(Mantis0 > 0xFFFFFF) Mantis0 = 0xFFFFFF;
    if(Mantis1 > 0xFFFFFF) Mantis1 = 0xFFFFFF;
    if(Mantis2 > 0xFFFFFF) Mantis2 = 0xFFFFFF;
    if(Mantis3 > 0xFFFFFF) Mantis3 = 0xFFFFFF;
    temp0.i = (Mantis0 & shft1_23) ? 0 : Sgn0 ;
    Temp0.i = (Mantis0 & 0x7FFFFF) | Sgn0;  /* eliminate bit 23 (hidden 1) and add exponent and sign */
    *dest++ = Temp0.f - temp0.f;            /* hidden 1 was not present, subtract it */
    temp1.i = (Mantis1 & shft1_23) ? 0 : Sgn1 ;
    Temp1.i = (Mantis1 & 0x7FFFFF) | Sgn1;  /* eliminate bit 23 (hidden 1) and add exponent and sign */
    *dest++ = Temp1.f - temp1.f;            /* hidden 1 was not present, subtract it */
    temp2.i = (Mantis2 & shft1_23) ? 0 : Sgn2 ;
    Temp2.i = (Mantis2 & 0x7FFFFF) | Sgn2;  /* eliminate bit 23 (hidden 1) and add exponent and sign */
    *dest++ = Temp2.f - temp2.f;            /* hidden 1 was not present, subtract it */
    temp3.i = (Mantis3 & shft1_23) ? 0 : Sgn3 ;
    Temp3.i = (Mantis3 & 0x7FFFFF) | Sgn3;  /* eliminate bit 23 (hidden 1) and add exponent and sign */
    *dest++ = Temp3.f - temp3.f;            /* hidden 1 was not present, subtract it */
    n -=4;
  }
  i0 = 0;
  Mantis0 = *stream++ ;
  if(n>2) Mantis2 = *stream++ ;
  if(stream_32bit){
    StReAm[1] = Mantis0 & 0xFFFF ;
    Mantis0 >>= 16;
    Mantis2 >>= 16;
  }else{
    StReAm[1] = Mantis0 >> 16;
    StReAm[1] = StReAm[1] & 0xFFFF ;
  }
    StReAm[0] = Mantis0 & 0xFFFF ;
    StReAm[2] = Mantis2 & 0xFFFF ;
  while(n-->0){
    Mantis = StReAm[i0++] ;
    Mantis = Mantis << Shift2;
    Mantis = Mantis + Minimum;                         /* regenerate mantissa, possibly not normalized */
    Sgn = (Mantis & shft1_31) | m23;
    if(Mantis < 0) { Mantis =- Mantis; }               /* need absolute value of Mantis */
    if(Mantis > 0xFFFFFF) Mantis = 0xFFFFFF;
    temp0.i = (Mantis & shft1_23) ? 0 : Sgn ;
    Temp0.i = (Mantis & 0x7FFFFF) | Sgn;       /* eliminate bit 23 (hidden 1) and add exponent and sign */
    *dest++ = Temp0.f - temp0.f;               /* if hidden 1 was not present, subtract it */
  }
  return 0;
}

#ifdef TEST_PACK
static INT_32 float_unpacker_1_orig(float *dest, INT_32 *header, INT_32 *stream, INT_32 npts)
{
  floatint temp,temp2;
  INT_32 n;
  INT_32 MaxExp, Mantis, Sgn, Minimum, Shift2, Fetch, Accu;

  Minimum = header[1];                     /* get Minimum, MaxExp, Shift2 from header */
  MaxExp = (header[0] >> 8) & 0xFF;
  Shift2 = header[0] & 0xFF;
  if (npts != header[2]) {     /* verify that the number of points is consistent with header */
    printf("float_unpacker_1: ERROR inconsistent number of points\n");
    return npts - header[2];   /* return discrepancy */
    }

  n=npts;
  if (MaxExp == 0) {
    while (n--) *dest++ = 0.0;
    return (0);
    }
  Accu = *stream++;                                    /* get first 32 bit token from stream */
  Fetch = 0;
  while(n--){
    Mantis = (Accu >> 16) & 0xFFFF;                    /* get upper 16 bits of token */
    Mantis = Mantis << Shift2;
    Mantis = Mantis + Minimum;                         /* regenerate mantissa, possibly not normalized */
    Sgn = (Mantis >> 31) & 1;
    if(Sgn) Mantis =- Mantis;                          /* need absolute value of Mantis */
    if (Mantis > 0xFFFFFF) Mantis = 0xFFFFFF;
    temp.i = (Mantis & (~(-1<<23))) | (MaxExp << 23);  /* eliminate bit 23 (hidden 1) and add exponent */
    temp.i = temp.i | (Sgn << 31);                     /* add sign in proper position */
    if(Mantis & (1<<23)) {
      *dest++ = temp.f;                                /* hidden 1 is genuine */
    }else{
      temp2.i= MaxExp << 23;                           /* subtract this bogus hidden 1 */
      temp2.i = temp2.i | (Sgn << 31);                 /* add sign in proper position */
      temp2.i = temp2.i & ( ~( (Mantis << 8) >> 31 ) );/* non zero only if hidden 1 is not present */
      *dest++ = temp.f - temp2.f;                      /* hidden 1 was not present, subtract it */
      }
    Accu = Accu << 16;                                 /* token must be in upper part of 32 bit word */
    if(Fetch) Accu = *stream++;                        /* new 32 bit word every other trip in loop */
    Fetch = Fetch ^ 1;                                 /* toggle Fetch */
    }
  return 0;
}
#ifdef ORIGINAL
static INT_32 float_unpacker_1(float *dest, INT_32 *header, INT_32 *stream, INT_32 npts)
{
  return float_unpacker_1_orig(dest, header, stream, npts);
}
#endif

static INT_32 c_float_unpacker_orig(float *dest, INT_32 *header, void *stream, INT_32 npts_in, INT_32 *nbits);
static ftnword f77name(float_unpacker_orig)(float *dest, INT_32 *header, INT_32 *stream, INT_32 *npts, INT_32 *nbits)
{
  return c_float_unpacker_orig(dest, header, stream, *npts, nbits);
}
#endif

/* =====================================================================================================
    SINGLE BLOCK floating point packer
    source  : pointer to input array of floating point numbers
    nbits   : number of useful bits in token
    header  : pointer to 64 bit header for this block
    stream  : pointer to packed stream (16 bits per token, 32 bit aligned at start)
    npts    : number of values to unpack  ( max 32768)
            : npts < 0 indicates a stream made of 16 bit tokens of type short arther than int

    return value is 0 if there is no error, the number of point discrepancy otherwise

   ===================================================================================================== */

static INT_32 float_packer_1(float *source, INT_32 nbits, INT_32 *header, INT_32 *stream, INT_32 npts)
{
  float *z=source;
  INT_32 *intsrc= (INT_32 *)source;
  floatint fmin,fmax,temp,temp2;
  float decoded;
  INT_32 n;  // npts < 0 if outgoing stream is 16 bit tokens (shorts)
  INT_32 MaxExp, Exp, Mask, Mantis, Shift, Minimum, Maximum, Src, Shift2, Store, Accu, Round, Sgn, MaxMax;
  int stream_32bit = 0;

#if defined(__SSE2__)
  __m128i maxexp;
  __m128i shift1_23 = _mm_set1_epi32(1 << 23);
  __m128i xFF = _mm_set1_epi32(0xFF);
  __m128i x7FFFFF = _mm_set1_epi32(0x7FFFFF);
  __m128i x1F = _mm_set1_epi32(0x1F);
  __m128i minimum;
  __m128i round;
  __m128i mask;
#endif

  n = (npts > 0) ? npts : -npts;
  if((*IsLittleEndian == 1) && (npts < 0)) stream_32bit=1;

#if defined(__FUTURE_SSE2__)
  {
    float tmin[4], tmax[4];     // not much of a gain so far, wait for avx-2 (longer vectors)
    __m128 ssemin, ssemax, z03, z47;
    ssemin = _mm_loadu_ps(z);
    ssemax = ssemin;
    while(n > 7){
      z03 = _mm_loadu_ps(z);
      z47 = _mm_loadu_ps(z+4);
      z = z + 8;
      n = n - 8;
      ssemin = _mm_min_ps(ssemin,z03);
      ssemax = _mm_max_ps(ssemax,z03);
      ssemin = _mm_min_ps(ssemin,z47);
      ssemax = _mm_max_ps(ssemax,z47);
    }
    _mm_storeu_ps(tmin,ssemin);
    _mm_storeu_ps(tmax,ssemax);
    fmin.f = tmin[0];
    fmax.f = tmax[0];
    fmin.f = fmin.f > tmin[1] ? tmin[1] : fmin.f;
    fmax.f = fmax.f < tmax[1] ? tmax[1] : fmax.f;
    fmin.f = fmin.f > tmin[2] ? tmin[2] : fmin.f;
    fmax.f = fmax.f < tmax[2] ? tmax[2] : fmax.f;
    fmin.f = fmin.f > tmin[3] ? tmin[3] : fmin.f;
    fmax.f = fmax.f < tmax[3] ? tmax[3] : fmax.f;
    while(n--){
      fmin.f = fmin.f > *z ? *z : fmin.f;
      fmax.f = fmax.f < *z ? *z : fmax.f;
      z++;
    }
  }
#else
  fmin.f = *z;
  fmax.f = *z;
  while(n--){                            /* get min and max value of field */
    fmin.f = fmin.f > *z ? *z : fmin.f;
    fmax.f = fmax.f < *z ? *z : fmax.f;
    z++;
    }
#endif
  MaxExp = (fmax.i >> 23) & 0xFF;         /* extract IEEE float 32 exponent */
  Exp    = (fmin.i >> 23) & 0xFF;         /* extract IEEE float 32 exponent */
  MaxExp = MaxExp > Exp ? MaxExp : Exp;   /* MaxExp is largest of the two exponents, used for normalization */

  Src    = fmax.i;                           /* dissect Maximum value */
  Mantis = (1 << 23) | ( 0x7FFFFF & Src );   /* get IEEE mantissa, restore hidden 1 */
  Exp    = (fmax.i >> 23) & 0xFF;            /* extract IEEE float 32 exponent */
  Shift  = MaxExp - Exp;                     /* normalize mantissa to largest exponent */
  if (Shift > 31) Shift = 31;
  Mantis = Mantis >> Shift;
  if( Src >> 31 ) Mantis = - Mantis;
  Maximum= Mantis;
  if (Exp < 1) Maximum = 0;

  Src    = fmin.i;                           /* dissect Minimum value */
  Mantis = (1 << 23) | ( 0x7FFFFF & Src );
  Exp    = (fmin.i >> 23) & 0xFF;
  Shift  = MaxExp - Exp;
  if (Shift > 31) Shift = 31;
  Mantis = Mantis >> Shift;
  if( Src >> 31 ) Mantis = - Mantis;
  Minimum= Mantis;
  if (Exp < 1) Minimum = 0;

  Maximum = Maximum - Minimum;              /* largest integer left after subtracting minimum mantissa */
  Shift2 = 0;
  Round  = 1;                               /* rounding quantity */
  Mask   = ~( -1 << nbits);                /* right mask of nbits bits */
  while ( Maximum > Mask ) {               /* Maximum must fit within *nbits bits */
    Maximum = Maximum >> 1;
    Round = Round << 1;
    Shift2++;
    }
  Round = Round >> 1;                       /* this bit ends up vis a vis last bit shifted out */
  header[1] = Minimum;                      /* store minimum, maxexp, shift2, npts into header (64 bits) */
  header[0] = header[0] | ((MaxExp & 0xFF) << 8) | (Shift2 & 0xFF);
  /* encode then decode max value to see if decoded value > original max */
  Src = fmax.i;                           /* encode */
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
  Mantis = Mantis << Shift2;                           /* decode */
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
    /*   fprintf(stderr,"decoded max > max \n");   */
    if(MaxMax > 127) Mask=MaxMax-1;   /* do not damage max error by more thatn 1%  */
  }

/* fprintf(stderr,"Debug+ MaxExp=%d\n",MaxExp);
  fprintf(stderr,"Debug+ min=%f fmin.i=%X max=%f Minimum=%d Maximum=%d\n",fmin.f,fmin.i,fmax.f,Minimum,Maximum); */
  n = (npts > 0) ? npts : -npts;
#ifndef ORIGINAL
#if defined(__SSE2__)
  maxexp = _mm_set1_epi32(MaxExp);
  minimum = _mm_set1_epi32(Minimum);
  round = _mm_set1_epi32(Round);
  mask = _mm_set1_epi32(Mask);
#endif
  while(n > 3){                               /* transform input floating point into 16 bit integers (chunks of 2 values) */
#if defined(__FUTURE_SSE2__)   // will need avx-2 instruction set to really work
    {
      __m128i mantis;
      __m128i exp;
      __m128i shift;
      __m128i sign;
      mantis = _mm_load_si128((const __m128i *)intsrc);
      intsrc = intsrc + 4;
      exp = mantis;
      exp = _mm_srli_epi32(exp,23);
      sign = mantis;
      sign = _mm_srai_epi32(sign,31);
      mantis = _mm_and_si128(mantis,x7FFFFF);
      mantis = _mm_or_si128(mantis,shift1_23);
      exp = _mm_and_si128(exp,xFF);
      shift = maxexp;
      shift = _mm_sub_epi32(shift,exp);
      shift = _mm_min_epi16(shift,x1F);    // shift = min(shift,31) (works because number < 16 bits)
      mantis = _mm_srl_epi32(mantis,shift);  // wrong instruction , need avx2 _mm[256]_srlv_epi32
      exp = _mm_xor_si128(exp,exp);
      exp = _mm_sub_epi32(exp,mantis);      // -mantis
      exp = _mm_and_si128(exp,sign);        // -mantis where sign is -1, 0 elsewhwere
      sign = _mm_andnot_si128(sign,mantis); // mantis where sign is 0, 0 elsewhwere
      sign = _mm_or_si128(sign,exp);        // -mantis where sign is -1, mantis where sign is 0
      sign = _mm_sub_epi32(sign,minimum);   // -minimum
      sign = _mm_add_epi32(sign,round);     // + round
      mantis = sign;   // should really be min(mantis,Mask)
      mantis = _mm_shufflelo_epi16(mantis,0xB1);   // 2,3,0,1 order
      _mm_storel_pi((__m64 *)stream,(__m128)mantis);
      stream = stream + 2;
    }
#else
    Src = *intsrc++;
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

    Accu = Mantis ;

    Src = *intsrc++;
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

    if(stream_32bit){
      *stream++ = (Mantis << 16) | Accu;             /* store the 2 tokens in 16 bit mode on little endian machine*/
    }else{
      *stream++ = (Accu << 16) | Mantis;             /* store the 2 tokens */
    }

    Src = *intsrc++;
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

    Accu = Mantis ;

    Src = *intsrc++;
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

    if(stream_32bit){
      *stream++ = (Mantis << 16) | Accu;             /* store the 2 tokens in 16 bit mode on little endian machine*/
    }else{
      *stream++ = (Accu << 16) | Mantis;             /* store the 2 tokens */
    }
#endif
    n = n - 4;
  }
#endif
  Store = 0;
  Accu = 0;
  while(n--){                               /* transform input floating point into 16 bit integers (remainder) */
    Src = *intsrc++;
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
//    Accu   = (Accu << 16) | Mantis;         /* insert into stream as 16 bit token */
//    if(Store) *stream++ = Accu;             /* store every other trip in the loop */
    if(Store) {
      if(stream_32bit){
        *stream++ = (Mantis << 16) | Accu;             /* store the 2 tokens in 16 bit mode on little endian machine*/
      }else{
        *stream++ = (Accu << 16) | Mantis;             /* store the 2 tokens */
      }
    }
    Store = Store ^ 1;
    Accu = Mantis;
    }
  if(Store) {        /* must store last ? (odd number of trips in loop) */
      if(stream_32bit){
        *stream++ = (Accu) ;                     /* store the last token in 16 bit mode on little endian machine*/
      }else{
        *stream++ = (Accu << 16)   ;             /* store the last token */
      }
  }
  return 0;
}

/* =====================================================================================================
    floating point unpacker (works by making multiple calls to the single block unpacker)
    dest    : pointer to output array of floating point numbers
    header  : pointer to 64 bit header for this block
    stream  : pointer to packed stream (16 bits per token, 32 bit aligned at start)
    npts    : pointer to number of values to unpack
    nbits   : pointer to number of useful bits in token (output)

    pointers are used where values could have been to make this routine FORTRAN callable

    subroutine float_unpacker(VALUES,HEADER,STREAM,NPTS,NBITS)
    integer *4 NPTS, HEADER(2), STREAM(NPTS/2, NBITS)
    real *4 VALUES(NPTS)
    return value is zero if OK, error code from float_unpacker_1 otherwise
   ===================================================================================================== */

INT_32 c_float_unpacker(float *dest, INT_32 *header, void *stream, INT_32 npts_in, INT_32 *nbits)
{
  INT_32 ierror;
  int npts;

  npts = (npts_in > 0) ? npts_in : -npts_in ;
  *nbits = ( (header[0]>>16) & 0xF) + 1 ;
  if(0xEFF != ( (header[0]>>20) & 0xFFF)) {
    printf("float_unpacker: ERROR invalid header \n");
    return -1;
    }
  if(npts != header[2]) {
    printf("float_unpacker: ERROR inconsistent number of points (header/request mismatch)\n");
    return -1;
    }
  ierror = float_unpacker_1(dest, header, stream, npts_in);
  if(ierror) return ierror;
  return 0;
}

static INT_32 c_float_unpacker_orig(float *dest, INT_32 *header, void *stream, INT_32 npts_in, INT_32 *nbits)
{
  INT_32 ierror;
  int npts;

  npts = (npts_in > 0) ? npts_in : -npts_in ;
  *nbits = ( (header[0]>>16) & 0xF) + 1 ;
  if(0xEFF != ( (header[0]>>20) & 0xFFF)) {
    printf("float_unpacker: ERROR invalid header \n");
    return -1;
    }
  if(npts != header[2]) {
    printf("float_unpacker: ERROR inconsistent number of points (header/request mismatch)\n");
    return -1;
    }
  ierror = float_unpacker_1_orig(dest, header, stream, npts);
  if(ierror) return ierror;
  return 0;
}

ftnword f77name(float_unpacker)(float *dest, INT_32 *header, INT_32 *stream, INT_32 *npts, INT_32 *nbits)
{
  return c_float_unpacker(dest, header, stream, *npts, nbits);
}

/* =====================================================================================================
    floating point packer (works by making multiple calls to the single block packer)
    source  : pointer to input array of floating point numbers
    nbits   : pointer to number of useful bits in token
    header  : pointer to 64 bit header for this block
    stream  : pointer to packed stream (16 bits per token, 32 bit aligned at start)
    npts    : pointer to number of values to unpack

    pointers are used where values could have been to make this routine FORTRAN callable

    integer function float_packer(VALUES,NBITS,HEADER,STREAM,NPTS)
    integer *4 NPTS, HEADER(2), NBITS, STREAM(NPTS/2)
    real *4 VALUES(NPTS)
    return value is 0 if there was no error, -1 if error occurred
   ===================================================================================================== */

INT_32 c_float_packer(float *source, INT_32 nbits, INT_32 *header, INT_32 *stream, INT_32 npts)
{

  if(nbits > 16 || nbits < 1) {
    printf("float_unpacker: ERROR nbits must be > 0 and <= 16 ,nbits = %d\n",nbits);
    return -1;
    }
  header[2] = (npts > 0) ? npts : -npts;      /* number of values */
  header[0] = ( 0xEFF << 20 );
  header[0] = header[0] | ( ( nbits - 1 ) << 16 );
//  fprintf(stderr,"c_float_packer: npts=%d\n",header[2]);
  if( float_packer_1(source, nbits, header, stream, npts) ) return -1;  /* return -1 on error */
  return  0 ;   /* return 0 if no error */
}
ftnword f77name(float_packer)(float *source, INT_32 *nbits, INT_32 *header, INT_32 *stream, INT_32 *npts)
{
//  fprintf(stderr,"PACKER: npts=%d\n",*npts);
  return c_float_packer(source,*nbits,header,stream,*npts);
}

/* =====================================================================================================
   get lengths of various elements of packed data
   header_size  : pointer to size of header part
   stream_size  : pointer to size of stream part
   npts         : pointer to number of values
   p1,p2        : reserved for future expansion, a value of zero is returned now

   subroutine float_packer_params(HEADER_SIZE,STREAM_SIZE,P1,P2,NPTS)
   integer *4 NPTS,HEADER_SIZE,STREAM_SIZE,P1,P2

  ===================================================================================================== */
void c_float_packer_params(INT_32 *header_size, INT_32 *stream_size, INT_32 *p1, INT_32 *p2, INT_32 npts_in)
{
  int npts;

  npts = (npts_in > 0) ? npts_in : -npts_in ;
  *header_size = 3;
  *header_size = *header_size * sizeof(INT_32);
  *stream_size = (npts + 1) / 2 ;           /* size used for stream, 1 INT_32 per 2 values */
  *stream_size = *stream_size * sizeof(INT_32);
  *p1 = 0;
  *p2 = 0;
}
void f77name(float_packer_params)(INT_32 *header_size, INT_32 *stream_size, INT_32 *p1, INT_32 *p2, INT_32 *npts)
{
  c_float_packer_params(header_size,stream_size,p1,p2,*npts);
}

/*====================================   TEST PROGRAM   =================================================*/

#ifdef TEST_PACK
#include <sys/time.h>
/* test program to verify that results are identical on all machines */
#define NPTS (3+800*600)
int main()
{
  struct timeval t1,t2;
  long long T1, T2;
  int duree;

  float source[NPTS];
  float source2[NPTS];
  double error,errormax,errorabs,erroravg;
  INT_32 nbits=14;
  INT_32 NBITS;
#ifdef ORIGINAL
  INT_32 npts2 = (NPTS);
#else
  INT_32 npts2 = (-NPTS);
#endif
  INT_32 npts;
  unsigned int header[1+2*((NPTS+32767)/32768)], stream[(NPTS+1)/2];
  unsigned int header2[1+2*((NPTS+32767)/32768)], stream2[(NPTS+1)/2];
  INT_32 signature;
  int i,j;
  INT_32 p1,p2,header_size,stream_size;

  npts = npts2;

  f77name(float_packer_params)(&header_size, &stream_size, &p1, &p2, &npts);
  printf("header_size,stream_size=%d,%d\n",header_size,stream_size);

  for ( i=0 ; i<NPTS ; i++ ) { source[i]=i*1.234-1123.123; };
  printf("source[0],source[1],source[NPTS-2],source[NPTS-1]=%f,%f,%f,%f\n",source[0],source[1],source[NPTS-2],source[NPTS-1]);

  gettimeofday(&t1,NULL);
  f77name(float_packer)(source, &nbits, (INT_32 *)header, (INT_32 *)stream, &npts);
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("packing time = %d usec, %dMtok/s\n",duree,NPTS/duree);
  signature=0;
//  for ( i=0 ; i< (1+2*((NPTS+32767)/32768))  ; i++ ) {
  for ( i=0 ; i< 4 ; i++){
    signature=signature^header[i];
    printf(" %8.8x",header[i]);
    }
  printf("\nafter packing header signature = %8.8x\n",signature);
  signature=0;
  for ( i=0 ; i<  ((NPTS+1)/2) ; i++ ) signature=signature^stream[i];
  printf("after packing stream signature = %8.8x\n",signature);

  npts = NPTS;

  gettimeofday(&t1,NULL);
  f77name(float_packer)(source, &nbits, (INT_32 *)header2, (INT_32 *)stream2, &npts);
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("packing time = %d usec, %dMtok/s\n",duree,NPTS/duree);
  signature=0;
//  for ( i=0 ; i< (1+2*((NPTS+32767)/32768))  ; i++ ) {
  for ( i=0 ; i< 4 ; i++){
    signature=signature^header2[i];
    printf(" %8.8x",header2[i]);
    }
  printf("\nafter packing header signature 2 = %8.8x\n",signature);
  signature=0;
  for ( i=0 ; i<  ((NPTS+1)/2) ; i++ ) signature=signature^stream2[i];
  printf("after packing stream signature 2 = %8.8x\n",signature);

#ifndef ORIGINAL
#ifdef FULL
  for ( i=0 ; i<NPTS ; i++ ) { source2[i]=-2000.; };
  gettimeofday(&t1,NULL);
  f77name(float_unpacker_orig)(source2, (INT_32 *)header2, (INT_32 *)stream2, &npts, &NBITS);
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("ORIGINAL unpacking time = %d usec, %dMtok/s, npts=%d\n",duree,NPTS/duree,npts);
  printf("source2[0],source2[1],source2[NPTS-2],source2[NPTS-1]=%f,%f,%f,%f\n",source2[0],source2[1],source2[NPTS-2],source2[NPTS-1]);

  errormax=0;
  errorabs=0;
  erroravg=0;
  for ( i=0 ; i<NPTS ; i++ ) {
    error = source2[i]-source[i];
    erroravg=erroravg+error;
    if(error<0) error=-error ;
    errorabs=errorabs+error ;
    errormax=error>errormax?error:errormax;
    }
  printf("after OLD unpacking errormax=%f,erroravg=%f, errorabs avg=%f\n",errormax,erroravg/NPTS,errorabs/NPTS);

#endif
#endif

  npts = npts2;

  for ( i=0 ; i<NPTS ; i++ ) { source2[i]=-2000.; };
  gettimeofday(&t1,NULL);
  f77name(float_unpacker)(source2, (INT_32 *)header, (INT_32 *)stream, &npts, &NBITS);
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("NEW unpacking time = %d usec, %dMtok/s, npts=%d\n",duree,NPTS/duree,npts);
  printf("source2[0],source2[1],source2[NPTS-2],source2[NPTS-1]=%f,%f,%f,%f\n",source2[0],source2[1],source2[NPTS-2],source2[NPTS-1]);

  errormax=0;
  errorabs=0;
  erroravg=0;
  for ( i=0 ; i<NPTS ; i++ ) {
    error = source2[i]-source[i];
    erroravg=erroravg+error;
    if(error<0) error=-error ;
    errorabs=errorabs+error ;
    errormax=error>errormax?error:errormax;
    }
  printf("after unpacking errormax=%f,erroravg=%f, errorabs avg=%f\n",errormax,erroravg/NPTS,errorabs/NPTS);

  for ( i=0 ; i<NPTS ; i++ ) { source2[i]=-2000.; };
  gettimeofday(&t1,NULL);
  f77name(float_unpacker)(source2, (INT_32 *)header, (INT_32 *)stream, &npts, &NBITS);
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("unpacking time = %d usec, %dMtok/s\n",duree,NPTS/duree);

  printf("source2[0],source2[1],source2[NPTS-2],source2[NPTS-1]=%f,%f,%f,%f\n",source2[0],source2[1],source2[NPTS-2],source2[NPTS-1]);
  printf("nbits = %d ,nbits from unpacker = %d\n",nbits,NBITS);

  errormax=0;
  errorabs=0;
  erroravg=0;
  for ( i=0 ; i<NPTS ; i++ ) {
    error = source2[i]-source[i];
    erroravg=erroravg+error;
    if(error<0) error=-error ;
//    if(error > 33.0) {
//      printf("ERROR: at point %d, expected %f, got %f\n",i,source[i],source2[i]);
//      exit(1);
//    }
    errorabs=errorabs+error ;
    errormax=error>errormax?error:errormax;
    }
  printf("after packing errormax=%f,erroravg=%f, errorabs avg=%f\n",errormax,erroravg/NPTS,errorabs/NPTS);

  for ( i=0 ; i<NPTS ; i++ ) { source[i]=source2[i] ; }

  for ( j=0 ; j< 9 ; j++ ) {     /* perform repacking-unpacking cycles to verify stability */
    gettimeofday(&t1,NULL);
    f77name(float_packer)(source2, &nbits, (INT_32 *)header, (INT_32 *)stream, &npts);
    gettimeofday(&t2,NULL);
    T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
    T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
    duree = T2-T1;
    printf("packing time = %d usec, %dMtok/s\n",duree,NPTS/duree);
/*
    for ( i=0 ; i<NPTS ; i++ ) source2[i]=0;
*/
#ifndef ORIGINAL
#ifdef FULL
    gettimeofday(&t1,NULL);
    f77name(float_unpacker_orig)(source2, (INT_32 *)header, (INT_32 *)stream, &npts, &NBITS);
    gettimeofday(&t2,NULL);
    T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
    T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
    duree = T2-T1;
    printf("unpacking orig time = %d usec, %dMtok/s\n",duree,NPTS/duree);
#endif
#endif
    gettimeofday(&t1,NULL);
    f77name(float_unpacker)(source2, (INT_32 *)header, (INT_32 *)stream, &npts, &NBITS);
    gettimeofday(&t2,NULL);
    T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
    T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
    duree = T2-T1;
    printf("unpacking NEW  time = %d usec, %dMtok/s\n",duree,NPTS/duree);
    }

  errormax=0;
  erroravg=0;
  for ( i=0 ; i<NPTS ; i++ ) {
    error = source2[i]-source[i];
    erroravg=erroravg+error;
    if(error<0) error=-error ;
    errormax=error>errormax?error:errormax;
    }
  printf("after REpacking errormax=%f,erroravg=%f\n",errormax,erroravg/NPTS);  /* better be zero */
  return (0);
}
#endif

