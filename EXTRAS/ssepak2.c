#include <immintrin.h>
#define VECTOR 8
int float_unpacker_sse2(float *dest, void *src, int n, const int Shift2, const int Minimum_, const int MaxExp_)
{
  unsigned int   *stream32 = (unsigned int   *)src;
  unsigned short *stream16 = (unsigned short *)src;
  int   __attribute__ ((aligned(64))) Mantis_[VECTOR];
  float __attribute__ ((aligned(64))) Temp_[VECTOR];
  __m128i Mantis0;
  __m128i Temp0, Temp02, X0, Mask0;
  __m128i Minimum, M23, Shift1_31, Xffffff, X7fffff;
  int i, i0, in, m23, shift1_31, xffffff, x7fffff;

  m23 = MaxExp_ << 23;                        // IEEE exponent for 32 bit float
  M23 = _mm_set1_epi32(m23);
  shift1_31 = 1 << 31;
  Shift1_31 = _mm_set1_epi32(shift1_31);
  Minimum = _mm_set1_epi32(Minimum_);
  xffffff = 0xFFFFFF;
  Xffffff = _mm_set1_epi32(xffffff);
  x7fffff = 0x7FFFFF;
  X7fffff = _mm_set1_epi32(x7fffff);

  for (i0 = 0 ; i0 < n ; i0 += VECTOR){
    in = i0 + VECTOR;
    in = (in > n) ? n : in;

    for (i = 0 ; i+i0 < in ; i++){          // load Mantis
      Mantis_[i  ] = stream16[i+i0] ;        // little endian swap as SSE means X86
    }

    Mantis0 = (__m128i)_mm_load_ps((const float *)&Mantis_[0]);   // load mantissa

    Mantis0 = _mm_slli_epi32(Mantis0,Shift2);     // mantis = mantis << 32
    Mantis0 = _mm_add_epi32(Minimum,Mantis0);     // mantis = mantis + minimum
    Temp02  = _mm_and_si128(Shift1_31,Mantis0);   // temp2 = mantis & (1 << 31)

    Mask0   = Mantis0;
    Mask0   = _mm_srai_epi32(Mask0,31);           // abs(mantis)
    Mantis0 = _mm_add_epi32(Mask0,Mantis0);
    Mantis0 = _mm_xor_si128(Mask0,Mantis0);

    X0      = Xffffff;                            // min(mantis,0xFFFFFF)
    X0      = _mm_sub_epi32(X0,Mantis0);
    Mask0   = X0;
    Mask0   = _mm_srai_epi32(Mask0,31);
    X0      = _mm_and_si128(X0,Mask0);
    Mantis0 = _mm_add_epi32(Mantis0,X0);

    Temp02  = _mm_or_si128(Temp02,M23);           // temp2 = temp2 | m23
    Temp0   = Temp02;                             // temp = temp2
    Mask0   = Mantis0;
    Mask0   = _mm_slli_epi32(Mask0,8);          // mask = (mantis <<8) >> 31
    Mask0   = _mm_srai_epi32(Mask0,31);
    Mask0   = _mm_andnot_si128(Mask0,Temp02);     // mask = (~mask) & temp2
    Mantis0 = _mm_and_si128(Mantis0,X7fffff);     // mantis = mantis & 0x7FFFFF
    Temp0   = _mm_or_si128(Temp0,Mantis0);        // temp = temp | mantis
    Temp0   = (__m128i)_mm_sub_ps((__m128)Temp0,(__m128)Mask0);

    _mm_store_ps(&Temp_[0],(__m128)Temp0);                                // store result

    for (i = 0 ; i+i0 < in ; i++){             //store result
     dest[i+i0] = Temp_[i] ;
    }
  }
  return (0) ;
}
int float_unpacker_sse2a(float *dest, void *src, int n, const int Shift2, const int Minimum_, const int MaxExp_)
{
  unsigned int   *stream32 = (unsigned int   *)src;
  unsigned short *stream16 = (unsigned short *)src;
  int   __attribute__ ((aligned(64))) Mantis_[VECTOR];
  float __attribute__ ((aligned(64))) Temp_[VECTOR];
  int i, i0, in;
  unsigned int m23, shift1_31, xffffff, x7fffff, Minimum;

  m23 = MaxExp_ << 23;                        // IEEE exponent for 32 bit float
  shift1_31 = 1 << 31;
  Minimum = Minimum_;
  xffffff = 0xFFFFFF;
  x7fffff = 0x7FFFFF;
  __asm__ __volatile__(
        "movd      (%0), %%xmm0 \n\t"    // m23
        "pshufd     $0,%%xmm0,%%xmm0\n\t"
        "movd      (%1), %%xmm1 \n\t"    // shift1_31
        "pshufd     $0,%%xmm1,%%xmm1\n\t"
        "movd      (%2), %%xmm2 \n\t"    // Minimum
        "pshufd     $0,%%xmm2,%%xmm2\n\t"
        "movd      (%3), %%xmm3 \n\t"    // 0xFFFFFF
        "pshufd     $0,%%xmm3,%%xmm3\n\t"
        "movd      (%4), %%xmm3 \n\t"    // 0x7FFFFF
        "pshufd     $0,%%xmm4,%%xmm4\n\t"
        "movd      (%5), %%xmm7 \n\t"    // Shift2
        : /* outputs */
        : /* inputs */ "r" (m23),"r" (shift1_31),"r" (Minimum),"r" (xffffff),"r" (x7fffff),"r" (Shift2)
        : /* clobbered */ "xmm0","xmm1","xmm2","xmm3","xmm4","xmm7" );

  for (i0 = 0 ; i0 < n ; i0 += VECTOR){
    in = i0 + VECTOR;
    in = (in > n) ? n : in;

    for (i = 0 ; i+i0 < in ; i++){           // load Mantis
      Mantis_[i  ] = stream16[i+i0] ;        // little endian swap as SSE means X86
    }
  __asm__ __volatile__(
        "movdqa      (%1), %%xmm5 \n\t"      // load Mantis_[0:3]
        "movdqa      16(%1), %%xmm6 \n\t"    // load Mantis_[4:7]

        "pslld       %%xmm7, %%xmm5 \n\t"    // mantis = mantis << shitf2
        "pslld       %%xmm7, %%xmm6 \n\t"
        "paddd       %%xmm2, %%xmm5 \n\t"    // mantis = mantis + minimum
        "paddd       %%xmm2, %%xmm6 \n\t"
        "movdqa      %%xmm5, %%xmm10 \n\t"   // temp2 = mantis
        "movdqa      %%xmm6, %%xmm11 \n\t"
        "pand        %%xmm1, %%xmm10 \n\t"    // temp2 = temp2 & (1 << 31)
        "pand        %%xmm1, %%xmm11 \n\t"

        "movdqa      %%xmm5, %%xmm12 \n\t"   // mask = mantis
        "movdqa      %%xmm6, %%xmm13 \n\t"
        "psrad       $31, %%xmm12 \n\t"      // abs(mantis)     mask = mask >>a 31
        "psrad       $31, %%xmm13 \n\t"
        "paddd       %%xmm12, %%xmm5 \n\t"
        "paddd       %%xmm13, %%xmm6 \n\t"
        "pxor        %%xmm12, %%xmm5 \n\t"
        "pxor        %%xmm13, %%xmm6 \n\t"

        "movdqa      %%xmm3, %%xmm8 \n\t"    // min(mantis,0xFFFFFF)  x0 = ffffff
        "movdqa      %%xmm3, %%xmm9 \n\t" 
        "psubd       %%xmm5, %%xmm8 \n\t"    //                       xo = x0 - mantis
        "psubd       %%xmm6, %%xmm9 \n\t"
        "movdqa      %%xmm8, %%xmm12 \n\t"   //                       mask = x0
        "movdqa      %%xmm9, %%xmm13 \n\t"
        "psrad       $31, %%xmm12 \n\t"      //                       mask = mask >>a 31
        "psrad       $31, %%xmm13 \n\t"
        "pand        %%xmm12, %%xmm8 \n\t"   //                       x0 = x0 & mask
        "pand        %%xmm13, %%xmm9 \n\t"
        "paddd       %%xmm8, %%xmm5 \n\t"    //                       mantis = mantis + x0
        "paddd       %%xmm9, %%xmm6 \n\t"

        "por         %%xmm0, %%xmm10 \n\t"   // temp2 = temp2 | m23
        "por         %%xmm0, %%xmm11 \n\t"
        "movdqa      %%xmm10, %%xmm14 \n\t"  // temp = temp2
        "movdqa      %%xmm11, %%xmm15 \n\t"
        "movdqa      %%xmm5, %%xmm12 \n\t"   // mask = mantis
        "movdqa      %%xmm6, %%xmm13 \n\t"
        "pslld       $8, %%xmm12 \n\t"       // mask = mask << 8
        "pslld       $8, %%xmm13 \n\t"
        "psrad       $31, %%xmm12 \n\t"      // mask = mask >>a 31
        "psrad       $31, %%xmm13 \n\t"
        "pandn       %%xmm10, %%xmm12 \n\t"  // mask = (~mask) & temp2  (new value for temp2)
        "pandn       %%xmm11, %%xmm13 \n\t"
        "pand        %%xmm3, %%xmm5 \n\t"    // mantis = mantis & 0x7FFFFF
        "pand        %%xmm3, %%xmm6 \n\t"
        "por         %%xmm5, %%xmm14 \n\t"   // temp = temp | mantis
        "por         %%xmm6, %%xmm15 \n\t"
        "subps       %%xmm12, %%xmm14 \n\t"  // temp = temp - mask
        "subps       %%xmm13, %%xmm15 \n\t"

        "movaps      %%xmm14, (%0) \n\t"     // store Temp_[0:3]
        "movaps      %%xmm15, 16(%0) \n\t"   // store Temp_[4:7]
        : /* outputs */ "=m" (Temp_), "=m" (Mantis_)
        : /* inputs */ "r" (Shift2)
        : /* clobbered */ "xmm0","xmm1","xmm2","xmm3","xmm4","xmm5","xmm6","xmm7","xmm8","xmm9","xmm10","xmm11","xmm12","xmm13","xmm14","xmm15" );
  
#ifdef NOT_USED
    Mantis0 = (__m128i)_mm_load_ps((const float *)&Mantis_[0]);   // load mantissa

    Mantis0 = _mm_slli_epi32(Mantis0,Shift2);     // mantis = mantis << 32
    Mantis0 = _mm_add_epi32(Minimum,Mantis0);     // mantis = mantis + minimum
    Temp02  = _mm_and_si128(Shift1_31,Mantis0);   // temp2 = mantis & (1 << 31)

    Mask0   = Mantis0;
    Mask0   = _mm_srai_epi32(Mask0,31);           // abs(mantis)
    Mantis0 = _mm_add_epi32(Mask0,Mantis0);
    Mantis0 = _mm_xor_si128(Mask0,Mantis0);

    X0      = Xffffff;                            // min(mantis,0xFFFFFF)
    X0      = _mm_sub_epi32(X0,Mantis0);
    Mask0   = X0;
    Mask0   = _mm_srai_epi32(Mask0,31);
    X0      = _mm_and_si128(X0,Mask0);
    Mantis0 = _mm_add_epi32(Mantis0,X0);

    Temp02  = _mm_or_si128(Temp02,M23);           // temp2 = temp2 | m23
    Temp0   = Temp02;                             // temp = temp2
    Mask0   = Mantis0;
    Mask0   = _mm_slli_epi32(Mask0,8);          // mask = (mantis <<8) >> 31
    Mask0   = _mm_srai_epi32(Mask0,31);
    Mask0   = _mm_andnot_si128(Mask0,Temp02);     // mask = (~mask) & temp2
    Mantis0 = _mm_and_si128(Mantis0,X7fffff);     // mantis = mantis & 0x7FFFFF
    Temp0   = _mm_or_si128(Temp0,Mantis0);        // temp = temp | mantis
    Temp0   = (__m128i)_mm_sub_ps((__m128)Temp0,(__m128)Mask0);

    _mm_store_ps(&Temp_[0],(__m128)Temp0);                                // store result
#endif
    for (i = 0 ; i+i0 < in ; i++){             //store result
     dest[i+i0] = Temp_[i] ;
    }
  }
  return (0) ;
}
