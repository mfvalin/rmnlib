#include <stdlib.h>
#include <stdio.h>
#ifdef __AVX2__
#include <immintrin.h>
#endif

#ifdef __AVX2__
void MinMaxSumsC(float *z_in, int n, float *Max, float *Min, float *Sum, float *Sum2)
#else
void MinMaxSums(float *z_in, int n, float *Max, float *Min, float *Sum, float *Sum2)
#endif
{
  int i;
  float tmax, tmin;
  double tsum, tsum2;

  tmax = z_in[0];
  tmin = z_in[0];
  tsum = 0.0;
  tsum2 = 0.0;

  for (i=0 ; i<n ; i++){
    tmax = (tmax > z_in[i]) ? tmax : z_in[i] ;
    tmin = (tmin < z_in[i]) ? tmin : z_in[i] ;
    tsum = tsum + z_in[i] ;
    tsum2 = tsum2 + z_in[i]*z_in[i] ;
  }

  *Max  = tmax;
  *Min  = tmin;
  *Sum  = tsum;
  *Sum2 = tsum2;
}

#ifdef __AVX2__
void MinMaxC(float *z_in, int n, float *Max, float *Min)
#else
void MinMax(float *z_in, int n, float *Max, float *Min)
#endif
{
  int i;
  float tmax, tmin;

  tmax = z_in[0];
  tmin = z_in[0];

  for (i=0 ; i<n ; i++){
    tmax = (tmax > z_in[i]) ? tmax : z_in[i] ;
    tmin = (tmin < z_in[i]) ? tmin : z_in[i] ;
  }

  *Max  = tmax;
  *Min  = tmin;
}

#ifdef __AVX2__
void MinMaxIndexC(float *z_in, int n, float *Max, float *Min, int *Imax, int *Imin)
#else
void MinMaxIndex(float *z_in, int n, float *Max, float *Min, int *Imax, int *Imin)
#endif
{
  int i;
  float tmax, tmin;
  int imin, imax;

  tmax = z_in[0];
  tmin = z_in[0];
  imax = 0;
  imin = 0;

  for (i=1 ; i<n ; i++){
    if(z_in[i] > tmax) { tmax = z_in[i] ; imax = i ; };
    if(z_in[i] < tmin) { tmin = z_in[i] ; imin = i ; };
  }

  *Max  = tmax;
  *Min  = tmin;
  *Imax = imax;
  *Imin = imin;
}

#ifdef __AVX2__
void MinMaxIndexSumsC(float *z_in, int n, float *Max, float *Min, int *Imax, int *Imin, float *Sum, float *Sum2)
#else
void MinMaxIndexSums(float *z_in, int n, float *Max, float *Min, int *Imax, int *Imin, float *Sum, float *Sum2)
#endif
{
  int i;
  float tmax, tmin;
  double tsum, tsum2;
  int imin, imax;

  tmax = z_in[0];
  tmin = z_in[0];
  imax = 0;
  imin = 0;
  tsum = 0.0;
  tsum2 = 0.0;

  for (i=1 ; i<n ; i++){
    if(z_in[i] > tmax) { tmax = z_in[i] ; imax = i ; };
    if(z_in[i] < tmin) { tmin = z_in[i] ; imin = i ; };
    tsum = tsum + z_in[i] ;
    tsum2 = tsum2 + z_in[i]*z_in[i] ;
  }

  *Max  = tmax;
  *Min  = tmin;
  *Imax = imax;
  *Imin = imin;
  *Sum  = tsum;
  *Sum2 = tsum2;
}

#ifdef __AVX2__
void MinMaxSums(float *z_in, int n, float *Max, float *Min, float *Sum, float *Sum2)
{
  float xmin[8], xmax[8];
  double ysum[4], ysum2[4];
  float smin, smax ;
  double ssum, ssum2;
  float *z = z_in;
  __m128 lo128, hi128;
  __m256  x0, x1, xxmin, xxmax;
  __m256d y0, y1, y2, y3, yysum, yysum2;
  int i, i0, i1, j;
  int block = 8;
  int limit ;
  int nminus ;

  nminus = (n / (2*block)) * (2*block);  // multiple of block * 2
  i1 = nminus/2;                         // multiple of block
  limit = i1 - block + 1;

  if(n > 2*block){
    xxmin  = _mm256_loadu_ps(&z[ 0]);
    xxmax  = xxmin;
    yysum  = _mm256_set1_pd(0.0D);
    yysum2 = _mm256_set1_pd(0.0D);

    for (i0 = 0 ; i0 < limit ; i0 += block, i1 += block){

      x0 = _mm256_loadu_ps(&z[i0]);   // stream from beginning
      x1 = _mm256_loadu_ps(&z[i1]);   // stream from half way point

      lo128 = _mm256_extractf128_ps(x0,0);  // convert 16 floats to 16 doubles
      hi128 = _mm256_extractf128_ps(x0,1);
      xxmax = _mm256_max_ps(xxmax,x0);
      xxmin = _mm256_min_ps(xxmin,x0);
      y0    = _mm256_cvtps_pd(lo128);
      y1    = _mm256_cvtps_pd(hi128);

      lo128 = _mm256_extractf128_ps(x1,0);
      hi128 = _mm256_extractf128_ps(x1,1);
      xxmax = _mm256_max_ps(xxmax,x1);
      xxmin = _mm256_min_ps(xxmin,x1);
      y2    = _mm256_cvtps_pd(lo128);
      y3    = _mm256_cvtps_pd(hi128);

      yysum = _mm256_add_pd(yysum,y0);    // double precision sum
      y0 = _mm256_mul_pd(y0,y0);          // square of value
      yysum2 = _mm256_add_pd(yysum2,y0);  // sum of squares

      yysum = _mm256_add_pd(yysum,y1);
      y1 = _mm256_mul_pd(y1,y1);
      yysum2 = _mm256_add_pd(yysum2,y1);

      yysum = _mm256_add_pd(yysum,y2);
      y2 = _mm256_mul_pd(y2,y2);
      yysum2 = _mm256_add_pd(yysum2,y2);

      yysum = _mm256_add_pd(yysum,y3);
      y3 = _mm256_mul_pd(y3,y3);
      yysum2 = _mm256_add_pd(yysum2,y3);

    }
    _mm256_storeu_ps(xmin,xxmin);
    _mm256_storeu_ps(xmax,xxmax);
    _mm256_storeu_pd(ysum,yysum);
    _mm256_storeu_pd(ysum2,yysum2);
    smax = xmax[0];
    smin = xmin[0];
    for (j=1 ; j<8 ; j++){
      smax = (smax > xmax[j]) ? smax : xmax[j] ;
      smin = (smin < xmin[j]) ? smin : xmin[j] ;
    }
    ssum = ysum[0] + ysum[1] + ysum[2] + ysum[3] ;
    ssum2 = ysum2[0] + ysum2[1] + ysum2[2] + ysum2[3] ;
  }else{   // n is less than 2 * blocks
    i1 = 0;
    smax = z_in[0] ;
    smin = z_in[0] ;
    ssum = 0.0;
    ssum2 = 0.0;
  }

  for(i = i1 ; i < n ; i++){
    smax = (smax > z_in[i]) ? smax : z_in[i] ;
    smin = (smin < z_in[i]) ? smin : z_in[i] ;
    ssum = ssum + z_in[i] ;
    ssum2 = ssum2 + z_in[i]*z_in[i] ;
  }
  *Max=smax;
  *Min=smin;
  *Sum=ssum;
  *Sum2=ssum2;
  return;
}
void MinMax(float *z_in, int n, float *Max, float *Min)  // universal version, can take advantage of AVX2 if present
{
  float smin, smax ;
  int i, j;
  int i0, i1;
#ifdef __AVX2__
  float *z = z_in;
  float zmin[8], zmax[8];
  __m256 z0, z1, zzmin0, zzmax0, zzmin1, zzmax1;
  int block = 8;
  int limit ;              //         = n - block + 1;
  int nminus ;
#endif
  if(n >= block){   // and avx2 is available  Cpu_has_feature(AVX2), needs #include cpu_type.h
#ifdef __AVX2__
//    nminus = (n / (2*block)) * (2*block);  // multiple of block * 2
//    i1 = nminus/2;                         // multiple of block
//    limit = i1 - block + 1;
    nminus = ((n+1)>>2);                // ceiling(n/2)
    limit = ((nminus+7)>>3)<<3 ;         // first multiple of 8 >= nminus

    zzmin0 = _mm256_loadu_ps(&z[0]);
    zzmax0 = zzmin0;
    zzmin1 = zzmin0;
    zzmax1 = zzmax0;

    for (i0 = 0, i1 = n - limit ; i0 < limit ; i0 += block, i1 += block){    //    i0 = 0, limit-1, 8  ; i1 = i1, n-1, 8

      z0 = _mm256_loadu_ps(&z[i0]);   // stream from beginning
      z1 = _mm256_loadu_ps(&z[i1]);   // stream from "half way point"

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

  for (i=i1 ; i<n ; i++){
    smax = (smax > z_in[i]) ? smax : z_in[i] ;
    smin = (smin < z_in[i]) ? smin : z_in[i] ;
  }
  *Max=smax;
  *Min=smin;
  return;
}
void MinMaxIndex(float *z_in, int n, float *Max, float *Min, int *Imax, int *Imin)
{
  float zmin[8], zmax[8];
  int   imin[8], imax[8];
  int   in07[8] = { 0, 1, 2, 3, 4, 5, 6, 7};
  float smin, smax ;
  int   xmin, xmax ;
  __m256 z0, z1, zzmin, zzmax;
  __m256i index0, index1, ixmax, ixmin, c8, msmin, msmax;
  int i, i0, i1, j;
  const int block = 8;
  int limit ;
  int nminus ;

  if(n < 2 * block) {
    smax = z_in[0];
    smin = smax;
    xmin = 0;
    xmax = 0;
    i1 = 0;
  }else{   // n is more than 2 blocks
    nminus = (n / (2*block)) * (2*block);  // multiple of block * 2
    i1 = nminus/2;                         // multiple of block
    limit = i1 - block + 1;

    zzmin  = _mm256_loadu_ps(&z_in[0]);
    zzmax  = zzmin;
    index0 = _mm256_loadu_si256((__m256i const *)&in07[0]);
    index1 = _mm256_set1_epi32(i1);
    index1 = _mm256_add_epi32(index1,index0);
    ixmin  = index0;
    ixmax  = index0;
    c8     = _mm256_set1_epi32(8);

    for (i0 = 0 ; i0 < limit ; i0 += block, i1 += block){
      z0 = _mm256_loadu_ps(&z_in[i0]);
      z1 = _mm256_loadu_ps(&z_in[i1]);

      msmin = (__m256i)_mm256_cmp_ps(zzmin,z0,_CMP_GT_OS);
      msmax = (__m256i)_mm256_cmp_ps(zzmax,z0,_CMP_LT_OS);
      zzmin = _mm256_min_ps(zzmin,z0);
      zzmax = _mm256_max_ps(zzmax,z0);
      ixmin = _mm256_blendv_epi8(ixmin,index0,msmin);
      ixmax = _mm256_blendv_epi8(ixmax,index0,msmax);
      index0 = _mm256_add_epi32(index0,c8);    // bump index

      msmin = (__m256i)_mm256_cmp_ps(zzmin,z1,_CMP_GT_OS);
      msmax = (__m256i)_mm256_cmp_ps(zzmax,z1,_CMP_LT_OS);
      zzmin = _mm256_min_ps(zzmin,z1);
      zzmax = _mm256_max_ps(zzmax,z1);
      ixmin = _mm256_blendv_epi8(ixmin,index1,msmin);
      ixmax = _mm256_blendv_epi8(ixmax,index1,msmax);
      index1 = _mm256_add_epi32(index1,c8);    // bump index
    }
    _mm256_storeu_ps(zmin,zzmin);
    _mm256_storeu_ps(zmax,zzmax);
    _mm256_storeu_si256((__m256i *)&imin[0],ixmin);
    _mm256_storeu_si256((__m256i *)&imax[0],ixmax);

    smax = zmax[0];
    xmax = 0;
    smin = zmin[0];
    xmin = 0;

    for (j=1 ; j<block ; j++) {  
      if(smax < zmax[j]) {smax = zmax[j] ; xmax = j ; }
      if(smin > zmin[j]) {smin = zmin[j] ; xmin = j ; }
    }
    xmax = imax[xmax];
    xmin = imin[xmin];
  }

  for(i = i1 ; i < n ; i++ ){
    if(smax < z_in[i]) { smax = z_in[i] ; xmax = i ; };
    if(smin > z_in[i]) { smin = z_in[i] ; xmin = i ; };
  }
  *Max  = smax;
  *Min  = smin;
  *Imax = xmax;
  *Imin = xmin;
  return;
}

void MinMaxIndexSums(float *z_in, int n, float *Max, float *Min, int *Imax, int *Imin, float *Sum1, float *Sum2)
{
  float zmin[8], zmax[8];
  int   imin[8], imax[8];
  double ysum[4], ysum2[4];
  int   in07[8] = { 0, 1, 2, 3, 4, 5, 6, 7};
  float smin, smax;
  double sum1, sum2 ;
  int   xmin, xmax ;
  __m256 z0, zzmin, zzmax;
  __m128 lo128, hi128;
  __m256i index0, ixmax, ixmin, c8, msmin, msmax;
  __m256d lo128d, hi128d, sumd, sumsqr;
  int i, j;
  const int block = 8;
  const int limit = n - block + 1;

  if(n < block) {
    smax = z_in[0];
    smin = smax;
    sum1 = smax;
    sum2 = sum1 * sum1;
    xmin = 0;
    xmax = 0;
    i = 1;
  }else{   // more than one block
    zzmin  = _mm256_loadu_ps(&z_in[0]);
    zzmax  = zzmin;
    index0  = _mm256_loadu_si256((__m256i const *)&in07[0]);
    ixmin  = index0;
    ixmax  = index0;
    c8     = _mm256_set1_epi32(8);
    sumd   = _mm256_set1_pd(0.0D);   // sums starts at 0.0
    sumsqr = _mm256_set1_pd(0.0D);
    sum1 = 0;
    sum2 = 0;

    for (i = 0 ; i < limit ; i += block){
      z0    = _mm256_loadu_ps(&z_in[i]);
      lo128 = _mm256_extractf128_ps(z0,0);
      hi128 = _mm256_extractf128_ps(z0,1);
      msmin = (__m256i)_mm256_cmp_ps(zzmin,z0,_CMP_GT_OS);
      msmax = (__m256i)_mm256_cmp_ps(zzmax,z0,_CMP_LT_OS);
      lo128d= _mm256_cvtps_pd(lo128);
      hi128d= _mm256_cvtps_pd(hi128);

      sumd  = _mm256_add_pd(sumd,lo128d);
      lo128d= _mm256_mul_pd(lo128d,lo128d);
      zzmin = _mm256_min_ps(zzmin,z0);
      sumd  = _mm256_add_pd(sumd,hi128d);
      hi128d= _mm256_mul_pd(hi128d,hi128d);
      zzmax = _mm256_max_ps(zzmax,z0);
      ixmin = _mm256_blendv_epi8(ixmin,index0,msmin);
      ixmax = _mm256_blendv_epi8(ixmax,index0,msmax);
      sumsqr= _mm256_add_pd(sumsqr,lo128d);
      sumsqr= _mm256_add_pd(sumsqr,hi128d);

      index0 = _mm256_add_epi32(index0,c8);    // bump index
    }
    _mm256_storeu_ps(zmin,zzmin);
    _mm256_storeu_ps(zmax,zzmax);
    _mm256_storeu_pd(ysum,sumd);
    _mm256_storeu_pd(ysum2,sumsqr);
    _mm256_storeu_si256((__m256i *)&imin[0],ixmin);
    _mm256_storeu_si256((__m256i *)&imax[0],ixmax);

    smax = zmax[0];
    xmax = 0;
    smin = zmin[0];
    xmin = 0;
    sum1 = ysum[0]  + ysum[1]  + ysum[2]  + ysum[3] ;
    sum2 = ysum2[0] + ysum2[1] + ysum2[2] + ysum2[3] ;

    for (j=1 ; j<block ; j++) {  
      if(smax < zmax[j]) {smax = zmax[j] ; xmax = j ; }
      if(smin > zmin[j]) {smin = zmin[j] ; xmin = j ; }
    }
    xmax = imax[xmax];
    xmin = imin[xmin];
  }

  for( ; i < n ; i++ ){
    if(smax < z_in[i]) { smax = z_in[i] ; xmax = i ; };
    if(smin > z_in[i]) { smin = z_in[i] ; xmin = i ; };
    sum1 = sum1 + z_in[i];
    sum2 = sum2 + z_in[i] * z_in[i];
  }
  *Max  = smax;
  *Min  = smin;
  *Imax = xmax;
  *Imin = xmin;
  *Sum1 = sum1;
  *Sum2 = sum2;
  return;
}
#endif
#define NPTS (200000*1024+5)
#include <sys/time.h>
main()
{
  struct timeval t1,t2;
  long long T1, T2;
  int duree;
  float Z[NPTS];
  float Max, Min, Sum, Sum2;
  double Div;
  int Imax, Imin;
  int i, j;

  Div = NPTS;
  gettimeofday(&t1,NULL);
  for(i=0 ; i<NPTS; i++) Z[i] = (((i-0.5*Div)/Div)*4.0*((i-0.5*Div)/Div)) ;
  Z[0] = .5 ; Z[3] = .5 ; Z[5] = .5 ; Z[7] = 0;
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("NPTS=%d, z[0]=%f,z[NPTS/2]=%f,z[NPTS-1]=%f\n",NPTS,Z[0],Z[NPTS/2],Z[NPTS-1]);
  printf("Initialization time = %d usec, %dMtok/s\n",duree,NPTS/duree);

#ifdef BENCH
  MinMaxSums(Z, NPTS, &Max, &Min, &Sum, &Sum2);
  MinMaxSums(Z, NPTS, &Max, &Min, &Sum, &Sum2);
  MinMaxSums(Z, NPTS, &Max, &Min, &Sum, &Sum2);
  MinMaxSums(Z, NPTS, &Max, &Min, &Sum, &Sum2);
  MinMaxSums(Z, NPTS, &Max, &Min, &Sum, &Sum2);
  MinMaxSums(Z, NPTS, &Max, &Min, &Sum, &Sum2);
#endif

  printf("=================== C Timings ===================\n\n");

  gettimeofday(&t1,NULL);
#ifdef __AVX2__
  MinMaxSumsC(Z, NPTS, &Max, &Min, &Sum, &Sum2);
#else
  MinMaxSums(Z, NPTS, &Max, &Min, &Sum, &Sum2);
#endif
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("MinMaxSumsC time = %d usec, %dMtok/s\n",duree,NPTS/duree);
  printf("Min=%f, Max=%f, Sum=%f, Sum2=%f \n",Min,Max,Sum,Sum2);

  gettimeofday(&t1,NULL);
#ifdef __AVX2__
  MinMaxC(Z, NPTS, &Max, &Min);
#else
  MinMax(Z, NPTS, &Max, &Min);
#endif
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("MinMaxC time = %d usec, %dMtok/s\n",duree,NPTS/duree);
  printf("Min=%f, Max=%f \n",Min,Max);

  gettimeofday(&t1,NULL);
#ifdef __AVX2__
  MinMaxIndexC(Z, NPTS, &Max, &Min, &Imax, &Imin);
#else
  MinMaxIndex(Z, NPTS, &Max, &Min, &Imax, &Imin);
#endif
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("MinMaxIndexC time = %d usec, %dMtok/s\n",duree,NPTS/duree);
  printf("Min=%f, Max=%f, Imin=%d, Imax=%d, %f, %f \n",Min,Max,Imin,Imax,Z[Imin],Z[Imax]);

  gettimeofday(&t1,NULL);
#ifdef __AVX2__
  MinMaxIndexSumsC(Z, NPTS, &Max, &Min, &Imax, &Imin, &Sum, &Sum2);
#else
  MinMaxIndexSums(Z, NPTS, &Max, &Min, &Imax, &Imin, &Sum, &Sum2);
#endif
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("MinMaxIndexSumsC time = %d usec, %dMtok/s\n",duree,NPTS/duree);
  printf("Min=%f, Max=%f, Imin=%d, Imax=%d, Sum=%f, Sum2=%f \n",Min,Max,Imin,Imax,Sum,Sum2);

#ifdef __AVX2__

  printf("=================== Accelerated Timings ===================\n\n");
  for (j=0 ; j < NREP ; j++) {

    printf("\n");
    gettimeofday(&t1,NULL);
    MinMaxSums(Z, NPTS, &Max, &Min, &Sum, &Sum2);
    gettimeofday(&t2,NULL);
    T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
    T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
    duree = T2-T1;
    printf("MinMaxSums time = %d usec, %dMtok/s\n",duree,NPTS/duree);
    if (j==0) printf("Min=%f, Max=%f, Sum=%f, Sum2=%f \n",Min,Max,Sum,Sum2);

    gettimeofday(&t1,NULL);
    MinMax(Z, NPTS, &Max, &Min);
    gettimeofday(&t2,NULL);
    T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
    T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
    duree = T2-T1;
    printf("MinMax time = %d usec, %dMtok/s\n",duree,NPTS/duree);
    if (j==0) printf("Min=%f, Max=%f \n",Min,Max);

    gettimeofday(&t1,NULL);
    MinMaxIndex(Z, NPTS, &Max, &Min, &Imax, &Imin);
    gettimeofday(&t2,NULL);
    T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
    T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
    duree = T2-T1;
    printf("MinMaxIndex time = %d usec, %dMtok/s\n",duree,NPTS/duree);
    // if (j==0) printf("Min=%f, Max=%f, Imin=%d, Imax=%d \n",Min,Max,Imin,Imax);
    if (j==0) printf("Min=%f, Max=%f, Imin=%d, Imax=%d, %f, %f \n",Min,Max,Imin,Imax,Z[Imin],Z[Imax]);
  }

  gettimeofday(&t1,NULL);
  MinMaxIndexSums(Z, NPTS, &Max, &Min, &Imax, &Imin, &Sum, &Sum2);
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("MinMaxIndexSums time = %d usec, %dMtok/s\n",duree,NPTS/duree);
  if (j==0) printf("Min=%f, Max=%f, Imin=%d, Imax=%d, Sum=%f, Sum2=%f \n",Min,Max,Imin,Imax,Sum,Sum2);

#endif

}
