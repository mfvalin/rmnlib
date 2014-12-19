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
  float zmin[4], zmax[4];
  double ysum[4], ysum2[4];
  float smin, smax ;
  double ssum, ssum2;
  float *z = z_in;
  __m128 z0, z1, z2, z3, zzmin, zzmax, zzsum, zzsum2;
  __m256d y0, y1, y2, y3, yysum, yysum2;
  int i;
  int limit = n - 15;

  zzmin  = _mm_loadu_ps(&z[00]);
  zzmax  = _mm_loadu_ps(&z[00]);
  yysum  = _mm256_set1_pd(0.0D);
  yysum2 = _mm256_set1_pd(0.0D);

  for (i = 0 ; i < limit ; i += 16){

    z0 = _mm_loadu_ps(&z[ 0]);
    z1 = _mm_loadu_ps(&z[ 4]);
    z2 = _mm_loadu_ps(&z[ 8]);
    z3 = _mm_loadu_ps(&z[12]);

    y0 = _mm256_cvtps_pd(z0);           // convert single precision to double for sums
    y1 = _mm256_cvtps_pd(z1);
    y2 = _mm256_cvtps_pd(z2);
    y3 = _mm256_cvtps_pd(z3);

    yysum = _mm256_add_pd(yysum,y0);    // double precision sum
    y0 = _mm256_mul_pd(y0,y0);          // square of value
    zzmin = _mm_min_ps(zzmin,z0);       // min
    zzmax = _mm_max_ps(zzmax,z0);       // max
    yysum2 = _mm256_add_pd(yysum2,y0);  // sum of squares

    yysum = _mm256_add_pd(yysum,y1);
    y1 = _mm256_mul_pd(y1,y1);
    zzmin = _mm_min_ps(zzmin,z1);
    zzmax = _mm_max_ps(zzmax,z1);
    yysum2 = _mm256_add_pd(yysum2,y1);

    yysum = _mm256_add_pd(yysum,y2);
    y2 = _mm256_mul_pd(y2,y2);
    zzmin = _mm_min_ps(zzmin,z2);
    zzmax = _mm_max_ps(zzmax,z2);
    yysum2 = _mm256_add_pd(yysum2,y2);

    yysum = _mm256_add_pd(yysum,y3);
    y3 = _mm256_mul_pd(y3,y3);
    zzmin = _mm_min_ps(zzmin,z3);
    zzmax = _mm_max_ps(zzmax,z3);
    yysum2 = _mm256_add_pd(yysum2,y3);

    z += 16;

  }
  _mm_storeu_ps(zmin,zzmin);
  _mm_storeu_ps(zmax,zzmax);
  _mm256_storeu_pd(ysum,yysum);
  _mm256_storeu_pd(ysum2,yysum2);
  smax = zmax[0];
  smax = (smax > zmax[1]) ? smax : zmax[1] ;
  smax = (smax > zmax[2]) ? smax : zmax[2] ;
  smax = (smax > zmax[3]) ? smax : zmax[3] ;
  smin = zmin[0];
  smin = (smin < zmin[1]) ? smin : zmin[1] ;
  smin = (smin < zmin[2]) ? smin : zmin[2] ;
  smin = (smin < zmin[3]) ? smin : zmin[3] ;
  ssum = ysum[0] + ysum[1] + ysum[2] + ysum[3] ;
  ssum2 = ysum2[0] + ysum2[1] + ysum2[2] + ysum2[3] ;

  while(i<n){
    smax = (smax > z_in[i]) ? smax : z_in[i] ;
    smin = (smin < z_in[i]) ? smin : z_in[i] ;
    ssum = ssum + z_in[i] ;
    ssum2 = ssum2 + z_in[i]*z_in[i] ;
    i++ ;
  }
  *Max=smax;
  *Min=smin;
  *Sum=ssum;
  *Sum2=ssum2;
  return;
}
void MinMax(float *z_in, int n, float *Max, float *Min)
{
  float zmin[8], zmax[8];
  float smin, smax ;
  float *z = z_in;
  __m256 z0, z1, z2, z3, zzmin, zzmax;
  int i;
//  int limit = n - 31;
  int limit = n - 15;

  zzmin  = _mm256_loadu_ps(&z[0]);
  zzmax  = _mm256_loadu_ps(&z[0]);

//  for (i = 0 ; i < limit ; i += 32){
  for (i = 0 ; i < limit ; i += 16){

    z0 = _mm256_loadu_ps(&z[ 0]);
    z1 = _mm256_loadu_ps(&z[ 8]);
//    z2 = _mm256_loadu_ps(&z[16]);
//    z3 = _mm256_loadu_ps(&z[24]);

    zzmin = _mm256_min_ps(zzmin,z0);       // min
    zzmax = _mm256_max_ps(zzmax,z0);       // max

    zzmin = _mm256_min_ps(zzmin,z1);
    zzmax = _mm256_max_ps(zzmax,z1);

//    zzmin = _mm256_min_ps(zzmin,z2);
//    zzmax = _mm256_max_ps(zzmax,z2);

//    zzmin = _mm256_min_ps(zzmin,z3);
//    zzmax = _mm256_max_ps(zzmax,z3);

//    z += 32;
    z += 16;

  }
  _mm256_storeu_ps(zmin,zzmin);
  _mm256_storeu_ps(zmax,zzmax);
  smax = zmax[0];
  smax = (smax > zmax[1]) ? smax : zmax[1] ;
  smax = (smax > zmax[2]) ? smax : zmax[2] ;
  smax = (smax > zmax[3]) ? smax : zmax[3] ;
  smax = (smax > zmax[4]) ? smax : zmax[4] ;
  smax = (smax > zmax[5]) ? smax : zmax[5] ;
  smax = (smax > zmax[6]) ? smax : zmax[6] ;
  smax = (smax > zmax[7]) ? smax : zmax[7] ;
  smin = zmin[0];
  smin = (smin < zmin[1]) ? smin : zmin[1] ;
  smin = (smin < zmin[2]) ? smin : zmin[2] ;
  smin = (smin < zmin[3]) ? smin : zmin[3] ;
  smin = (smin < zmin[4]) ? smin : zmin[4] ;
  smin = (smin < zmin[5]) ? smin : zmin[5] ;
  smin = (smin < zmin[6]) ? smin : zmin[6] ;
  smin = (smin < zmin[7]) ? smin : zmin[7] ;

  while(i<n){
    smax = (smax > z_in[i]) ? smax : z_in[i] ;
    smin = (smin < z_in[i]) ? smin : z_in[i] ;
    i++ ;
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
  float *z = z_in;
  __m256 z0, zzmin, zzmax;
  __m256i index, ixmax, ixmin, c8, msmin, msmax;
  int i, j;
  const int block = 8;
  const int limit = n - block + 1;

  if(n < block) {
    smax = z_in[0];
    smin = smax;
    xmin = 0;
    xmax = 0;
    i = 1;
  }else{
    zzmin  = _mm256_loadu_ps(&z[0]);
    zzmax  = _mm256_loadu_ps(&z[0]);
    index  = _mm256_loadu_si256((__m256i const *)&in07[0]);
    ixmin  = _mm256_loadu_si256((__m256i const *)&in07[0]);
    ixmax  = _mm256_loadu_si256((__m256i const *)&in07[0]);
    c8     = _mm256_set1_epi32(8);

    for (i = 0 ; i < limit ; i += block){
      z0 = _mm256_load_ps(z+i);

      msmin = (__m256i)_mm256_cmp_ps(zzmin,z0,_CMP_GT_OS);
      msmax = (__m256i)_mm256_cmp_ps(zzmax,z0,_CMP_LT_OS);
      zzmin = _mm256_min_ps(zzmin,z0);
      zzmax = _mm256_max_ps(zzmax,z0);
      ixmin = _mm256_blendv_epi8(ixmin,index,msmin);
      ixmax = _mm256_blendv_epi8(ixmax,index,msmax);
      index = _mm256_add_epi32(index,c8);    // bump index
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

  for( ; i < n ; i++ ){
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
  float *z = z_in;
  __m256 z0, zzmin, zzmax;
  __m128 lo128, hi128;
  __m256i index, ixmax, ixmin, c8, msmin, msmax;
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
  }else{
    zzmin  = _mm256_loadu_ps(&z[0]);
    zzmax  = _mm256_loadu_ps(&z[0]);
    index  = _mm256_loadu_si256((__m256i const *)&in07[0]);
    ixmin  = _mm256_loadu_si256((__m256i const *)&in07[0]);
    ixmax  = _mm256_loadu_si256((__m256i const *)&in07[0]);
    c8     = _mm256_set1_epi32(8);
    sumd   = _mm256_set1_pd(0.0D);   // sums starts at 0.0
    sumsqr = _mm256_set1_pd(0.0D);
    sum1 = 0;
    sum2 = 0;

    for (i = 0 ; i < limit ; i += block){
      z0    = _mm256_load_ps(z+i);
      lo128 = _mm256_extractf128_ps(z0,0);
      hi128 = _mm256_extractf128_ps(z0,1);
      lo128d= _mm256_cvtps_pd(lo128);
      hi128d= _mm256_cvtps_pd(hi128);

      sumd  = _mm256_add_pd(sumd,lo128d);
      lo128d= _mm256_mul_pd(lo128d,lo128d);
      sumd  = _mm256_add_pd(sumd,hi128d);
      hi128d= _mm256_mul_pd(hi128d,hi128d);
      sumsqr= _mm256_add_pd(sumsqr,lo128d);
      sumsqr= _mm256_add_pd(sumsqr,hi128d);

      msmin = (__m256i)_mm256_cmp_ps(zzmin,z0,_CMP_GT_OS);
      msmax = (__m256i)_mm256_cmp_ps(zzmax,z0,_CMP_LT_OS);
      zzmin = _mm256_min_ps(zzmin,z0);
      zzmax = _mm256_max_ps(zzmax,z0);
      ixmin = _mm256_blendv_epi8(ixmin,index,msmin);
      ixmax = _mm256_blendv_epi8(ixmax,index,msmax);
      index = _mm256_add_epi32(index,c8);    // bump index
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
  int i;

  Div = NPTS;
  for(i=0 ; i<NPTS; i++) Z[i] = (((i-0.5*Div)/Div)*4.0*((i-0.5*Div)/Div)) ;
  Z[0] = .5 ; Z[3] = .5 ; Z[5] = .5 ; Z[7] = 0;
  printf("NPTS=%d, z[0]=%f,z[NPTS/2]=%f,z[NPTS-1]=%f\n",NPTS,Z[0],Z[NPTS/2],Z[NPTS-1]);

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

  gettimeofday(&t1,NULL);
  MinMaxSums(Z, NPTS, &Max, &Min, &Sum, &Sum2);
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("MinMaxSums time = %d usec, %dMtok/s\n",duree,NPTS/duree);
  printf("Min=%f, Max=%f, Sum=%f, Sum2=%f \n",Min,Max,Sum,Sum2);

  gettimeofday(&t1,NULL);
  MinMax(Z, NPTS, &Max, &Min);
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("MinMax time = %d usec, %dMtok/s\n",duree,NPTS/duree);
  printf("Min=%f, Max=%f \n",Min,Max);

  gettimeofday(&t1,NULL);
  MinMaxIndex(Z, NPTS, &Max, &Min, &Imax, &Imin);
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("MinMaxIndex time = %d usec, %dMtok/s\n",duree,NPTS/duree);
  printf("Min=%f, Max=%f, Imin=%d, Imax=%d \n",Min,Max,Imin,Imax);
  printf("Min=%f, Max=%f, Imin=%d, Imax=%d, %f, %f \n",Min,Max,Imin,Imax,Z[Imin],Z[Imax]);

  gettimeofday(&t1,NULL);
  MinMaxIndexSums(Z, NPTS, &Max, &Min, &Imax, &Imin, &Sum, &Sum2);
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("MinMaxIndexSums time = %d usec, %dMtok/s\n",duree,NPTS/duree);
  printf("Min=%f, Max=%f, Imin=%d, Imax=%d \n",Min,Max,Imin,Imax);
  printf("Min=%f, Max=%f, Imin=%d, Imax=%d, Sum=%f, Sum2=%f \n",Min,Max,Imin,Imax,Sum,Sum2);

#endif

}
