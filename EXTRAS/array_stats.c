#include <stdlib.h>
#include <stdio.h>
#ifdef __SSE2__
#include <immintrin.h>
#endif

#ifdef __SSE2__
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

#ifdef __SSE2__
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

#ifdef __SSE2__
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
  int nm63 = n - 15;

  zzmin  = _mm_loadu_ps(&z[00]);
  zzmax  = _mm_loadu_ps(&z[00]);
  yysum  = _mm256_set1_pd(0.0D);
  yysum2 = _mm256_set1_pd(0.0D);

  for (i = 0 ; i < nm63 ; i += 16){

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
  int nm63 = n - 31;

  zzmin  = _mm256_loadu_ps(&z[00]);
  zzmax  = _mm256_loadu_ps(&z[00]);

  for (i = 0 ; i < nm63 ; i += 32){

    z0 = _mm256_loadu_ps(&z[ 0]);
    z1 = _mm256_loadu_ps(&z[ 8]);
    z2 = _mm256_loadu_ps(&z[16]);
    z3 = _mm256_loadu_ps(&z[24]);

    zzmin = _mm256_min_ps(zzmin,z0);       // min
    zzmax = _mm256_max_ps(zzmax,z0);       // max

    zzmin = _mm256_min_ps(zzmin,z1);
    zzmax = _mm256_max_ps(zzmax,z1);

    zzmin = _mm256_min_ps(zzmin,z2);
    zzmax = _mm256_max_ps(zzmax,z2);

    zzmin = _mm256_min_ps(zzmin,z3);
    zzmax = _mm256_max_ps(zzmax,z3);

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
#endif
#define NPTS (20000*1024+5)
#include <sys/time.h>
main()
{
  struct timeval t1,t2;
  long long T1, T2;
  int duree;
  float Z[NPTS];
  float Max, Min, Sum, Div, Sum2;
  int i;

  Div = NPTS;
  for(i=0 ; i<NPTS; i++) Z[i] = ((i-0.5*Div)/Div)*4.0*((i-0.5*Div)/Div);
  printf("z[0]=%f,z[NPTS/2]=%f,z[NPTS-1]=%f\n",Z[0],Z[NPTS/2],Z[NPTS-1]);

#ifdef __SSE2__XX
  printf("Test with sse2\n");
  MinMaxSums(Z, NPTS, &Max, &Min, &Sum, &Sum2);
  MinMaxSums(Z, NPTS, &Max, &Min, &Sum, &Sum2);
  MinMaxSums(Z, NPTS, &Max, &Min, &Sum, &Sum2);
  MinMaxSums(Z, NPTS, &Max, &Min, &Sum, &Sum2);
  MinMaxSums(Z, NPTS, &Max, &Min, &Sum, &Sum2);
  MinMaxSums(Z, NPTS, &Max, &Min, &Sum, &Sum2);
#endif

  gettimeofday(&t1,NULL);
#ifdef __SSE2__
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
#ifdef __SSE2__
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

#ifdef __SSE2__
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
#endif

}
