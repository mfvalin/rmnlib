#include <time.h>
#include <stdlib.h>
#include <stdio.h>
main()
{
  struct timespec res, res1, res2;
  int n=20;
  int cres = clock_getres(CLOCK_REALTIME,&res);
  fprintf(stderr,"clkres = %d sec %d nsec\n",res.tv_sec,res.tv_nsec);
  while(n-- > 0) {
    cres = clock_gettime(CLOCK_REALTIME,&res1);
    cres = clock_gettime(CLOCK_REALTIME,&res2);
//    fprintf(stderr,"delta = %d %d\n",res2.tv_sec-res1.tv_sec,res2.tv_nsec-res1.tv_nsec);
    cres = clock_gettime(CLOCK_MONOTONIC,&res1);
    cres = clock_gettime(CLOCK_MONOTONIC,&res2);
    fprintf(stderr,"delta = %d %d\n",res2.tv_sec-res1.tv_sec,res2.tv_nsec-res1.tv_nsec);
    cres = clock_gettime(CLOCK_MONOTONIC_RAW,&res1);
    cres = clock_gettime(CLOCK_MONOTONIC_RAW,&res2);
//    fprintf(stderr,"delta = %d %d\n",res2.tv_sec-res1.tv_sec,res2.tv_nsec-res1.tv_nsec);
  }
}