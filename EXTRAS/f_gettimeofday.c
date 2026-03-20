#include <stdlib.h>
#include <sys/time.h>

#pragma weak f_gettimeofday_=f_gettimeofday
#pragma weak f_gettimeofday__=f_gettimeofday
long long f_gettimeofday_();
long long f_gettimeofday__();

long long f_gettimeofday()
{
  struct timeval tv;
  long long temp;
  if(gettimeofday(&tv,NULL) != 0) return   -1;
  temp = tv.tv_sec;
  temp *= 1000000;
  temp += tv.tv_usec;
  return temp;
}