/* Hopefully useful routines for C and FORTRAN programming
 * Copyright (C) 2015  Environnement Canada
 *
 * This code is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This code is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this code; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */
/*
 NOTES    - ALGORITHM FROM "COMMUNICATIONS OF THE ACM" (1968), PAGE 657.
          - IT COVERS A PERIOD OF 7980 YEARS WITH DAY 1 STARTING
            AT YEAR=-4713, MONTH=11, DAY=25
*/
/* original fortran code
   I, J, K = year, month, day
      JD        = K-32075+1461*(I+4800+(J-14)/12)/4
     1             +367*(J-2-(J-14)/12*12)/12-3
     2             *((I+4900+(J-14)/12)/100)/4
*/
long long int JulianSecond(int year, int month, int day, int hour, int minute, int second)
{
  int jd, js;
  long long jsec;
  /* compute julian day */
  jd = day-32075+1461*(year+4800+(month-14)/12)/4
       +367*(month-2-(month-14)/12*12)/12
       -3*((year+4900+(month-14)/12)/100)/4 ;
  js = second + (60*minute) + (3600*hour) ;
  jsec = jd ;
  jsec = jsec * (24*3600) + js;
}
/* original fortran code
   I, J, K = year, month, day
      L= JD+68569
      N= 4*L/146097
      L= L-(146097*N+3)/4
      I= 4000*(L+1)/1461001
      L= L-1461*I/4+31
      J= 80*L/2447
      K= L-2447*J/80
      L= J/11
      J= J+2-12*L
      I= 100*(N-49)+I+L
*/
void DateFromJulian (long long jsec, int *yyyymmdd, int *hhmmss)
{
  int js;
  long long jr8;
  int h, m, s;
  int yr, mth, day;
  int temp1, temp2, jday;

  jr8 = jsec / (24*3600) ;           /* julian day */
  jday = jr8;

  js  = jsec - (jr8 * 24 * 3600) ;   /* seconds in day */
  h = js / 3600 ;                    /* hours */
  m = (js - h * 3600) / 60 ;         /* minutes */
  s = js - (h * 3600) - (m *  60) ;  /* seconds */
  *hhmmss = s + (m * 100) + (h * 10000);

  temp1= jday+68569 ;
  temp2= 4*temp1/146097 ;
  temp1= temp1-(146097*temp2+3)/4 ;
  yr= 4000*(temp1+1)/1461001 ;
  temp1= temp1-1461*yr/4+31 ;
  mth= 80*temp1/2447 ;
  day= temp1-2447*mth/80 ;
  temp1= mth/11 ;
  mth= mth+2-12*temp1 ;
  yr= 100*(temp2-49)+yr+temp1 ;
  *yyyymmdd = day + (mth * 100) + (yr * 10000) ;
}
#if defined(SELF_TEST)
#include <stdio.h>
#include <stdlib.h>
main()
{
  long long stamp;
  int yyyymmdd, hhmmss;

  stamp = JulianSecond(2015,01,31,22,45,15) ; /* jan 31 2015 22:45:15 */
  stamp = stamp + 5415 ;                      /* + 1:30:15   */
  DateFromJulian(stamp,&yyyymmdd,&hhmmss);
  fprintf(stdout,"new date = %8.8d:%6.6d\n",yyyymmdd,hhmmss);
  fprintf(stdout,"expected = %8.8d:%6.6d\n",20150201,1530) ; /* feb 1 2015 00:1530 */

  stamp = JulianSecond(2016,02,28,22,45,15) ; /* feb 28 2016 22:45:15 , leap year test */
  stamp = stamp + 5415 ;                      /* + 1:30:15   */
  DateFromJulian(stamp,&yyyymmdd,&hhmmss);
  fprintf(stdout,"new date = %8.8d:%6.6d\n",yyyymmdd,hhmmss);
  fprintf(stdout,"expected = %8.8d:%6.6d\n",20160229,1530) ; /* feb 1 2015 00:1530 */

  stamp = JulianSecond(2000,02,28,22,45,15) ; /* feb 28 2016 22:45:15 , leap year test */
  stamp = stamp + 5415 ;                      /* + 1:30:15   */
  DateFromJulian(stamp,&yyyymmdd,&hhmmss);
  fprintf(stdout,"new date = %8.8d:%6.6d\n",yyyymmdd,hhmmss);
  fprintf(stdout,"expected = %8.8d:%6.6d\n",20000229,1530) ; /* feb 1 2000 00:1530 */

  stamp = JulianSecond(2100,02,28,22,45,15) ; /* feb 28 2100 22:45:15 , leap year test */
  stamp = stamp + 5415 ;                      /* + 1:30:15   */
  DateFromJulian(stamp,&yyyymmdd,&hhmmss);
  fprintf(stdout,"new date = %8.8d:%6.6d\n",yyyymmdd,hhmmss);
  fprintf(stdout,"expected = %8.8d:%6.6d\n",21000301,1530) ; /* feb 1 2015 00:1530 */
}
#endif