#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <glob.h>

#define min(a,b) \
   ({ __typeof__ (a) _a = (a); \
       __typeof__ (b) _b = (b); \
     _a < _b ? _a : _b; })

#pragma weak get_first_glob_match__=get_first_glob_match
#pragma weak get_first_glob_match_=get_first_glob_match
int get_first_glob_match__(char *pattern, char *answer, int lpat, int maxanswer);
int get_first_glob_match_(char *pattern, char *answer, int lpat, int maxanswer);
/*
 * get the name of a file that matches a file pattern (shell style pattern)
 * the functions returns the length of the matching file name
 * C API
 * int get_first_glob_match(char *pattern, char *answer, int lpat, int maxanswer)
 *   pattern   (IN) : file pattern to match
 *   lpat      (IN) : length of string in pattern (if <= 0, string is NULL ttermanated)
 *   answer    (OUT): character array to receive matching filename
 *   maxanswer (IN) : length of array answer
 *   the function returns the length of the matching filename
 * 
 * FORTRAN API
 * integer function get_first_glob_match(pattern,answer)
 *   character(len=*) , intent(IN)  :: pattern
 *   character(len=*) , intent(OUT) :: answer
 *   the function returns the length of the matching filename
 *   (answer(1:function value) contains th ematching filename)
 *   (answer(function value+1) contains a NULL character
 * 
 * examples:
 * 
 * character(len=2048) :: filename
 * integer nchars
 * nchars=get_first_glob_match("sub/??*.std",filename)
 * 
 * char filename[2048];
 * int nchars;
 * nchars=get_first_glob_match("sub/??*.std",filename,0,sizeof(filename))
 * 
 * the first nchars characters of filename will contain then name os the first standard file 
 * in the sub  subdirectory of the current directory whose name has 
 * at least 2 characters before the .std extension
 * 
 * NOTE:
 *   this function uses the glob() library call, NO SORTING is done on names in case of a multiple match
 *   ANY name among the matching filenames may therefore be returned
 * 
 */
int get_first_glob_match(const char *pattern, char *answer, int lpat, int maxanswer)
{
  glob_t globtab;
  char localpat[2048];
  int i;

  for(i=0 ; i<maxanswer ; i++) answer[i] = ' ';          /* fill answer with blanks */
  answer[maxanswer-1] = '\0'; /* and null terminate it */

  if(lpat<=0) lpat=sizeof(localpat);
  strncpy( localpat , pattern, min(sizeof(localpat)-1 , (lpat+1)) );
  localpat[sizeof(localpat)-1] = '\0';
  for(i=0 ; i<sizeof(localpat) ; i++)
  {
    if(localpat[i]==' ') { localpat[i]='\0' ; break; };  /* get first blank in pattern if any and replace it with null */
  }

  if( 0 == glob(&localpat[0],GLOB_NOSORT,NULL,&globtab) )
  {
    if(globtab.gl_pathc > 0)
    {
      strncpy(answer,globtab.gl_pathv[0],maxanswer-1);
    }
    i = strlen(answer);
  }else{
    i = 0;
    answer[0]='\0';
  }
  globfree(&globtab);
  return(i);
}
#if defined(SELF_TEST)
int main(int argc, char **argv)
{
  char answer[2048];
  int n = get_first_glob_match(argv[1],answer,0,sizeof(answer));
  printf("%d characters, first match='%s'\n",n,answer);
  return(0);
}
#endif
