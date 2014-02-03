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
int get_first_glob_match__(char *pattern, char *filename, int lpat, int maxnamelength);
int get_first_glob_match_(char *pattern, char *filename, int lpat, int maxnamelength);
/*
 * get the name of a file that matches a file pattern (shell style pattern)
 * the functions returns the length of the matching file name
 * C API
 * int get_first_glob_match(char *pattern, char *filename, int lpat, int maxnamelength)
 *   pattern   (IN) : file pattern to match
 *   lpat      (IN) : length of string in pattern (if <= 0, string is NULL ttermanated)
 *   filename    (OUT): character array to receive matching filename
 *   maxnamelength (IN) : length of array filename
 *   the function returns the length of the matching filename
 * 
 * FORTRAN API
 * integer function get_first_glob_match(pattern,filename)
 *   character(len=*) , intent(IN)  :: pattern
 *   character(len=*) , intent(OUT) :: filename
 *   the function returns the length of the matching filename
 *   (filename(1:function value) contains th ematching filename)
 *   (filename(function value+1) contains a NULL character
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
int get_first_glob_match(const char *pattern, char *filename, int lpat, int maxnamelength)
{
  glob_t globtab;
  char localpat[2048];
  int i;

  for(i=0 ; i<maxnamelength ; i++) filename[i] = ' ';          /* fill filename with blanks */
  filename[maxnamelength-1] = '\0'; /* and null terminate it */

  if(lpat<=0) lpat=sizeof(localpat);
  strncpy( localpat , pattern, min(sizeof(localpat)-1 , (lpat+1)) );
  localpat[sizeof(localpat)-1] = '\0';
  for(i=0 ; i<sizeof(localpat) ; i++)
  {
    if(localpat[i]==' ') { localpat[i]='\0' ; break; };     /* get first blank in pattern if any and replace it with null */
  }

  if( 0 == glob(&localpat[0],GLOB_NOSORT,NULL,&globtab) )
  {
    if(globtab.gl_pathc > 0)                                /* there is at least a match */
    {
      strncpy(filename,globtab.gl_pathv[0],maxnamelength-1);      /* copy first match into filename */
      i = strlen(filename);    /* set i to length of filename */
    }else{
      i = 0;   /* no match */
    }
  }else{
    i = 0;   /* error in glob  */
  }
  globfree(&globtab);
  filename[i]='\0';
  return(i);    /* return length of first match */
}

#pragma weak cstring_to_fstring_=cstring_to_fstring
#pragma weak cstring_to_fstring__=cstring_to_fstring
int cstring_to_fstring_(char *cstring, int cstringlen);
int cstring_to_fstring__(char *cstring, int cstringlen);
/*
 * convert a potentially null terminated string into a Fortran blank filled string
 * Fortran API
 * integer function cstring_to_fstring(string)
 *   character(len=*) :: string
 * the function returns the position of the null character if one is found,
 * the length of the original string otherwise
 */
int cstring_to_fstring(char *cstring, int cstringlen)
{
  int i;
  int result;
  for( i=0 ; i<cstringlen ; i++ )  /* find first null */
  {
    if(cstring[i] == '\0')
    {
      cstring[i] = ' ' ; break ;   /* replace null with blank */
    }
  }
  result = i; /* return length oforiginal string */
  while( ++i < cstringlen ) cstring[i] = ' ' ;  /* fill rest of string with blanks */
  return result;
}
#if defined(SELF_TEST)
int main(int argc, char **argv)
{
  char filename[2048];
  int n = get_first_glob_match(argv[1],filename,0,sizeof(filename));
  printf("%d characters, first match='%s'\n",n,filename);
  return(0);
}
#endif
