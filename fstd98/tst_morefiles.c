#include "qstdir.h"

void mainc_()
{
  int index,recno,pageno,handle;

  recno = 255;
  index = 134;
  pageno = 1023;
/*  recno=0; index=0; pageno=0; */
  handle = MAKE_RND_HANDLE(pageno,recno,index);
  printf("handle =%i\n",handle);

  index=-1; recno=-1, pageno=-1;
  recno = RECORD_FROM_HANDLE(handle);
  pageno = PAGENO_FROM_HANDLE(handle);
  index = INDEX_FROM_HANDLE(handle);
  printf("index=%i pageno=%i recno=%i\n",index,pageno,recno);
}