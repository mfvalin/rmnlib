#include <rpnmacros.h>
ftnword f77name(bozo)(void *name, void *type, void *etik, void *grtyp,
                        char *cname, char *ctype, char *cetik, char *cgrtyp, void *holocar,
                        F2Cl l1, F2Cl l2, F2Cl l3, F2Cl l4)
{
  int ier;
 
  printf("fstcvt cname=%s,ctype=%s,cetik=%s,cgrtyp=%s\n",cname,ctype,cetik,cgrtyp);   
  ier = f77name(fstcvt2)(name,type,etik,grtyp,cname,ctype,cetik,cgrtyp,holocar,l1,l2,l3,l4);
  return((ftnword) ier);
}

