#include <rpnmacros.h>
#include "qstdir.h"
ftnword f77name(tstcp)(char *f_etiket, char *f_typvar, char *f_nomvar,
                        F2Cl ll1, F2Cl ll2, F2Cl ll3)
{
  int l1=ll1, l2=ll2, l3=ll3;

  char etiket[13];
  char typvar[3];
  char nomvar[5];

  printf("\nDebug l1=%d l2=%d l3=%d\n",l1,l2,l3);
  str_cp_init(etiket,13,f_etiket,l1);
  str_cp_init(typvar,3,f_typvar,l2);
  str_cp_init(nomvar,5,f_nomvar,l3);
  
  printf("Debug etiket= [%s]\n",etiket);
  printf("Debug typvar= [%s]\n",typvar);
  printf("Debug nomvar= [%s]\n",nomvar);
}
  
