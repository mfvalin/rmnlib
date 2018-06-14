#include <stdio.h>
#include <stdlib.h>

int rpn_fortran_callback_(char *VERB, void *callback, char *OPTIONS,
    void *Private_data,void *Private_data_2,  int l_VERB, int l_OPTIONS) {

 fprintf(stderr,"rpn_fortran_callback: l_VERB = %d \n",l_VERB);
 fprintf(stderr,"rpn_fortran_callback: l_OPTIONS = %d \n",l_OPTIONS);
 return (0);
}

