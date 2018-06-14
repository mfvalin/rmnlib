#include <stdio.h>
#include <stdlib.h>

int rpn_fortran_callback_(char *VERB, void *callback(), char *OPTIONS,
    int *Private_data,int *Private_data_2, int l_VERB, int l_OPTIONS) {
//    int *Private_data,int *Private_data_2, void *extra,  int l_VERB, int l_OPTIONS) {

 fprintf(stderr,"rpn_fortran_callback: VERB[1] = '%c'\n",VERB[1]);
 fprintf(stderr,"rpn_fortran_callback: OPTIONS[0] = '%c'\n",OPTIONS[0]);
 fprintf(stderr,"rpn_fortran_callback: Private_data   = %d\n",*Private_data  );
 fprintf(stderr,"rpn_fortran_callback: Private_data_2 = %d\n",*Private_data_2);
 fprintf(stderr,"rpn_fortran_callback: l_VERB = %d \n",l_VERB);
 fprintf(stderr,"rpn_fortran_callback: l_OPTIONS = %d \n",l_OPTIONS);
// fprintf(stderr,"rpn_fortran_callback: extra = %16.16lx %16.16p\n",*((long*)extra),extra);
 fprintf(stderr,"rpn_fortran_callback: Private_data_2 = %16.16p\n",Private_data_2);
 fprintf(stderr,"rpn_fortran_callback: Private_data   = %16.16p\n",Private_data);
 fprintf(stderr,"rpn_fortran_callback: OPTIONS        = %16.16p\n",OPTIONS        );
 fprintf(stderr,"rpn_fortran_callback: callback       = %16.16p\n",callback    );
 fprintf(stderr,"rpn_fortran_callback: VERB           = %16.16p\n",VERB        );
 fprintf(stderr,"calling callback\n");
 *(callback) ();
 return (0);
}

