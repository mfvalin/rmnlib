#include <stdio.h>
#include <ISO_Fortran_binding.h>
int rank_2018(CFI_cdesc_t *this){
  fprintf(stderr,"version = %8.8x, rank = %d, element length = %ld\n", this->version, this->rank, this->elem_len) ;
  return this->rank ;
}
