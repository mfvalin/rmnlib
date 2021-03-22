#include <stdio.h>
#include <stdlib.h>

#pragma weak unsat
void unsat() ;

//static inline int IsValidAddress(void *p){
//  return (p == NULL) ;
//}
#if defined(NO_BUG)
static inline int IsNullAddress(void *p){
  return (p == NULL) ;
}
#endif

int main(int argc, char **argv){
printf("unsat = %p \n",unsat);
printf("unsat = %16.16llx \n",unsat);
printf("NULL = %16.16llx \n",NULL);
#if defined(NO_BUG)
if( IsNullAddress(unsat) ) {
#else
if(unsat == NULL){
#endif
  printf("unsat is NULL\n");
}else{
  printf("callink unsat\n");
  unsat();
}
}

