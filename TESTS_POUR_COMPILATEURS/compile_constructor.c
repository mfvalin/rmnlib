void Some_name(void) ;

__attribute__((constructor)) int My_Init() {
   Some_name();
   return(1);
}

