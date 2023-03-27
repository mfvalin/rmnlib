void Some_name(void) ;

__attribute__ ((constructor)) int Ny_Init() {
   Some_name();
   return(1);
}

