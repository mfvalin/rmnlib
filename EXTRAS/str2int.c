#include <stdio.h>
// convert a 32 bit unsigned integer to a 4 character string
// the string must be able to accomodate at least 5 characters
void int_to_str4(unsigned int i, unsigned char *s)
{
  int loop = 4;
  while(loop--) { *s++ = i >> 24 ; i <<= 8; }
  *s = '\0';
  return ;
}
// convert a 64 bit unsigned integer to a 8 character string
// the string must be able to accomodate at least 9 characters
void longlong_to_str8(unsigned long long i, unsigned char *s)
{
  int loop = 8;
  while(loop--) { *s++ = i >> 56 ; i <<= 8; }
  *s = '\0';
  return ;
}
// convert a character string to a 32 bit unsigned integer
// if the string is shorter than 4 characters, it will be LEFT aligned and right padded with zeros
// if the string is longer than 4 characters, the last 4 characters will be stored in the integer
// the first (length_of_string - 4) characters will be lost
unsigned int str_to_int(unsigned char *s)
{
  unsigned int temp = 0 ;
  int loop = 4;
  while(loop--) { temp = (temp << 8) | *s; if(*s) s++; }
  while(*s) temp = (temp << 8) | *s++; 
  return(temp);
}
// convert a character string to a 64 bit unsigned integer
// if the string is shorter than 8 characters, it will be LEFT aligned and right padded with zeros
// if the string is longer than 8 characters, the last 8 characters will be stored in the integer
// the first (length_of_string - 8) characters will be lost
unsigned long long str_to_longlong(unsigned char *s)
{
  unsigned long long temp = 0 ;
  int loop = 8;
  while(loop--) { temp = (temp << 8) | *s; if(*s) s++; }
  while(*s) temp = (temp << 8) | *s++; 
  return(temp);
}
#if defined(SELF_TEST)
main()
{
  unsigned int value;
  unsigned long long value8;
  unsigned char str[5];
  unsigned char str_to_[9];

  printf(" ABCD - %08x\n",str_to_int("ABCD"));
  printf(" ABC  - %08x\n",str_to_int("ABC "));
  printf(" ABCDEF - %08x\n",str_to_int("ABCDEF"));
  printf(" ABC - %08x\n",str_to_int("ABC"));

  value = str_to_int("ABCD");
  int_to_str4(value,str);
  printf(" '%s' value=%08x, str='%s'\n","ABCD",value,str);
  value = str_to_int("ABC ");
  int_to_str4(value,str);
  printf(" '%s' value=%08x, str='%s'\n","ABC ",value,str);
  value = str_to_int("ABC");
  int_to_str4(value,str);
  printf(" '%s' value=%08x, str='%s'\n","ABC",value,str);
  value = str_to_int("AB");
  int_to_str4(value,str);
  printf(" '%s' value=%08x, str='%s'\n","AB",value,str);
  value = str_to_int("A");
  int_to_str4(value,str);
  printf(" '%s' value=%08x, str='%s'\n","A",value,str);
  value = str_to_int("ABCDEF");
  int_to_str4(value,str);
  printf(" '%s' value=%08x, str='%s'\n","ABCDEF",value,str);
  value8 = str_to_longlong("ABCDEFGH");
  longlong_to_str8(value8,str_to_);
  printf(" '%s' value8=%16.16Lx, str2='%s'\n","ABCDEFGH",value8,str_to_);
  value8 = str_to_longlong("ABCDE");
  longlong_to_str8(value8,str_to_);
  printf(" '%s' value8=%16.16Lx, str2='%s'\n","ABCDE",value8,str_to_);
  value8 = str_to_longlong("ABCDEFGHIJ");
  longlong_to_str8(value8,str_to_);
  printf(" '%s' value8=%16.16Lx, str2='%s'\n","ABCDEFGHIJ",value8,str_to_);
}
#endif
