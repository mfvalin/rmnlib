#include <stdio.h>
#include <ctype.h>

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

// convert a character string to a 32 bit unsigned integer. lower case characters will be "uppercased"
// if the string is shorter than 4 characters, it will be LEFT aligned and right padded with zeros
// if the string is longer than 4 characters, the last 4 characters will be stored in the integer
// the first (length_of_string - 4) characters will be lost
unsigned int str_to_int_u(unsigned char *s)
{
  unsigned int temp = 0 ;
  int loop = 4;
  while(loop--) { temp = (temp << 8) | (toupper(*s)); if(*s) s++; }
  while(*s) { temp = (temp << 8) | (toupper(*s)) ; s++; }
  return(temp);
}

// convert a character string to a 32 bit unsigned integer. upper case characters will be "lowercased"
// if the string is shorter than 4 characters, it will be LEFT aligned and right padded with zeros
// if the string is longer than 4 characters, the last 4 characters will be stored in the integer
// the first (length_of_string - 4) characters will be lost
unsigned int str_to_int_l(unsigned char *s)
{
  unsigned int temp = 0 ;
  int loop = 4;
  while(loop--) { temp = (temp << 8) | (tolower(*s)); if(*s) s++; }
  while(*s) { temp = (temp << 8) | (tolower(*s)) ; s++; }
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

// convert a character string to a 64 bit unsigned integer. upper case characters will be "uppercased"
// if the string is shorter than 8 characters, it will be LEFT aligned and right padded with zeros
// if the string is longer than 8 characters, the last 8 characters will be stored in the integer
// the first (length_of_string - 8) characters will be lost
unsigned long long str_to_longlong_u(unsigned char *s)
{
  unsigned long long temp = 0 ;
  int loop = 8;
  while(loop--) { temp = (temp << 8) | (toupper(*s)); if(*s) s++; }
  while(*s)  { temp = (temp << 8) | (toupper(*s)) ; s++; }
  return(temp);
}

// convert a character string to a 64 bit unsigned integer. upper case characters will be "lowercased"
// if the string is shorter than 8 characters, it will be LEFT aligned and right padded with zeros
// if the string is longer than 8 characters, the last 8 characters will be stored in the integer
// the first (length_of_string - 8) characters will be lost
unsigned long long str_to_longlong_l(unsigned char *s)
{
  unsigned long long temp = 0 ;
  int loop = 8;
  while(loop--) { temp = (temp << 8) | (tolower(*s)); if(*s) s++; }
  while(*s)  { temp = (temp << 8) | (tolower(*s)) ; s++; }
  return(temp);
}
#if defined(SELF_TEST)
main()
{
  unsigned int value;
  unsigned long long value8;
  unsigned char str[5];
  unsigned char str2[9];

  printf(" ABCD - %08x\n",str_to_int("ABCD"));
  printf(" ABC  - %08x\n",str_to_int("ABC "));
  printf(" ABCDEF - %08x\n",str_to_int("ABCDEF"));
  printf(" ABC - %08x\n",str_to_int("ABC"));

  value = str_to_int_u("abcd");
  int_to_str4(value,str);
  printf(" '%s' value=%08x, str_u='%s'\n","abcd",value,str);
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
  value = str_to_int_l("ABCDEF");
  int_to_str4(value,str);
  printf(" '%s' value=%08x, str_l='%s'\n","ABCDEF",value,str);
  value8 = str_to_longlong_l("ABCDEFGH");
  longlong_to_str8(value8,str2);
  printf(" '%s' value8=%16.16Lx, str2_l='%s'\n","ABCDEFGH",value8,str2);
  value8 = str_to_longlong("ABCDE");
  longlong_to_str8(value8,str2);
  printf(" '%s' value8=%16.16Lx, str2='%s'\n","ABCDE",value8,str2);
  value8 = str_to_longlong_u("abcdefghij");
  longlong_to_str8(value8,str2);
  printf(" '%s' value8=%16.16Lx, str2_u='%s'\n","abcdefghij",value8,str2);
}
#endif
