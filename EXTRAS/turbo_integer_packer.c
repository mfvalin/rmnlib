#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#define WITH_OFFSET__ + signed_offset
#define WITH_OFFSET

#ifndef USE_MY_MACROS

#include <pack_macros_64.h>

#else

#define get_bits_64(unpacked,nbits,temp,bleft) \
        unpacked = (temp >> (64 - nbits)) WITH_OFFSET; \
        temp <<= nbits;                                   \
        bleft -= nbits;

#define check_unpack_64(temp,bleft,packed) \
        if(bleft <= 0) {                 \
          temp = temp >> (-bleft) ;      \
          temp |= packed;                \
          temp <<= (-bleft);             \
          bleft += 32;                   \
        }

#define declare_pack_64(temp,bleft,nbits)   \
        unsigned long long temp;            \
        int bleft, nbits;

#define start_pack_64(temp,bleft)  \
        temp = 0;                  \
        bleft = 32;

#define put_bits_64(unpacked,nbits,temp,bleft,mask)   \
        bleft -= nbits;                               \
        temp = (temp << nbits) | (unpacked & mask)

#define check_pack_64(temp,bleft,packed)  \
        if(bleft <= 0) {                  \
          *packed++ = temp  >> (-bleft);  \
          bleft += 32;                    \
        }

#define end_pack_64(temp,bleft,packed)             \
        if(bleft < 32) *packed++ = temp << bleft;

#endif
        
void IntegerUnpacker_32(void *stream, void *dst,int nbits_in, int nbitt,int n,int offset)
{
    unsigned int *packed = ( unsigned int *)stream;
    int *unpacked = ( int *)dst;
    unsigned long long temp;
    unsigned int token, token1, token2, token3;
    int bleft, mbleft;
    int signed_offset = 0;
    int nbits;

    nbits = (nbits_in < 0) ? -nbits_in : nbits_in;
    if(nbits > 32 || nbitt > nbits) return;

    packed += offset/32;
    bleft = 32 - (offset % 32);
    token = *packed++;
    if(nbits_in < 0) signed_offset = -(1 << (nbitt-1));

    if(bleft == 32) {     /* we are aligned on a 32 bit boundary */
      if(nbits == 32) {
        while(n > 1) {
          *unpacked++ = token WITH_OFFSET;
          token = *packed++;
          *unpacked++ = token WITH_OFFSET;
          token = *packed++;
          n -= 2;
        }
        while(n-- > 0) {
          *unpacked++ = token WITH_OFFSET;
          token = *packed++;
        }
        return;
      }
      if(nbits == 24) {
        while(n > 3){
          token1 = *packed++;
          token2 = *packed++;
          token3 = *packed++;
          *unpacked++ = (token >> 8) WITH_OFFSET;
          *unpacked++ = (((token & 0xFF) << 16) | (token1 >>16)) WITH_OFFSET;
          *unpacked++ = (((token1 & 0xFFFF) << 8)  | (token2 >> 24)) WITH_OFFSET;
          *unpacked++ = (token2 & 0xFFFFFF) WITH_OFFSET;
          token = token3;
          n -= 4;
        }
      }
      if(nbits == 16) {
        while(n > 3){
          *unpacked++ = (token >> 16) WITH_OFFSET;
          *unpacked++ = (token & 0xFFFF) WITH_OFFSET;
          token = *packed++;
          *unpacked++ = (token >> 16) WITH_OFFSET;
          *unpacked++ = (token & 0xFFFF) WITH_OFFSET;
          token = *packed++;
          n -= 4;
        }
      }
      if(nbits == 12) {
        while(n > 7){
          token2 = *packed++;
          token3 = *packed++;
          token1  = *packed++;
          *unpacked++ = ((token >> 20)) WITH_OFFSET;
          *unpacked++ = ((token >> 8) & 0xFFF) WITH_OFFSET;
          *unpacked++ = (((token & 0xFF) << 4) | (token2 >> 28)) WITH_OFFSET;
          *unpacked++ = ((token2 >> 16) & 0xFFF) WITH_OFFSET;
          *unpacked++ = ((token2 >>  4) & 0xFFF) WITH_OFFSET;
          *unpacked++ = (((token2 & 0xF) << 8) | (token3 >> 24)) WITH_OFFSET;
          *unpacked++ = ((token3 >>  12) & 0xFFF) WITH_OFFSET;
          *unpacked++ = (token3 & 0xFFF) WITH_OFFSET;
          token = token1;
          n -= 8;
        }
      }
      if(nbits == 8) {
        while(n > 7){
          *unpacked++ = ((token >> 24)) WITH_OFFSET;
          *unpacked++ = ((token >> 16) & 0xFF) WITH_OFFSET;
          *unpacked++ = ((token >>  8) & 0xFF) WITH_OFFSET;
          *unpacked++ = (token & 0xFF) WITH_OFFSET;
          token = *packed++;
          *unpacked++ = ((token >> 24)) WITH_OFFSET;
          *unpacked++ = ((token >> 16) & 0xFF) WITH_OFFSET;
          *unpacked++ = ((token >>  8) & 0xFF) WITH_OFFSET;
          *unpacked++ = (token & 0xFF) WITH_OFFSET;
          token = *packed++;
          n -= 8;
        }
      }
      if(nbits == 6) {
        while(n > 15){
          token2 = *packed++;
          token3 = *packed++;
          token1  = *packed++;
          *unpacked++ = ((token >> 26)) WITH_OFFSET;
          *unpacked++ = ((token >> 20) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token >> 14) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token >>  8) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token >>  2) & 0x3F) WITH_OFFSET;
          *unpacked++ = (((token & 0x3) << 4) | (token2 >> 28)) WITH_OFFSET;
          *unpacked++ = ((token2 >> 22) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token2 >> 16) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token2 >> 10) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token2 >> 4) & 0x3F) WITH_OFFSET;
          *unpacked++ = (((token2 & 0xF) << 2) | (token3 >> 30)) WITH_OFFSET;
          *unpacked++ = ((token3 >>  24) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token3 >>  18) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token3 >>  12) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token3 >>   6) & 0x3F) WITH_OFFSET;
          *unpacked++ = (token3 & 0x3F) WITH_OFFSET;
          token = token1;
          n -= 16;
        }
      }
      if(nbits == 4) {
        while(n > 7){
          *unpacked++ = ((token >> 28)) WITH_OFFSET;
          *unpacked++ = ((token >> 24) & 0xF) WITH_OFFSET;
          *unpacked++ = ((token >> 20) & 0xF) WITH_OFFSET;
          *unpacked++ = ((token >> 16) & 0xF) WITH_OFFSET;
          *unpacked++ = ((token >> 12) & 0xF) WITH_OFFSET;
          *unpacked++ = ((token >>  8) & 0xF) WITH_OFFSET;
          *unpacked++ = ((token >>  4) & 0xF) WITH_OFFSET;
          *unpacked++ = (token & 0xF) WITH_OFFSET;
          token = *packed++;
          n -= 8;
        }
      }
      if(nbits == 2) {
        while(n > 15){
          *unpacked++ = ((token >> 30)) WITH_OFFSET;
          *unpacked++ = ((token >> 28) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 26) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 24) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 22) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 20) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 18) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 16) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 14) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 12) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 10) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >>  8) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >>  6) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >>  4) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >>  2) & 0x3) WITH_OFFSET;
          *unpacked++ = (token & 0x3) WITH_OFFSET;
          token = *packed++;
          n -= 16;
        }
      }
      if(nbits == 1) {
        while(n > 31){
          *unpacked++ = ((token >> 31) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 30) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 29) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 28) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 27) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 26) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 25) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 24) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 23) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 22) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 21) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 20) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 19) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 18) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 17) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 16) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 15) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 14) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 13) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 12) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 11) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 10) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  9) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  8) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  7) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  6) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  5) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  4) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  3) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  2) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  1) & 1) WITH_OFFSET;
          *unpacked++ = (token  & 1) WITH_OFFSET;
          token = *packed++;
          n -= 32;
        }
      }
    }   /* if bleft == 32 */
    temp = token;
    temp <<= 32;
    temp |= *packed++;
    if(bleft < 32) temp <<= (32-bleft);
    if(nbits > 16) {
      while(n > 1){
        get_bits_64(*unpacked++,nbits,temp,bleft)
        check_unpack_64(temp,bleft,*packed++)
        get_bits_64(*unpacked++,nbits,temp,bleft)
        check_unpack_64(temp,bleft,*packed++)
        n -= 2;
      }
    }else{
      while(n > 1){
        get_bits_64(*unpacked++,nbits,temp,bleft)
        get_bits_64(*unpacked++,nbits,temp,bleft)
        check_unpack_64(temp,bleft,*packed++)
        n -= 2;
      }
    }
    while(n-- > 0){
        get_bits_64(*unpacked++,nbits,temp,bleft)
        check_unpack_64(temp,bleft,*packed++)
    }
}

void IntegerUnpacker_16(void *stream, void *dst,int nbits, int nbitt,int n,int offset)
{
    unsigned int *packed = ( unsigned int *)stream;
    short *unpacked = ( short *)dst;
    unsigned long long temp;
    unsigned int token, token1, token2, token3;
    int bleft;
    int signed_offset = 0;

    if(nbits > 16 || nbitt > nbits) return;

    packed += offset/32;
    bleft = 32 - (offset % 32);
    token = *packed++;
    if(nbits == -1) signed_offset = -(1 << (nbitt-1));

    if(bleft == 32) {
      if(nbits == 16) {
        while(n > 3){
          *unpacked++ = (token >> 16) WITH_OFFSET;
          *unpacked++ = (token & 0xFFFF) WITH_OFFSET;
          token = *packed++;
          *unpacked++ = (token >> 16) WITH_OFFSET;
          *unpacked++ = (token & 0xFFFF) WITH_OFFSET;
          token = *packed++;
          n -= 4;
        }
      }
      if(nbits == 12) {
        while(n > 7){
          token2 = *packed++;
          token3 = *packed++;
          token1  = *packed++;
          *unpacked++ = ((token >> 20)) WITH_OFFSET;
          *unpacked++ = ((token >> 8) & 0xFFF) WITH_OFFSET;
          *unpacked++ = (((token & 0xFF) << 4) | (token2 >> 28)) WITH_OFFSET;
          *unpacked++ = ((token2 >> 16) & 0xFFF) WITH_OFFSET;
          *unpacked++ = ((token2 >>  4) & 0xFFF) WITH_OFFSET;
          *unpacked++ = (((token2 & 0xF) << 8) | (token3 >> 24)) WITH_OFFSET;
          *unpacked++ = ((token3 >>  12) & 0xFFF) WITH_OFFSET;
          *unpacked++ = (token3 & 0xFFF) WITH_OFFSET;
          token = token1;
          n -= 8;
        }
      }
      if(nbits == 8) {
        while(n > 7){
          *unpacked++ = ((token >> 24)) WITH_OFFSET;
          *unpacked++ = ((token >> 16) & 0xFF) WITH_OFFSET;
          *unpacked++ = ((token >>  8) & 0xFF) WITH_OFFSET;
          *unpacked++ = (token & 0xFF) WITH_OFFSET;
          token = *packed++;
          *unpacked++ = ((token >> 24)) WITH_OFFSET;
          *unpacked++ = ((token >> 16) & 0xFF) WITH_OFFSET;
          *unpacked++ = ((token >>  8) & 0xFF) WITH_OFFSET;
          *unpacked++ = (token & 0xFF) WITH_OFFSET;
          token = *packed++;
          n -= 8;
        }
      }
      if(nbits == 6) {
        while(n > 15){
          token2 = *packed++;
          token3 = *packed++;
          token1  = *packed++;
          *unpacked++ = ((token >> 26)) WITH_OFFSET;
          *unpacked++ = ((token >> 20) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token >> 14) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token >>  8) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token >>  2) & 0x3F) WITH_OFFSET;
          *unpacked++ = (((token & 0x3) << 4) | (token2 >> 28)) WITH_OFFSET;
          *unpacked++ = ((token2 >> 22) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token2 >> 16) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token2 >> 10) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token2 >> 4) & 0x3F) WITH_OFFSET;
          *unpacked++ = (((token2 & 0xF) << 2) | (token3 >> 30)) WITH_OFFSET;
          *unpacked++ = ((token3 >>  24) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token3 >>  18) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token3 >>  12) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token3 >>   6) & 0x3F) WITH_OFFSET;
          *unpacked++ = (token3 & 0x3F) WITH_OFFSET;
          token = token1;
          n -= 16;
        }
      }
      if(nbits == 4) {
        while(n > 7){
          *unpacked++ = ((token >> 28)) WITH_OFFSET;
          *unpacked++ = ((token >> 24) & 0xF) WITH_OFFSET;
          *unpacked++ = ((token >> 20) & 0xF) WITH_OFFSET;
          *unpacked++ = ((token >> 16) & 0xF) WITH_OFFSET;
          *unpacked++ = ((token >> 12) & 0xF) WITH_OFFSET;
          *unpacked++ = ((token >>  8) & 0xF) WITH_OFFSET;
          *unpacked++ = ((token >>  4) & 0xF) WITH_OFFSET;
          *unpacked++ = (token & 0xF) WITH_OFFSET;
          token = *packed++;
          n -= 8;
        }
      }
      if(nbits == 2) {
        while(n > 15){
          *unpacked++ = ((token >> 30)) WITH_OFFSET;
          *unpacked++ = ((token >> 28) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 26) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 24) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 22) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 20) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 18) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 16) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 14) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 12) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 10) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >>  8) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >>  6) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >>  4) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >>  2) & 0x3) WITH_OFFSET;
          *unpacked++ = (token & 0x3) WITH_OFFSET;
          token = *packed++;
          n -= 16;
        }
      }
      if(nbits == 1) {
        while(n > 31){
          *unpacked++ = ((token >> 31) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 30) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 29) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 28) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 27) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 26) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 25) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 24) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 23) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 22) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 21) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 20) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 19) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 18) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 17) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 16) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 15) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 14) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 13) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 12) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 11) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 10) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  9) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  8) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  7) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  6) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  5) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  4) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  3) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  2) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  1) & 1) WITH_OFFSET;
          *unpacked++ = (token  & 1) WITH_OFFSET;
          token = *packed++;
          n -= 32;
        }
      }
    }   /* if bleft == 32 */
    temp = token;
    temp <<= 32;
    temp |= *packed++;
    if(bleft < 32) temp <<= (32-bleft);
    while(n > 1){
        get_bits_64(*unpacked++,nbits,temp,bleft)
        get_bits_64(*unpacked++,nbits,temp,bleft)
        check_unpack_64(temp,bleft,*packed++)
      n -= 2;
    }
    while(n-- > 0){
        get_bits_64(*unpacked++,nbits,temp,bleft)
        check_unpack_64(temp,bleft,*packed++)
    }
}

void IntegerUnpacker_8(void *stream, void *dst,int nbits, int nbitt,int n,int offset)
{
    unsigned int *packed = ( unsigned int *)stream;
    char *unpacked = ( char *)dst;
    unsigned long long temp;
    unsigned int token, token1, token2, token3;
    int bleft;
    int signed_offset = 0;

    if(nbits > 8 || nbitt > nbits) return;

    packed += offset/32;
    bleft = 32 - (offset % 32);
    token = *packed++;
    if(nbits == -1) signed_offset = -(1 << (nbitt-1));

    if(bleft == 32){
      if(nbits == 8) {
        while(n > 7){
          *unpacked++ = ((token >> 24)) WITH_OFFSET;
          *unpacked++ = ((token >> 16) & 0xFF) WITH_OFFSET;
          *unpacked++ = ((token >>  8) & 0xFF) WITH_OFFSET;
          *unpacked++ = (token & 0xFF) WITH_OFFSET;
          token = *packed++;
          *unpacked++ = ((token >> 24)) WITH_OFFSET;
          *unpacked++ = ((token >> 16) & 0xFF) WITH_OFFSET;
          *unpacked++ = ((token >>  8) & 0xFF) WITH_OFFSET;
          *unpacked++ = (token & 0xFF) WITH_OFFSET;
          token = *packed++;
          n -= 8;
        }
      }
      if(nbits == 6) {
        while(n > 15){
          token2 = *packed++;
          token3 = *packed++;
          token1  = *packed++;
          *unpacked++ = ((token >> 26)) WITH_OFFSET;
          *unpacked++ = ((token >> 20) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token >> 14) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token >>  8) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token >>  2) & 0x3F) WITH_OFFSET;
          *unpacked++ = (((token & 0x3) << 4) | (token2 >> 28)) WITH_OFFSET;
          *unpacked++ = ((token2 >> 22) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token2 >> 16) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token2 >> 10) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token2 >> 4) & 0x3F) WITH_OFFSET;
          *unpacked++ = (((token2 & 0xF) << 2) | (token3 >> 30)) WITH_OFFSET;
          *unpacked++ = ((token3 >>  24) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token3 >>  18) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token3 >>  12) & 0x3F) WITH_OFFSET;
          *unpacked++ = ((token3 >>   6) & 0x3F) WITH_OFFSET;
          *unpacked++ = (token3 & 0x3F) WITH_OFFSET;
          token = token1;
          n -= 16;
        }
      }
      if(nbits == 4) {
        while(n > 7){
          *unpacked++ = ((token >> 28)) WITH_OFFSET;
          *unpacked++ = ((token >> 24) & 0xF) WITH_OFFSET;
          *unpacked++ = ((token >> 20) & 0xF) WITH_OFFSET;
          *unpacked++ = ((token >> 16) & 0xF) WITH_OFFSET;
          *unpacked++ = ((token >> 12) & 0xF) WITH_OFFSET;
          *unpacked++ = ((token >>  8) & 0xF) WITH_OFFSET;
          *unpacked++ = ((token >>  4) & 0xF) WITH_OFFSET;
          *unpacked++ = (token & 0xF) WITH_OFFSET;
          token = *packed++;
          n -= 8;
        }
      }
      if(nbits == 2) {
        while(n > 15){
          *unpacked++ = ((token >> 30)) WITH_OFFSET;
          *unpacked++ = ((token >> 28) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 26) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 24) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 22) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 20) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 18) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 16) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 14) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 12) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >> 10) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >>  8) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >>  6) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >>  4) & 0x3) WITH_OFFSET;
          *unpacked++ = ((token >>  2) & 0x3) WITH_OFFSET;
          *unpacked++ = (token & 0x3) WITH_OFFSET;
          token = *packed++;
          n -= 16;
        }
      }
      if(nbits == 1) {
        while(n > 31){
          *unpacked++ = ((token >> 31) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 30) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 29) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 28) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 27) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 26) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 25) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 24) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 23) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 22) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 21) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 20) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 19) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 18) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 17) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 16) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 15) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 14) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 13) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 12) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 11) & 1) WITH_OFFSET;
          *unpacked++ = ((token >> 10) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  9) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  8) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  7) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  6) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  5) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  4) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  3) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  2) & 1) WITH_OFFSET;
          *unpacked++ = ((token >>  1) & 1) WITH_OFFSET;
          *unpacked++ = (token  & 1) WITH_OFFSET;
          token = *packed++;
          n -= 32;
        }
      }
    }   /* if bleft == 32 */
    temp = token;
    temp <<= 32;
    temp |= *packed++;
    if(bleft < 32) temp <<= (32-bleft);
    while(n > 3){
      *unpacked++ = (temp >> (64 - nbits)) WITH_OFFSET;
      temp <<= nbits;
      bleft -= nbits;
      *unpacked++ = (temp >> (64 - nbits)) WITH_OFFSET;
      temp <<= nbits;
      bleft -= nbits;
      *unpacked++ = (temp >> (64 - nbits)) WITH_OFFSET;
      temp <<= nbits;
      bleft -= nbits;
      *unpacked++ = (temp >> (64 - nbits)) WITH_OFFSET;
      temp <<= nbits;
      bleft -= nbits;
      if(bleft <= 0) {
        token = *packed++;
        if(bleft == 0){
          temp |= token;
        }else{
          temp = temp >> (-bleft) ;
          temp |= token;
          temp <<= (- bleft);
        }
        bleft += 32;
      }
      n -= 4;
    }
    while(n-- > 0){
      *unpacked++ = (temp >> (64 - nbits)) WITH_OFFSET;
      temp <<= nbits;
      bleft -= nbits;
      if(bleft <= 0) {
        token = *packed++;
        if(bleft == 0){
          temp |= token;
        }else{
          temp = temp >> (-bleft) ;
          temp |= token;
          temp <<= (- bleft);
        }
        bleft += 32;
      }
    }
}

void IntegerPacker_32(void *stream, void *src,int nbits, int nbitt,int n,int offset)
{
    unsigned int *packed = ( unsigned int *)stream;
    unsigned int *unpacked = ( unsigned int *)src;
    unsigned long long temp;
    unsigned int maskk, token, token1, token2, token3, token4, token5, token6, token7, token8;
    int bleft, i;

    if(nbits > 32 || nbitt > nbits) return;
    packed += offset/32;
    bleft = 32 - (offset % 32);
    maskk = 0xFFFFFFFF;
    if(nbitt < 32) maskk = ~(0xFFFFFFFF << nbitt);

    if(bleft == 32){     /* we are aligned on a 32 bit boundary */
      const unsigned int mask = maskk;
      if(nbits == 32) {
        packed = (unsigned int *)memcpy(stream,src,n*sizeof(unsigned int));
        return;
      }
      temp = 0;
      if(nbits == 24){
        while(n > 3){
          token1 = (*unpacked++) & mask;
          token2 = (*unpacked++) & mask;
          token3 = (*unpacked++) & mask;
          token4 = (*unpacked++) & mask;
          *packed++ = (token1 << 8) | (token2 >> 16);
          *packed++ = (token2 << 16) | (token3 >> 8);
          *packed++ = (token3 << 24) | token4;
          n -= 4;
        }
      }
      if(nbits == 16){
        while(n > 3){
          token1 = (*unpacked++) & mask;
          token2 = (*unpacked++) & mask;
          token3 = (*unpacked++) & mask;
          token4 = (*unpacked++) & mask;
          *packed++ = (token1 << 16) | token2;
          *packed++ = (token3 << 16) | token4;
          n -= 4;
        }
      }
      if(nbits == 12){
        while(n > 7){
          token1 = (*unpacked++) & mask;
          token2 = (*unpacked++) & mask;
          token3 = (*unpacked++) & mask;
          token4 = (*unpacked++) & mask;
          *packed++ = (token1 << 20) | (token2 << 8) | (token3 >> 4);
          token1 = (*unpacked++) & mask;
          token2 = (*unpacked++) & mask;
          *packed++ = (token3 << 28) | (token4 << 16) | (token1 << 4) | (token2 >> 8);
          token3 = (*unpacked++) & mask;
          token4 = (*unpacked++) & mask;
          *packed++ = (token2 << 24) |  (token3 << 12) | token4;
          n -= 8;
        }
      }
      if(nbits == 8){
        while(n > 7){
          token1 = (*unpacked++) & mask;
          token2 = (*unpacked++) & mask;
          token3 = (*unpacked++) & mask;
          token4 = (*unpacked++) & mask;
          *packed++ = (token1 << 24) | (token2 << 16) | (token3 << 8) | token4;
          token1 = (*unpacked++) & mask;
          token2 = (*unpacked++) & mask;
          token3 = (*unpacked++) & mask;
          token4 = (*unpacked++) & mask;
          *packed++ = (token1 << 24) |  (token2 << 16) | (token3 << 8) | token4;
          n -= 8;
        }
      }
      if(nbits == 6){
        while(n > 15){
          token1 = (*unpacked++) & mask;
          token2 = (*unpacked++) & mask;
          token3 = (*unpacked++) & mask;
          token4 = (*unpacked++) & mask;
          token5 = (*unpacked++) & mask;
          token6 = (*unpacked++) & mask;
          token7 = (*unpacked++) & mask;
          token8 = (*unpacked++) & mask;
          *packed++ = (token1 << 26) | (token2 << 20) | (token3 << 14) | (token4 << 8) | (token5 << 2) | (token6 >> 4);
          token1 = (*unpacked++) & mask;
          token2 = (*unpacked++) & mask;
          token3 = (*unpacked++) & mask;
          *packed++ = (token6 << 28) | (token7 << 22) | (token8 << 16) |  (token1 << 10) | (token2 << 4) | (token3 >> 2);
          token4 = (*unpacked++) & mask;
          token5 = (*unpacked++) & mask;
          token6 = (*unpacked++) & mask;
          token7 = (*unpacked++) & mask;
          token8 = (*unpacked++) & mask;
          *packed++ = (token3 << 30) | (token4 << 24) |  (token5 << 18) | (token6 << 12) |  (token7 << 6) | token8;
          n -= 16;
        }
      }
      if(nbits == 4){
        while(n > 7){
          token = (*unpacked++) & mask;
          token = (token << 4) | ((*unpacked++) & mask);
          token = (token << 4) | ((*unpacked++) & mask);
          token = (token << 4) | ((*unpacked++) & mask);
          token = (token << 4) | ((*unpacked++) & mask);
          token = (token << 4) | ((*unpacked++) & mask);
          token = (token << 4) | ((*unpacked++) & mask);
          *packed++ = (token << 4) | ((*unpacked++) & mask);
          n -= 8;
        }
      }
      if(nbits == 2){
        while(n > 15){
          token = (*unpacked++) & mask;
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          *packed++ = (token << 2) | ((*unpacked++) & mask);
          n -= 16;
        }
      }
      if(nbits == 1){
        while(n > 31){
          token = 0;
          i = 8;
          while(i-- != 0) {
            token = (token << 1) | ((*unpacked++) & 1);
            token = (token << 1) | ((*unpacked++) & 1);
            token = (token << 1) | ((*unpacked++) & 1);
            token = (token << 1) | ((*unpacked++) & 1);
          }
          *packed++ = token;
          n -= 32;
        }
      }
    }  /* if bleft == 32 */
    if(bleft < 32) temp = *packed >> bleft;
    {
    const unsigned int mask = maskk;
    while(n > 1){
      token = (*unpacked++) & mask;
      bleft -= nbits;
      temp = (temp << nbits) | token;
      if(bleft <= 0) {
        if(bleft < 0) temp >>= (-bleft);
        *packed++ = temp;
        bleft += 32;
        temp = token;
      }
      token = (*unpacked++) & mask;
      bleft -= nbits;
      temp = (temp << nbits) | token;
      if(bleft <= 0) {
        if(bleft < 0) temp >>= (-bleft);
        *packed++ = temp;
        bleft += 32;
        temp = token;
      }
      n -= 2;
    }
    while(n-- > 0){
      token = (*unpacked++) & mask;
      bleft -= nbits;
      temp = (temp << nbits) | token;
      if(bleft <= 0) {
        if(bleft < 0) temp >>= (-bleft);
        *packed++ = temp;
        bleft += 32;
        temp = token;
      }
    }
    }
    if(bleft < 32) *packed = (temp << bleft);
}

void IntegerPacker_16(void *stream, void *src,int nbits, int nbitt,int n,int offset)
{
    unsigned int *packed = ( unsigned int *)stream;
    unsigned short *unpacked = ( unsigned short *)src;
    unsigned long long temp;
    unsigned int mask, token, token1, token2, token3, token4, token5, token6, token7, token8;
    int bleft, i;

    if(nbits > 16 || nbitt > nbits) return;
    packed += offset/32;
    bleft = 32 - (offset % 32);
    temp = 0;
    mask = ~(0xFFFFFFFF << nbitt);
    if(bleft == 32) {
      if(nbits == 16){
        while(n > 3){
          token1 = (*unpacked++) & mask;
          token2 = (*unpacked++) & mask;
          token3 = (*unpacked++) & mask;
          token4 = (*unpacked++) & mask;
          *packed++ = (token1 << 16) | token2;
          *packed++ = (token3 << 16) | token4;
          n -= 4;
        }
      }
      if(nbits == 12){
        while(n > 7){
          token1 = (*unpacked++) & mask;
          token2 = (*unpacked++) & mask;
          token3 = (*unpacked++) & mask;
          token4 = (*unpacked++) & mask;
          *packed++ = (token1 << 20) | (token2 << 8) | (token3 >> 4);
          token1 = (*unpacked++) & mask;
          token2 = (*unpacked++) & mask;
          *packed++ = (token3 << 28) | (token4 << 16) | (token1 << 4) | (token2 >> 8);
          token3 = (*unpacked++) & mask;
          token4 = (*unpacked++) & mask;
          *packed++ = (token2 << 24) |  (token3 << 12) | token4;
          n -= 8;
        }
      }
      if(nbits == 8){
        while(n > 7){
          token1 = (*unpacked++) & mask;
          token2 = (*unpacked++) & mask;
          token3 = (*unpacked++) & mask;
          token4 = (*unpacked++) & mask;
          *packed++ = (token1 << 24) | (token2 << 16) | (token3 << 8) | token4;
          token1 = (*unpacked++) & mask;
          token2 = (*unpacked++) & mask;
          token3 = (*unpacked++) & mask;
          token4 = (*unpacked++) & mask;
          *packed++ = (token1 << 24) |  (token2 << 16) | (token3 << 8) | token4;
          n -= 8;
        }
      }
      if(nbits == 6){
        while(n > 15){
          token1 = (*unpacked++) & mask;
          token2 = (*unpacked++) & mask;
          token3 = (*unpacked++) & mask;
          token4 = (*unpacked++) & mask;
          token5 = (*unpacked++) & mask;
          token6 = (*unpacked++) & mask;
          token7 = (*unpacked++) & mask;
          token8 = (*unpacked++) & mask;
          *packed++ = (token1 << 26) | (token2 << 20) | (token3 << 14) | (token4 << 8) | (token5 << 2) | (token6 >> 4);
          token1 = (*unpacked++) & mask;
          token2 = (*unpacked++) & mask;
          token3 = (*unpacked++) & mask;
          *packed++ = (token6 << 28) | (token7 << 22) | (token8 << 16) |  (token1 << 10) | (token2 << 4) | (token3 >> 2);
          token4 = (*unpacked++) & mask;
          token5 = (*unpacked++) & mask;
          token6 = (*unpacked++) & mask;
          token7 = (*unpacked++) & mask;
          token8 = (*unpacked++) & mask;
          *packed++ = (token3 << 30) | (token4 << 24) |  (token5 << 18) | (token6 << 12) |  (token7 << 6) | token8;
          n -= 16;
        }
      }
      if(nbits == 4){
        while(n > 7){
          token = (*unpacked++) & mask;
          token = (token << 4) | ((*unpacked++) & mask);
          token = (token << 4) | ((*unpacked++) & mask);
          token = (token << 4) | ((*unpacked++) & mask);
          token = (token << 4) | ((*unpacked++) & mask);
          token = (token << 4) | ((*unpacked++) & mask);
          token = (token << 4) | ((*unpacked++) & mask);
          *packed++ = (token << 4) | ((*unpacked++) & mask);
          n -= 8;
        }
      }
      if(nbits == 2){
        while(n > 15){
          token = (*unpacked++) & mask;
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          *packed++ = (token << 2) | ((*unpacked++) & mask);
          n -= 16;
        }
      }
      if(nbits == 1){
        while(n > 31){
          token = 0;
          i = 8;
          while(i-- != 0) {
            token = (token << 1) | ((*unpacked++) & 1);
            token = (token << 1) | ((*unpacked++) & 1);
            token = (token << 1) | ((*unpacked++) & 1);
            token = (token << 1) | ((*unpacked++) & 1);
          }
          *packed++ = token;
          n -= 32;
        }
      }
    }   /* if bleft == 32 */
    if(bleft < 32) temp = *packed >> bleft;
    while(n > 1){
      token = (*unpacked++) & mask;
      bleft -= nbits;
      temp = (temp << nbits) | token;
      if(bleft <= 0) {
        if(bleft < 0) temp >>= (-bleft);
        *packed++ = temp;
        bleft += 32;
        temp = token;
      }
      token = (*unpacked++) & mask;
      bleft -= nbits;
      temp = (temp << nbits) | token;
      if(bleft <= 0) {
        if(bleft < 0) temp >>= (-bleft);
        *packed++ = temp;
        bleft += 32;
        temp = token;
      }
      n -= 2;
    }
    while(n-- > 0){
      token = (*unpacked++) & mask;
      bleft -= nbits;
      temp = (temp << nbits) | token;
      if(bleft <= 0) {
        if(bleft < 0) temp >>= (-bleft);
        *packed++ = temp;
        bleft += 32;
        temp = token;
      }
    }
    if(bleft < 32) *packed = (temp << bleft);
}

void IntegerPacker_8(void *stream, void *src,int nbits, int nbitt,int n,int offset)
{
    unsigned int *packed = ( unsigned int *)stream;
    unsigned char *unpacked = ( unsigned char *)src;
    unsigned long long temp;
    unsigned int mask, token, token1, token2, token3, token4, token5, token6, token7, token8;
    int bleft, i;

    if(nbits > 8 || nbitt > nbits) return;
    packed += offset/32;
    bleft = 32 - (offset % 32);
    temp = 0;
    mask = ~(0xFFFFFFFF << nbitt);
    if(bleft == 32) {
      if(nbits == 8){
        while(n > 7){
          token1 = (*unpacked++) & mask;
          token2 = (*unpacked++) & mask;
          token3 = (*unpacked++) & mask;
          token4 = (*unpacked++) & mask;
          *packed++ = (token1 << 24) | (token2 << 16) | (token3 << 8) | token4;
          token1 = (*unpacked++) & mask;
          token2 = (*unpacked++) & mask;
          token3 = (*unpacked++) & mask;
          token4 = (*unpacked++) & mask;
          *packed++ = (token1 << 24) |  (token2 << 16) | (token3 << 8) | token4;
          n -= 8;
        }
      }
      if(nbits == 6){
        while(n > 15){
          token1 = (*unpacked++) & mask;
          token2 = (*unpacked++) & mask;
          token3 = (*unpacked++) & mask;
          token4 = (*unpacked++) & mask;
          token5 = (*unpacked++) & mask;
          token6 = (*unpacked++) & mask;
          token7 = (*unpacked++) & mask;
          token8 = (*unpacked++) & mask;
          *packed++ = (token1 << 26) | (token2 << 20) | (token3 << 14) | (token4 << 8) | (token5 << 2) | (token6 >> 4);
          token1 = (*unpacked++) & mask;
          token2 = (*unpacked++) & mask;
          token3 = (*unpacked++) & mask;
          *packed++ = (token6 << 28) | (token7 << 22) | (token8 << 16) |  (token1 << 10) | (token2 << 4) | (token3 >> 2);
          token4 = (*unpacked++) & mask;
          token5 = (*unpacked++) & mask;
          token6 = (*unpacked++) & mask;
          token7 = (*unpacked++) & mask;
          token8 = (*unpacked++) & mask;
          *packed++ = (token3 << 30) | (token4 << 24) |  (token5 << 18) | (token6 << 12) |  (token7 << 6) | token8;
          n -= 16;
        }
      }
      if(nbits == 4){
        while(n > 7){
          token = (*unpacked++) & mask;
          token = (token << 4) | ((*unpacked++) & mask);
          token = (token << 4) | ((*unpacked++) & mask);
          token = (token << 4) | ((*unpacked++) & mask);
          token = (token << 4) | ((*unpacked++) & mask);
          token = (token << 4) | ((*unpacked++) & mask);
          token = (token << 4) | ((*unpacked++) & mask);
          *packed++ = (token << 4) | ((*unpacked++) & mask);
          n -= 8;
        }
      }
      if(nbits == 2){
        while(n > 15){
          token = (*unpacked++) & mask;
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          token = (token << 2) | ((*unpacked++) & mask);
          *packed++ = (token << 2) | ((*unpacked++) & mask);
          n -= 16;
        }
      }
      if(nbits == 1){
        while(n > 31){
          token = 0;
          i = 8;
          while(i-- != 0) {
            token = (token << 1) | ((*unpacked++) & 1);
            token = (token << 1) | ((*unpacked++) & 1);
            token = (token << 1) | ((*unpacked++) & 1);
            token = (token << 1) | ((*unpacked++) & 1);
          }
          *packed++ = token;
          n -= 32;
        }
      }
    }   /* if bleft == 32 */
    if(bleft < 32) temp = *packed >> bleft;
    while(n > 2){
      token = (*unpacked++) & mask;
      bleft -= nbits;
      temp = (temp << nbits) | token;
      if(bleft <= 0) {
        if(bleft < 0) temp >>= (-bleft);
        *packed++ = temp;
        bleft += 32;
        temp = token;
      }
      token = (*unpacked++) & mask;
      bleft -= nbits;
      temp = (temp << nbits) | token;
      if(bleft <= 0) {
        if(bleft < 0) temp >>= (-bleft);
        *packed++ = temp;
        bleft += 32;
        temp = token;
      }
      n -= 2;
    }
    while(n-- > 0){
      token = (*unpacked++) & mask;
      bleft -= nbits;
      temp = (temp << nbits) | token;
      if(bleft <= 0) {
        if(bleft < 0) temp >>= (-bleft);
        *packed++ = temp;
        bleft += 32;
        temp = token;
      }
    }
    if(bleft < 32) *packed = (temp << bleft);
}

#ifdef SELF_TEST
int mytest(int NDATA, int OFFSET);
#define NREP 10
int main(){
  int status;
//  status = mytest(4095,0);
//  if(status != 0) exit(1);
  status = mytest(1200*800+3,0);
  if(status != 0) exit(1);
  status = mytest(1200*800+3,13);
  if(status != 0) exit(1);
  return 0;
}
int mytest(int NDATA, int OFFSET)
{
  int nbits;
  unsigned int packed[NDATA+10];
  unsigned int unpacked[NDATA];
  unsigned short unpacked_s[NDATA];
  unsigned char unpacked_c[NDATA];
  unsigned int ref[NDATA];
  unsigned short ref_s[NDATA];
  unsigned char ref_c[NDATA];
  int i, j, errors;
  struct timeval t0, t1;
  long long time0, time1;
  int tm1, tm2, tm1a, tm2a;
  int limit;
  unsigned int min, max, offset;
  int nbytes=NDATA*4;

  fprintf(stderr,"============== data size = %d,  offset = %d\n",NDATA,OFFSET);
  for( nbits = 1 ; nbits <= 32 ; nbits++) {
    if(nbits > 1 && (nbits & 1) == 1) nbits++;
    if(nbits > 16 && (nbits & 2) == 2) nbits+=2;
    if(nbits < 17) limit = 1 << nbits;
    if(nbits > 16) limit = 1 << 16;
    max = 0;
    min = 0xFFFFFFFF;
    offset = 0xFFFF;
    if(nbits > 16) offset >>= 32 - nbits;
    for ( i = 0 ; i < NDATA ; i++ ){
      ref[i] = i % limit;
      if(nbits > 16 && ref[i] != 0) ref[i] = (ref[i] << (nbits-16)) | offset;
      min = min > ref[i] ? ref[i] : min;
      max = max < ref[i] ? ref[i] : max;
      unpacked[i] = ref[i];
    }
    fprintf(stderr,"%8.8x %8.8x ",min,max);
    for ( i = 0 ; i < NDATA ; i++ )packed[i]=0x12345678;

    tm1 = 1000000;
    tm1a = 0;
    for(i=0 ; i<NREP;i++){
      errors = gettimeofday(&t0,NULL);
      IntegerPacker_32(packed, ref, nbits, nbits, NDATA,OFFSET);
      errors = gettimeofday(&t1,NULL);
      time0 = t0.tv_sec;
      time0 = time0*1000000 + t0.tv_usec;
      time1 = t1.tv_sec;
      time1 = time1*1000000 + t1.tv_usec;
      time1 = time1 - time0;
      if(time1 < tm1) tm1 = time1;
      if(time1 > tm1a) tm1a = time1;
    }

    tm2 = 1000000;
    tm2a = 0;
    for(i=0 ; i<NREP;i++){
      for ( j = 0 ; j < NDATA ; j++ )unpacked[j]=0x12345678;
      errors = gettimeofday(&t0,NULL);
      IntegerUnpacker_32(packed, unpacked, nbits, nbits, NDATA,OFFSET);
      errors = gettimeofday(&t1,NULL);
      time0 = t0.tv_sec;
      time0 = time0*1000000 + t0.tv_usec;
      time1 = t1.tv_sec;
      time1 = time1*1000000 + t1.tv_usec;
      time1 = time1 - time0;
      if(time1 < tm2) tm2 = time1;
      if(time1 > tm2a) tm2a = time1;
    }
    errors = 0;
    for ( i = 0 ; i < NDATA ; i++ ){
      if(unpacked[i] != ref[i]){
        errors++;
//        fprintf(stderr," %d",i);
      }
    }
//    if(errors >= 1)fprintf(stderr,"\n");
    fprintf(stderr,"nbits = %2d(%2d), errors = %d, pack = %4d %4d GB/s, unpack = %4d %4d GB/s , pack-unpack= %4d usec\n",nbits,OFFSET,errors,tm1,nbytes/tm1,tm2,nbytes/tm2,tm1-tm2);
    if(errors > 0) return errors;
  }

  nbytes /= 2;
  for( nbits = 1 ; nbits <= 16 ; nbits++) {
    if(nbits > 1 && (nbits & 1) == 1) nbits++;
    limit = 1 << nbits;
    max = 0;
    min = 0xFFFFFFFF;
    for ( i = 0 ; i < NDATA ; i++ ){
      ref[i] = i % limit;
      ref_s[i] = ref[i];
      min = min > ref[i] ? ref[i] : min;
      max = max < ref[i] ? ref[i] : max;
      unpacked_s[i] = ref_s[i];
    }
    fprintf(stderr,"%8.8x %8.8x ",min,max);
    for ( i = 0 ; i < NDATA ; i++ )packed[i]=0x12345678;

    tm1 = 1000000;
    tm1a = 0;
    for(i=0 ; i<NREP;i++){
      errors = gettimeofday(&t0,NULL);
      IntegerPacker_16(packed, ref_s, nbits, nbits, NDATA,OFFSET);
      errors = gettimeofday(&t1,NULL);
      time0 = t0.tv_sec;
      time0 = time0*1000000 + t0.tv_usec;
      time1 = t1.tv_sec;
      time1 = time1*1000000 + t1.tv_usec;
      time1 = time1 - time0;
      if(time1 < tm1) tm1 = time1;
      if(time1 > tm1a) tm1a = time1;
    }
    tm2 = 1000000;
    tm2a = 0;
    for(i=0 ; i<NREP;i++){
      for ( j = 0 ; j < NDATA ; j++ )unpacked_s[j]=0x5678;
      errors = gettimeofday(&t0,NULL);
      IntegerUnpacker_16(packed, unpacked_s, nbits, nbits, NDATA,OFFSET);
      errors = gettimeofday(&t1,NULL);
      time0 = t0.tv_sec;
      time0 = time0*1000000 + t0.tv_usec;
      time1 = t1.tv_sec;
      time1 = time1*1000000 + t1.tv_usec;
      time1 = time1 - time0;
      if(time1 < tm2) tm2 = time1;
      if(time1 > tm2a) tm2a = time1;
    }
    errors = 0;
    for ( i = 0 ; i < NDATA ; i++ ){
      if(unpacked_s[i] != ref[i]){
        errors++;
//        fprintf(stderr," %d %d %d %d |",i,ref[i],ref_s[i],unpacked_s[i]);
      }
    }
//    if(errors >= 1)fprintf(stderr,"\n");
    fprintf(stderr,"nbits = %2d(%2d), errors = %d, pack = %4d %4d GB/s, unpack = %4d %4d GB/s , pack-unpack= %4d usec\n",nbits,OFFSET,errors,tm1,nbytes/tm1,tm2,nbytes/tm2,tm1-tm2);
    if(errors > 0) return errors;
  }

  nbytes /= 2;
  for( nbits = 1 ; nbits <= 8 ; nbits++) {
    limit = 1 << nbits;
    max = 0;
    min = 0xFFFFFFFF;
    for ( i = 0 ; i < NDATA ; i++ ){
      ref[i] = i % limit;
      ref_c[i] = ref[i];
      min = min > ref[i] ? ref[i] : min;
      max = max < ref[i] ? ref[i] : max;
      unpacked_c[i] = ref_c[i];
    }
    fprintf(stderr,"%8.8x %8.8x ",min,max);
    for ( i = 0 ; i < NDATA ; i++ )packed[i]=0x12345678;

    tm1 = 1000000;
    tm1a = 0;
    for(i=0 ; i<NREP;i++){
      errors = gettimeofday(&t0,NULL);
      IntegerPacker_8(packed, ref_c, nbits, nbits, NDATA,OFFSET);
      errors = gettimeofday(&t1,NULL);
      time0 = t0.tv_sec;
      time0 = time0*1000000 + t0.tv_usec;
      time1 = t1.tv_sec;
      time1 = time1*1000000 + t1.tv_usec;
      time1 = time1 - time0;
      if(time1 < tm1) tm1 = time1;
      if(time1 > tm1a) tm1a = time1;
    }

    tm2 = 1000000;
    tm2a = 0;
    for(i=0 ; i<NREP;i++){
      for ( j = 0 ; j < NDATA ; j++ )unpacked_c[j]=0x78;
      errors = gettimeofday(&t0,NULL);
      IntegerUnpacker_8(packed, unpacked_c, nbits, nbits, NDATA,OFFSET);
      errors = gettimeofday(&t1,NULL);
      time0 = t0.tv_sec;
      time0 = time0*1000000 + t0.tv_usec;
      time1 = t1.tv_sec;
      time1 = time1*1000000 + t1.tv_usec;
      time1 = time1 - time0;
      if(time1 < tm2) tm2 = time1;
      if(time1 > tm2a) tm2a = time1;
    }
    errors = 0;
    for ( i = 0 ; i < NDATA ; i++ ){
      if(unpacked_c[i] != ref_c[i]){
        errors++;
//        fprintf(stderr," %d",i);
      }
    }
//    if(errors >= 1)fprintf(stderr,"\n");
    fprintf(stderr,"nbits = %2d(%2d), errors = %d, pack = %4d %4d GB/s, unpack = %4d %4d GB/s , pack-unpack= %4d usec\n",nbits,OFFSET,errors,tm1,nbytes/tm1,tm2,nbytes/tm2,tm1-tm2);
    if(errors > 0) return errors;
  }
  return 0;
}
#endif
