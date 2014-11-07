
#define declare_pack_64(temp,bleft,nbits)   \
        unsigned long long temp;            \
        int bleft, nbits;

#define start_unpack_64(temp,bleft,packed,offset)  \
        temp = 8 * sizeof(*packed);                \
        packed = packed + ( offset / temp );       \
        bleft = temp - ( offset % temp );          \
        temp = *packed++;                          \
        temp <<= 32;                               \
        temp |= *packed++;                         \
        temp <<= (32-bleft);

#define get_bits_64(unpacked,nbits,temp,bleft) \
        unpacked = (temp >> (64 - nbits)) WITH_OFFSET; \
        temp <<= nbits;                                \
        bleft -= nbits;
        
#define get_bits_64_signed(unpacked,nbits,temp,bleft)   \
        unpacked = (temp >> 32)   ;                     \
        temp <<= nbits;                                 \
        unpacked = unpacked >> (32 - nbits)  ;          \
        bleft -= nbits;


#define check_unpack_64(temp,bleft,packed) \
        if(bleft <= 0) {                   \
          temp = temp >> (-bleft) ;        \
          temp |= *packed++;               \
          temp <<= (-bleft);               \
          bleft += 32;                     \
        }

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

