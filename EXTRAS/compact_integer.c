/* RMNLIB - Library of useful routines for C and FORTRAN programming
 * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
 *                          Environnement Canada
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/types.h>
#include <rmnlib.h>

typedef struct
    {
#if defined(Little_Endian)
      word numOfBitsPerToken : 6, SHIFT : 6, unused : 12, ID : 8;
#else
      word ID : 8, unused : 12, SHIFT : 6, numOfBitsPerToken : 6;
#endif
      word numOfPackedToken : 32;
      word minValue         : 32;
      word maxValue         : 32;
}integer_header;

#define CommonDeclarations \
    int wordSize;     \
    word cleanupMask;     \
    int minSignedInteger=0, maxSignedInteger=0;     \
    word minUnsignedInteger=0, maxUnsignedInteger=0;     \
    word maxRange;     \
    word maxSpan;     \
     \
    int positiveMask;     \
    word *arrayOfPacked;     \
    int i, k;     \
    int intCount;     \
    int bitRequiredForRange, shiftRequired = 0;     \
     \
    /* variables used by packer  */     \
    int lastPackBit, spaceInLastWord, lastSlot;     \
    word lastWordShifted, tempInt;     \
    word *packHeader;     \
     \
    /* variables used by unpacker  */     \
    int firstPackBit, bitPackInFirstWord, currentSlot;     \
    word currentWord, packInt;     \
    int significantBit, inSignificantBit;     \
    word *arrayPtr;     \
    int tokenSize, ShiftIntended, elementCountFromHeader;     \
    int  minSigned;     \
    word minUnsigned;     \
    integer_header *theHeader;

#define CommonCode \
   if ( bitSizeOfPackedToken == 0 )  { return 0; }  /* OOPS, token size is 0 */     \
     /*    determine wordsize and others     */     \
   wordSize                 = 8 * sizeof(word);     \
   theHeader                = (integer_header *)packedHeader;     \
   packHeader               = (word *)packedHeader;     \
   arrayOfPacked            = (word  *)packedArrayOfInt;     \
   intCount                 = elementCount;     \
   if ( (opCode==1) || (opCode==3) ) {  /*   packing   */     \
     if ( packedHeader != NULL ) { /* pack header required, pack (X - Xmin)  */     \
         if ( opCode == 1 ) {     \
             constructHeader(arrayOfUnsignedUnpacked, minUnsignedInteger, maxUnsignedInteger);     \
         }else{     \
             constructHeader(arrayOfSignedUnpacked, minSignedInteger, maxSignedInteger);      \
         }     \
     }else{ /* pack header not required, pack X itself */     \
        if ( bitSizeOfPackedToken == -1 ) { /*  compute bitSizeOfPackedToken */     \
            ComputeBitSize(arrayOfSignedUnpacked,arrayOfUnsignedUnpacked,opCode)     \
        };/* if */     \
     };/* else */     \
   }else{      /* opCode == 2 or 4, unpacking */     \
       ProcessPackHeader ;     \
   };     \
   /*  compute signed number adjustment  */     \
   positiveMask = ( opCode < 3 ) ? 0 : ( 1 << ( bitSizeOfPackedToken - 1 ));     \
   cleanupMask = ~(-1 << (wordSize-bitSizeOfPackedToken));

#define Opcode1234     \
   if ( opCode == 1 )   /* pack unsigned */    \
     {    \
       Pack(arrayOfUnsignedUnpacked, minUnsignedInteger);     \
     }    \
   else if ( opCode == 3 )   /* pack signed */    \
     {    \
       Pack(arrayOfSignedUnpacked, minSignedInteger);        \
     }    \
   else if ( opCode == 2 )   /* unpack unsigned */    \
     {    \
       Unpack(arrayOfUnsignedUnpacked, ShiftIntended, tokenSize,  minUnsigned, intCount);    \
     }    \
   else if ( opCode == 4 )   /* unpack signed */    \
     {    \
       Unpack(arrayOfSignedUnpacked, ShiftIntended, tokenSize,  minSigned, intCount);    \
     }

#define ProcessPackHeader                                     \
       if ( packHeader != NULL ) {                             \
           theHeader     = (integer_header *)packedHeader;     \
           tokenSize     = theHeader->numOfBitsPerToken;       \
           ShiftIntended = theHeader->SHIFT;                   \
           intCount      = theHeader->numOfPackedToken;        \
           minSigned     = theHeader->minValue;                \
           minUnsigned   = theHeader->minValue;                \
         }                                                     \
       else {                                                  \
           tokenSize     = bitSizeOfPackedToken;               \
           ShiftIntended = 0;                                  \
           intCount      = intCount;                           \
           minSigned     = minSignedInteger;                   \
           minUnsigned   = minUnsignedInteger;                 \
         };

#define ComputeBitSize(arrayOfSignedUnpacked,arrayOfUnsignedUnpacked,opCode)   \
            /*   obtain minimum, maximun, span */                              \
            if ( opCode == 3)  {   /* signed number */                         \
                FindMinMax(arrayOfSignedUnpacked, minSignedInteger, maxSignedInteger);  \
                maxSpan = ( abs(minSignedInteger) > maxSignedInteger ) ? abs(minSignedInteger) : maxSignedInteger;  \
            } else if (  opCode == 1 ) { /* unsigned number */                 \
                maxSpan = arrayOfUnsignedUnpacked[0];                          \
                for(i=stride; i < intCount*stride ; i+=stride) {               \
                    maxSpan |= arrayOfUnsignedUnpacked[i];                     \
                };                                                             \
            };                                                                 \
            bitSizeOfPackedToken = 0; /* compute bitSizeOfPackedToken */       \
            while ( maxSpan != 0 ) {                                           \
                maxSpan = maxSpan >> 1;                                        \
                bitSizeOfPackedToken++;                                        \
            };                                                               \
            if ( opCode == 3 )  {/* accomodate the sign bit */                 \
                bitSizeOfPackedToken++;                                        \
            };                                                               \
            cleanupMask = ((word)(~0)>>(wordSize-bitSizeOfPackedToken));

/***********************************************************
 *                                                         *
 *  Objective : find min and max of an integer array       *
 *                                                         *
 *  Argument   :                                           *
 *   IN    arrayOfUnpacked    integer array                *
 *   OUT   min                minimun integer              *
 *   OUT   max                maximun integer              *
 *                                                         *
 **********************************************************/
#define FindMinMax(arrayOfUnpacked, min, max)             \
{                                                         \
                                                          \
  min = arrayOfUnpacked[0];                               \
  max = arrayOfUnpacked[0];                               \
                                                          \
  for(i=stride; i < intCount*stride ; i+=stride)          \
    {                                                     \
      if ( arrayOfUnpacked[i] < min )                     \
        {                                                 \
          min = arrayOfUnpacked[i];                       \
        }                                                 \
      else if ( arrayOfUnpacked[i] > max )                \
        {                                                 \
          max = arrayOfUnpacked[i];                       \
        };                                                \
    };                                                    \
}                                                         \


/*********************************************************************
 *                                                                   *
 *  Objective : obtain and stuff a chain of 32 bit word into an array*
 *              ( macro tobe used in another macro "pack" )          *
 *                                                                   *
 *  Argument  :                                                      *
 *   IN   arrayOfUnpacked       integer array                        *
 *   IN   min                   minimun integer                      *
 *                                                                   *
 ********************************************************************/
#define pack32Bit(arrayOfUnpacked, min)                                  \
{                                                                        \
          i = intCount;                                                  \
          if ( packHeader != NULL )                                      \
            {                                                            \
             while ( i-- )                                               \
              {                                                          \
               *arrayPtr = ((*arrayOfUnpacked - min ) >> shiftRequired );\
               arrayPtr++;                                               \
               arrayOfUnpacked+=stride;                                  \
              };                                                         \
            }                                                            \
          else                                                           \
            {                                                            \
             while ( i-- )                                               \
              {                                                          \
               *arrayPtr = *arrayOfUnpacked + positiveMask;              \
               arrayPtr++;                                               \
               arrayOfUnpacked+=stride;                                  \
              };                                                         \
            };                                                           \
};                                                                       \


/*************************************************************************
 *                                                                       *
 *  Objective : obtain and stuff a chain of word(<32 bit) into an array  *
 *              ( tobe used in another macro "pack" )                    *
 *                                                                       *
 *  Argument  :                                                          *
 *   IN   arrayOfUnpacked       integer array                            *
 *   IN   min                   minimun integer                          *
 *                                                                       *
 ************************************************************************/
#define packBit(arrayOfUnpacked, min)                                    \
        {                                                                \
          if (packHeader !=NULL)                                         \
            {                                                            \
             for ( i = 0; i < intCount*stride; i+=stride)                \
              {                                                          \
               tempInt = ((arrayOfUnpacked[i]- min ) >> shiftRequired ); \
               tempInt &= cleanupMask;                                   \
               stuff(tempInt, arrayPtr, wordSize, bitSizeOfPackedToken,  \
                     lastWordShifted, spaceInLastWord);                  \
              }; /* for */                                               \
            }                                                            \
          else                                                           \
            {                                                            \
              for ( i = 0; i < intCount*stride; i+=stride)               \
              {                                                          \
              tempInt = arrayOfUnpacked[i] + positiveMask;               \
              tempInt &= cleanupMask;                                    \
              stuff(tempInt, arrayPtr, wordSize, bitSizeOfPackedToken,   \
                    lastWordShifted, spaceInLastWord);                   \
              };                                                         \
            };                                                           \
        };                                                               \

/*********************************************************************
 *                                                                   *
 *  Objective : pack a chain of word into an array                   *
 *                                                                   *
 *  Argument  :                                                      *
 *   IN   arrayOfUnpacked       integer array                        *
 *   IN   min                   minimun integer                      *
 *                                                                   *
 ********************************************************************/
#define Pack(arrayOfUnpacked, min)                                       \
{                                                                        \
     /* initialize local variables*/                                     \
                                                                         \
      lastPackBit = off_set;                                             \
      spaceInLastWord =  wordSize - ( lastPackBit % wordSize );          \
      lastSlot = ( lastPackBit / wordSize );                             \
                                                                         \
      if ( spaceInLastWord == wordSize )                                 \
        {                                                                \
          lastWordShifted = 0;                                           \
        }                                                                \
      else                                                               \
        {                                                                \
          lastWordShifted = arrayOfPacked[lastSlot] >> spaceInLastWord ; \
        };                                                               \
                                                                         \
      arrayPtr = &arrayOfPacked[lastSlot];                               \
      if (( spaceInLastWord == wordSize ) &&                             \
          ( bitSizeOfPackedToken == wordSize ))                          \
        {                                                                \
          pack32Bit(arrayOfUnpacked, min);                               \
        }                                                                \
      else                                                               \
        {                                                                \
          packBit(arrayOfUnpacked, min);                                 \
        };                                                               \
                                                                         \
       /* squeezes hole left in the integer array */                     \
                                                                         \
      if ( spaceInLastWord < wordSize )                                  \
        {                                                                \
         *arrayPtr = ( lastWordShifted << spaceInLastWord) |             \
                     ( *arrayPtr & ~(-1 << spaceInLastWord));            \
         };                                                              \
                                                                         \
                                                                         \
      return bitSizeOfPackedToken;                                       \
}                                                                        \

/*******************************************************************
 *                                                                 *
 *  Objective : construct pack header                              *
 *                                                                 *
 *  Argument  :                                                    *
 *    IN     arrayOfUnpacked        integer array                  *
 *    IN     min                    mininum integer                *
 *    IN     max                    maximum integer                *
 *                                                                 *
 ******************************************************************/
#define constructHeader(arrayOfUnpacked, min, max)                       \
 {                                                                       \
  word tempUnsignedMax, tempUnsignedMin;                                 \
                                                                         \
  /*      obtain min, max & range         */                             \
                                                                         \
   FindMinMax(arrayOfUnpacked, min, max);                                \
   if ( (max > 0) && (min < 0) )                                         \
     /* prevent signed overflow */                                       \
     {                                                                   \
       tempUnsignedMax = max;                                            \
       tempUnsignedMin = -min;                                           \
       maxRange = tempUnsignedMax + tempUnsignedMin;                     \
     }                                                                   \
   else                                                                  \
     maxRange = max - min;                                               \
                                                                         \
   /*  compute shift required */                                         \
   bitRequiredForRange = 0;                                              \
   while ( maxRange != 0 )                                               \
     {                                                                   \
       maxRange = maxRange >> 1;                                         \
       bitRequiredForRange++;                                            \
     };                                                                  \
                                                                         \
   shiftRequired = 0;                                                    \
   if ( bitSizeOfPackedToken == -1 )                                     \
     {                                                                   \
       bitSizeOfPackedToken = bitRequiredForRange;                       \
     }                                                                   \
   else                                                                  \
     {                                                                   \
       while ( (bitRequiredForRange-bitSizeOfPackedToken) > 0 )          \
         {                                                               \
           shiftRequired++;                                              \
           bitRequiredForRange--;                                        \
         };                                                              \
     };                                                                  \
                                                                         \
   /*   construct pack header */                                         \
   packHeader[0] = 0xFD000000 | (shiftRequired <<6)|bitSizeOfPackedToken;\
   packHeader[1] = intCount;                                             \
   packHeader[2] = min;                                                  \
   packHeader[3] = max;                                                  \
                                                                         \
 };                                                                      \
        
/*******************************************************************
 *                                                                 *
 *  Objective : unpack an array                                    *
 *                                                                 *
 *  Argument  :                                                    *
 *    IN     arrayOfUnpacked        integer array                  *
 *    IN     requiredShift          shift required                 *
 *    IN     bitSizeOfPackedToken   pack token size                *
 *    IN     min                    mininum integer                *
 *    IN     intCount               element in the packed array    *
 *                                                                 *
 ******************************************************************/
#define Unpack(arrayOfUnpacked, requiredShift, tokenSize, min, intCount)                  \
 {                                                                                        \
                                                                                          \
    /*    initialize variables         */                                                 \
    firstPackBit = off_set;                                                               \
    bitPackInFirstWord =  wordSize - ( firstPackBit % wordSize );                         \
    currentSlot = ( firstPackBit / wordSize );                                            \
    currentWord = arrayOfPacked[currentSlot] << ( wordSize - bitPackInFirstWord );        \
    positiveMask = -positiveMask;                                                         \
                                                                                          \
    if ( tokenSize > wordSize )                                                           \
      {                                                                                   \
        significantBit = wordSize;                                                        \
        inSignificantBit = tokenSize - wordSize;                                          \
      }                                                                                   \
    else                                                                                  \
      {                                                                                   \
        significantBit = tokenSize;                                                       \
        inSignificantBit = 0;                                                             \
      };                                                                                  \
                                                                                          \
    /**    unpack integer numbers             */                                          \
    arrayPtr = &arrayOfPacked[currentSlot];                                               \
    if ( packHeader != NULL )                                                             \
      {                                                                                   \
        for ( i = 0; i < intCount*stride; i+=stride)                                      \
          {                                                                               \
            extract(packInt, arrayPtr, wordSize, significantBit,                          \
                    currentWord, bitPackInFirstWord);                                     \
                                                                                          \
            /*      truncate extra bit        */                                          \
            if ( inSignificantBit > 0 )                                                   \
              {                                                                           \
                discard(arrayPtr, wordSize, inSignificantBit,                             \
                        currentWord, bitPackInFirstWord);                                 \
              }                                                                           \
            arrayOfUnpacked[i] =  (packInt << requiredShift ) + min;                      \
          }                                                                               \
      }                                                                                   \
    else                                                                                  \
      {                                                                                   \
        for ( i = 0; i < intCount*stride; i+=stride)                                      \
          {                                                                               \
            extract(packInt, arrayPtr, wordSize, significantBit,                          \
                    currentWord, bitPackInFirstWord);                                     \
                                                                                          \
            /*      truncate extra bit if necessary           */                          \
            if ( inSignificantBit > 0 )                                                   \
              {                                                                           \
                discard(arrayPtr, wordSize, inSignificantBit, currentWord,                \
                        bitPackInFirstWord);                                              \
              }                                                                           \
                                                                                          \
            arrayOfUnpacked[i] =  packInt + positiveMask;                                 \
          }                                                                               \
      };                                                                                  \
                                                                                          \
    return bitSizeOfPackedToken;                                                          \
 };                                                                                       \

/***********************************************************************************************
 *                                                                                             *
 * Author   : Jianhui He, 1997                                                                 *
 * Refactored : M.Valin , 2014                                                                 *
 *                                                                                             *
 * Objective: transform integer array between pack and unpacked format                         *
 *                                                                                             *
 * Arguments:                                                                                  *
 *    IN/OUT  unpackedArray          unpacked integer array                                    *
 *    IN/OUT  packedArrayOfInt       packed integer array                                      *
 *    IN      elementCount           total count of element in unpacked integer array          *
 *    IN      bitSizeOfPackedToken   packed integer size in bit                                *
 *                                   when equal "-1", autodetection of bit length is on        *
 *    IN      offset                 in packing   : last bit of integer packed inside array    *
 *                                   in unpacking : first bit of integer packed inside array   *
 *    IN      stride                 unpacked integer spacing indicator                        *
 *    IN      opCode                 1: unsigned pack                                          *
 *                                   2: unsigned unpack                                        *
 *                                   3: signed pack                                            *
 *                                   4: signed unpack                                          *
 **********************************************************************************************/
int  compact_integer( void *unpackedArray, void *packedHeader, void *packedArrayOfInt, 
                       int elementCount, int bitSizeOfPackedToken, int off_set, 
                       int stride, int opCode)
{
   word *arrayOfUnsignedUnpacked = (word *)unpackedArray;
   int  *arrayOfSignedUnpacked = (int *)unpackedArray;

   CommonDeclarations ;
   CommonCode ;

   Opcode1234
   else
     {
       printf("\n compact_integer: opCode:%d is not defined \n", opCode);
       return 0;
     };/* if */
 
  return intCount;  /* unused, function must return something */
} /* end compact_integer */

/***********************************************************************************************
 *                                                                                             *
 * Author   : M. Lepine, sept 2005                                                             *
 * Refactored, implemented signed pack/unpack  : M.Valin , 2014                                *
 *                                                                                             *
 * Objective: transform array of short integer between pack and unpacked format                *
 *                                                                                             *
 * Arguments:                                                                                  *
 *    IN/OUT  unpackedArrayOfShort   unpacked array of short integer                           *
 *    IN/OUT  packedArrayOfInt       packed integer array                                      *
 *    IN      elementCount           total count of element in unpacked integer array          *
 *    IN      bitSizeOfPackedToken   packed integer size in bit                                *
 *                                   when equal "-1", autodetection of bit length is on        *
 *    IN      offset                 in packing   : the last bit of integer packed inside array*
 *                                   in unpacking : the first bit of integer packed inside array*
 *    IN      stride                 unpacked integer spacing indicator                        *
 *    IN      opCode                 5: unsigned short pack                                    *
 *                                   6: unsigned short unpack                                  *
 *                                   7: signed short pack                                      *
 *                                   8: signed short unpack                                    *
 **********************************************************************************************/
int  compact_short( void *unpackedArray, void *packedHeader, void *packedArrayOfInt, 
                       int elementCount, int bitSizeOfPackedToken, int off_set, 
                       int stride, int opCode)
{
   unsigned short *arrayOfUnsignedUnpacked = (unsigned short *)unpackedArray;
   short  *arrayOfSignedUnpacked = (short *)unpackedArray;

   CommonDeclarations ;
   opCode = opCode - 4;
   CommonCode ;

   Opcode1234
   else
     {
       printf("\n compact_short: opCode:%d is not defined \n", opCode+4);
       return 0;
     };/* if */
 
  return intCount;  /* unused, function must return something */
} /* end compact_short */

/***********************************************************************************************
 *                                                                                             *
 * Author   : M. Lepine, sept 2005                                                             *
 * Refactored, implemented signed pack/unpack  : M.Valin , 2014                                *
 *                                                                                             *
 * Objective: transform array of short integer between pack and unpacked format                *
 *                                                                                             *
 * Arguments:                                                                                  *
 *    IN/OUT  unpackedArrayOfBytes   unpacked array of bytes                                   *
 *    IN/OUT  packedArrayOfInt       packed integer array                                      *
 *    IN      elementCount           total count of element in unpacked integer array          *
 *    IN      bitSizeOfPackedToken   packed integer size in bit                                *
 *                                   when equal "-1", autodetection of bit length is on        *
 *    IN      offset                 in packing   : the last bit of integer packed inside array*
 *                                   in unpacking : the first bit of integer packed inside array*
 *    IN      stride                 unpacked integer spacing indicator                        *
 *    IN      opCode                 9: unsigned char pack                                     *
 *                                  10: unsigned char unpack                                   *
 *                                  11: signed char pack                                       *
 *                                  12: signed char unpack                                     *
 **********************************************************************************************/
int  compact_char( void *unpackedArray, void *packedHeader, void *packedArrayOfInt, 
                       int elementCount, int bitSizeOfPackedToken, int off_set, 
                       int stride, int opCode)
{
   unsigned char *arrayOfUnsignedUnpacked = (unsigned char *)unpackedArray;
   char  *arrayOfSignedUnpacked = (char *)unpackedArray;

   CommonDeclarations ;
   opCode = opCode - 8;
   CommonCode ;

   Opcode1234
   else
     {
       printf("\n compact_char: opCode:%d is not defined \n", opCode+8);
       return 0;
     };/* if */
 
  return intCount;  /* unused, function must return something */
} /* end compact_char */

int  compact_short2( void *unpackedArrayOfShort, void *packedHeader, void *packedArrayOfInt, 
                       int elementCount, int bitSizeOfPackedToken, int off_set, 
                       int stride, int opCode)
{
    int wordSize;
    word cleanupMask;
    int minSignedInteger=0, maxSignedInteger=0;
    word minUnsignedInteger=0, maxUnsignedInteger=0;
    word maxRange;
    word maxSpan;
   

    int positiveMask;
    unsigned short *arrayOfUnsignedUnpacked;
    short *arrayOfSignedUnpacked;
    word *arrayOfPacked;
    int i, k;
    int intCount;
    int bitRequiredForRange, shiftRequired = 0;

    

    /****************************************
     *                                      *
     *     variables used by the packer     *
     *                                      *
     ***************************************/
    int lastPackBit, spaceInLastWord, lastSlot;
    word lastWordShifted, tempInt;
    word *packHeader;
    
    /***************************************
     *                                     *
     *    variables used by the unpacker   *
     *                                     *
     **************************************/
    int firstPackBit, bitPackInFirstWord, currentSlot;
    word currentWord, packInt;
    int significantBit, inSignificantBit;
    word *arrayPtr;
    int tokenSize, ShiftIntended, elementCountFromHeader;
    int  minSigned;
    word minUnsigned;
    integer_header *theHeader;


/*  printf("minSignedInteger=%d minUnsignedInteger=%d \n",minSignedInteger,minUnsignedInteger); */

    /********************************
     *                              *
     *   handle abnormal condition  *
     *                              *
     ********************************/
    /* token size is 0 */
    if ( bitSizeOfPackedToken == 0 )
      { 
        return 0;
      };  
    /********************************************************
     *                                                      *
     *    determine wordsize and others                     * 
     *                                                      *
     ********************************************************/
    wordSize                 = 8 * sizeof(word);
    arrayOfUnsignedUnpacked     = (short *)unpackedArrayOfShort;
    arrayOfSignedUnpacked       = (short *)unpackedArrayOfShort;
    theHeader                = (integer_header *)packedHeader;
    packHeader               = (word *)packedHeader;
    arrayOfPacked            = (word  *)packedArrayOfInt;
    intCount                 = elementCount;
    cleanupMask              = ((word)(~0)>>(wordSize-bitSizeOfPackedToken));
      
    opCode = opCode - 4;
   if (opCode==1 || opCode==3)  /* packing */
   {
     if ( packedHeader != NULL )
       /*******************************************************************
        *                                                                  *
        *  pack header is required, (X - Xmin) is used as packInt          *
        *                                                                  *
        *******************************************************************/
       {
             if (opCode==1) constructHeader(arrayOfUnsignedUnpacked, minUnsignedInteger, maxUnsignedInteger);
             if (opCode==3) constructHeader(arrayOfSignedUnpacked, minSignedInteger, maxSignedInteger);
       }
    else
      /*************************************************************
       *                                                           *
       *   pack header not required, X itself is used as packInt,  *
       *   determines bitSizeOfPackedToken, if not available       *
       *                                                           *
       ************************************************************/
      {
        if ( bitSizeOfPackedToken == -1 )
          {
#ifdef use_old_code
            /**********************************************************
             *                                                        *
             *   obtain minimum, maximun, span                        *
             *                                                        *
             *********************************************************/
            if ( opCode == 1 )
              {
                /* unsigned integer number */
                maxSpan = arrayOfUnsignedUnpacked[0];

                for(i=stride; i < intCount*stride ; i+=stride)
                  {
                    maxSpan |= arrayOfUnsignedUnpacked[i];
                  };
              };
            if ( opCode == 3 )
              {
                /* signed integer number */
                FindMinMax(arrayOfSignedUnpacked, minSignedInteger, maxSignedInteger);

                maxSpan    = ( abs(minSignedInteger) > maxSignedInteger ) ? abs(minSignedInteger) :
                             maxSignedInteger;
              };
          
            /************************************************************
             *                                                          *
             *           derive bitSizeOfPackedToken                    *
             *                                                          *
             ***********************************************************/
            bitSizeOfPackedToken = 0;
            while ( maxSpan != 0 )
              {
                maxSpan = maxSpan >> 1;
                bitSizeOfPackedToken++;
              };
            if ( opCode == 3 )
              {/* accomodate the signed bit */
                bitSizeOfPackedToken++;
              };
#else
            ComputeBitSize(arrayOfSignedUnpacked,arrayOfUnsignedUnpacked,opCode) ;
#endif
            cleanupMask = ((word)(~0)>>(wordSize-bitSizeOfPackedToken));
          };/* if */



      };/* else */
   } 
   else/* opCode == 6 or 8 */
    /************************************************
     *                                              *
     *         collect info for the unpacking       *
     *                                              *
     ***********************************************/
     {
#ifdef use_old_code
       if ( packHeader != NULL )
         {
           theHeader     = (integer_header *)packedHeader;
           tokenSize     = theHeader->numOfBitsPerToken;
           ShiftIntended = theHeader->SHIFT;
           intCount      = theHeader->numOfPackedToken;
           minSigned     = theHeader->minValue;
           minUnsigned   = theHeader->minValue;
         }
       else
         {
           tokenSize     = bitSizeOfPackedToken;
           ShiftIntended = 0;
           intCount      = intCount;
           minSigned     = minSignedInteger;
           minUnsigned   = minUnsignedInteger;
         };
#else
       ProcessPackHeader ;
#endif
     };
 
   

   /**********************************************
    *                                            *
    *  compute signed int adjustment,            *
    * since 1,2,3,4 without header all needs it  *
    *                                            *
    *********************************************/
   positiveMask = ( opCode < 3 ) ? 0 : ( 1 << ( bitSizeOfPackedToken - 1 ));


   /***********************************************
    *                                             *
    *   pack                                      *
    *                                             *
    **********************************************/
   if ( opCode == 1 || opCode==3)
     {
       if ( opCode == 1) Pack(arrayOfUnsignedUnpacked, minUnsignedInteger);
       if ( opCode == 3) Pack(arrayOfSignedUnpacked, minSignedInteger);
     }
   /***********************************************
    *                                             *
    *   unpack                                    *
    *                                             *
    **********************************************/
   else if ( opCode == 2 || opCode == 4 )
     {
       if ( opCode == 2 ) Unpack(arrayOfUnsignedUnpacked, ShiftIntended, tokenSize, minUnsigned, intCount);
       if ( opCode == 4 ) Unpack(arrayOfSignedUnpacked, ShiftIntended, tokenSize, minSigned, intCount);
     }
   else
     {
       printf("\n compact_short: opCode:%d is not defined \n", opCode);
       return 0;
     };/* if */
 
  return intCount;  /* unused, function must return something */
 

} /* end compact_short2 */

int  compact_char2( void *unpackedArrayOfBytes, void *packedHeader, void *packedArrayOfInt, 
                       int elementCount, int bitSizeOfPackedToken, int off_set, 
                       int stride, int opCode)
{
    int wordSize;
    word cleanupMask;
    int minSignedInteger=0, maxSignedInteger=0;
    word minUnsignedInteger=0, maxUnsignedInteger=0;
    word maxRange;
    word maxSpan;

    int positiveMask;
    unsigned char *arrayOfUnsignedUnpacked;
    char *arrayOfSignedUnpacked;
    word *arrayOfPacked;
    int i, k;
    int intCount;
    int bitRequiredForRange, shiftRequired = 0;

    /****************************************
     *                                      *
     *     variables used by the packer     *
     *                                      *
     ***************************************/
    int lastPackBit, spaceInLastWord, lastSlot;
    word lastWordShifted, tempInt;
    word *packHeader;
    /***************************************
     *                                     *
     *    variables used by the unpacker   *
     *                                     *
     **************************************/
    int firstPackBit, bitPackInFirstWord, currentSlot;
    word currentWord, packInt;
    int significantBit, inSignificantBit;
    word *arrayPtr;
    int tokenSize, ShiftIntended, elementCountFromHeader;
    int  minSigned;
    word minUnsigned;
    integer_header *theHeader;

/*  printf("minSignedInteger=%d minUnsignedInteger=%d \n",minSignedInteger,minUnsignedInteger); */
    /********************************
     *                              *
     *   handle abnormal condition  *
     *                              *
     ********************************/
    /* token size is 0 */
    if ( bitSizeOfPackedToken == 0 )
      { 
        return 0;
      };  
    /********************************************************
     *                                                      *
     *    determine wordsize and others                     * 
     *                                                      *
     ********************************************************/
    wordSize                 = 8 * sizeof(word);
    arrayOfUnsignedUnpacked      = (unsigned char *)unpackedArrayOfBytes;
    arrayOfSignedUnpacked        = (char *)unpackedArrayOfBytes;
    theHeader                = (integer_header *)packedHeader;
    packHeader               = (word *)packedHeader;
    arrayOfPacked            = (word  *)packedArrayOfInt;
    intCount                 = elementCount;
    cleanupMask              = ((word)(~0)>>(wordSize-bitSizeOfPackedToken));

    opCode = opCode -8;
   if (opCode==1 || opCode==3)  /* packing */
   {
     if ( packedHeader != NULL )
       /*******************************************************************
        *                                                                  *
        *  pack header is required, (X - Xmin) is used as packInt          *
        *                                                                  *
        *******************************************************************/
       {
             if (opCode==1 ) constructHeader(arrayOfUnsignedUnpacked, minUnsignedInteger, maxUnsignedInteger);
             if (opCode==3) constructHeader(arrayOfSignedUnpacked,   minSignedInteger,   maxSignedInteger);
       }
    else
      /*************************************************************
       *                                                           *
       *   pack header not required, X itself is used as packInt,  *
       *   determines bitSizeOfPackedToken, if not available       *
       *                                                           *
       ************************************************************/
      {
        if ( bitSizeOfPackedToken == -1 )
          {
#ifdef use_old_code
             /*********************************************************
             *                                                        *
             *   obtain minimum, maximun, span                        *
             *                                                        *
             *********************************************************/
            if ( opCode == 1 )                /* unsigned byte  */
              {
                maxSpan = arrayOfUnsignedUnpacked[0];
                for(i=stride; i < intCount*stride ; i+=stride)
                  {
                    maxSpan |= arrayOfUnsignedUnpacked[i];
                  }; 
              };
            if ( opCode == 3 )                /* signed byte  */
              {
                FindMinMax(arrayOfSignedUnpacked, minSignedInteger, maxSignedInteger);
                maxSpan    = ( abs(minSignedInteger) > maxSignedInteger ) ? abs(minSignedInteger) :
                             maxSignedInteger;
              };
          
            /************************************************************
             *                                                          *
             *           derive bitSizeOfPackedToken                    *
             *                                                          *
             ***********************************************************/
            bitSizeOfPackedToken = 0;
            while ( maxSpan != 0 )
              {
                maxSpan = maxSpan >> 1;
                bitSizeOfPackedToken++;
              };
            if ( opCode == 3 )     /* accomodate the sign bit */
              {
                bitSizeOfPackedToken++;
              };
#else
            ComputeBitSize(arrayOfSignedUnpacked,arrayOfUnsignedUnpacked,opCode) ;
#endif
//            cleanupMask = ((word)(~0)>>(wordSize-bitSizeOfPackedToken));
            cleanupMask = ~(-1 << (wordSize-bitSizeOfPackedToken));
          };/* if */

      };/* else */
   } 
   else                      /* opCode == 10 or 12 */
    /************************************************
     *                                              *
     *         collect info for the unpacking       *
     *                                              *
     ***********************************************/
     {
#ifdef use_old_code
       if ( packHeader != NULL )
         {
           theHeader     = (integer_header *)packedHeader;
           tokenSize     = theHeader->numOfBitsPerToken; 
           ShiftIntended = theHeader->SHIFT;
           intCount      = theHeader->numOfPackedToken;
           minSigned     = theHeader->minValue;
           minUnsigned   = theHeader->minValue;
         }
       else
         {
           tokenSize     = bitSizeOfPackedToken;
           ShiftIntended = 0;
           intCount      = intCount;
           minSigned     = minSignedInteger;
           minUnsigned   = minUnsignedInteger;
         };
#else
       ProcessPackHeader ;
#endif
     };
   /**********************************************
    *                                            *
    *  compute signed int adjustment,            *
    * since 1,2,3,4 without header all needs it  *
    *                                            *
    *********************************************/
   positiveMask = ( opCode < 3 ) ? 0 : ( 1 << ( bitSizeOfPackedToken - 1 ));

   if ( opCode == 1 || opCode == 3 )     /*    pack  */
     {
       if ( opCode == 1) Pack(arrayOfUnsignedUnpacked, minUnsignedInteger);
       if ( opCode == 3) Pack(arrayOfSignedUnpacked, minSignedInteger);
     }
   else if ( opCode == 2 || opCode == 4 )  /*   unpack  */
     {
       if ( opCode == 2 ) Unpack(arrayOfUnsignedUnpacked, ShiftIntended, tokenSize,minUnsigned, intCount);
       if ( opCode == 4 ) Unpack(arrayOfSignedUnpacked, ShiftIntended, tokenSize,minSigned, intCount);
     }
   else
     {
       printf("\n compact_char: opCode:%d is not defined \n", opCode);
       return 0;
     };/* if */
 
  return intCount;  /* unused, function must return something */
 

} /* end compact_char2 */

#ifdef SELFTEST
#define DATASIZE 128
#define PAKBUFSZ 256
#define verify(ref,buf,what) \
   errors=0; \
   min = ref[0] ; max = ref[0] ; \
   for (i=0 ; i<datasize ; i++ ){ \
     if(ref[i*stride] != buf[i*stride]) { \
       errors++ ; \
       fprintf(stderr,"i = %d, expected %d, got %d \n",i,ref[i*stride],buf[i*stride]); \
       break ; \
     }  \
     if(ref[i*stride] > max) max = ref[i*stride] ;\
     if(ref[i*stride] < min) min = ref[i*stride] ;\
   } \
   if(errors > 0) fprintf(stderr,"verifying %d %s points, min = %d, max = %d, errors=%d\n",datasize,what,min,max,errors); \
   if(errors > 0) exit(1);

#define nullify(buf) \
   for (i=0 ; i<DATASIZE ; i++ )buf[i]=0;

#define zeroheader \
   my_header.numOfBitsPerToken = 63; \
   my_header.SHIFT = 63;

main()
{
  int pakbuf[PAKBUFSZ];
  integer_header my_header;
  int datasize=DATASIZE;
  int stride = 1;

  int ibuf[DATASIZE];
  short sbuf[DATASIZE];
  char cbuf[DATASIZE];
  unsigned int uibuf[DATASIZE];
  unsigned short usbuf[DATASIZE];
  unsigned char ucbuf[DATASIZE];

  int iref[DATASIZE];
  short sref[DATASIZE];
  char cref[DATASIZE];
  unsigned int uiref[DATASIZE];
  unsigned short usref[DATASIZE];
  unsigned char ucref[DATASIZE];

  int i, status, errors, min, max;

  for (i=0 ; i<DATASIZE ; i++ ){
    ucref[i] = i + i;
    cref[i]  = i + i - 128;
    usref[i] = ucref[i] ; usref[i] = usref[i] * 16;
    sref[i]  = cref[i]  ; sref[i]  = sref[i] * 16;
    uiref[i] = usref[i] ; uiref[i] = uiref[i] * 16;
    iref[i]  = sref[i]  ; iref[i]  = iref[i]  * 16;
  }

  fprintf(stderr,"integer/short/char signed/unsigned packer test\n");
  fprintf(stderr,"==========  no header, no stride, no offset ==========\n");
/* 
 * status = packer(unpacked,Header,packed,Count,Nbits,offset,stride,Opcode)
 * 1: unsigned pack, 2: unsigned unpac, 3: signed pack, 4: signed unpack
 * +4 for shorts, =* for chars
 * */
  fprintf(stderr,"==========  packing integers  ==========\n");
  status = compact_integer(uiref, NULL , pakbuf, DATASIZE, 16, 0, 1, 1); nullify(uibuf);
  status = compact_integer(uibuf, NULL , pakbuf, DATASIZE, 16, 0, 1, 2);
  verify(uiref,uibuf,"unsigned int")
  status = compact_integer(iref, NULL , pakbuf, DATASIZE, 16, 0, 1, 3); nullify(ibuf);
  status = compact_integer(ibuf, NULL , pakbuf, DATASIZE, 16, 0, 1, 4);
  verify(iref,ibuf,"signed int")

  fprintf(stderr,"==========  packing shorts  ==========\n");
  status = compact_short(usref, NULL , pakbuf, DATASIZE, 12, 0, 1, 5); nullify(usbuf); nullify(uibuf);
  status = compact_short(usbuf, NULL , pakbuf, DATASIZE, 12, 0, 1, 6);
  verify(usref,usbuf,"unsigned short")
  status = compact_integer(uibuf, NULL , pakbuf, DATASIZE, 12, 0, 1, 2);
  verify(usref,uibuf,"unsigned short > unsigned int")
  status = compact_short(sref, NULL , pakbuf, DATASIZE, 12, 0, 1, 7); nullify(sbuf); nullify(ibuf);
  status = compact_short(sbuf, NULL , pakbuf, DATASIZE, 12, 0, 1, 8);
  verify(sref,sbuf,"signed short")

  fprintf(stderr,"==========  packing chars  ==========\n");
  status = compact_char(ucref, NULL , pakbuf, DATASIZE, 8, 0, 1, 9); nullify(ucbuf); nullify(usbuf); nullify(uibuf);
  status = compact_char(ucbuf, NULL , pakbuf, DATASIZE, 8, 0, 1, 10);
  verify(ucref,ucbuf,"unsigned char")
  status = compact_short(usbuf, NULL , pakbuf, DATASIZE, 8, 0, 1, 6);
  verify(ucref,usbuf,"unsigned char > unsigned short")
  status = compact_integer(uibuf, NULL , pakbuf, DATASIZE, 8, 0, 1, 2);
  verify(ucref,uibuf,"unsigned char > unsigned int")
  status = compact_char(cref, NULL , pakbuf, DATASIZE, 8, 0, 1, 11); nullify(cbuf); nullify(sbuf); nullify(ibuf);
  status = compact_char(cbuf, NULL , pakbuf, DATASIZE, 8, 0, 1, 12);
  verify(cref,cbuf,"signed char")

  fprintf(stderr,"==========  with header, no stride, no offset ==========\n");
  status = compact_integer(uiref, &my_header , pakbuf, DATASIZE, 16, 0, 1, 1); nullify(uibuf);
  status = compact_integer(uibuf, &my_header , pakbuf, DATASIZE, 16, 0, 1, 2);
  verify(uiref,uibuf,"unsigned int")
  status = compact_integer(iref, &my_header , pakbuf, DATASIZE, 16, 0, 1, 3); nullify(ibuf);
  status = compact_integer(ibuf, &my_header , pakbuf, DATASIZE, 16, 0, 1, 4);
  verify(iref,ibuf,"signed int")

  for (stride=2 ; stride <= DATASIZE-1 ; stride++) {
  fprintf(stderr,"==========  NO header, with stride %d, no offset ==========\n", stride);
  datasize = DATASIZE / stride ;

  zeroheader;
  status = compact_integer(uiref, NULL , pakbuf, datasize, 16, 0, stride, 1); nullify(uibuf);
  status = compact_integer(uibuf, NULL , pakbuf, datasize, 16, 0, stride, 2);
  verify(uiref,uibuf,"unsigned int")
  zeroheader;
  status = compact_integer(iref, NULL , pakbuf, datasize, 16, 0, stride, 3); nullify(ibuf);
  status = compact_integer(ibuf, NULL , pakbuf, datasize, 16, 0, stride, 4);
  verify(iref,ibuf,"signed int")

  zeroheader;
  status = compact_short(usref, NULL , pakbuf, datasize, 12, 0, stride, 1+4); nullify(usbuf); nullify(uibuf);
  status = compact_short(usbuf, NULL , pakbuf, datasize, 12, 0, stride, 2+4);
  verify(usref,usbuf,"unsigned short")
  zeroheader;
  status = compact_short(sref, NULL , pakbuf, datasize, 12, 0, stride, 3+4); nullify(sbuf); nullify(ibuf);
  status = compact_short(sbuf, NULL , pakbuf, datasize, 12, 0, stride, 4+4);
  verify(sref,sbuf,"signed short")
  status = compact_integer(ibuf, NULL , pakbuf, datasize, 12, 0, stride, 4);
  verify(sref,ibuf,"signed short > signed int")

  zeroheader;
  status = compact_char(ucref, NULL , pakbuf, datasize, 8, 0, stride, 1+8); nullify(ucbuf); nullify(uibuf);
  status = compact_char(ucbuf, NULL , pakbuf, datasize, 8, 0, stride, 2+8);
  verify(ucref,ucbuf,"unsigned char")
  zeroheader;
  status = compact_char(cref, NULL , pakbuf, datasize, 8, 0, stride, 3+8); nullify(cbuf); nullify(ibuf);
  status = compact_char(cbuf, NULL , pakbuf, datasize, 8, 0, stride, 4+8);
  verify(cref,cbuf,"signed char")
  status = compact_short(sbuf, NULL , pakbuf, datasize, 8, 0, stride, 4+4);
  verify(cref,sbuf,"signed char > signed short")
  status = compact_integer(ibuf, NULL , pakbuf, datasize, 8, 0, stride, 4);
  verify(cref,ibuf,"signed char > signed int")
  }

  stride = 1 ; datasize = DATASIZE / stride ;

  for (stride=2 ; stride <= DATASIZE-1 ; stride++) {
  fprintf(stderr,"==========  with header, with stride %d, no offset ==========\n", stride);
  datasize = DATASIZE / stride ;

  zeroheader;
  status = compact_integer(uiref, &my_header , pakbuf, datasize, 16, 0, stride, 1); nullify(uibuf);
  status = compact_integer(uibuf, &my_header , pakbuf, datasize, 16, 0, stride, 2);
  verify(uiref,uibuf,"unsigned int")
  zeroheader;
  status = compact_integer(iref, &my_header , pakbuf, datasize, 16, 0, stride, 3); nullify(ibuf);
  status = compact_integer(ibuf, &my_header , pakbuf, datasize, 16, 0, stride, 4);
  verify(iref,ibuf,"signed int")

  zeroheader;
  status = compact_short(usref, &my_header , pakbuf, datasize, 12, 0, stride, 1+4); nullify(usbuf); nullify(uibuf);
  status = compact_short(usbuf, &my_header , pakbuf, datasize, 12, 0, stride, 2+4);
  verify(usref,usbuf,"unsigned short")
  zeroheader;
  status = compact_short(sref, &my_header , pakbuf, datasize, 12, 0, stride, 3+4); nullify(sbuf); nullify(ibuf);
  status = compact_short(sbuf, &my_header , pakbuf, datasize, 12, 0, stride, 4+4);
  verify(sref,sbuf,"signed short")
  status = compact_integer(ibuf, &my_header , pakbuf, datasize, 12, 0, stride, 4);
  verify(sref,ibuf,"signed short > signed int")

  zeroheader;
  status = compact_char(ucref, &my_header , pakbuf, datasize, 8, 0, stride, 1+8); nullify(ucbuf); nullify(uibuf);
  status = compact_char(ucbuf, &my_header , pakbuf, datasize, 8, 0, stride, 2+8);
  verify(ucref,ucbuf,"unsigned char")
  zeroheader;
  status = compact_char(cref, &my_header , pakbuf, datasize, 8, 0, stride, 3+8); nullify(cbuf); nullify(ibuf);
  status = compact_char(cbuf, &my_header , pakbuf, datasize, 8, 0, stride, 4+8);
  verify(cref,cbuf,"signed char")
  status = compact_short(sbuf, &my_header , pakbuf, datasize, 8, 0, stride, 4+4);
  verify(cref,sbuf,"signed char > signed short")
  status = compact_integer(ibuf, &my_header , pakbuf, datasize, 8, 0, stride, 4);
  verify(cref,ibuf,"signed char > signed int")

  }

  stride = 1 ; datasize = DATASIZE / stride ;

  fprintf(stderr,"==========  with header, no stride, no offset, autobitsize ==========\n");
  zeroheader;
  status = compact_integer(uiref, &my_header , pakbuf, DATASIZE, -1, 0, 1, 1); nullify(uibuf);
  fprintf(stderr,"computed nbits = %d\n",my_header.numOfBitsPerToken);
  status = compact_integer(uibuf, &my_header , pakbuf, DATASIZE, -1, 0, 1, 2);
  verify(uiref,uibuf,"unsigned int")
  zeroheader;
  status = compact_integer(iref, &my_header , pakbuf, DATASIZE, -1, 0, 1, 3); nullify(ibuf);
  fprintf(stderr,"computed nbits = %d\n",my_header.numOfBitsPerToken);
  status = compact_integer(ibuf, &my_header , pakbuf, DATASIZE, -1, 0, 1, 4);
  verify(iref,ibuf,"signed int")

  zeroheader;
  status = compact_short(usref, &my_header , pakbuf, DATASIZE, -1, 0, 1, 5); nullify(usbuf);
  fprintf(stderr,"computed nbits = %d\n",my_header.numOfBitsPerToken);
  status = compact_short(usbuf, &my_header , pakbuf, DATASIZE, -1, 0, 1, 6);
  verify(usref,usbuf,"unsigned short")
  zeroheader;
  status = compact_short(sref, &my_header , pakbuf, DATASIZE, -1, 0, 1, 7); nullify(sbuf);
  fprintf(stderr,"computed nbits = %d\n",my_header.numOfBitsPerToken);
  status = compact_short(sbuf, &my_header , pakbuf, DATASIZE, -1, 0, 1, 8);
  verify(sref,sbuf,"signed short")

  zeroheader;
  status = compact_char(ucref, &my_header , pakbuf, DATASIZE, -1, 0, 1, 9); nullify(ucbuf);
  fprintf(stderr,"computed nbits = %d\n",my_header.numOfBitsPerToken);
  status = compact_char(ucbuf, &my_header , pakbuf, DATASIZE, -1, 0, 1, 10);
  verify(ucref,ucbuf,"unsigned char")
  zeroheader;
  status = compact_char(cref, &my_header , pakbuf, DATASIZE, -1, 0, 1, 11); nullify(cbuf);
  fprintf(stderr,"computed nbits = %d\n",my_header.numOfBitsPerToken);
  status = compact_char(cbuf, &my_header , pakbuf, DATASIZE, -1, 0, 1, 12);
  verify(cref,cbuf,"signed char")

  fprintf(stderr,"==========  TEST COMPLETE ==========\n");
}
#endif
