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
#include <math.h>
#include "rmnlib.h"
extern double f77name(f_pow)(double *base, wordint *i);




/***********************************************************************************************
 *                                                                                             *
 * Author   : Jianhui He, 1997                                                                 *
 *                                                                                             *
 * Objective: (routine to be included by compact.c)                                            *
 *            pack floating point numbers( float and double ), into integers                   *
 *            and                                                                              *
 *            unpack integers( pack formated ) into floating point numbers                     *
 *                                                                                             *
 * Arguments:                                                                                  *
 *    IN/OUT  unpackedArrayOfFloat   array of floating point numbers                           *
 *    IN/OUT  packedHeader           format information of packed integer numbers              *
 *    IN/OUT  packedArrayOfInt       array Of Integers                                         *
 *    IN      elementCount           total count of element in floating point array            *
 *    IN      bitSizeOfPackedToken   integer size in bit                                       *
 *    IN      offset                 in packing   : the last bit of integer packed inside array*
 *                                   in unpacking : the first bit of integer packed into array *
 *    IN      stride                 floating point number spacing indicator                   *
 *    IN      opCode                 operator(FLOAT_PACK, FLOAT_UNPACK)                        *
 *    IN      hasMissing             1: inicate existence of missing value in                  *
 *                                      floating point array                                   *
 *                                   0: otherwise                                              *
 *    IN      missingTag             missing value identifier                                  *
 **********************************************************************************************/
void *compact_FLOAT_4_8(void *unpackedArrayOfFloat, void *packedHeader, void *packedArrayOfInt,
                    int elementCount, int bitSizeOfPackedToken, int off_set, int stride,
                    int opCode, int hasMissing, void *missingTag )
{


  /*******************************************
   *                                         *
   *    declare header type                  *
   *                                         *
   ******************************************/
    typedef struct
    {
#if defined(Little_Endian)
      word counter : 20, marker : 12, minSign : 4, minExpo : 12, rangeExpo : 16;
      word minMantisa32 : 32, emptySpace : 8, bitSize : 8, minMantisa16 : 16;
#else
      word marker : 12, counter : 20, rangeExpo : 16, minExpo : 12, minSign : 4;
      word minMantisa32 : 32, minMantisa16 : 16, bitSize : 8, emptySpace : 8 ;
#endif
    }xxpack_struct_data;


    /****************************************
     *                                      *
     *     variables used by the packer     *
     *                                      *
     ***************************************/
    int wordSize;
    FLOAT_4_8 *arrayOfFloat;
    word *packHeader, *arrayOfInt;
    int i, k;
    word floatCount;

    double maxFloat, minFloat;
    ALL_FLOAT rangeTemplate;
    double desiredRange;
    word signOfMinFloat;
    word scaledExpOfMinFloat, scaledExpOfRange;
    double mulFactor;
    int  lastPackBit, spaceInLastWord, lastSlot;
    word lastWordShifted;
    unsigned int tempInt;
    ALL_FLOAT minFloatTemplate;
    word tempFloat;
    word tempMantisa1, tempMantisa2;
    word *arrayPtr, *arrayOfUnpacked;
    int  headerStyle;
    word headerType, countLower20, countUpper8;


    /*****************************************
     *                                       *
     *    variables used by the unpacker     *
     *                                       *
     ****************************************/
    xxpack_struct_data *theHeader;
    word currentWord;
    word intCount;

    word rangeExponent;
    int firstPackBit;
    word bitPackInFirstWord;
    int currentSlot;
    word packInt;
    word tempExp;
    word rangeExpo;
    ALL_FLOAT floatTemplate;
    int significantBit, inSignificantBit;
    float missingValueTag = *((FLOAT_4_8 *)missingTag);
    word missingToken;
    int tempExpo;
    word * tempArrayOfInt;
    ftnword *tempArrayOfFtnword;
    int tokenSize;
    int EffectivePackedTokenSize=0;      /* only set with special case when bitSizeOfPackedToken > 64 */
    int token16OnNEC = 0;

    /****************************************
     *                                      *
     *   handle abnormal condition          *
     *                                      *
     ***************************************/
    /* token size is 0 */
    if ( bitSizeOfPackedToken == 0 )
      {
        return NULL;
      };
    /* missing value handling routine fails if token size is 1 */
    if (( bitSizeOfPackedToken == 1 ) &&  hasMissing )
      {
        return NULL;
      };


    if (bitSizeOfPackedToken > 64) {
      EffectivePackedTokenSize = bitSizeOfPackedToken >> 6;
      bitSizeOfPackedToken &= 0x3F;
/*      fprintf(stderr,"Debug+++ compact_float nbits > 64 EffectivePackedTokenSize=%d bitSizeOfPackedToken=%d opCode=%d\n",
             EffectivePackedTokenSize,bitSizeOfPackedToken,opCode); */
      }
    else
      EffectivePackedTokenSize = bitSizeOfPackedToken;
    /****************************************
     *                                      *
     *   obtain an array of power of 2      *
     *                                      *
     ***************************************/
    if ( ! powerOf2sInitialized )
      {
        powerOf2s[0] = 1.0;
        for ( i = 1; i < powerSpan; i++)
          {
            powerOf2s[i] = 2.0 *powerOf2s[i-1];
          };
        powerOf2sInitialized = 1;
      };


    /****************************************
     *                                      *
     *    determine wordsize                *
     *                                      *
     ***************************************/
    wordSize = 8 * sizeof(word);


    if ( opCode == FLOAT_PACK )
      /********************************************************************
       *                                                                  *
       *       compact a floating point array into an integer array       *
       *                                                                  *
       ********************************************************************/
    {

      /*********************************************************
       *                                                       *
       *           declare variables                           *
       *                                                       *
       *********************************************************/
      arrayOfFloat = (FLOAT_4_8 *)unpackedArrayOfFloat;
      tempArrayOfInt = NULL;

      tempArrayOfInt = (word *)malloc(sizeof(word)*elementCount*stride);
      packHeader   = (word  *)packedHeader;
      arrayOfInt   = (word  *)packedArrayOfInt;
      floatCount = elementCount;


      /*********************************************************
       *                                                       *
       *  determine the missing token and header style         *
       *                                                       *
       ********************************************************/
      if ( bitSizeOfPackedToken != wordSize )
        {
          missingToken = ~(-1 << bitSizeOfPackedToken);
        }
      else
        {
          missingToken = ~0;
        };

      if (( &packHeader[3] == arrayOfInt ) && ( off_set == 24 )
          ||
          ( &packHeader[0] == arrayOfInt ) && ( off_set == 120 ) )
        {
          headerStyle = 1;
          /*  message de warning enleve
          if ( floatCount > (powerOf2s[20]-1) )
            {
              printf("\n element count overflow in xxpack header \n");
            }
          */
        }
      else
        {
          headerStyle = 2;
          if ( floatCount > (powerOf2s[28]-1) )
            {
              printf("\n element count overflow in xxpack header \n");
              return NULL;
            }
        };
      countLower20 = (floatCount << 12 ) >> 12;
      countUpper8  = (floatCount << 4 ) >> 24;



      /*********************************************************
       *                                                       *
       *      obtain the minimum and maximun                   *
       *                                                       *
       ********************************************************/
      if ( hasMissing == 0 )
        /*****************************************
         *                                       *
         * no missing value indicated            *
         *                                       *
         ****************************************/
        {
          maxFloat = arrayOfFloat[0];
          minFloat = arrayOfFloat[0];

          for(i=stride; i < floatCount*stride ; i+=stride)
            {
               if ( arrayOfFloat[i] < minFloat )
                {
                  minFloat = arrayOfFloat[i];
                }
              else if ( arrayOfFloat[i] > maxFloat )
                {
                  maxFloat = arrayOfFloat[i];
                }; /* if */
            }; /* for */
        }
      else
        /*****************************************
         *                                       *
         *  existence of missing value indicated *
         *                                       *
         ****************************************/
        {
          /* initialize min and max */
          i = 0 ;
          while ( arrayOfFloat[i] == missingValueTag )
            i += stride;
          maxFloat = arrayOfFloat[i];
          minFloat = arrayOfFloat[i];
          /* traverse the array to search the actual min and max */
          for(i=stride; i < floatCount*stride ; i+=stride)
            {
              if ( arrayOfFloat[i] == missingValueTag  )
                {
                  /* ignore the missing value */
                }
              else if ( arrayOfFloat[i] < minFloat )
                {
                  minFloat = arrayOfFloat[i];
                }
              else if ( arrayOfFloat[i] > maxFloat )
                {
                  maxFloat = arrayOfFloat[i];
                }; /* if */
            }; /* for */
        }; /* else */

      if ((maxFloat > MAX_RANGE) || (minFloat < -MAX_RANGE)) {
        fprintf(stderr,
                "\n***ERROR: floating point packer, number too large minFloat=%E maxFloat=%E\n",
                minFloat,maxFloat);
        exit(33);
      }
      rangeTemplate.XD = (maxFloat - minFloat)*2;
      minFloatTemplate.XD = minFloat;



      /***********************************************************
       *                                                         *
       *      obtain range & minimum                             *
       *                                                         *
       **********************************************************/
      rangeTemplate.MD.mantis1 = 0;
      rangeTemplate.MD.mantis2 = 0;
      rangeTemplate.MD.mantis3 = 0;
      if (rangeTemplate.XD == 0)
        tempInt = 0;
      else
        tempInt = (INT_64) (( maxFloat - minFloat ) * powerOf2s[bitSizeOfPackedToken] / rangeTemplate.XD);

      if ( ( tempInt == missingToken )  && ( hasMissing ) )
        {

          rangeTemplate.MD.expo++;
        }

    /************************************************
     *                                              *
     *       obtain the scaled exponent of range    *
     *       and minumum float                      *
     *                                              *
     ************************************************/
      /* 1024 -1 = 1023 adjusted bias to account for the hidden leading mantisa bit */
      tempExpo = (rangeTemplate.XD == 0) ? 0 : (rangeTemplate.MD.expo - 1023);
      /*  tempExpo = rangeTemplate.MD.expo - 1023;  */

      scaledExpOfMinFloat = minFloatTemplate.MD.expo - 1023 + 1024 - 48;


      scaledExpOfRange = tempExpo - bitSizeOfPackedToken;


    /*************************************************************
     *                                                           *
     *      obtain desired range and                             *
     *      sign of minimum floating point number                *
     *                                                           *
     *************************************************************/
    desiredRange = rangeTemplate.XD;
    signOfMinFloat = ( minFloat < 0 )? 1 : 0;
    if ( minFloat == 0.0 )
      {
        scaledExpOfMinFloat = scaledExpOfMinFloat & 0x00000111;
      };


    /*************************************************************************
     *                                                                       *
     *              initialize the header of the integer  array              *
     *              ===========================================              *
     *                                                                       *
     * position 0: the total number of floating point number being packed    *
     * position 1: range's exponent scaled, minimum float's exponent scaled  *
     *             down by 48 and its sign                                   *
     * position 2: mantisa of minimun float with the hidden leading bit and  *
     *             scaled 48 bit to right                                    *
     * position 3: bit size of each packed integer                           *
     *                                                                       *
     ************************************************************************/
    if ( headerStyle == 1 )
      {
        if (hasMissing == 1)
          {
            headerType = 0x7ef;
          }
        else
          {
            headerType = 0x7ff;
          };
      }
    else
      {
        if (hasMissing == 1 )
          {
            headerType = 0xfef;
          }
        else
          {
            headerType = 0xfff;
          };
      };



    packHeader[0] = headerType << 20 | countLower20;

    packHeader[1] = ((scaledExpOfRange + 4096) << 16) |
                    ((scaledExpOfMinFloat << 4) | signOfMinFloat);

        if ( minFloat == 0.0 )
          {
            packHeader[2] = 0;
          }
        else
          {
            tempMantisa1 = minFloatTemplate.MD.mantis1;
            tempMantisa2 = minFloatTemplate.MD.mantis2;
            packHeader[2] = ( -1 << (wordSize - 1) ) | (tempMantisa1 << 11) | (tempMantisa2 << 8);
          };
    packHeader[3] = bitSizeOfPackedToken << 8 | countUpper8;

    /**************************************************************************
     *                                                                        *
     *  obtain multiplication factor                                          *
     *                                                                        *
     *************************************************************************/


    {

      double two=2.0;
      ftnword expos_ftn;
      expos_ftn = tempExpo;
      mulFactor = powerOf2s[bitSizeOfPackedToken] / f77name(f_pow)(&two,&expos_ftn);
    }

    /***********************************************************************
     *                                                                     *
     *    transform the floating point  into                               *
     *    the desired integer representation                               *
     *    and pack them into an integer array                              *
     *                                                                     *
     **********************************************************************/
        /******************************
         *                            *
         * initialize other variables *
         *                            *
         *****************************/
        lastPackBit = off_set;
        spaceInLastWord =  wordSize - ( lastPackBit % wordSize );
        lastSlot = ( lastPackBit / wordSize );

        if ( spaceInLastWord == wordSize )
          {
            lastWordShifted = 0;
          }
        else
          {
            lastWordShifted = arrayOfInt[lastSlot] >> spaceInLastWord ;
          };

        arrayPtr = &arrayOfInt[lastSlot];
        arrayOfUnpacked = (word *)arrayOfFloat;
        if (( spaceInLastWord == wordSize ) && ( bitSizeOfPackedToken == wordSize ))
          /**************************
           *                        *
           *   direct copy          *
           *                        *
           *************************/
          {

            for ( i = 0; i < floatCount*stride; i+=stride)
              {
                if ( ( hasMissing == 1 ) && ( arrayOfFloat[i] == missingValueTag ) )
                  {
                    tempInt = missingToken;
                  }
                else
                  {
                    tempInt = ( arrayOfFloat[i] - minFloat ) * mulFactor ;


                  };
                *arrayPtr = tempInt;
                arrayPtr++;
              }

          }
        else
          /**************************
           *                        *
           *  bit by bit shuffle    *
           *                        *
           *************************/
          {

            for ( i = 0; i < floatCount*stride; i+=stride)
              {

                if ( ( hasMissing == 1 ) && ( arrayOfFloat[i] == missingValueTag ) )
                  {
                    tempInt = missingToken;
                  }
                else
                  {
                    tempInt = (INT_64) (( arrayOfFloat[i] - minFloat ) * mulFactor) ;
                  };

                stuff(tempInt, arrayPtr, wordSize, EffectivePackedTokenSize, lastWordShifted,
                      spaceInLastWord)

                  }; /* for */
          };

        /********************************
         *                              *
         *  squeezes hole left in       *
         *   the integer array          *
         *                              *
         *******************************/
        if ( spaceInLastWord < wordSize )
          {
            *arrayPtr = ( lastWordShifted << spaceInLastWord) |
              ( *arrayPtr & ~(-1 << spaceInLastWord));
          };

    free (tempArrayOfInt);
    tempArrayOfInt = NULL;
    return (word *)arrayOfInt;

  }


  else if ( opCode == FLOAT_UNPACK )
    /**********************************************************************************
     *                                                                                *
     *       u n p a c k e d                                                          *
     *                                                                                *
     *********************************************************************************/
  {

    /********************************************************
     *                                                      *
     *    initialize variables                              *
     *    retrieve information from the header              *
     *                                                      *
     *******************************************************/
    token16OnNEC= 0;
    arrayOfFloat   = (FLOAT_4_8 *)unpackedArrayOfFloat;
    tempArrayOfInt = NULL;

    /*******************************************
     *                                         *
     * extra space neccessary since this array *
     * is delcared as a three dimension array  *
     * [ stride, 2, elementCount/(stride*2)+1 ]*
     * in fotran routine aazz1                 *
     *                                         *
     ******************************************/
    tempArrayOfInt = (word *)malloc(sizeof(word)*(elementCount+2)*stride);
    tempArrayOfFtnword = NULL;

    tempArrayOfFtnword = (ftnword *)malloc(sizeof(ftnword)*elementCount*stride);
    theHeader      = ( xxpack_struct_data *) packedHeader;
    arrayOfInt     = ( word *) packedArrayOfInt;
    if (( theHeader->marker == 0x7ff ) || ( theHeader->marker == 0x7ef ))
      {
      if ( theHeader->counter != elementCount )
         /*************************************
          *                                   *
          *  legacy data with more than       *
          *  2POW(20) elements but only       *
          *  120-bit header and 20-bit ninj   *
          *                                   *
          ************************************/
         {
         intCount = elementCount;
         if ((intCount & 0x3777777) != (elementCount & 0x3777777))
           printf(
 "warning !  UNPACK: ninj from argument: %d different from ninj from header: %d\n argument ninj is used \n",
 elementCount, theHeader->counter);
         }
      else
         {
         intCount = theHeader->counter;
         };
      }
    else
      {
        intCount = (theHeader->emptySpace)<<20 | theHeader->counter ;
      };

    tokenSize = theHeader->bitSize;
    EffectivePackedTokenSize = tokenSize;
    if ( tokenSize != wordSize )
      {
        missingToken = ~(-1 << tokenSize);
      }
    else
      {
        missingToken = ~0;
      };



    /*******************************************************
     *                                                     *
     *    machine dependent info                           *
     *                                                     *
     ******************************************************/

    rangeExponent = theHeader->rangeExpo - 4096 + 127 + tokenSize;
/*
    if (rangeExponent >= 127)
        mulFactor = powerOf2s[rangeExponent-127] / powerOf2s[tokenSize];
    else
        mulFactor = (1. / (powerOf2s[127-rangeExponent])) / powerOf2s[tokenSize];
*/
    {
      double two=2.0, expos;
      int expos_i;
      wordint expos_ftn;  /* bug on the NEC, can not pass expos_i directly to f_pow */
      expos_i = (rangeExponent - 127 - tokenSize);
      expos_ftn = expos_i;
      mulFactor = f77name(f_pow)(&two,&expos_ftn);
    }
    if ( ( theHeader->minMantisa32 == 0 ) || ( theHeader->minExpo < 849 ) )
      {
        minFloat = 0;
      }
    else
      {
        minFloatTemplate.M.sign = theHeader->minSign;
        minFloatTemplate.M.expo = theHeader->minExpo + 127 - 1024 + 48;
        minFloatTemplate.M.mantis = (theHeader->minMantisa32 >> 8 ) & 0x7fffff;
        minFloat = minFloatTemplate.X;
        /*
        printf("Debug sign=%d minExpo= %d expo=%d mantis=%x minFloat=%f mulFactor=%f \n",
               minFloatTemplate.M.sign,theHeader->minExpo,
               minFloatTemplate.M.expo,minFloatTemplate.M.mantis,minFloat,mulFactor);
        */
      };
        firstPackBit = off_set;
        bitPackInFirstWord =  wordSize - ( firstPackBit % wordSize );
        currentSlot = ( firstPackBit / wordSize );
        currentWord = arrayOfInt[currentSlot] << ( wordSize - bitPackInFirstWord );
        /*
        printf("Debug firstPackBit=%d bitPackInFirstWord=%d currentSlot=%d currentWord=%d\n",
               firstPackBit,bitPackInFirstWord,currentSlot,currentWord);
        */
        if ( tokenSize > wordSize )
          {
            significantBit = wordSize;
            inSignificantBit = tokenSize - wordSize;
          }
        else
          {
            significantBit = EffectivePackedTokenSize;
            inSignificantBit = 0;
          };



        /******************************************************
         *                                                    *
         *    unpack floating point numbers                   *
         *    from its integer representation                 *
         *                                                    *
         *****************************************************/
        arrayPtr = &arrayOfInt[currentSlot];
        for ( i = 0; i < intCount*stride; i+=stride)
          {
            extract(packInt, arrayPtr, wordSize, significantBit,
                    currentWord, bitPackInFirstWord);
            /*            printf("Debug i=%d packInt=%X\n",i,packInt);  */

            /******************************************
             *                                        *
             *   truncate extra bit if necessary      *
             *                                        *
             *****************************************/
            if ( inSignificantBit > 0 )
              {
                discard(arrayPtr, wordSize, inSignificantBit, currentWord, bitPackInFirstWord);
              }


            if ( ( hasMissing == 1 ) && ( packInt == missingToken ) )
              {
                arrayOfFloat[i] = missingValueTag;

              }
            else if ( packInt == 0 )
              {
                arrayOfFloat[i] = minFloat;
              }
            else
              {
                arrayOfFloat[i] = (packInt  *  mulFactor) * 1.0000000000001 + minFloat;
              }
          };/* for */
    /*************************
     *                       *
     *    book keeping       *
     *                       *
     ************************/
    free( tempArrayOfInt );
    free( tempArrayOfFtnword);
    tempArrayOfInt = NULL;
    tempArrayOfFtnword= NULL;
    return ((word *)arrayOfFloat);

  }
  else
  {
    printf("\n opCode is not defined \n");
    return NULL;
  };/* if */



} /* end compact_float */
#undef compact_FLOAT_4_8
#undef isDouble
#undef FLOAT_4_8
