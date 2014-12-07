/* RMNLIB - Library of useful routines for C and FORTRAN programming
 * Copyright (C) 1975-2014  Division de Recherche en Prevision Numerique
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

#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <rpnmacros.h>
#include <unistd.h>
#include <zfstlib.h>

#define WITH_OFFSET
#include <pack_macros_64.h>

/*****************************************************************************
 *                                                                           *
 *  Objective : extract a token from a stream of 64 bit packed tokens stored in 32 bit words   *
 *  Arguments :                                                              *
 *      OUT     packedToken           token extracted                        *
 *   IN/OUT     packedWordPtr         pointer to the token to be extracted   *
 *   IN         bitSizeOfPackedToken  size of a token in bit                 *
 *   IN/OUT     packedWord            word holding the desired token         *
 *   IN/OUT     bitPackInWord         no. of bits remained packed            *
 *                                    in the packedWord                      *
 *                                                                           *
 ****************************************************************************/
#if defined (TEST_TURBO)
#define extract32( Token, packedWordPtr, notused ,bitSizeOfPackedToken, packedWord, bitPackInWord)  \
  {                                                                       \
        Token = (packedWord >> ( 32 - bitSizeOfPackedToken ) );           \
        packedWord <<= bitSizeOfPackedToken;                              \
        bitPackInWord -= bitSizeOfPackedToken;                            \
        if(bitPackInWord <= 0 )                                           \
          {                                                               \
            packedWordPtr++;                                              \
            packedWord = *packedWordPtr;                                  \
            if(bitPackInWord<0) {                                         \
              Token |= ( packedWord >> ( 32 + bitPackInWord));            \
              packedWord <<= ( - bitPackInWord );                         \
            }                                                             \
            bitPackInWord += 32  ;                                        \
          }                                                               \
  }
#endif
/*
-----------------------------------------------------------------------
  Librairie de compression des enregistrements de fichiers standards RPN
  Version 1.0 - Juillet 2004
  Version 2.0 - Avril 2006
  Yves Chartier - RPN - Juillet 2004
  Michel Valin - RPN - 2014 : Optimisations (minimum et parallelogram)
--------------------------------------------------------------------------
*/

#if defined(TEST_TURBO)
void unpackTokensMinimumOLD(void *ufld, void *z, int ni, int nj, int nbits, int istep, word *header);
void unpackTokensParallelogramOLDOLD(void *ufld, void *z, int ni, int nj, int nbits, int istep, word *header);
void unpackTokensParallelogramOLD(void *ufld, void *z, int ni, int nj, int nbits, int istep, word *header);
void packTokensParallelogramOLD(unsigned int z[], int *zlng, unsigned short ufld[], int ni, int nj, int nbits, int istep, word *header);
void packTokensMinimumOLD(unsigned int z[], int *zlng, unsigned short ufld[], int ni, int nj, int nbits, int istep, word *header);
#endif
#if defined(USE_FSTZIP_SAMPLE)
void unpackTokensSample(unsigned int zc[], int diffs[], unsigned int z[], int nicoarse, int njcoarse,  int ni, int nj, int nbits, int step, word *header, int start);
void c_fstunzip_sample(void *fld, void *zfld, int ni, int nj, int step, int nbits, word *header);
void c_fstzip_sample(unsigned int *zfld, int *zlng, unsigned short *fld, int ni, int nj, int step, int nbits, word *header);
void calcul_ninjcoarse(int *nicoarse, int *njcoarse, int ni, int nj, int ajus_x, int ajus_y, int istep);
void calcule_entropie(float *entropie, unsigned short *bitstream, int npts, int nbits);
void fixpredflds(int *predfld, int *zc, int ni, int nj, int nicoarse, int njcoarse, int step, int ajus_x, int ajus_y);
int is_on_coarse(int i, int j, int ni, int nj, int step);
void packTokensSample(unsigned int z[], int *zlng, unsigned int zc[], int nicoarse, int njcoarse, int diffs[], int ni, int nj, int nbits, int step, word *header, int start, int end);
void calcul_ajusxy(int *ajus_x, int *ajus_y, int ni, int nj, int istep);
#endif

int armn_compress(void *fld, int ni, int nj, int nk, int nbits, int op_code);
void c_armn_compress_setlevel(int level);
int  c_armn_compress_getlevel();
int c_armn_compress_getlevel_hint(int ni, int nj, int nk, int nbits);
void c_armn_compress_setswap(int swapState);
int  c_armn_compress_getswap();
void c_armn_compress_option(char *option, char *value);

int c_fstzip_getlevel();
void c_fstzip_setlevel(int level);
void c_fstzip(void *zfld, int *zlng, void *fld, int ni, int nj, int methode, int degre, int step, int nbits, int bzip);
void  packTokensMinimum(unsigned int z[], int *zlng, unsigned short ufld[], int ni, int nj, int nbits, int istep, word *header);
void  packTokensParallelogram(unsigned int z[], int *zlng, unsigned short ufld[], int ni, int nj, int nbits, int istep, word *header);

void c_fstunzip(void *fld, void *zfld, int ni, int nj, int nbits);
void unpackTokensMinimum(void *ufld, void *z, int ni, int nj, int nbits, int istep, word *header);
void unpackTokensParallelogram(void *ufld, void *z, int ni, int nj, int nbits, int istep, word *header);

void init_comp_settings(char *comp_settings);


#if defined(TEST_TURBO)
static int USE_NEW=1;   /* use new code , USE_NEW=0 only used for regression and speed tests */
#endif

static int fstcompression_level = -1;
static int swapStream           =  1;
#define FASTLOG_SIZE 257
static unsigned char fastlog[FASTLOG_SIZE];   // fast way to get base 2 log of 1 thru 255 (special twist for 0)
static int once = 0;
static int zfst_msglevel = 2;

static unsigned int LittleEndianInteger = 1;
static unsigned char *IsLittleEndian = (unsigned char *)&LittleEndianInteger;  /* *IsLittleEndian == 1 on little endian machines, 0 en big endian */

/*-------------------------------------------------------------------------------------------------------------------- */
/*               Main entry points to the compression routines (armn_compress, c_fstzip, c_fstunzip)                   */
/*-------------------------------------------------------------------------------------------------------------------- */
int armn_compress(void *fld, int ni, int nj, int nk, int nbits, int op_code_in)
{
  unsigned short *unzfld;
  unsigned int *us_fld, *zfld_minimum, *zfld_lle;
  int zlng_minimum, zlng_lle, lng_origin;
  int i, j, nbits_needed;
  void *junk;
  int op_code = op_code_in;
  int data_len=32;

  int limite = (1 + ni*nj)/2;

//  int initial_compression_level;
  
  if(op_code_in > 256) {
      op_code = op_code_in & 0xFF;
      data_len = op_code_in >> 8;
  }

//  initial_compression_level = c_armn_compress_getlevel();
  lng_origin = (1+ni*nj*nk*16/8);

//  if (initial_compression_level == -1) {   // redundant, fstcompression_level is initialized by c_armn_compress_getlevel
//    fstcompression_level = BEST;
//  }

  if (once == 0) {
   nbits_needed = 1;
   i = 1; fastlog[0] = 0; fastlog[FASTLOG_SIZE-1] = 9;
   for(j=2 ; j<FASTLOG_SIZE ; j*=2 , nbits_needed++) {  // get rid of need for -lm when loading
     while(i<j) {
       fastlog[i++] = nbits_needed;
     }
   }
   once = 1;
  }

  switch (op_code) {

    case COMPRESS:
      if (nbits > 16 || ni == 1 || nj == 1) {
        if (zfst_msglevel <= 2) {
          fprintf(stderr, "Can not compress if nbits>16 or ni=1 or nj=1 ... Returning original field\n\n");
        }
        return -1;
      }

      us_fld = (unsigned int *) fld;
//      zfld_minimum = (unsigned int *) malloc(sizeof(unsigned int)*ni*nj*nk);
//      zfld_lle     = (unsigned int *) malloc(sizeof(unsigned int)*ni*nj*nk);

      if ((swapStream == 1) && (*IsLittleEndian == 1) && (data_len==32)) {
        for (i=0; i < limite; i++) {
          us_fld[i] = (us_fld[i] >> 16) | (us_fld[i] << 16);
        }
      }

//      if (FAST == fstcompression_level || (ni < 16) || (nj < 16) || (nbits <= 4)) {   /* do not use parallelogram method */
        if (FAST == c_armn_compress_getlevel_hint(ni,nj,nk,nbits) ) {   /* do not use parallelogram method */
        zfld_minimum = (unsigned int *) malloc(sizeof(unsigned int)*ni*nj*nk);     /* this needed space can be computed less aggressively */
        c_fstzip(zfld_minimum, &zlng_minimum, us_fld, ni, nj, MINIMUM, 0, 5, nbits, 0);
        if (zlng_minimum >= lng_origin) {

          if ((swapStream == 1) && (*IsLittleEndian == 1) && (data_len==32)) {
            for (i=0; i < limite; i++) {
              us_fld[i] = (us_fld[i] >> 16) | (us_fld[i] << 16);
            }
          }

          if (zfst_msglevel <= 2) {
            fprintf(stderr, "Compressed field is larger than original... Returning original\n\n");
          }
          free(zfld_minimum);
//          free(zfld_lle);
          return -1;
        } else {
          junk = memcpy(fld, zfld_minimum, zlng_minimum);
          free(zfld_minimum);
//          free(zfld_lle);
          return zlng_minimum;
        }
      }

      zfld_lle     = (unsigned int *) malloc(sizeof(unsigned int)*ni*nj*nk);     /* this needed space can be computed less aggressively */
      c_fstzip(zfld_lle,     &zlng_lle,     us_fld, ni, nj, PARALLELOGRAM, 1, 3, nbits, 0);

      if (zlng_lle >= lng_origin) {

        if ((swapStream == 1) && (*IsLittleEndian == 1) && (data_len==32)) {
          for (i=0; i < limite; i++) {
              us_fld[i] = (us_fld[i] >> 16) | (us_fld[i] << 16);
          }
        }

        if (zfst_msglevel <= 2) {
          fprintf(stderr, "Compressed field is larger than original... Returning original\n\n");
        }
//        free(zfld_minimum);
        free(zfld_lle);
        return -1;
      }

      junk = memcpy(fld, zfld_lle, zlng_lle);
//      free(zfld_minimum);
      free(zfld_lle);
  /*  printf("zlng_lle: %d\n", zlng_lle);*/
      return zlng_lle;
      break;

    case UNCOMPRESS:
    if (nbits > 16 || ni == 1 || nj == 1)
      {
      return (1+ni*nj*nk*nbits/8);
      }

    us_fld = (unsigned int *) fld;


    unzfld = (unsigned short *)malloc(sizeof(unsigned int)*ni*nj);
    c_fstunzip((unsigned int *)unzfld, (unsigned int *)fld, ni, nj, nbits);
    junk = memcpy(fld, unzfld, (1+ni*nj/2)*sizeof(unsigned int));

    if ((swapStream == 1) && (*IsLittleEndian == 1) && (data_len==32)) {
      for (i=0; i < limite; i++) {
        us_fld[i] = (us_fld[i] >> 16) | (us_fld[i] << 16);
      }
    }

    free(unzfld);
    return ni*nj*sizeof(short);
    break;
  }
return 0;
}

/**********************************************************************************************************************************/

void c_fstzip(void *zfld, int *zlng, void *fld, int ni, int nj, int methode, int degre, int step, int nbits, int bzip)
{
  _fstzip zfstzip;
   void *junk;

  junk = memset(&zfstzip, (int) 0, sizeof(_fstzip));
  
  if(methode == AUTO) { 
    methode = c_armn_compress_getlevel_hint(ni, nj, 1, nbits);   /* pass nk = 1 to c_armn_compress_getlevel_hint */
    step = (methode == MINIMUM)       ? 5 : step; 
    step = (methode == PARALLELOGRAM) ? 3 : step; 
  }

  zfstzip.predictor_type = methode;
  zfstzip.step           = step;     /* usually 5 for MINIMUM and 3 for PARALLELOGRAM */
  zfstzip.degree         = degre;    /* no longer used since deprecation of SAMPLE option */
  zfstzip.nbits          = nbits;
  zfstzip.levels         = 1;        /* no longer used since deprecation of SAMPLE option */
  zfstzip.version        = 0;

  switch (methode)
    {
    case MINIMUM:
      packTokensMinimum(zfld, zlng, (unsigned short *)fld, ni, nj, nbits, step,  (word *) &zfstzip);
      break;

    case PARALLELOGRAM:
      packTokensParallelogram(zfld, zlng, (unsigned short *)fld, ni, nj, nbits, step,  (word *) &zfstzip);
      break;

    case SAMPLE:
      fprintf(stderr, "FATAL: The SAMPLE option has been deactivated as of April 2006.\n");
      exit(13);
/*      c_fstzip_sample(zfld, zlng, (unsigned short *)fld, ni, nj, step, nbits, (word *) &zfstzip);*/
      break;

    default:
      break;
    }
 }

/**********************************************************************************************************************************/

void c_fstunzip(void *fld, void *zfld, int ni, int nj, int nbits)
{
  _fstzip zfstzip;
  void *junk;

  junk = memset(&zfstzip, (int) 0, sizeof(_fstzip));
  junk = memcpy(&zfstzip, zfld, sizeof(_fstzip));

  switch (zfstzip.predictor_type)
    {
    case MINIMUM:
      unpackTokensMinimum(fld, zfld, ni, nj, zfstzip.nbits, zfstzip.step, (word *)&zfstzip);
      break;

    case PARALLELOGRAM:
      unpackTokensParallelogram(fld, zfld, ni, nj, zfstzip.nbits, zfstzip.step, (word *)&zfstzip);
      break;
#if defined(USE_FSTZIP_SAMPLE)
    case SAMPLE:
      c_fstunzip_sample(fld, zfld, ni, nj, zfstzip.step, zfstzip.nbits, (word *)&zfstzip);
      break;
#endif
    default:
      fprintf(stderr, "**************************************************************************\n");
      fprintf(stderr, "****                                                                  ****\n");
      fprintf(stderr, "****  Unsupported compression algorithm...                            ****\n");
      fprintf(stderr, "****  Contact MRB computer support for advice... service.rpn@ec.gc.ca ****\n");
      fprintf(stderr, "****  Exiting now...                                                  ****\n");
      fprintf(stderr, "****                                                                  ****\n");
      fprintf(stderr, "**************************************************************************\n");
      exit(13);
      break;
    }

}
/**********************************************************************************************************************************/
#if defined(USE_FSTZIP_SAMPLE)
void c_fstzip_sample(unsigned int *zfld, int *zlng, unsigned short *fld, int ni, int nj, int step, int nbits, word *header)
  {
  int ajus_x, ajus_y, ninj, i, j, k;
  unsigned int *zc,  *zc1, *zc2;
  int *ufld, *predfld, *idiffs, lclstep, lclni, lclnj, start, end;
  int nic,njc,nic1, njc1, nic2,njc2,ajus_x1,ajus_x2,ajus_y1,ajus_y2;
  int *predfld1, *predfld2, *idiffs1, *idiffs2;

  ninj = ni * nj;

  lclstep = step;
  lclni = ni;
  lclnj = nj;
  ufld = malloc(ninj*sizeof(int));

  for (i=0; i < ninj; i++)
    {
    ufld[i] = fld[i];
    }

  calcul_ajusxy(&ajus_x, &ajus_y, ni, nj, step);
  calcul_ninjcoarse(&nic, &njc, ni, nj, ajus_x, ajus_y, step);

  calcul_ajusxy(&ajus_x1, &ajus_y1, nic, njc, step);
  calcul_ninjcoarse(&nic1, &njc1, nic, njc, ajus_x1, ajus_y1, step);

  calcul_ajusxy(&ajus_x2, &ajus_y2, nic1, njc1, step);
  calcul_ninjcoarse(&nic2, &njc2, nic1, njc1, ajus_x2, ajus_y2, step);

  zc = malloc(nic*njc*sizeof(int));
  predfld = malloc(ninj*sizeof(int));
  idiffs = malloc(ninj*sizeof(int));

  zc1 = malloc(nic1*njc1*sizeof(int));
  predfld1 = malloc(nic*njc*sizeof(int));
  idiffs1 = malloc(nic*njc*sizeof(int));

  zc2 = malloc(nic2*njc2*sizeof(int));
  predfld2 = malloc(nic1*njc1*sizeof(int));
  idiffs2 = malloc(nic1*njc1*sizeof(int));

  /*************** Niveau 1 *******************/

  f77name(fill_coarse_grid)(zc, &nic, &njc, ufld, &lclni, &lclnj, &lclstep);
  f77name(fill_coarse_nodes)(predfld, &lclni, &lclnj, zc, &nic, &njc, &lclstep);
  f77name(ibicubic_int4)(predfld,&lclni,&lclnj,&lclstep,&ajus_x, &ajus_y);
  f77name(fill_coarse_nodes)(predfld, &lclni, &lclnj, zc, &nic, &njc, &lclstep);

  for (j=1; j <= nj; j++)
    {
    for (i=1; i <= ni; i++)
      {
      k = FTN2C(i, j, ni);
      idiffs[k] = ufld[k] - predfld[k];
      }
    }


  /*************** Niveau 2 *******************/


  f77name(fill_coarse_grid)(zc1, &nic1, &njc1, zc, &nic, &njc, &lclstep);
  f77name(fill_coarse_nodes)(predfld1, &nic, &njc, zc1, &nic1, &njc1, &lclstep);
  f77name(ibicubic_int4)(predfld1,&nic,&njc,&lclstep,&ajus_x1, &ajus_y1);
  f77name(fill_coarse_nodes)(predfld1, &nic, &njc, zc1, &nic1, &njc1, &lclstep);

  for (j=1; j <= njc; j++)
    {
    for (i=1; i <= nic; i++)
      {
      k = FTN2C(i, j, nic);
      idiffs1[k] = zc[k] - predfld1[k];
      }
    }

  /*************** Niveau 3 *******************/


  f77name(fill_coarse_grid)(zc2, &nic2, &njc2, zc1, &nic1, &njc1, &lclstep);
  f77name(fill_coarse_nodes)(predfld2, &nic1, &njc1, zc2, &nic2, &njc2, &lclstep);
  f77name(ibicubic_int4)(predfld2,&nic1,&njc1,&lclstep,&ajus_x2, &ajus_y2);
  f77name(fill_coarse_nodes)(predfld2, &nic1, &njc1, zc2, &nic2, &njc2, &lclstep);

  for (j=1; j <= njc1; j++)
    {
    for (i=1; i <= nic1; i++)
      {
      k = FTN2C(i, j, nic1);
      idiffs2[k] = zc1[k] - predfld2[k];
      }
    }

  start = 1;
  end = 0;
  packTokensSample(zfld, zlng, zc2, nic2, njc2, idiffs2, nic1, njc1, nbits, step, header, start, end);


  start = 0;
  end = 0;
  packTokensSample(zfld, zlng, zc1, nic1, njc1, idiffs1, nic, njc, nbits, step, header, start, end);


  start = 0;
  end = 1;
  packTokensSample(zfld, zlng, zc, nic, njc, idiffs, ni, nj, nbits, step, header, start, end);

  free  (ufld);
  free(idiffs);
  free(idiffs1);
  free(idiffs2);
  free(predfld);
  free(predfld1);
  free(predfld2);
  free(zc);
  free(zc1);
  free(zc2);
  }

/**********************************************************************************************************************************/

void c_fstunzip_sample(unsigned short *fld, unsigned int *zfld, int ni, int nj, int step, int nbits, word *header)
  {
  unsigned int *zc, *zc1, *zc2;
  int *idiffs;
  int ajus_x, ajus_y, ninj, i, j, k;
  int *predfld, lclstep, start;
  int nic,njc,nic1, njc1, nic2,njc2,ajus_x1,ajus_x2,ajus_y1,ajus_y2;
  int *predfld1, *predfld2, *idiffs1, *idiffs2;

  ninj = ni * nj;

  lclstep = step;
  calcul_ajusxy(&ajus_x, &ajus_y, ni, nj, step);
  calcul_ninjcoarse(&nic, &njc, ni, nj, ajus_x, ajus_y, step);

  calcul_ajusxy(&ajus_x1, &ajus_y1, nic, njc, step);
  calcul_ninjcoarse(&nic1, &njc1, nic, njc, ajus_x1, ajus_y1, step);

  calcul_ajusxy(&ajus_x2, &ajus_y2, nic1, njc1, step);
  calcul_ninjcoarse(&nic2, &njc2, nic1, njc1, ajus_x2, ajus_y2, step);

  zc = malloc(nic*njc*sizeof(int));
  predfld = malloc(ninj*sizeof(int));
  idiffs = malloc(ninj*sizeof(int));

  zc1 = malloc(nic1*njc1*sizeof(int));
  predfld1 = malloc(nic*njc*sizeof(int));
  idiffs1 = malloc(nic*njc*sizeof(int));

  zc2 = malloc(nic2*njc2*sizeof(int));
  predfld2 = malloc(nic1*njc1*sizeof(int));
  idiffs2 = malloc(nic1*njc1*sizeof(int));

  /*************** Niveau 3 *******************/


  start = 1;
  unpackTokensSample(zc2, idiffs2, zfld, nic2, njc2, nic1, njc1, nbits, step, header, start);

  f77name(fill_coarse_nodes)(zc1, &nic1, &njc1, zc2, &nic2, &njc2, &lclstep);
  f77name(ibicubic_int4)(zc1,&nic1,&njc1,&lclstep,&ajus_x2, &ajus_y2);
  f77name(fill_coarse_nodes)(zc1, &nic1, &njc1, zc2, &nic2, &njc2, &lclstep);

  for (j=1; j <= njc1; j++)
    {
    for (i=1; i <= nic1; i++)
      {
      k = FTN2C(i, j, nic1);
      zc1[k] += idiffs2[k];
/*      if (zc1[k] < 0) zc1[k] = 0;*/
      }
    }

    /*************** Niveau 2 *******************/


  start = 0;
  unpackTokensSample(zc1, idiffs1, zfld, nic1, njc1, nic, njc, nbits, step, header, start);

  f77name(fill_coarse_nodes)(zc, &nic, &njc, zc1, &nic1, &njc1, &lclstep);
  f77name(ibicubic_int4)(zc,&nic,&njc,&lclstep,&ajus_x1, &ajus_y1);
  f77name(fill_coarse_nodes)(zc, &nic, &njc, zc1, &nic1, &njc1, &lclstep);

  for (j=1; j <= njc; j++)
    {
    for (i=1; i <= nic; i++)
      {
      k = FTN2C(i, j, nic);
      zc[k] += idiffs1[k];
/*      if (zc[k] < 0) zc[k] = 0;*/
      }
    }


    /*************** Niveau 1 *******************/


  start = 0;
  unpackTokensSample(zc, idiffs, zfld, nic, njc, ni, nj, nbits, step, header, start);

  f77name(fill_coarse_nodes)(predfld, &ni, &nj, zc, &nic, &njc, &lclstep);
  f77name(ibicubic_int4)(predfld,&ni,&nj,&lclstep,&ajus_x, &ajus_y);

  for (j=1; j <= nj; j++)
    {
    for (i=1; i <= ni; i++)
      {
      k = FTN2C(i, j, ni);
      predfld[k] += idiffs[k];
      if (predfld[k] < 0) predfld[k] = 0;
      }
    }


  f77name(fill_coarse_nodes)(predfld, &ni, &nj, zc, &nic, &njc, &lclstep);

  for (j=1; j <= nj; j++)
    {
    for (i=1; i <= ni; i++)
      {
      k = FTN2C(i, j, ni);
      fld[k] = (unsigned short) predfld[k];
      }
    }

  free(idiffs);
  free(idiffs1);
  free(idiffs2);
  free(predfld);
  free(predfld1);
  free(predfld2);
  free(zc);
  free(zc1);
  free(zc2);
  }
#endif
/**********************************************************************************************************************************/
/*
 Given a stream of unsigned shorts, this routine re-encodes the stream by the minimum tile method.
 The stream is decomposed into a number "step * step" tiles, for each of which the minimum value is stored, and the difference between
 each cell and the minimum for each member of the tile

 Output stream
 Token 0 contains the zfstzip structure. Starting from Token 1, we have
 |---------------|---------------|---------------------------------|
 |  nbits_needed | local_min     | local_value - local_min         |
 |---------------|---------------|---------------------------------|
 |  4 bits       |  nbits        | step * step * nbits_needed      |
 |---------------|---------------|---------------------------------|

 With the exception that if the field is invariant within the tile, nbits_needed = 0 and we only encode
 |---------------|---------------|
 |  nbits_needed | local_min     |
 |---------------|---------------|
 |  4 bits       |  nbits        |
 |---------------|---------------|

 Variables :
   z      : output compressed byte stream
   zlng   : the length of the stream, in bytes
   ufld   : the source unsigned short stream
   ni, nj : dimensions of the field
   nbits  : the number of bits used to pack the original stream - normally btn 2 and 16
   istep  : the size of the tile (normally 5)
   header : contents of the zfstzip structure for info about compression parameters
*/

#if defined(TEST_TURBO)
void packTokensMinimumOLD(unsigned int z[], int *zlng, unsigned short ufld[], int ni, int nj, int nbits, int istep, word *header)
{
  unsigned int i, j, k, m, n;
  unsigned int lastWordShifted, spaceInLastWord, lastSlot;
  int lcl_m, lcl_n;

  float entropie;
  unsigned int *cur, local_min, local_max, local_var;
  unsigned int local_bins[24];
  unsigned int lcl, nbits_needed, lsum;
  unsigned char debug;
  void *junk;

  debug = 0;
  lastSlot = 0;
  cur = z;

   if (debug)
   {
   for (i=0; i <= 16; i++)
      {
      local_bins[i] = 0;
      }
   }

  lastWordShifted = 0;
  spaceInLastWord = 32;
  junk = memcpy(cur, header, sizeof(unsigned int));
  cur++;
  *cur = 0;
  for (j=1; j <= nj; j+=istep)
    {
    lcl_n = ((j + istep - 1) >= nj ? nj - j : istep - 1);
    for (i=1; i <= ni; i+=istep)
      {
      k = FTN2C(i,j,ni);
      local_min = ufld[k];
      local_max = local_min;
      lcl_m = ((i + istep - 1) >= ni ? ni - i: istep - 1);
      for (n=0; n <= lcl_n; n++)
        {
        for (m=0; m <= lcl_m; m++)
          {
          k = FTN2C(i+m,j+n,ni);
          if (local_min > ufld[k]) local_min = ufld[k];
          if (local_max < ufld[k]) local_max = ufld[k];
          }
        }
      local_var = local_max - local_min;
      if (local_var == 0)
        {
        nbits_needed = 0;
        }
      else
        {
        if (local_var < (FASTLOG_SIZE-1))
         {
         nbits_needed = fastlog[local_var];
         }
       else
         {
         nbits_needed = 8 + fastlog[local_var>>8];
         }
        }
      if (nbits_needed == 16) nbits_needed = 15;
      stuff(nbits_needed, cur, 32, 4, lastWordShifted, spaceInLastWord);
      switch (nbits_needed)
        {
        case 0:
        stuff(local_min, cur, 32, nbits, lastWordShifted, spaceInLastWord);
        break;

        case 15:
        for (n=0; n <= lcl_n; n++)
          {
          for (m=0; m <= lcl_m; m++)
            {
            k = FTN2C(i+m,j+n,ni);
            stuff(ufld[k], cur, 32, 16, lastWordShifted, spaceInLastWord);
            }
          }
        break;

        default:
        stuff(local_min, cur, 32, nbits, lastWordShifted, spaceInLastWord);
        for (n=0; n <= lcl_n; n++)
          {
          for (m=0; m <= lcl_m; m++)
            {
            k = FTN2C(i+m,j+n,ni);
            lcl = ufld[k] - local_min;
            stuff(lcl, cur, 32, nbits_needed, lastWordShifted, spaceInLastWord);
            }
          }
        break;
        }
       }
    }


  lcl = 0;
  stuff(lcl, cur, 32, 16, lastWordShifted, spaceInLastWord);
  stuff(lcl, cur, 32, 16, lastWordShifted, spaceInLastWord);

   *zlng = 1 + (int) (cur-z) * 4;
 debug=0;
if (debug)
  {
  entropie = 8.0*(*zlng)/(ni*nj);
  printf("total_space:\t%d\tentropie:\t%f\n", *zlng,entropie );
  for (i=0; i <= 16; i++)
    {
    printf("%05d ", i);
    }
  printf("\n");
  lsum = 0;
  for (i=0; i <= 16; i++)
    {
    lsum+=local_bins[i];
    printf("%05d ", local_bins[i]);
    }
  printf("---- npts : %d \n", lsum);
  }
}

#endif

void packTokensMinimum(unsigned int *z, int *zlng, unsigned short *ufld, int ni, int nj, int nbits, int istep, word *header)
{
  unsigned int i, j, k, m, n;
  int spaceInLastWord;
  int lcl_m, lcl_n;

  unsigned int *cur, local_min, local_max, local_var;
  unsigned int lcl, nbits_needed;
  unsigned long long temp;
  void *junk;

#if defined(TEST_TURBO)
  if(USE_NEW == 0) {
    packTokensMinimumOLD(z, zlng, ufld, ni, nj, nbits, istep, header);
    return ;
  }
#endif

  cur = z;

  junk = memcpy(cur, header, sizeof(unsigned int));
  cur++;
  *cur = 0;

  start_pack_64(temp,spaceInLastWord)
  for (j=1; j <= nj; j+=istep) {
    lcl_n = ((j + istep - 1) >= nj ? nj - j : istep - 1);
    for (i=1; i <= ni; i+=istep) {
      k = FTN2C(i,j,ni);
      local_min = ufld[k];
      local_max = local_min;
      lcl_m = ((i + istep - 1) >= ni ? ni - i: istep - 1);
      for (n=0; n <= lcl_n; n++)
        {
          for (m=0; m <= lcl_m; m++)
            {
            local_var = ufld[k+m] ;
            if (local_min > local_var) local_min = local_var;
            if (local_max < local_var) local_max = local_var;
            }
        k += ni ;
        }
      local_var = local_max - local_min;
      if (local_var < (FASTLOG_SIZE-1)) nbits_needed = fastlog[local_var];
      else                 nbits_needed = 8 + fastlog[local_var>>8];

      if (nbits_needed >= 15) { nbits_needed = 15; local_min = 0 ; }
      put_bits_64(nbits_needed,4,temp,spaceInLastWord,0xFFFF)
      check_pack_64(temp,spaceInLastWord,cur)

      if (nbits_needed == 15) {
        nbits_needed = 16;
      } else {
        put_bits_64(local_min,nbits,temp,spaceInLastWord,0xFFFF)
        check_pack_64(temp,spaceInLastWord,cur)
      }

      k = FTN2C(i,j,ni);
      if (nbits_needed > 0) {
        for (n=0; n <= lcl_n; n++) {
          if(lcl_m == 4) {
            lcl = ufld[k+0] - local_min;
            put_bits_64(lcl,nbits_needed,temp,spaceInLastWord,0xFFFF)
//            check_pack_64(temp,spaceInLastWord,cur)
            lcl = ufld[k+1] - local_min;
            put_bits_64(lcl,nbits_needed,temp,spaceInLastWord,0xFFFF)
            check_pack_64(temp,spaceInLastWord,cur)

            lcl = ufld[k+2] - local_min;
            put_bits_64(lcl,nbits_needed,temp,spaceInLastWord,0xFFFF)
//            check_pack_64(temp,spaceInLastWord,cur)
            lcl = ufld[k+3] - local_min;
            put_bits_64(lcl,nbits_needed,temp,spaceInLastWord,0xFFFF)
            check_pack_64(temp,spaceInLastWord,cur)

            lcl = ufld[k+4] - local_min;
            put_bits_64(lcl,nbits_needed,temp,spaceInLastWord,0xFFFF)
            check_pack_64(temp,spaceInLastWord,cur)
          }else{
            for (m=0; m <= lcl_m; m++) {
              lcl = ufld[k+m] - local_min;
              put_bits_64(lcl,nbits_needed,temp,spaceInLastWord,0xFFFF)
              check_pack_64(temp,spaceInLastWord,cur)
            }
          }
          k += ni ;
        }  /*  for n  */
      }   /* if nbits_needed   */
    }  /* for i */
  }  /* for j */

  lcl = 0;
  end_pack_64(temp,spaceInLastWord,cur)
  *zlng = 1 + (int) (cur-z) * 4;
}

/**********************************************************************************************************************************/
/* See the documentation of "packTokensMinimum" for the structure of the compressed stream */

#if defined(TEST_TURBO)
void unpackTokensMinimumOLD(void *ufld_in, void *z_in, int ni, int nj, int nbits, int istep, word *header)
{
  unsigned short *ufld = (unsigned short *)ufld_in;
  unsigned int *z = (unsigned int *)z_in;
  int i, j, k, m, n;
  int bitPackInWord;

  unsigned int *cur, local_min;
  unsigned int  nbits_needed, curword;
  int lcl_m, lcl_n;
  void *junk;

  bitPackInWord = 32;

/*   memset(ufld, NULL, ni*nj*sizeof(short)); */
  cur = z;
  junk = memcpy(header, cur, sizeof(unsigned int));
  cur++;
  curword = *cur;
  for (j=1; j <= nj; j+=istep)
    {
    lcl_n = ((j + istep - 1) >= nj ? nj - j : istep - 1);
    for (i=1; i <= ni; i+=istep)
      {
      lcl_m = ((i + istep - 1) >= ni ? ni - i : istep - 1);
      extract32(nbits_needed, cur, 32, 4, curword, bitPackInWord);
      switch (nbits_needed)
        {
        case 0:
        extract32(local_min, cur, 32, nbits, curword, bitPackInWord);
        for (n=0; n <= lcl_n; n++)
          {
          for (m=0; m <= lcl_m; m++)
            {
            k = FTN2C(i+m,j+n,ni);
            ufld[k] = local_min;
            }
          }
        break;

        case 15:
        case 16:
        for (n=0; n <= lcl_n; n++)
          {
          for (m=0; m <= lcl_m; m++)
            {
            k = FTN2C(i+m,j+n,ni);
            extract32(ufld[k], cur, 32, 16, curword, bitPackInWord);
            }
          }
        break;

        default:
        extract(local_min, cur, 32, nbits, curword, bitPackInWord);
        for (n=0; n <= lcl_n; n++)
          {
          for (m=0; m <= lcl_m; m++)
            {
            k = FTN2C(i+m,j+n,ni);
            extract32(ufld[k], cur, 32, nbits_needed, curword, bitPackInWord);
            ufld[k] += local_min;
            }
          }
        break;
        }
      }
    }

}
#endif
void unpackTokensMinimum(void *ufld_in, void *z_in, int ni, int nj, int nbits, int istep, word *header)
{
  unsigned short *ufld = (unsigned short *)ufld_in;
  unsigned int *z = (unsigned int *)z_in;
  int i, j, k, m, n;
  int bitPackInWord;

  unsigned int *cur;
  unsigned int  nbits_needed, token, local_min;
  int lcl_m, lcl_n;
  unsigned long long temp;
  void *junk;

#if defined(TEST_TURBO)
  if(USE_NEW == 0) {
    unpackTokensMinimumOLD(ufld_in, z_in, ni, nj, nbits, istep, header);
    return;
  }
#endif

  cur = z;
//  junk = memcpy(header, cur, sizeof(unsigned int));
  cur++;   /* skip header */
  temp = *cur++;
  temp = (temp << 32)  | *cur++;
  bitPackInWord = 32;
  for (j=1; j <= nj; j+=istep) {
    lcl_n = ((j + istep) > nj ? nj - j : istep - 1);
    for (i=1; i <= ni; i+=istep) {
      get_bits_64(nbits_needed,4,temp,bitPackInWord)
      check_unpack_64(temp,bitPackInWord,cur)
      lcl_m = ((i + istep) > ni ? ni - i : istep - 1);
      k = FTN2C(i,j,ni) ;
      if(nbits_needed >= 15) {
          local_min = 0 ;
          nbits_needed = 16;
      } else {
          get_bits_64(local_min,nbits,temp,bitPackInWord)
          check_unpack_64(temp,bitPackInWord,cur)
      }
      if (nbits_needed == 0) {
        for (n=0; n <= lcl_n; n++) {
          for (m=0; m <= lcl_m; m++) {
            ufld[k+m] = local_min;
          }
          k = k + ni ;    /* bump along column */
        }
      }else{
        for (n=0; n <= lcl_n; n++) {
          if(lcl_m == 4){
            get_bits_64(token,nbits_needed,temp,bitPackInWord)
            ufld[k] = token + local_min;
            get_bits_64(token,nbits_needed,temp,bitPackInWord)
            ufld[k+1] = token + local_min;
            check_unpack_64(temp,bitPackInWord,cur)      /* as nbits <= 16 , check_pack_64 is only needed every other time */

            get_bits_64(token,nbits_needed,temp,bitPackInWord)
            ufld[k+2] = token + local_min;
            get_bits_64(token,nbits_needed,temp,bitPackInWord)
            ufld[k+3] = token + local_min;
            check_unpack_64(temp,bitPackInWord,cur)      /* as nbits <= 16 , check_pack_64 is only needed every other time */

            get_bits_64(token,nbits_needed,temp,bitPackInWord)
            check_unpack_64(temp,bitPackInWord,cur)
            ufld[k+4] = token + local_min;
          }else{
            for (m=0; m <= lcl_m; m++) {
              get_bits_64(token,nbits_needed,temp,bitPackInWord)
              check_unpack_64(temp,bitPackInWord,cur)
              ufld[k+m] = token + local_min;
            }
          }
          k = k + ni ;    /* bump along column */
        }
      }
    }
  }
}

/**********************************************************************************************************************************/
/* See the documentation of "packTokensMinimum" for the structure of the compressed stream */

#if defined(TEST_TURBO)
void packTokensParallelogramOLD(unsigned int z[], int *zlng, unsigned short ufld[], int ni, int nj, int nbits, int istep, word *header)
{
  unsigned int i, j, k, m, n;
  unsigned int lastWordShifted, spaceInLastWord, lastSlot;
  int lcl_m, lcl_n;

  float entropie, rlog2;
  unsigned int *cur;
  int local_min, local_max, local_var;
  unsigned int local_bins[24];
  unsigned int lcl, nbits_needed, lsum;
  unsigned char debug;
  int k11, k12, k21, k22, nbits2;
  unsigned int nbits_req_container, gt16, token;
  int *ufld_dst, *ufld4;
  void *junk;

  debug = 0;
  lastSlot = 0;
  cur = z;
   if (debug)
   {
   for (i=0; i < 24; i++)
      {
      local_bins[i] = 0;
      }
   }

   if (once == 0) {
      nbits_needed = 1;
      i = 1; fastlog[0] = 0;
      k = 2;
      for(k=2 ; k<FASTLOG_SIZE ; k*=2 , nbits_needed++) {  // get rid of need for -lm when loading
        while(i<k) {
          fastlog[i++] = nbits_needed;
        }
      }
      once = 1;
   }

  ufld_dst=(int *) malloc(ni*nj*sizeof(int));

  for (j=1; j <= nj; j++)
   {
   k = FTN2C(1,j,ni);
   ufld_dst[k] = 0;
   }

  for (i=1; i <= ni; i++)
   {
   k = FTN2C(i,1,ni);
   ufld_dst[k] = 0;
   }

  for (j=2; j<=nj; j++)
   {
   for (i=2; i <=ni; i++)
      {
      k22 = FTN2C(i,  j,  ni);
      ufld_dst[k22] = ufld[k22] - (ufld[k22-ni]+ufld[k22-1]-ufld[k22-1-ni]);
      }
   }

/*  ufld4=(int *)malloc(ni*nj*sizeof(int));
  for (i=0; i < ni*nj; i++)
    {
    ufld4[i] = ufld[i];
    }
  f77name(lorenzo2)(ufld_dst, ufld4, &ni, &nj);
  free(ufld4);*/

  nbits_req_container = 4;

  i = 0;
  gt16 = 0;
  if (nbits >= 15)
   {
   while (i < ni*nj && gt16 == 0)
      {
      if (65535 < abs(ufld_dst[i]))
         {
         nbits_req_container = 5;
         gt16 = 1;
         }
      i++;
      }
   }

  lastWordShifted = 0;
  spaceInLastWord = 32;
  junk = memcpy(cur, header, sizeof(unsigned int));
  cur++;
  *cur = 0;

  stuff(nbits_req_container, cur, 32, 3, lastWordShifted, spaceInLastWord);

  for (i=1; i <= ni; i++)
   {
   k = FTN2C(i,1,ni);
   stuff(ufld[k], cur, 32, nbits, lastWordShifted, spaceInLastWord);
   }

  for (j=2; j <= nj; j++)
   {
   k = FTN2C(1,j,ni);
   stuff(ufld[k], cur, 32, nbits, lastWordShifted, spaceInLastWord);
   }

  for (j=2; j <= nj; j+=istep)
    {
    lcl_n = ((j + istep - 1) >= nj ? nj - j : istep - 1);
    for (i=2; i <= ni; i+=istep)
      {
      k = FTN2C(i,j,ni);
      local_max = ufld_dst[k];
      lcl_m = ((i + istep - 1) >= ni ? ni - i : istep - 1);
      for (n=0; n <= lcl_n; n++)
        {
        for (m=0; m <= lcl_m; m++)
          {
          k = FTN2C(i+m,j+n,ni);
          if (local_max < abs(ufld_dst[k])) local_max = abs(ufld_dst[k]);
          }
        }
      if (local_max == 0)
        {
        nbits_needed = 0;
        }
      else
        {
        if (local_max < (FASTLOG_SIZE-1))
         {
         nbits_needed = fastlog[local_max];
         }
        else if (local_max > 65535)
         {
//         nbits_needed =(int)(1+log(local_max+0.5)/log(2.0));
         nbits_needed = 16 + fastlog[local_max>>16];
         }
        else if (local_max > 16777215)
         {
//         nbits_needed =(int)(1+log(local_max+0.5)/log(2.0));
         nbits_needed = 24 + fastlog[local_max>>24];
         }
        else
         {
         nbits_needed = 8 + fastlog[local_max>>8];
         }
        }
        if (nbits_needed == 16) nbits_needed = 15;
/*      local_bins[nbits_needed]++;  */
      stuff(nbits_needed, cur, 32, nbits_req_container, lastWordShifted, spaceInLastWord);
      switch (nbits_needed)
        {
        case 0:
        break;

        case 15:
        for (n=0; n <= lcl_n; n++)
          {
          for (m=0; m <= lcl_m; m++)
            {
            k = FTN2C(i+m,j+n,ni);
            token = (unsigned int) (ufld_dst[k] & ~((-1)<<17));
            stuff(token, cur, 32, 17, lastWordShifted, spaceInLastWord);
            }
          }
        break;

        default:
        nbits2 = nbits_needed + 1;
        for (n=0; n <= lcl_n; n++)
          {
          for (m=0; m <= lcl_m; m++)
            {
            k = FTN2C(i+m,j+n,ni);
            token = (unsigned int) (ufld_dst[k] & ~((-1)<<nbits2));
            stuff(token, cur, 32, nbits2, lastWordShifted, spaceInLastWord);
            }
          }
        break;
        }

       }
    }


    lcl = 0;
    stuff(lcl, cur, 32, 16, lastWordShifted, spaceInLastWord);
    stuff(lcl, cur, 32, 16, lastWordShifted, spaceInLastWord);

   *zlng = 1 + (int) (cur-z) * 4;
    free(ufld_dst);

/*   entropie = 8.0*(*zlng)/(ni*nj);*/
debug=0;
if (debug)
  {
  printf("total_space:\t%d\tentropie:\t%f\n", *zlng,entropie );
  for (i=0; i <= 20; i++)
    {
    printf("%05d ", i);
    }
  printf("\n");
  lsum = 0;
  for (i=0; i <= 20; i++)
    {
    lsum+=local_bins[i];
    printf("%05d ", local_bins[i]);
    }
  printf("---- npts : %d\n", lsum);
  }
}
#endif
void packTokensParallelogram(unsigned int *z, int *zlng, unsigned short *ufld, int ni, int nj, int nbits, int istep, word *header)
{
  unsigned int i, j, k, m, n;
  int spaceInLastWord;
  int lcl_m, lcl_n;

  unsigned int *cur;
  int k11, nbits2, local_max, local_var;
  unsigned int nbits_needed;
  unsigned int nbits_req_container, token, mask;
  int ufld_dst[9];  /* must be at least istep*istep */
  unsigned long long temp;
  void *junk;

#if defined(TEST_TURBO)
  if(USE_NEW == 0) {
    packTokensParallelogramOLD(z, zlng, ufld, ni, nj, nbits, istep, header);
    return ;
  }
#endif
  cur = z;

  if (once == 0)
  {
    i=0 ; n = 0 ; k = 1;
    while( k < 257 )
      {
      while(i < k) fastlog[i++] = n;  // get rid of need for -lm when loading
      k <<= 1;
      n++;
      }
    once = 1;
  }

  nbits_req_container = 5;  /* assume worst case */
//  lastWordShifted = 0;
  spaceInLastWord = 32;
  junk = memcpy(cur, header, sizeof(unsigned int));
  cur++;
  *cur = 0;

  start_pack_64(temp,spaceInLastWord)
  put_bits_64(nbits_req_container,3,temp,spaceInLastWord,0xFFFF)
  check_pack_64(temp,spaceInLastWord,cur)

  for (i=0; i < ni; i++) {  /* row one */
    token = ufld[i];
    put_bits_64(token,nbits,temp,spaceInLastWord,0xFFFF)
    check_pack_64(temp,spaceInLastWord,cur)
  }

  k = ni;
  for (j=1; j < nj; j++) { /* column one */
    token = ufld[k];
    put_bits_64(token,nbits,temp,spaceInLastWord,0xFFFF)
    check_pack_64(temp,spaceInLastWord,cur)
    k += ni;
  }

  for (j=2; j <= nj; j+=istep) {
    lcl_n = ((j + istep - 1) >= nj ? nj - j : istep - 1);
    for (i=2; i <= ni; i+=istep) {
      k = FTN2C(i,j,ni);
      lcl_m = ((i + istep - 1) >= ni ? ni - i : istep - 1);
      local_max = 0;
      k11 = 0;
      for (n=0; n <= lcl_n; n++) {
        for (m=0; m <= lcl_m; m++) {
          ufld_dst[k11] = ufld[k+m] + ufld[k-1-ni+m] - ufld[k-ni+m] - ufld[k-1+m];
          local_var = abs(ufld_dst[k11++]);
          if (local_max < local_var) local_max = local_var ;
        }
        k += ni;
      }

      nbits_needed = 0;
      while(local_max > (FASTLOG_SIZE-1)) { nbits_needed += 8 ; local_max >>= 8; }
      nbits_needed += fastlog[local_max] ;

      if (nbits_needed == 16) nbits_needed = 15;
/*      local_bins[nbits_needed]++;  */
      put_bits_64(nbits_needed,nbits_req_container,temp,spaceInLastWord,0xFFFF)
      check_pack_64(temp,spaceInLastWord,cur)
      nbits2 = nbits_needed + 1;
      mask =  ~((-1)<<nbits2);
      if (nbits_needed == 15) nbits2 = 17;
      if (nbits_needed > 0) {
        if(nbits2>16){   /* tokens are 17 bits, check_pack_64 done every iteration */
          for (m=0; m < k11; m=m+1) {
            token = (unsigned int) ufld_dst[m];
            put_bits_64(token,nbits2,temp,spaceInLastWord,mask)
            check_pack_64(temp,spaceInLastWord,cur)
          }
        }else{   /* tokens are 16 bits or less, check_pack_64 done every other iteration */
          for (m=0; m < k11-1; m=m+2) {
            token = (unsigned int) ufld_dst[m];
            put_bits_64(token,nbits2,temp,spaceInLastWord,mask)
            token = (unsigned int) ufld_dst[m+1];
            put_bits_64(token,nbits2,temp,spaceInLastWord,mask)
            check_pack_64(temp,spaceInLastWord,cur)
          }
          if(k11 & 1){   /* if number of tokens is odd */
            token = (unsigned int) ufld_dst[m];
            put_bits_64(token,nbits2,temp,spaceInLastWord,mask)
            check_pack_64(temp,spaceInLastWord,cur)
          }
        }
      }
    }
  }
  end_pack_64(temp,spaceInLastWord,cur)  /* final padding, force token alignment and ejection */
  *zlng = 1 + (int) (cur-z) * 4;         /* return length */
}

#if defined(TEST_TURBO)
void unpackTokensParallelogramOLDOLD(void *ufld_in, void *z_in, int ni, int nj, int nbits, int istep, word *header)
{
  unsigned short *ufld = ufld_in;
  unsigned int *z = z_in;
  int i, j, k, m, n;
  int bitPackInWord;

  unsigned int *cur, local_min;
  unsigned int  nbits_needed, curword;
  int lcl_m, lcl_n;
  int *ufld_tmp;
  int k11, k12, k21, k22;
  unsigned int nbits_req_container, gt16, token, nbits2;
  void *junk;

  bitPackInWord = 32;

  cur = z;
  junk = memcpy(header, cur, sizeof(unsigned int));
  cur++;
  curword = *cur;
  ufld_tmp = (int *) malloc(ni*nj*sizeof(int));

  extract(nbits_req_container, cur, 32, 3, curword, bitPackInWord);

  for (i=1; i <= ni; i++)
   {
   k = FTN2C(i,1,ni);
   extract(token, cur, 32, nbits, curword, bitPackInWord);
   ufld[k] = token;
   }

  for (j=2; j <= nj; j++)
   {
   k = FTN2C(1,j,ni);
   extract(token, cur, 32, nbits, curword, bitPackInWord);
   ufld[k] = token;
   }


  for (j=2; j <= nj; j+=istep)
    {
    lcl_n = ((j + istep - 1) >= nj ? nj - j : istep - 1);
    for (i=2; i <= ni; i+=istep)
      {
      lcl_m = ((i + istep - 1) >= ni ? ni - i : istep - 1);
      extract(nbits_needed, cur, 32, nbits_req_container, curword, bitPackInWord);
      switch (nbits_needed)
        {
        case 0:
        for (n=0; n <= lcl_n; n++)
          {
          for (m=0; m <= lcl_m; m++)
            {
            k = FTN2C(i+m,j+n,ni);
            ufld_tmp[k] = 0;
            }
          }
        break;

        case 15:
        case 16:
        for (n=0; n <= lcl_n; n++)
          {
          for (m=0; m <= lcl_m; m++)
            {
              k = FTN2C(i+m,j+n,ni);
              extract(token, cur, 32, 17, curword, bitPackInWord);
              ufld_tmp[k] = token;
              ufld_tmp[k] = (ufld_tmp[k] << 15) >> 15;
            }
          }
        break;

        default:
        nbits2 = nbits_needed + 1;
        for (n=0; n <= lcl_n; n++)
          {
          for (m=0; m <= lcl_m; m++)
            {
            k = FTN2C(i+m,j+n,ni);
            extract(token, cur, 32, nbits2, curword, bitPackInWord);
            ufld_tmp[k] = token;
            ufld_tmp[k] = (ufld_tmp[k] << (32-nbits2)) >> (32-nbits2);
            }
          }
         }

        }
      }

  for (j=2; j<=nj; j++)
   {
   for (i=2; i <=ni; i++)
      {
      k11 = FTN2C(i-1,j-1,ni);
      k12 = FTN2C(i-1,j  ,ni);
      k21 = FTN2C(i,  j-1,ni);
      k22 = FTN2C(i,  j,  ni);
      ufld[k22] =  ufld_tmp[k22] + (ufld[k21]+ufld[k12]-ufld[k11]);
      }
   }

      free(ufld_tmp);
    }

void unpackTokensParallelogramOLD(void *ufld_in, void *z_in, int ni, int nj, int nbits, int istep, word *header)
{
  unsigned short *ufld = (unsigned short *)ufld_in;
  unsigned int *z = (unsigned int *)z_in;
  int i, j, k, m, n;
  int bitPackInWord;

  unsigned int *cur, local_min;
  unsigned int  nbits_needed, curword;
  int lcl_m, lcl_n, rowbump;
  unsigned int nbits_req_container, nbits2;
  int token, token2 ;
  void *junk;

  bitPackInWord = 32;
  cur = z;
  junk = memcpy(header, cur, sizeof(unsigned int));
  cur++;
  curword = *cur;

  extract32(nbits_req_container, cur, 32, 3, curword, bitPackInWord);
  for (k=0; k < ni; k++)  /* get first row */
   {
   extract32(token, cur, 32, nbits, curword, bitPackInWord);
   ufld[k] = token;
   }
  k=ni ;
  for (j=2; j <= nj; j++) /* get first column (elements 2->nj) */
   {
   extract32(token, cur, 32, nbits, curword, bitPackInWord);
   ufld[k] = token;
   k += ni ;
   }

  for (j=2; j <= nj; j+=istep)
    {
    lcl_n = ((j + istep - 1) >= nj ? nj - j : istep - 1);
    for (i=2; i <= ni; i+=istep)
      {
      lcl_m = ((i + istep - 1) >= ni ? ni - i : istep - 1);
      k = FTN2C(i,j,ni);
      rowbump = ni - lcl_m - 1 ;
      extract32(nbits_needed, cur, 32, nbits_req_container, curword, bitPackInWord);
      switch (nbits_needed)
        {
        case 0:
        for (n=0; n <= lcl_n; n++)
          {
          for (m=0; m <= lcl_m; m++)
            {
            ufld[k] = 0 + ufld[k-1] + ufld[k-ni] - ufld[k-ni-1] ; /* apply parallelogram rule */
            k++ ;                   /* bump along row */
            }
          k = k + rowbump ;  /* bump along column, going back to proper point on row */
          }
        break;

        default:
        if(nbits_needed == 15 ) nbits_needed = 16;
        nbits2 = nbits_needed + 1;
        for (n=0; n <= lcl_n; n++)
          {
          if(lcl_m == 2){
            extract32(token, cur, 32, nbits2, curword, bitPackInWord);
            token2 = (token << (32-nbits2)) >> (32-nbits2);
            ufld[k] = token2 + ufld[k-1] + ufld[k-ni] - ufld[k-ni-1] ;  /* apply parallelogram rule */
            k++ ;
            extract32(token, cur, 32, nbits2, curword, bitPackInWord);
            token2 = (token << (32-nbits2)) >> (32-nbits2);
            ufld[k] = token2 + ufld[k-1] + ufld[k-ni] - ufld[k-ni-1] ;  /* apply parallelogram rule */
            k++ ;
            extract32(token, cur, 32, nbits2, curword, bitPackInWord);
            token2 = (token << (32-nbits2)) >> (32-nbits2);
            ufld[k] = token2 + ufld[k-1] + ufld[k-ni] - ufld[k-ni-1] ;  /* apply parallelogram rule */
            k++ ;
          }else{
            for (m=0; m <= lcl_m; m++)
              {
              extract32(token, cur, 32, nbits2, curword, bitPackInWord);
              token2 = (token << (32-nbits2)) >> (32-nbits2);
              ufld[k] = token2 + ufld[k-1] + ufld[k-ni] - ufld[k-ni-1] ;  /* apply parallelogram rule */
              k++ ;                   /* bump along row */
              }
          }
          k = k + rowbump ;  /* bump along column, going back to proper point on row */
          }
        }
      }
    }
}
#endif
int ParallelogramDaTa = 0;
int *unpackTokensParallelogramDATA = &ParallelogramDaTa;

#define UFLD_I ufld
void unpackTokensParallelogram(void *ufld_in, void *z_in, int ni, int nj, int nbits, int istep, word *header)
{
  unsigned short *ufld = ufld_in;
  unsigned int *z = (unsigned int *)z_in;

#if defined(TEST_TURBO)
  if(USE_NEW == 0) {
    unpackTokensParallelogramOLD(ufld_in, z_in, ni, nj, nbits, istep, header);
//    unpackTokensParallelogramOLDOLD(ufld_in, z_in, ni, nj, nbits, istep, header);
    return;
  }
#endif
/* from here on , same code for short and integer decompressors */

  unsigned short jarray[nj+3];
  int i, j, k;
  int bitPackInWord;

  unsigned int *cur;
  unsigned int  nbits_needed;
  int lcl_m, lcl_n;
  unsigned int nbits_req_container, nbits2;
  int token, token2 ;
  int istart, iend, jstart, jend;
  int k0, k1, k1mni;
  unsigned long long CurToken;
  void *junk;

  cur = z;
  junk = memcpy(header, cur, sizeof(unsigned int));
  cur++;

  UFLD_I = (unsigned short *)ufld_in;
  bitPackInWord = 32;

  CurToken = *cur++;
  CurToken = (CurToken << 32) | *cur++;
  get_bits_64(nbits_req_container,3,CurToken,bitPackInWord)
  check_unpack_64(CurToken,bitPackInWord,cur)
  for (i=0; i < ni; i++)  /* get first row (elements 11>ni) */
  {
    get_bits_64(token,nbits,CurToken,bitPackInWord)
    check_unpack_64(CurToken,bitPackInWord,cur)
    UFLD_I[i] = token;
  }
  for (j=1; j < nj; j++) /* get first column (elements 2->nj) */
  {
    get_bits_64(token,nbits,CurToken,bitPackInWord)
    check_unpack_64(CurToken,bitPackInWord,cur)
    jarray[j] = token;
  }

  for (jstart=1,k=ni; jstart < nj; jstart+=istep,k=k+istep*ni)  /* loop over rows, process istep rows at a time */
  {
    jend = jstart + istep -1;
    jend = (jend > nj-1) ? nj - 1 : jend;
    lcl_n = jend - jstart + 1;

    for (i=0 ; i<lcl_n; i++) UFLD_I[k+i*ni] = jarray[jstart+i];

    for (istart=1; istart < ni; istart+=istep)   /* process row (elements 2->ni) */
    {
      iend = istart + istep -1;
      iend = (iend > ni-1) ? ni - 1 : iend;
      lcl_m = iend - istart + 1;
      k0 = k;
      get_bits_64(nbits_needed,nbits_req_container,CurToken,bitPackInWord)
      check_unpack_64(CurToken,bitPackInWord,cur)
      switch (nbits_needed)
      {
        case 0:
        for (j=jstart; j<=jend ; j++)
        {
          k1 = k0 + istart;
          for (i=istart; i<=iend ; i++)
          {
            UFLD_I[k1] = 0 + UFLD_I[k1-1] + UFLD_I[k1-ni] - UFLD_I[k1-ni-1] ; /* apply parallelogram rule */
            k1++ ;                   /* bump along row */
          }
          k0 = k0 + ni ;  /* bump along column, going back to proper point on row */
        }
        break;

        default:
        if(nbits_needed == 15 ) nbits_needed = 16;
        nbits2 = nbits_needed + 1;
        if(lcl_m*lcl_n == 9){

          if(nbits2 > 0){
            const int nbits32 = 32 - nbits2;
            int t11, t21, t31, t12, t22, t32, t13, t23;

            /* parallelogram rule : z[i,j] = token + z[i-1,j] + z[i,j-1] - z[i-1,j-1]  */

            k1 = k0 + istart; k1mni = k1 - ni;
            get_bits_64_signed(token2,nbits2,nbits32,CurToken,bitPackInWord)
            check_unpack_64(CurToken,bitPackInWord,cur)

            t11 = token2 + UFLD_I[k1-1] + UFLD_I[k1mni] - UFLD_I[k1mni-1] ;
            UFLD_I[k1] = t11;
//            UFLD_I[k1] = token2 + UFLD_I[k1-1] + UFLD_I[k1mni] - UFLD_I[k1mni-1] ;  /* apply parallelogram rule */

            get_bits_64_signed(token2,nbits2,nbits32,CurToken,bitPackInWord)
            check_unpack_64(CurToken,bitPackInWord,cur)

            t21 = token2 + t11 + UFLD_I[k1mni+1] - UFLD_I[k1mni] ;
            UFLD_I[k1+1] = t21;
//            UFLD_I[k1+1] = token2 + UFLD_I[k1] + UFLD_I[k1mni+1] - UFLD_I[k1mni] ;
            get_bits_64_signed(token2,nbits2,nbits32,CurToken,bitPackInWord)
            check_unpack_64(CurToken,bitPackInWord,cur)

            t31 = token2 + t21 + UFLD_I[k1mni+2] - UFLD_I[k1mni+1] ;
            UFLD_I[k1+2] = t31;
//            UFLD_I[k1+2] = token2 + UFLD_I[k1+1] + UFLD_I[k1mni+2] - UFLD_I[k1mni+1] ;

            k0 = k0 + ni ;  /* bump along column, going back to proper point on row */
            k1 = k0 + istart; k1mni = k1 - ni;

            get_bits_64_signed(token2,nbits2,nbits32,CurToken,bitPackInWord)
            check_unpack_64(CurToken,bitPackInWord,cur)

            t12 = token2 + UFLD_I[k1-1] + t11 - UFLD_I[k1mni-1] ;
            UFLD_I[k1] = t12;
//            UFLD_I[k1] = token2 + UFLD_I[k1-1] + UFLD_I[k1mni] - UFLD_I[k1mni-1] ;  /* apply parallelogram rule */

            get_bits_64_signed(token2,nbits2,nbits32,CurToken,bitPackInWord)
            check_unpack_64(CurToken,bitPackInWord,cur)
            t22 = token2 + t12 + t21 - t11 ;
            UFLD_I[k1+1] = t22;
//            UFLD_I[k1+1] = token2 + UFLD_I[k1] + UFLD_I[k1mni+1] - UFLD_I[k1mni] ;

            get_bits_64_signed(token2,nbits2,nbits32,CurToken,bitPackInWord)
            check_unpack_64(CurToken,bitPackInWord,cur)
            t32 = token2 + t22 + t31 - t21 ;
            UFLD_I[k1+2] = t32;
//            UFLD_I[k1+2] = token2 + UFLD_I[k1+1] + UFLD_I[k1mni+2] - UFLD_I[k1mni+1] ;

            k0 = k0 + ni ;  /* bump along column, going back to proper point on row */
            k1 = k0 + istart; k1mni = k1 - ni;

            get_bits_64_signed(token2,nbits2,nbits32,CurToken,bitPackInWord)
            check_unpack_64(CurToken,bitPackInWord,cur)
            t13 = token2 + UFLD_I[k1-1] + t12 - UFLD_I[k1mni-1] ;
            UFLD_I[k1] = t13;
//            UFLD_I[k1] = token2 + UFLD_I[k1-1] + UFLD_I[k1mni] - UFLD_I[k1mni-1] ;  /* apply parallelogram rule */

            get_bits_64_signed(token2,nbits2,nbits32,CurToken,bitPackInWord)
            check_unpack_64(CurToken,bitPackInWord,cur)
            t23 = token2 + t13 + t22 - t12 ;
            UFLD_I[k1+1] = t23;
//            UFLD_I[k1+1] = token2 + UFLD_I[k1] + UFLD_I[k1mni+1] - UFLD_I[k1mni] ;

            get_bits_64_signed(token2,nbits2,nbits32,CurToken,bitPackInWord)
            check_unpack_64(CurToken,bitPackInWord,cur)
            UFLD_I[k1+2] = token2 + t23 + t32 - t22 ;

          }else{
          }

        }else{       /*   (lcl_m*lcl_n != 9), not a full tile   */
          const int nbits32 = 32 - nbits2;
          for (j=jstart; j<=jend ; j++)
          {
            k1 = k0 + istart;
            for (i=istart; i<=iend ; i++) {
                get_bits_64_signed(token2,nbits2,nbits32,CurToken,bitPackInWord)
                check_unpack_64(CurToken,bitPackInWord,cur)
                UFLD_I[k1] = token2 + UFLD_I[k1-1] + UFLD_I[k1-ni] - UFLD_I[k1-ni-1] ;  /* apply parallelogram rule */
                k1++ ;                   /* bump along row */
            }
            k0 = k0 + ni ;  /* bump along column, going back to proper point on row */
          }
        }
      }   /* switch */
    }     /* end of loop over row elements   */
  }       /* end of loop over blocks of rows */
}


/**********************************************************************************************************************************/
/*
 Given a stream of unsigned shorts, this routine re-encodes the stream by the minimum tile method.
 The stream is decomposed into a number "step * step" tiles, for each of which the minimum value is stored, and the difference between
 each cell and the minimum for each member of the tile

 Output stream
 Token 0 contains the zfstzip structure. Starting from Token 1, we have
 |---------------|---------------|---------------------------------|
 |  nbits_needed | local_min     | local_value - local_min         |
 |---------------|---------------|---------------------------------|
 |  4 bits       |  nbits        | step * step * nbits_needed      |
 |---------------|---------------|---------------------------------|

 With the exception that if the field is invariant within the tile, nbits_needed = 0 and we only encode
 |---------------|---------------|
 |  nbits_needed | local_min     |
 |---------------|---------------|
 |  4 bits       |  nbits        |
 |---------------|---------------|

 Variables :
   z      : output compressed byte stream
   zlng   : the length of the stream, in bytes
   ufld   : the source unsigned short stream
   ni, nj : dimensions of the field
   nbits  : the number of bits used to pack the original stream - normally btn 2 and 16
   istep  : the size of the tile (normally 5)
   header : contents of the zfstzip structure for info about compression parameters
*/



#if defined(USE_FSTZIP_SAMPLE)
/**********************************************************************************************************************************/
/*
 Given a stream of unsigned shorts, this routine re-encodes the stream by the bi-cubic tile method.
 The stream is decomposed into a number "step * step" tiles, for each of which the the values in the tile are predicted by
 bicubic interpolation. What is stored is the prediction error, ie the difference between the predicted value the original value.

 This function is a front end to the "packTokenSample" function, which it calls 3 times

 Initially, we have

 |-------------------------------------------------------------------
 | zc                                | diffs                        |
 |-------------------------------------------------------------------

 Then zc is split in 2 parts, zc1 and diffs1

 |-------------------------------------------------------------------
 | zc1            | diffs1           | diffs                        |
 |-------------------------------------------------------------------

 and finally zc1 is split in zc2 and idiffs2

 |-------------------------------------------------------------------
 | zc2 |  diffs2  | diffs1           | diffs                        |
 |-------------------------------------------------------------------

 At the uncompression level,
 zc2 and diffs2 will be merged to reform zc1
 zc1 and diffs1 will be merged to reform zc, and
 zc and diffs will be merged to reform the original stream
 */

/**********************************************************************************************************************************/
/*
 Given a stream of unsigned shorts, this routine re-encodes the stream by the bi-cubic tile method.
 The stream is decomposed into a number "step * step" tiles, for each of which the the values in the tile are predicted by
 bicubic interpolation. What is stored is the prediction error, ie the difference between the predicted value the original value.

 Output stream
 Token 0 contains the zfstzip structure.
 Token 1 starts with the 3 bits token"nbits_req". This token has the value 4 or 5. "nbits_req" defines how many bits are required
 to encode the differences. If the number is <=16, nbits_req = 4. Otherwise nbits_req = 5. The condition where nbits_req = 5 occurs very rarely,
 but it does can occur, for instance if we encode 16-bit tokens, if the field is noisy then we can have prediction errors demanding more than 16 bits
 then we will have nbits_req = 5
 Starting from Token 1, we have
 |-----------------------------|-----------|--------------|----------------------------------------|--------------|-----------------------------------
 |  zcoarse                    | nbits_req | nbits_needed | diffs (tile 0)                         | nbits_needed | diffs (tile 1)
 |-----------------------------|-----------|--------------|----------------------------------------|--------------|-----------------------------------
    nicoarse*njcoarse*nbits    | 3         | nbits_req    | (step* step - 1)* nbits_needed         | nbits_needed | (step* step - 1)* nbits_needed
 |-----------------------------|-----------|--------------|----------------------------------------|--------------|-----------------------------------


 Variables :
   z      : output compressed byte stream
   zlng   : the length of the stream, in bytes
   zc     : a coarse grid used to generate the interpolated values
   nicoarse, njcoarse : dimensions of the coarse grid
   diffs  : the prediction errors
   ni, nj : dimension of the source field
   nbits  : the number of bits used to pack the original stream - normally btn 2 and 16
   istep  : the size of the tile (normally 5)
   header : contents of the zfstzip structure for info about compression parameters
   start  : flag to indicate the status of the stream
            if == 1, we start a new stream
            if == 0, we continue where we left
   end    : flag to indicate the end of the stream
            if == 0, nothing is done
            if == 1, a 32-bit word containing 0 closes the stream
*/

void packTokensSample(unsigned int z[], int *zlng, unsigned int zc[], int nicoarse, int njcoarse, int diffs[], int ni, int nj, int nbits, int step, word *header, int start, int end)
{
  int i, j, k, m, n;
  static unsigned int lastWordShifted, spaceInLastWord, lastSlot;
  static unsigned int *cur;
  int lcl_m, lcl_n;

  float entropie;
  int local_max;
  unsigned int nbits_req_container, local_bins[24];
  int lcl, nbits_needed, nbits2,lsum;
  unsigned char debug;
  unsigned int token, gt16;
  void *junk;

  if (start == 1)
    {
    lastSlot = 0;
    cur = z;
    junk = memset(z, (int) 0, ni*nj*sizeof(unsigned int));
    for (i=0; i <= 20; i++)
      {
      local_bins[i] = 0;
      }

    lastWordShifted = 0;
    spaceInLastWord = 32;
    junk = memcpy(cur, header, sizeof(unsigned int));
    cur++;
    *cur = 0;
    }

  nbits_req_container = 4;
  if (start == 1)
    {
    for (j=1; j <= njcoarse; j++)
      {
      for (i=1; i <= nicoarse; i++)
        {
        k = FTN2C(i,j,nicoarse);
        stuff(zc[k], cur, 32, nbits, lastWordShifted, spaceInLastWord);
        }
      }
    }

  i = 0;
  gt16 = 0;
  while (i < ni*nj && gt16 == 0)
    {
    if (65535 < abs(diffs[i]))
      {
      nbits_req_container = 5;
      gt16 = 1;
      }
    i++;
    }


  stuff(nbits_req_container, cur, 32, 3, lastWordShifted, spaceInLastWord);

  for (j=1; j <= nj; j+=step)
    {
    lcl_n = ((j + step - 1) >= nj ? nj - j: step - 1);
    for (i=1; i <= ni; i+=step)
      {
      k = FTN2C(i,j,ni);
      local_max = abs(diffs[k]);
      lcl_m = ((i + step - 1) >= ni ? ni - i : step - 1);
      for (n=0; n <= lcl_n; n++)
        {
        for (m=0; m <= lcl_m; m++)
          {
          if (!(m == 0 && n == 0))
            {
            k = FTN2C(i+m,j+n,ni);
            if (local_max < abs(diffs[k])) local_max = abs(diffs[k]);
            }
          }
        }
      if (local_max == 0)
        {
        nbits_needed = 0;
        }#endif

      else
        {
        nbits_needed = (int)(1+log(local_max+0.5)/log(2.0));
        }
      if (nbits_needed == 16) nbits_needed = 15;
      local_bins[nbits_needed]++;
      stuff(nbits_needed, cur, 32, nbits_req_container, lastWordShifted, spaceInLastWord);
      switch (nbits_needed)
        {
        case 0:
        break;

        case 15:
        case 16:
        for (n=0; n <= lcl_n; n++)
          {
          for (m=0; m <= lcl_m; m++)
            {
            if (!(m == 0 && n == 0))
              {
              k = FTN2C(i+m,j+n,ni);
              token = (unsigned int) (diffs[k] & ~((-1)<<17));
              stuff(token, cur, 32, 17, lastWordShifted, spaceInLastWord);
              }
            }
          }
        break;

        default:
        nbits2 = nbits_needed + 1;
        for (n=0; n <= lcl_n; n++)
          {
          for (m=0; m <= lcl_m; m++)
            {
            if (!(m == 0 && n == 0))
              {
              k = FTN2C(i+m,j+n,ni);
              token = (unsigned int) (diffs[k] & ~((-1)<<nbits2));
              stuff(token, cur, 32, nbits2, lastWordShifted, spaceInLastWord);
              }
            }
          }
        break;
        }

       }
    }


  if (end == 1)
    {
    lcl = 0;
    stuff(lcl, cur, 32, 16, lastWordShifted, spaceInLastWord);
    stuff(lcl, cur, 32, 16, lastWordShifted, spaceInLastWord);
    }

   *zlng = 1 + (int) (cur-z) * 4;
   entropie = 8.0*(*zlng)/(ni*nj);
debug=0;
if (debug)
  {
  printf("total_space:\t%d\tentropie:\t%f\n", *zlng,entropie );
  for (i=0; i <= 20; i++)
    {
    printf("%05d ", i);
    }
  printf("\n");
  lsum = 0;
  for (i=0; i <= 20; i++)
    {
    lsum+=local_bins[i];
    printf("%05d ", local_bins[i]);
    }
  printf("---- npts : %d\n", lsum);
  }
}

/**********************************************************************************************************************************/
/* See documentation for packTokenSample */

void unpackTokensSample(unsigned int zc[], int diffs[], unsigned int z[], int nicoarse, int njcoarse,  int ni, int nj, int nbits, int step, word *header, int start)
{
  int i, j, k, m, n;
  static unsigned int *cur, curword;
  static int bitPackInWord;

  unsigned int nbits_req_container;
  unsigned int nbits_needed, nbits2, token;
  int lcl_m, lcl_n;
  void *junk;

  if (start == 1)
    {
    bitPackInWord = 32;

    junk = memset(zc, (int ) 0, nicoarse*njcoarse*sizeof(int));
    cur = z;
    junk = memcpy(header, cur, sizeof(unsigned int));
    cur++;
    curword = *cur;
    }
  junk = memset(diffs, (int) 0, ni*nj*sizeof(int));

  if (start == 1)
    {
    for (j=1; j <= njcoarse; j++)
      {
      for (i=1; i <= nicoarse; i++)
        {
        k = FTN2C(i,j,nicoarse);
        extract(zc[k], cur, 32, nbits, curword, bitPackInWord);
        }
      }
    }

  extract(nbits_req_container, cur, 32, 3, curword, bitPackInWord);
/*  printf("Apres nicoarse*njcoarse (cur-z) = %d\n", cur-z);*/
  for (j=1; j <= nj; j+=step)
    {
    lcl_n = ((j + step - 1) >= nj ? nj - j : step - 1);
    for (i=1; i <= ni; i+=step)
      {
      lcl_m = ((i + step - 1) >= ni ? ni - i : step - 1);
      extract(nbits_needed, cur, 32, nbits_req_container, curword, bitPackInWord);
      switch (nbits_needed)
        {
        case 0:
        for (n=0; n <= lcl_n; n++)
          {
          for (m=0; m <= lcl_m; m++)
            {
            k = FTN2C(i+m,j+n,ni);
            diffs[k] = 0;
            }
          }
        break;

        case 15:
        case 16:
        for (n=0; n <= lcl_n; n++)
          {
          for (m=0; m <= lcl_m; m++)
            {
            if (!(m == 0 && n == 0))
              {
              k = FTN2C(i+m,j+n,ni);
              extract(token, cur, 32, 17, curword, bitPackInWord);
              diffs[k] = token;
              diffs[k] = (diffs[k] << 15) >> 15;
              }
            }
          }
        break;

        default:
        nbits2 = nbits_needed + 1;
        for (n=0; n <= lcl_n; n++)
          {
          for (m=0; m <= lcl_m; m++)
            {
            if (!(m == 0 && n == 0))
              {
              k = FTN2C(i+m,j+n,ni);
              extract(token, cur, 32, nbits2, curword, bitPackInWord);
              diffs[k] = token;
              diffs[k] = (diffs[k] << (32-nbits2)) >> (32-nbits2);
              }
            }
          }
        break;
        }
      }
    }

}

#endif

/**********************************************************************************************************************/

void init_comp_settings(char *comp_settings)
{
char *env_info = NULL;

env_info = getenv("ARMN_COMPRESS");

if (env_info == NULL)
  {
  strcpy(comp_settings, "fast");
  fstcompression_level = FAST;
  }
else
  {
  fstcompression_level = BEST;
  }

}

/**********************************************************************************************************************/

void c_armn_compress_option(char *option, char *value)
{
  int i;
  static char *msgtab[7] =
  {"DEBUG","INFORM","WARNIN","ERRORS","FATALE","SYSTEM","CATAST"};
//  static int nivmsg[7] = {0,2,4,6,8,10,10};

   if (strcmp(option,"MSGLVL") == 0)
    {
      for (i = 0; i < 7; i++) {
        if (strcmp(msgtab[i],value) == 0)
        {
        zfst_msglevel = i;
        break;
        }
      }
    }

  }

/**********************************************************************************************************************/
#ifdef USE_FSTZIP_SAMPLE
/* This function computes how many points are leftover on the east/north sides of the grid  */

void calcul_ajusxy(int *ajus_x, int *ajus_y, int ni, int nj, int istep)
{
  *ajus_x = (ni-1) % istep;
  *ajus_y = (nj-1) % istep;
}

/**********************************************************************************************************************/

/* This function computes the dimensions of the coarse grid, given ni,nj,tile step and a correction factor  */
/* to compensate for the grid size when the last point of the source grid is not a multiple of step         */

void calcul_ninjcoarse(int *nicoarse, int *njcoarse, int ni, int nj, int ajus_x, int ajus_y, int istep)
{
  int correction_x = 0;
  int correction_y = 0;

  if (ajus_x != 0)
    {
    correction_x = 1;
    }

  if (ajus_y != 0)
    {
    correction_y = 1;
    }

 if (ni > 1 && nj > 1)
    {
    *nicoarse = correction_x + (ni+istep-1) / istep;
    *njcoarse = correction_y + (nj+istep-1) / istep;
    }
  else
    {
    if (ni == 1)
      {
      *nicoarse = 1;
      *njcoarse = correction_y + (nj+istep-1) / istep;
       }
    else
      {
      *nicoarse = correction_x + (ni+istep-1) / istep;
      *njcoarse = 1;
      }
    }
  }

/**********************************************************************************************************************/

/* This function tests if the point (i,j) is part of the coarse grid */

int is_on_coarse(int i, int j, int ni, int nj, int step)
{
if (0 == ((i-1)%step) && 0 == ((j-1) % step)) return 1;
if (i==ni && j == nj) return 1;
if (i==ni && 0 == ((j-1) % step)) return 1;
if (j==nj && 0 == ((i-1) % step)) return 1;

return 0;

}

/**********************************************************************************************************************/
void fixpredflds(int *predfld, int *zc, int ni, int nj, int nicoarse, int njcoarse, int step, int ajus_x, int ajus_y)
{
int i,j,k,icoarse, jcoarse, kcoarse, ifine, jfine;

  if (ajus_x > 0)
    {
    for (i=0; i <= ajus_x; i++)
      {
      ifine=(nicoarse-2)*step+i+1;
      for (j=1; j <= nj; j++)
        {
        if (!(is_on_coarse(ifine, j, ni, nj, step)))
          {
          jcoarse = (j-1+step) / step;
          kcoarse = FTN2C(nicoarse, jcoarse, nicoarse);
          k = FTN2C(ifine, j, ni);
          predfld[k] = zc[kcoarse];
          }
        }
      }
    }

  if (ajus_y > 0)
    {
    for (j=0; j <= ajus_y; j++)
      {
      jfine = step*(njcoarse-2)+j+1;
      for (i=1; i <= ni; i++)
        {
        if (!(is_on_coarse(i, jfine, ni, nj, step)))
          {
          icoarse = (i-1+step) / step;
          kcoarse = FTN2C(icoarse, njcoarse, nicoarse);
          k = FTN2C(i, jfine, ni);
          predfld[k] = zc[kcoarse];
          }
        }
      }
    }
}
#endif
/**********************************************************************************************************************/
#if defined(ENTROPIE)
void calcule_entropie(float *entropie, unsigned short *bitstream, int npts, int nbits)
{
  int i,lbin,sizebins;
  int imin, imax,nbits_local;
  float prob,range;
  unsigned int *bins;

  *entropie = 0.0;
  imin = bitstream[0];
  imax = imin;
  for (i=1; i < npts; i++)
    {
    if (bitstream[i] < imin) imin = bitstream[i];
    if (bitstream[i] > imax) imax = bitstream[i];
    }

  range = (float)(imax - imin);
  nbits_local = 1+(int)(log(1.0*(imax-imin))/log(2.0));
/*  printf("imin : %d imax : %d range: %d, nbits : %d\n", imin, imax, imax-imin, nbits_local);*/


  sizebins = 1<<nbits_local;
  bins = (unsigned int *) calloc(sizebins,sizeof(unsigned int));

  for (i=0; i < npts; i++)
    {
    lbin = bitstream[i] - imin;
    bins[lbin]++;
    }

  for (i=0; i < (int)(range); i++)
    {
    if (bins[i] != 0)
      {
      prob = (float)bins[i]/(float)(npts);
      /*       printf("i: %d count: %d prob: %f contrib : %f \n", i, bins[i], prob, (prob * log(prob)/log(2.0)));*/
      *entropie += (prob * log(prob)/log(2.0));
      }
    }

  *entropie = -1.0 * (*entropie);
  free(bins);
  /*printf("Entropie totale : %f\n", *entropie);*/
}
#endif

/**********************************************************************************************************************/
void f77name(armn_compress_setlevel)(wordint *level)
{
  int local_level;

  local_level = *level;

  c_armn_compress_setlevel(local_level);
}

/**********************************************************************************************************************/
void c_armn_compress_setlevel(int level)
{
  switch(level)
    {
    case BEST:
    fstcompression_level = BEST;
    fprintf(stdout, "Setting level to BEST : %d\n", level);
    break;

    case FAST:
    fstcompression_level = FAST;
    fprintf(stdout, "Setting level to FAST : %d\n", level);
    break;

    default:
    fprintf(stdout, "Wrong compression level : %d\n", level);
    fprintf(stdout, "Setting level to fast : %d\n", level);
    fstcompression_level = FAST;
    }
}

/**********************************************************************************************************************/
int f77name(armn_compress_getlevel)()
{
return  c_armn_compress_getlevel();
}

/**********************************************************************************************************************/
int c_armn_compress_getlevel_hint(int ni, int nj, int nk, int nbits)
{
  if (FAST == fstcompression_level || (ni < 16) || (nj < 16) || (nbits <= 4)) return (FAST) ;
  return (BEST) ;
}
int c_armn_compress_getlevel()
{
  if(fstcompression_level == -1) fstcompression_level = BEST;
  return fstcompression_level;
}

/**********************************************************************************************************************/
void f77name(armn_compress_setswap)(wordint *swap)
{
  int local_swap;

  local_swap = *swap;

  c_armn_compress_setlevel(local_swap);
}

/**********************************************************************************************************************/
void c_armn_compress_setswap(int swapState)
{
  switch(swapState)
    {
    case 0:
    case 1:
    swapStream = swapState;
    /*fprintf(stdout, "Setting swapState to : %d\n", swapState);*/
    break;

    default:
    fprintf(stdout, "Wrong swapState : %d -- should be 0 (no swap) or 1 (swap)\n", swapState);
    fprintf(stdout, "Current swap state unchanged\n");
    }
}

/**********************************************************************************************************************/
int f77name(armn_compress_getswap)()
{
return  c_armn_compress_getswap();
}

/**********************************************************************************************************************/
int c_armn_compress_getswap()
{
  return swapStream;
}
/*************************************   TEST PROGRAM **************************************/
#if defined(TEST_TURBO)
#define NI 2003
#define NJ 1404
#include <sys/time.h>
int main()
{
  unsigned int buffer[NI*NJ*17/16];
//  float fbuf[NI*NJ];
  unsigned int *pbuf;
  int token, curword, bitPackInWord;
  short zo[NI*NJ];
  short zi[NI*NJ];
  int i,j ;
  int nbits = 16;
  int zlng = 0;
  int errors;
  struct timeval t1,t2;
  long long T1, T2;
  int duree;
  float rlog2;
  int nbits_needed;
  int err2 = 0;

  printf("INFO: second stage compression test \n");
//  rlog2 = 1.0/log(2.0);  /* initialize log tables for MINIMUM */
//  for (i=0; i < 256; i++)
//     {
//     fastlog[i] = (int)(1+log(i+0.5)*rlog2);
//     }
   nbits_needed = 1;
   i = 1; fastlog[0] = 0; fastlog[FASTLOG_SIZE-1] = 9;
   for(j=2 ; j<FASTLOG_SIZE ; j*=2 , nbits_needed++) {  // get rid of need for -lm when loading
     while(i<j) {
       fastlog[i++]  = nbits_needed;
     }
   }
   fprintf(stderr,"fastlog table initialized \n");
  for (i = 1 ; i <= NI ; i++)
    for (j = 1 ; j <= NJ ; j++ )
//    { zi[FTN2C(i,j,NI)] = i + j ; }
    { zi[FTN2C(i,j,NI)] = i*7.567 + j*7.234 ; }
  for (i = 3 ; i <= NI ; i+=9)
    for (j = 3 ; j <= NJ ; j+=6 )
    { zi[FTN2C(i,j,NI)] = 60000 ; zi[FTN2C(i-1,j-1,NI)] = 60000 ;}
//  c_armn_compress_setlevel(FAST);
  if(NI > 1000 && NJ > 1000){
    for (i = 450 ; i< 550 ; i++)
      for (j = 450 ; j< 550 ; j++)
        { zi[FTN2C(i,j,NI)] = 65535 ; }
  }
  if(NI > 1000 && NJ > 1000){
    for (i = 550 ; i< 650 ; i++)
      for (j = 550 ; j< 550 ; j++)
        { zi[FTN2C(i,j,NI)] = 32767 ; }
  }
  USE_NEW=0 ;
  c_fstzip(buffer, &zlng, zi, NI, NJ, MINIMUM, 0, 5, nbits, 0);
  c_fstzip(buffer, &zlng, zi, NI, NJ, MINIMUM, 0, 5, nbits, 0);
  gettimeofday(&t1,NULL);
  USE_NEW=0 ;
  c_fstzip(buffer, &zlng, zi, NI, NJ, MINIMUM, 0, 5, nbits, 0);
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("MINIMUM OLD packing time = %d usec, %d MTok/sec\n",duree,(NI*NJ)/duree);
  printf("INFO: MINIMUM compressed length = %d buffer : %8.8x %8.8x %8.8x, new=%d\n",zlng,buffer[0],buffer[1],buffer[2],USE_NEW);

  gettimeofday(&t1,NULL);
  USE_NEW=1 ;
  c_fstzip(buffer, &zlng, zi, NI, NJ, MINIMUM, 0, 5, nbits, 0);
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("MINIMUM NEW packing time = %d usec, %d MTok/sec\n",duree,(NI*NJ)/duree);
  printf("INFO: MINIMUM compressed length = %d buffer : %8.8x %8.8x %8.8x, new=%d\n",zlng,buffer[0],buffer[1],buffer[2],USE_NEW);

  errors = 0; USE_NEW=0 ;
  for (i = 1 ; i <= NI ; i++)
    for (j = 1 ; j <= NJ ; j++ )
    { zo[FTN2C(i,j,NI)] = 32767 ; }
  gettimeofday(&t1,NULL);
  c_fstunzip(zo, buffer, NI, NJ, nbits);
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("MINIMUM unpacking old time = %d usec, %d MTok/sec\n",duree,(NI*NJ)/duree);
  for (i=0 ; i<NI*NJ ; i++) { if(zi[i] != zo[i]) { errors++; } }
  printf("INFO: decompression errors = %d\n",errors);

  errors = 0; USE_NEW=1 ;
  for (i = 1 ; i <= NI ; i++)
    for (j = 1 ; j <= NJ ; j++ )
    { zo[FTN2C(i,j,NI)] = 32767 ; }
  gettimeofday(&t1,NULL);
  c_fstunzip(zo, buffer, NI, NJ, nbits);
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("MINIMUM unpacking new time = %d usec, %d MTok/sec\n",duree,(NI*NJ)/duree);
  err2 = 0;
  for (i=0 ; i<NI*NJ ; i++) { if(zi[i] != zo[i]) { errors++; if(errors<10)fprintf(stderr,"i=%d, expected=%d, got=%d\n",i,zi[i],zo[i]); } }
  printf("INFO: decompression errors = %d\n",errors);

  printf("------------------------------------------------------------\n");

  USE_NEW=0 ;
  c_fstzip(buffer, &zlng, zi, NI, NJ, PARALLELOGRAM, 1, 3, nbits, 0);
  c_fstzip(buffer, &zlng, zi, NI, NJ, PARALLELOGRAM, 1, 3, nbits, 0);
  gettimeofday(&t1,NULL);
  c_fstzip(buffer, &zlng, zi, NI, NJ, PARALLELOGRAM, 1, 3, nbits, 0);
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("PARALLELOGRAM OLD packing time = %d usec, %d MTok/sec\n",duree,(NI*NJ)/duree);
  printf("INFO: PARALLELOGRAM compressed length = %d buffer : %8.8x %8.8x %8.8x, new=%d\n",zlng,buffer[0],buffer[1],buffer[2],USE_NEW);

  USE_NEW=1 ;
  gettimeofday(&t1,NULL);
  c_fstzip(buffer, &zlng, zi, NI, NJ, PARALLELOGRAM, 1, 3, nbits, 0);
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("PARALLELOGRAM NEW packing time = %d usec, %d MTok/sec\n",duree,(NI*NJ)/duree);
  printf("INFO: PARALLELOGRAM compressed length = %d buffer : %8.8x %8.8x %8.8x, new=%d\n",zlng,buffer[0],buffer[1],buffer[2],USE_NEW);

  errors = 0; USE_NEW=0 ;
  for (i = 1 ; i <= NI ; i++)
    for (j = 1 ; j <= NJ ; j++ )
    { zo[FTN2C(i,j,NI)] = 32767 ; }
  gettimeofday(&t1,NULL);
  c_fstunzip(zo, buffer, NI, NJ, nbits);
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("PARALLELOGRAM unpacking old time = %d usec, %d MTok/sec\n",duree,(NI*NJ)/duree);
  for (i=0 ; i<NI*NJ ; i++) { if(zi[i] != zo[i]) { errors++; if(errors<10) printf(" expected %d, got %d at %d \n",zi[i],zo[i],i); } }
  printf("INFO: decompression errors = %d / %d\n",errors,NI*NJ);

  c_fstzip(buffer, &zlng, zi, NI, NJ, PARALLELOGRAM, 1, 3, nbits, 0);
  USE_NEW=1 ;
  for (i = 1 ; i <= NI ; i++)
    for (j = 1 ; j <= NJ ; j++ )
    { zo[FTN2C(i,j,NI)] = 32767 ; }
  gettimeofday(&t1,NULL);
  c_fstunzip(zo, buffer, NI, NJ, nbits);
  gettimeofday(&t2,NULL);
  T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
  T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
  duree = T2-T1;
  printf("PARALLELOGRAM unpacking new time = %d usec, %d MTok/sec\n",duree,(NI*NJ)/duree);
  errors = 0;
  for (i=0 ; i<NI*NJ ; i++) { if(zi[i] != zo[i]) { errors++; if(errors<10) printf(" expected %d, got %d at %d \n",zi[i],zo[i],i); } }
  printf("INFO: decompression errors = %d / %d\n",errors,NI*NJ);
//  for (i=0 ; i<=16 ; i++) { printf("%d:%d ",i,bit_tab[i]);} printf("\n");
#if defined(EXTRA_TEST_CODE)
  for (i=2 ; i<=17 ; i+=5) {
    pbuf=buffer;
    curword=*pbuf;
    bitPackInWord = 29;
    gettimeofday(&t1,NULL);
    for (j=0;j<NI*NJ;j++){
      extract32(token, pbuf,32, i, curword, bitPackInWord);
      zo[j]=token;
    }
    gettimeofday(&t2,NULL);
    T1 = t1.tv_sec ; T1 = T1*1000000 + t1.tv_usec ;
    T2 = t2.tv_sec ; T2 = T2*1000000 + t2.tv_usec ;
    duree = T2-T1;
    printf("extracting %d bit slices time = %d usec\n",i,duree);
  }
#endif
  return (0) ;
}
#endif
