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

#include "pack_macros_64.h"
#define WITH_OFFSET
static double pow2(int i)
{
  union{
    double d;
    long long l;
  } dl;
  long long exp;
  exp = i + 1023;
  exp = (exp < 0) ? 0 : exp;
  exp = (exp > 2047) ? 2047 : exp;
  dl.l = exp << 52;
  return(dl.d);
}

#define powerSpan 65
#define MAX_RANGE 1.0e+38
static double powerOf2s[powerSpan];
static int powerOf2sInitialized = 0;

#define isDouble 1
#define FLOAT_4_8 double
#define compact_FLOAT_4_8 compact_double
#include "compact_type_1.hc"


#define isDouble 0
#define FLOAT_4_8 float
#define compact_FLOAT_4_8 compact_float
#include "compact_type_1.hc"
