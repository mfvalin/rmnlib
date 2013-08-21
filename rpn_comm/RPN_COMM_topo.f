*/* RMNLIB - Library of useful routines for C and FORTRAN programming
* * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
* *                          Environnement Canada
* *
* * This library is free software; you can redistribute it and/or
* * modify it under the terms of the GNU Lesser General Public
* * License as published by the Free Software Foundation,
* * version 2.1 of the License.
* *
* * This library is distributed in the hope that it will be useful,
* * but WITHOUT ANY WARRANTY; without even the implied warranty of
* * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* * Lesser General Public License for more details.
* *
* * You should have received a copy of the GNU Lesser General Public
* * License along with this library; if not, write to the
* * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
* * Boston, MA 02111-1307, USA.
* */

	integer function RPN_COMM_topo(nxg,minx,maxx,nxl,nxlmax,
     %                   halox,nx0,alongx,fill)
	use rpn_comm
	implicit none
	integer nxg,minx,maxx,nxl,nxlmax,halox,nx0
	logical fill, alongx
*
*arguments
*  I	nxg	Global dimension (1D) of data
*  O	minx,maxx
*		dimensions for local array
**
!	include 'rpn_comm.h'
*
        integer gmin, gmax, ierr
        integer count(pe_nx + pe_ny)
        integer depl(pe_nx + pe_ny)
	integer RPN_COMM_limit
*
        if (alongx) then
          ierr = RPN_COMM_limit(pe_mex, pe_nx, 1, nxg, gmin, gmax,
     &                    count, depl)
        else
          ierr = RPN_COMM_limit(pe_mey, pe_ny, 1, nxg, gmin, gmax,
     &                    count, depl)
        end if
*
	if(ierr.ne.0) then
	   write(rpn_u,*) 'RPN_COMM_topo: invalid distribution, ABORT'
	   call RPN_COMM_finalize(ierr)
	   stop
	endif

        nx0 = gmin
        minx = 1 - halox
        nxl = gmax - gmin + 1
        nxlmax = count(1)
        maxx = nxlmax + halox
        if(fill) maxx = maxx + 1 - mod(nxlmax,2)
*
        RPN_COMM_topo = 0
        return
        end
