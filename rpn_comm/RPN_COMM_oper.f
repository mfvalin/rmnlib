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

        integer function RPN_COMM_oper(op)
	use rpn_comm
c	Luc Corbeil, 2000-11-20
c	lien entre datatype et MPI_datatype

        implicit none
        include 'mpif.h'
        character(len=*) op
        character(len=32) operation

        call rpn_comm_low2up(op,operation)

        if (operation(1:11).eq.'MPI_OP_NULL') then
           RPN_COMM_oper=MPI_OP_NULL
           return
        endif
        if (operation(1:7).eq.'MPI_MAX') then
           RPN_COMM_oper=MPI_MAX
           return
        endif
        if (operation(1:7).eq.'MPI_MIN') then
           RPN_COMM_oper=MPI_MIN
           return
        endif
        if (operation(1:7).eq.'MPI_SUM') then
           RPN_COMM_oper=MPI_SUM
           return
        endif
        if (operation(1:8).eq.'MPI_PROD') then
           RPN_COMM_oper=MPI_PROD
           return
        endif
        if (operation(1:8).eq.'MPI_LAND') then
           RPN_COMM_oper=MPI_LAND
           return
        endif
        if (operation(1:8).eq.'MPI_BAND') then
           RPN_COMM_oper=MPI_BAND
           return
        endif
        if (operation(1:7).eq.'MPI_LOR') then
           RPN_COMM_oper=MPI_LOR
           return
        endif
        if (operation(1:7).eq.'MPI_BOR') then
           RPN_COMM_oper=MPI_BOR
           return
        endif
        if (operation(1:8).eq.'MPI_LXOR') then
           RPN_COMM_oper=MPI_LXOR
           return
        endif
        if (operation(1:8).eq.'MPI_BXOR') then
           RPN_COMM_oper=MPI_BXOR
           return
        endif
        if (operation(1:10).eq.'MPI_MAXLOC') then
           RPN_COMM_oper=MPI_MAXLOC
           return
        endif
        if (operation(1:10).eq.'MPI_MINLOC') then
           RPN_COMM_oper=MPI_MINLOC
           return
        endif


        write(rpn_u,*) 'Unknown operation, aborting'
          stop
          
        return
        end
