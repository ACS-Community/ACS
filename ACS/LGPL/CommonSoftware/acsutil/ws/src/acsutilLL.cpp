/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration)
*    and Cosylab 2002, All rights reserved
*
*    This library is free software; you can redistribute it and/or
*    modify it under the terms of the GNU Lesser General Public
*    License as published by the Free Software Foundation; either
*    version 2.1 of the License, or (at your option) any later version.
*
*    This library is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*    Lesser General Public License for more details.
*
*    You should have received a copy of the GNU Lesser General Public
*    License along with this library; if not, write to the Free Software
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: acsutilLL.cpp,v 1.2 2003/09/09 13:36:58 gchiozzi Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram 2002-01-07 created 
*/

#include "acsutilLL.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

unsigned long long string2LLU(char *str2Convert)
{

#ifndef NO_LLU_SUPPORT
    unsigned long long convNum;
    sscanf (str2Convert, "%llu", &convNum); 
    
    return convNum;
#else
    int size =strlen(str2Convert);
    unsigned long long result=0;

    for (int i=0; i<size; i++)
	result = result *10  + str2Convert[i]-'0';
 
   return result;
#endif
}

///TO DO: optimize LLU2String fro platfroms that support LL
char *LLU2String(unsigned long long numToConv)
{
    const int size = 20;  // max value 1.84 x 10^19
    // char* pattern="0123456789";
 
    static char printBuf[]="                    "; // size =20 -> max value 1.84 x 10^19
/*    char printBuf[size+1];
    char* strPtr;
    for (int i = 0; i < size; i++)
        printBuf[i] = ' ';
    printBuf[size] = '\0';
    */
    int i;
    for (i = size - 1; i >= 0; i--) {
        printBuf[i] = numToConv % 10 + '0';
        numToConv /= 10;
        if (numToConv == 0)
            break;
        }

//    strPtr = strpbrk(printBuf, pattern);

    return printBuf+i; //strPtr;
}

#ifdef NO_LLU_SUPPORT
unsigned long long LLUModulus(unsigned long long a,
			      unsigned long long b)
{
  if (b==0) return 0;

  unsigned long long bit = 1;
  unsigned long long a_half(a);
  a_half >>= 1;
  while(a_half >= b)
    {
      bit <<= 1;
      b <<= 1;
    }
  do
    {
      if(a >= b)
        {
	  a -= b;
        }
      b >>= 1;
      bit >>= 1;
    } while(bit != 0);
    
  return a;
}
#endif



