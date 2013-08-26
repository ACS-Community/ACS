/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration),
*    All rights reserved
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
* "@(#) $Id: baciUtil.cpp,v 1.4 2007/06/12 08:02:23 nbarriga Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate 2005-01-17 created
*/

#include "baciUtil.h"

ACE_RCSID(baci, baci, "$Id: baciUtil.cpp,v 1.4 2007/06/12 08:02:23 nbarriga Exp $");

namespace baci {

#ifdef MAKE_VXWORKS
unsigned long long lpow10(int value)
{
  unsigned long long result;
  result = 1;
  for (int i=1; i < value; i++) 
      {
      result *= 10;
      }
  return result ;
}
 
unsigned long long convString2LLU(char *str2Convert)
{
   const int size=20;
   char  readBuf[size+1];
   unsigned long long result;
 
   result = 0;
   for (int i = 0; i < size; i++) readBuf[i] = ' ';
   readBuf[size] = '\0';
 
   strcpy(readBuf, str2Convert);
   int n = strlen(readBuf);
   
   for ( int j = 0; j < n; j++)
       {
       switch (readBuf[j]) {
       case '0':
	   break;
       case '1': 
	   result += 1 * lpow10(n-j); 
	   break;
       case '2': 
	   result += 2 * lpow10(n-j); 
	   break;
       case '3': 
	   result += 3 * lpow10(n-j); 
	   break;
       case '4': 
	   result += 4 * lpow10(n-j); 
	   break;
       case '5': 
	   result += 5 * lpow10(n-j); 
	   break;
       case '6': 
	   result += 6 * lpow10(n-j); 
	   break;
       case '7': 
	   result += 7 * lpow10(n-j); 
	   break;
       case '8': 
	   result += 8 * lpow10(n-j); 
	   break;
       case '9': 
	   result += 9 * lpow10(n-j); 
	   break;
       }
       }
 
   return result;
}

/*
msekoran: I suggest better alogrithm (but it is not error proof)
unsigned long long convString2LLU(char *str2Convert)
{
   for (char * p = str2Convert; *p; p++)
       result = 10 * result + (*p-'0');
   
   return result;
}
*/

char *printLLUasString(unsigned long long numToPrint)
{
    const int size = 20;  // max value 1.84 x 10^19
    char* pattern="0123456789";
 
    char printBuf[size+1];
    char* strPtr;
 
    for (int i = 0; i < size; i++)
        printBuf[i] = ' ';
    printBuf[size] = '\0';
 
    for (int i = size - 1; i >= 0; i--) {
        printBuf[i] = numToPrint % 10 + '0';
        numToPrint /= 10;
        if (numToPrint == 0)
	    {
            break;
	    }
        }
 
    strPtr = strpbrk(printBuf, pattern);
    return strPtr;
}
#endif

#ifdef MAKE_VXWORKS

ACS::TimeInterval ModulusAlgorithm(ACS::TimeInterval a,
			      ACS::TimeInterval b)
{
  if (b==0) 
      {
      return 0;
      }

  ACS::TimeInterval bit = 1;
  ACS::TimeInterval a_half(a);
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


ACS::TimeInterval calculateModulus(ACS::TimeInterval t1, ACS::TimeInterval t2) {

#ifndef MAKE_VXWORKS
  return t1 % t2;
#else
  return ModulusAlgorithm(t1, t2);
#endif
}

 }; 

/*___oOo___*/


