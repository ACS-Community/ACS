#ifndef baciUtil_H
#define baciUtil_H

/*******************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2003 
*
*This library is free software; you can redistribute it and/or
*modify it under the terms of the GNU Lesser General Public
*License as published by the Free Software Foundation; either
*version 2.1 of the License, or (at your option) any later version.
*
*This library is distributed in the hope that it will be useful,
*but WITHOUT ANY WARRANTY; without even the implied warranty of
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*Lesser General Public License for more details.
*
*You should have received a copy of the GNU Lesser General Public
*License along with this library; if not, write to the Free Software
*Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: baciUtil.h,v 1.3 2007/06/12 08:02:23 nbarriga Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bgustafs 2001-07-12 changed name of parameter in setCompletion
* msekoran  2001/03/04 modified
*/

/** 
 * @file 
 * Header file BACI.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "baciExport.h"
#include "acsutilTimeStamp.h"

namespace baci {

#ifdef MAKE_VXWORKS

unsigned long long lpow10(int value);
unsigned long long convString2LLU(char *str2Convert);
char *printLLUasString(unsigned long long numToPrint);

ACS::TimeInterval ModulusAlgorithm(ACS::TimeInterval a,
			      ACS::TimeInterval b);
#endif

ACS::TimeInterval calculateModulus(ACS::TimeInterval t1, ACS::TimeInterval t2);

 }; 

/* ------------------------------------------------------------------------ */

#endif /* baci_H */ 


