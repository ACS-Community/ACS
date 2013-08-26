#ifndef _ACS_LL_H_
#define _ACS_LL_H_
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
* "@(#) $Id: acsutilLL.h,v 1.2 2005/08/26 22:22:54 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram 2002-01-07 created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

/** @file acsutilLL.h
 * acsLL provides support for Unsigned Long Long:
 * - printing unsigned long long out (converting to/form unsigned long long from/to string)
 * - calculating modules
 */

#ifdef MAKE_VXWORKS
  #define NO_LLU_SUPPORT
#endif

/**
 * Converts from string to unsigned long long. Conversion function exists for platforms 
 * that support unsigned long long and for platforms that do not support unsigned long long.
 * @param str - string to be converted to unsigned long long
 *  
 */
unsigned long long string2LLU(char *str);

/**
 * Converts from unsigned long long to a character string.
 * @param Unsigned long long value.
 * @return Unsigned long long value in string format.
 */
char *LLU2String(unsigned long long);


#ifdef NO_LLU_SUPPORT


#define ACS_LLU "%s"
/**
 * VxWorks does not support printf operations on ULL values.
 * Therefore, we have to provide it.
 */
#define ACS_LLU_PRINTF(a) LLU2String(a)


/**
 * VxWorks does not support modulus operations on ULL values.
 * Therefore, we have to provide it.
 */
#define ACS_LLU_MOD(a,b) LLUModulus(a, b)
unsigned long long LLUModulus(unsigned long long a,
			      unsigned long long b);

#else

/**
 * Macros ACS_LLU and ACS_LLU_PRINTF provide support for printing unsigned long long.
 * Usage: ACE_OS::(f/s)printf ("Example :" ACS_LLU "\n", ACS_LLU_PRINTF(a)); 
 */
#define ACS_LLU "%llu"
#define ACS_LLU_PRINTF(a) a

/**
 * Macro ACS_LLU_SCANFF is used as ACS_LLU_PRINTF but in a scanf function and it provides support for unsigned long long.
 * Usage: scanf (buf, "Example :" ACS_LLU , ACS_LLU_SCANF(a)); 
 */
#define ACS_LLU_SCANF(a) a

/**
 * Macro for calculating modules.
 */
#define ACS_LLU_MOD(a,b) a%b

#endif

#endif /*!_H*/
