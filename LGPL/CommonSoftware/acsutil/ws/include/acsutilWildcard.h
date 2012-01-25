#ifndef acsutilWildcard_h
#define acsutilWildcard_h

/*******************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2002
* Copyright by ESO (in the framework of the ALMA collaboration)
* and Cosylab 2002, All rights reserved
* 
* This implementation was adpoted from:
*
* Copyright (C) 1996, 1997, 1998, 1999, 2000 Florian Schintke
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software  
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA02111-1307  USA
*
* F.Schintke, the author of the original code, has authorized to 
* distribute these files under LGPL License.
*
* "@(#) $Id: acsutilWildcard.h,v 1.17 2012/01/25 16:26:58 acaproni Exp $"
* 
* ----------------------
* Implementation of the UN*X wildcards
* Supported wild-characters: '*', '?'; sets: [a-z], '!' negation
* Examples:
*	 '[a-g]l*i?n' matches 'florian'
*	 '[!abc]*e' matches 'smile'
*	 '[-z] matches 'a'
*
* TODO: java and python translates a wilcard string into a regular expression and then
*       matches the string using regular expresson libraries. With a low priority we should
*       do the same in C++.
*
* 
* who       when      what
* --------  --------  ----------------------------------------------
* gchiozzi 2002-12-05 Added proper GPL licence header
* msekoran  2001/06/20  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

/** @file acsutilWildcard.h
 * Header for wildcard related functions.
 */

#include <acsutil.h>
#include <acsutilExport.h>

/**
 * Class which implements UNIX style wildcards and tests to see
 * if strings match the wildcard.
 */
class acsutil_EXPORT Wildcard
{
    
  public:
    
    /** 
     * This function implements the UN*X wildcards.
     * @param wildcard Wildcard to be used.
     * @param test Value which we want to see if it matches the wildcard.
     * @return 0 if wildcard does not match *test. 1 - if wildcard 
     * matches test.
     */
    static int wildcardfit (const char *wildcard, const char *test);
    
  private:
    /**
     * Scans a set of characters and returns 0 if the set mismatches at this
     * position in the teststring and 1 if it is matching
     * wildcard is set to the closing ] and test is unmodified if mismatched
     * and otherwise the char pointer is pointing to the next character
     * @param wildcard UNIX style wildcard to be used
     * @param test String we will test against the wildcard.
     * @return 0 if the set mismatches. 1 otherwise.
     */
    static int set (const char **wildcard, const char **test);
    
    /**
     * Scans an asterisk.
     * @param wildcard UNIX style wildcard to be used
     * @param test String we will test against the wildcard.
     * @return ???
     */
    static int asterisk (const char **wildcard, const char **test);
};


#endif   /* acsutilWildcard_h */ 

