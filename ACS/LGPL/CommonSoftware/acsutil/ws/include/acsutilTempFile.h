#ifndef acsutilTempFile_h
#define acsutilTempFile_h

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
* "@(#) $Id: acsutilTempFile.h,v 1.16 2005/08/26 22:22:54 dfugate Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2002/03/18  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

/** @file acsutilTempFile.h
 * Header used to obtain temporary files.
 */

#include <acsutil.h>
#include <ace/SString.h>


/**
 * This function is use to generated a temporary file name, including the full path.
 * The following policy is used:
 * - if <fileNameEnvVar> is defined then file should be created using value (also path) 
 *   from env. variable(s) and ACS_TMP is suppressed.
 * - if ACS_TMP is defined than temporary files are put in ACS_TMP directory
 * - if none of the above is defined, all files are put in $ACSDATA/tmp (only /tmp if $ACSDATA not defined) directory
 * @param fileNameEnvVar temp. file specific env. var. (overrides all options), e.g. "ACS_RECOVERY_FILE"
 * @param fileName only bare file name, e.g. "local_cache.dat"
 * @return Full path of the temporary file.
 */
ACE_CString getTempFileName(const ACE_TCHAR * fileNameEnvVar, const ACE_TCHAR * fileName);

#endif  /* acsutilTempFile_h */

// ************************************************************************
//
// REVISION HISTORY:
//
// $Log: acsutilTempFile.h,v $
// Revision 1.16  2005/08/26 22:22:54  dfugate
// Improved inline documentation a great deal.
//
// Revision 1.15  2003/03/10 14:33:09  rgeorgie
// LGPL
//
// Revision 1.14  2002/12/05 12:31:55  vltsccm
// gchiozzi: Added proper GPL licence header to wildcard library
//
// ************************************************************************
