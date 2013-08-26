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
* "@(#) $Id: acsutilTempFile.cpp,v 1.18 2010/09/13 16:38:21 tstaig Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2002/03/18  created 
*/

#include <vltPort.h>
#include <acsutilTempFile.h>

#define ACS_TEMP_DIR "ACS_TMP"
#define DEFAULT_TEMP_DIR "ACSDATA"
#define TEMP_DIR "tmp"

ACE_CString
getTempFileName(const ACE_TCHAR * fileNameEnvVar, const ACE_TCHAR * fileName)
{

  char *envVal = 0;

  // check for <fileNameEnvVal> env. var
  if (fileNameEnvVar && (envVal=getenv(fileNameEnvVar)) && *envVal)
      return envVal;

  // fileName is needed, check if defined
  if (!fileName || !(*fileName))
      return ACE_CString();

  // preallocate
  ACE_CString path((const char*)0, 30);

  // check if another temp. dir. is specified
  envVal = getenv(ACS_TEMP_DIR);
  if (envVal && *envVal)
      path = envVal;
  // use default
  else
  {
      // resolve default env. var.
      envVal = getenv(DEFAULT_TEMP_DIR);
      if (envVal && *envVal)
	  path = envVal;

      path += ACE_DIRECTORY_SEPARATOR_CHAR;
      path += TEMP_DIR;

      // add host name (env. var. is being read to be consitent with scripts)
      envVal = getenv("HOST");
      if (envVal && *envVal) 
      {
          path += ACE_DIRECTORY_SEPARATOR_CHAR;
          path += envVal;
      }
  }
  // add acs instance path (env. var. is being read to be consitent with scripts)
  envVal = getenv("ACS_INSTANCE");
  if (envVal && *envVal) 
  {
      path += ACE_DIRECTORY_SEPARATOR_CHAR;
      path += ACE_CString("ACS_INSTANCE.");
      path += envVal;
  }

  path += ACE_DIRECTORY_SEPARATOR_CHAR;
  path += fileName;

  return path;
}


// ************************************************************************
//
// REVISION HISTORY:
//
// $Log: acsutilTempFile.cpp,v $
// Revision 1.18  2010/09/13 16:38:21  tstaig
// Changes in regard to COMP-4482 and COMP-4305.
// Logs and files should be stored in instance-specific directory.
//
// Revision 1.17  2009/11/04 12:43:31  msekoran
// ACSDATA/tmp/HOST
//
// Revision 1.16  2003/03/10 14:33:43  rgeorgie
// LGPL
//
// Revision 1.15  2003/03/10 14:28:22  bjeram
// changes according the changes in TAO x.3
//
// Revision 1.14  2002/12/05 12:31:59  vltsccm
// gchiozzi: Added proper GPL licence header to wildcard library
//
// Revision 1.13  2002/12/03 20:00:57  vltsccm
// acsutil1.13
//
// Revision 1.12  2002/11/25 15:16:32  vltsccm
// acsutil1.12
//
// Revision 1.11  2002/09/23 09:35:32  vltsccm
// acsutil1.11
//
// Revision 1.10  2002/04/05 13:02:57  vltsccm
// acsutil1.10
//
// Revision 1.9  2002/04/04 11:06:25  vltsccm
// acsutil1.9
//
// Revision 1.8  2002/03/27 16:41:53  vltsccm
// acsutil1.8
//
// Revision 1.7  2002/03/27 16:41:53  vltsccm
// acsutil1.7
//
// Revision 1.6  2002/03/27 16:41:52  vltsccm
// acsutil1.6
//
// Revision 1.5  2002/03/27 16:41:52  vltsccm
// acsutil1.5
//
// Revision 1.4  2002/03/27 16:41:51  vltsccm
// acsutil1.4
//
// Revision 1.3  2002/03/27 16:41:51  vltsccm
// acsutil1.3
//
// Revision 1.2  2002/03/27 16:41:51  vltsccm
// acsutil1.2
//
// Revision 1.1  2002/03/27 16:41:50  vltsccm
// acsutil1.1
//
// Revision 1.0  2002/03/27 16:41:50  vltsccm
// acsutil1.0
//
//
// ************************************************************************
