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
* "@(#) $Id: cdbINIReader.cpp,v 1.27 2006/09/01 02:20:54 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2001/05/09  created 
*/

#include <vltPort.h>

#include "cdbINIReader.h"
#include "logging.h"

ACE_RCSID(cdb, cdbINIReader, "$Id: cdbINIReader.cpp,v 1.27 2006/09/01 02:20:54 cparedes Exp $");

namespace cdb {

Table* INIReader::createTable( int argc, char** argv, CORBA::ORB_ptr orb)
{
  const char *fileName = ""; 
  for(int i=0; i<argc; i++ ) {
    if( ACE_OS::strcmp( argv[i], "-FILE_NAME") == 0 ) {
      if( i<argc-1 )
        fileName = argv[i+1];
    }
  }
  return new INIReader( fileName );
}

INIReader::INIReader(const char * fileName) : 
  m_initialized(FALSE)
{

  m_fileHandle = ACE_OS::fopen (fileName, "r");

  if (m_fileHandle==0)
    {
      ACS_SHORT_LOG ((LM_INFO,"Unable to open INI file (r) '%s'", fileName));
      return;
    }

  m_initialized = TRUE;
  *m_section = 0;
}

INIReader::~INIReader() 
{
  if (m_initialized)
    {
      ACE_OS::fclose (m_fileHandle);
    }
}

Boolean
INIReader::isInitialized()
{
  return m_initialized;
}

Boolean
//INIReader::NextSection(String &strRecordName, String &strFieldName, String &strValue)
INIReader::NextSection(ACE_TCHAR * strRecordName, ACE_TCHAR * strFieldName, ACE_TCHAR * strValue)
{
  if (!m_initialized)
  {
    return FALSE;
  }

  // buffer
  ACE_TCHAR line[INI_MAX_LINE_LENGTH];
  
  int len;
  char* separator;

  ACE_TCHAR *name;
  ACE_TCHAR *value;

  // read field here
  while (ACE_OS::fgets(line, INI_MAX_LINE_LENGTH, m_fileHandle)!=NULL)
    {
      // trim here

      // separate field name from value

      len = ACE_OS::strlen(line);
      if (len==0) 
      {
      	continue;
      }
      

      if ((separator = ACE_OS::strnchr (line, INI_BEGIN_SECTION_CHAR, len)) &&
	  (line[len-2]==INI_END_SECTION_CHAR))
	{
	  line[len-2]=0;
	  ACE_OS::strncpy(m_section, line+1, len-2);
	}
      else if ((separator = ACE_OS::strnchr (line, INI_SEPARATOR_CHAR, len)))
	{

	  line[len-1] = 0;	// cut-off new-line char
	  value = separator+1;
	  
	  *separator = 0;	// replace separator with 0
	  name = line;
	  
	  ACE_OS::strcpy(strRecordName, m_section);
	  ACE_OS::strcpy(strFieldName, name);
	  ACE_OS::strcpy(strValue, value);

	  //ACE_OS::printf("[%s:%s]='%s'\n", m_section, name, value);

	  return TRUE;


	}


    }

  return FALSE;
}

Boolean
INIReader::Rewind()
{
  if (!m_initialized)
  {
    return FALSE;
  }

  ACE_OS::rewind(m_fileHandle);
  return TRUE;
}

Boolean
INIReader::CreateRecord(const String &strRecordName,
			Boolean bTruncate)
{
  ACE_UNUSED_ARG(strRecordName);
  ACE_UNUSED_ARG(bTruncate);
  return FALSE;
}

ULong
INIReader::GetRecordState(const String &strRecordName)
{
  ACE_UNUSED_ARG(strRecordName);
  return 0;
}

Boolean
INIReader::GetField(const String &strRecordName,
		    const String &strFieldName,
		    Field &fld)
{
  if (!m_initialized)
  {
    return FALSE;
  }

  // move to the start of the file
  ACE_OS::rewind(m_fileHandle); 

  // buffer
  ACE_TCHAR line[INI_MAX_LINE_LENGTH];
  ACE_TCHAR section[INI_MAX_LINE_LENGTH];
  int len;
  char* separator;

  // empty
  *section = 0;

  // for the test
  ACE_TCHAR *name;
  ACE_TCHAR *value;

  // read field here
  while (ACE_OS::fgets(line, INI_MAX_LINE_LENGTH, m_fileHandle)!=NULL)
    {
      // trim here

      // separate field name from value

      len = ACE_OS::strlen(line);
      if (len==0) 
      {
      	continue;
      }
      

      if ((separator = ACE_OS::strnchr (line, INI_BEGIN_SECTION_CHAR, len)) &&
	  (line[len-2]==INI_END_SECTION_CHAR))
	{
	  line[len-2]=0;
	  ACE_OS::strncpy(section, line+1, len-2);
	}
      else if ((ACE_OS::strcmp(section, strRecordName.c_str())==0) &&
	       (separator = ACE_OS::strnchr (line, INI_SEPARATOR_CHAR, len)))
	{

	  line[len-1] = 0;	// cut-off new-line char
	  value = separator+1;
	  
	  *separator = 0;	// replace separator with 0
	  name = line;
	  
	  if ((ACE_OS::strcmp(name, strFieldName.c_str())==0))
	    {
	      fld.FromString(value);
	      return TRUE;
	    }

	}


    }

  return FALSE;
}

Boolean
INIReader::SetField(const String &strRecordName,
		    const String &strFieldName,
		    const Field &fld,
		    Boolean bCreate)
{
  ACE_UNUSED_ARG(strRecordName);
  ACE_UNUSED_ARG(strFieldName);
  ACE_UNUSED_ARG(fld);
  ACE_UNUSED_ARG(bCreate);
  return FALSE;
}

Boolean
INIReader::RemoveField(const String &strRecordName,
		       const String &strFieldName)
{
  ACE_UNUSED_ARG(strRecordName);
  ACE_UNUSED_ARG(strFieldName);
  return FALSE;
}

Boolean
INIReader::GetRecord(const String &strRecordName,
		     Record &rec,
		     Boolean bCreate,
		     Boolean bAppend)
{
  ACE_UNUSED_ARG(strRecordName);
  ACE_UNUSED_ARG(rec);
  ACE_UNUSED_ARG(bCreate);
  ACE_UNUSED_ARG(bAppend);
  return FALSE;
}
  
Boolean
INIReader::SetRecord(const String &strRecordName,
		     const Record &recSrc,
		     Boolean bCreate,
		     Boolean bAll)
{
  ACE_UNUSED_ARG(strRecordName);
  ACE_UNUSED_ARG(recSrc);
  ACE_UNUSED_ARG(bCreate);
  ACE_UNUSED_ARG(bAll);
  return FALSE;
}

Boolean
INIReader::RemoveRecord(const String &strRecordName)
{
  ACE_UNUSED_ARG(strRecordName);
  return FALSE;
}

Boolean
INIReader::GetChildren(const String &strRecordName,
		       StringArray &astrChildren)
{
  ACE_UNUSED_ARG(strRecordName);
  ACE_UNUSED_ARG(astrChildren);
  return FALSE;
}

 }; 

// ************************************************************************
//
// REVISION HISTORY:
//
// $Log: cdbINIReader.cpp,v $
// Revision 1.27  2006/09/01 02:20:54  cparedes
// small change, NAMESPACE_BEGIN / NAMESPACE_END / NAMESPACE_USE macross to clean up a little the cpp code
//
// Revision 1.26  2005/02/14 10:39:36  acaproni
// Some changes to reduce the coding standards number of errors
//
// Revision 1.25  2003/08/18 12:36:00  rgeorgie
// LGPL header added
//
// Revision 1.24  2003/01/28 16:44:03  vltsccm
// gchiozzi: patch for cdb module to create lib/endorsed directory, since CVS cannot restore empty directories
//
// Revision 1.23  2003/01/24 10:44:25  vltsccm
// cdb1.23
//
// Revision 1.22  2003/01/20 15:12:32  vltsccm
// cdb1.22
//
// Revision 1.21  2003/01/20 10:46:06  vltsccm
// cdb1.21
//
// Revision 1.20  2002/12/05 16:04:11  vltsccm
// cdb1.20
//
// Revision 1.19  2002/11/25 16:05:04  vltsccm
// cdb1.19
//
// Revision 1.18  2002/11/13 14:53:17  vltsccm
// cdb1.18
//
// Revision 1.17  2002/11/13 10:22:43  vltsccm
// cdb1.17
//
// Revision 1.16  2002/11/06 08:37:38  vltsccm
// cdb1.16
//
// Revision 1.15.1.23  2002/11/05 16:05:26  vltsccm
// cdb1.15.1.23
//
// Revision 1.15.1.22  2002/11/05 13:46:44  vltsccm
// cdb1.15.1.22
//
// Revision 1.15.1.21  2002/11/05 10:41:26  vltsccm
// cdb1.15.1.21
//
// Revision 1.15.1.20  2002/11/01 12:49:15  vltsccm
// cdb1.15.1.20
//
// Revision 1.15.1.19  2002/10/30 07:56:56  vltsccm
// cdb1.15.1.19
//
// Revision 1.15.1.18  2002/10/25 12:44:39  vltsccm
// cdb1.15.1.18
//
// Revision 1.15.1.17  2002/10/24 13:08:56  vltsccm
// cdb1.15.1.17
//
// Revision 1.15.1.16  2002/10/16 11:44:28  vltsccm
// cdb1.15.1.16
//
// Revision 1.15.1.15  2002/10/14 22:26:32  vltsccm
// cdb1.15.1.15
//
// Revision 1.15.1.14  2002/10/14 12:18:57  vltsccm
// cdb1.15.1.14
//
// Revision 1.15.1.13  2002/10/04 16:20:38  vltsccm
// cdb1.15.1.13
//
// Revision 1.15.1.12  2002/10/02 12:54:25  vltsccm
// cdb1.15.1.12
//
// Revision 1.15.1.11  2002/10/01 10:33:37  vltsccm
// cdb1.15.1.11
//
// Revision 1.15.1.10  2002/09/30 13:57:45  vltsccm
// cdb1.15.1.10
//
// Revision 1.15.1.9  2002/09/26 14:13:15  vltsccm
// cdb1.15.1.9
//
// Revision 1.15.1.8  2002/09/26 07:47:06  vltsccm
// cdb1.15.1.8
//
// Revision 1.15.1.7  2002/09/17 16:19:24  vltsccm
// cdb1.15.1.7
//
// Revision 1.15.1.6  2002/09/17 11:15:50  vltsccm
// cdb1.15.1.6
//
// Revision 1.15.1.5  2002/09/02 09:37:08  vltsccm
// cdb1.15.1.5
//
// Revision 1.15.1.4  2002/08/09 09:35:26  vltsccm
// cdb1.15.1.4
//
// Revision 1.15.1.3  2002/07/24 07:29:13  vltsccm
// cdb1.15.1.3
//
// Revision 1.15.1.2  2002/07/12 09:58:20  vltsccm
// cdb1.15.1.2
//
// Revision 1.15+.1.1  2002/07/09 09:40:32  vltsccm
// cdb1.15.1
//
// Revision 1.15  2002/02/05 17:50:09  vltsccm
// cdb1.15
//
// Revision 1.14  2002/01/14 21:14:19  vltsccm
// cdb1.14
//
// Revision 1.13  2001/10/19 09:56:24  vltsccm
// cdb1.13
//
// Revision 1.12  2001/09/18 10:07:13  vltsccm
// cdb1.12
//
// Revision 1.11  2001/07/12 07:48:29  vltsccm
// cdb1.11
//
// Revision 1.10  2001/07/11 09:16:27  vltsccm
// cdb1.10
//
// Revision 1.9  2001/07/11 09:16:27  vltsccm
// cdb1.9
//
// Revision 1.8  2001/07/11 09:16:27  vltsccm
// cdb1.8
//
// Revision 1.7  2001/07/11 09:16:26  vltsccm
// cdb1.7
//
// Revision 1.6  2001/07/11 09:16:26  vltsccm
// cdb1.6
//
// Revision 1.5  2001/07/11 09:16:26  vltsccm
// cdb1.5
//
// Revision 1.4  2001/07/11 09:16:25  vltsccm
// cdb1.4
//
// Revision 1.3  2001/07/11 09:16:25  vltsccm
// cdb1.3
//
// Revision 1.2  2001/07/11 09:16:25  vltsccm
// cdb1.2
//
// Revision 1.1  2001/07/11 09:16:24  vltsccm
// cdb1.1
//
// Revision 1.0  2001/07/11 09:16:24  vltsccm
// cdb1.0
//
//
// ************************************************************************

/*___oOo___*/
