/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2011
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
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: cdbIMDB.cpp,v 1.28 2011/10/28 15:05:05 hsommer Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* almadev  2011-10-28  created
*/

#include <vltPort.h>

#include "cdbIMDB.h"

#include <algorithm>

namespace cdb {

Table* IMDB::createTable( int argc, char** argv, CORBA::ORB_ptr orb)
{
  return new IMDB();
}

IMDB::IMDB() {}

IMDB::~IMDB() {}

Boolean IMDB::CreateRecord(const String &strRecordName,
                           Boolean bTruncate)
{
  MapStrRec::iterator iter = m_mpRecords.find(strRecordName);
  if(iter != m_mpRecords.end())
    {
      if(!bTruncate)
      {
        return FALSE;
      }
      iter->second.clear();
    }
  else
    {
      m_mpRecords[strRecordName] = MapStringToField();
    }

  return FALSE;
}

ULong IMDB::GetRecordState(const String &strRecordName)
{
  MapStrRec::iterator iter = m_mpRecords.find(strRecordName);
  if(iter == m_mpRecords.end())
  {
    return 0;
  }
  else
    return CDB_RECORD_READABLE | CDB_RECORD_WRITABLE | CDB_RECORD_REMOVABLE;
}

Boolean IMDB::GetField(const String &strRecordName,
                       const String &strFieldName,
                       Field &fld)
{
  MapStrRec::iterator iter = m_mpRecords.find(strRecordName);
  if(iter == m_mpRecords.end()) 
  {
  	return FALSE;
  }
  MapStringToField::iterator iter2 = iter->second.find(strFieldName);
  if(iter2 == iter->second.end()) 
  {
  	return FALSE;
  }
  fld = iter2->second;
  return TRUE;
}

Boolean IMDB::SetField(const String &strRecordName,
                       const String &strFieldName,
                       const Field &fld,
                       Boolean bCreate)
{
  MapStrRec::iterator iter = m_mpRecords.find(strRecordName);
  if(iter == m_mpRecords.end())
    {
      return FALSE;
    }  
  MapStringToField::iterator iter2 = iter->second.find(strFieldName);
  if(iter2 == iter->second.end())
    {
      if(!bCreate) 
      {
      	return FALSE;
      }
      iter->second[strFieldName] = fld;
      return TRUE;
    }

  iter2->second = fld;

  return TRUE;
}

Boolean IMDB::RemoveField(const String &/*strRecordName*/,
                          const String &/*strFieldName*/)
{
  return FALSE;
}

Boolean IMDB::GetRecord(const String &strRecordName,
                        Record &rec,
                        Boolean bCreate,
                        Boolean bAppend)
{
  MapStrRec::iterator iter = m_mpRecords.find(strRecordName);
  if(iter == m_mpRecords.end())
    {
      if(!bCreate) 
      {
      	return FALSE;
      }
      rec.SetOrigin(strRecordName, this);
      if(!bAppend)
      {
        rec.Map().clear();
      }
      return TRUE;
    }

  rec.SetOrigin(strRecordName, this);
  if(!bAppend)
  {
    rec.Map().clear();
  }
  MapStringToField::iterator it = iter->second.begin();
  while(it != iter->second.end())
    {
      rec.Map().insert(*it);
      ++it;
    }
  return TRUE;
}
  
Boolean IMDB::SetRecord(const String &strRecordName,
                        const Record &recSrc,
                        Boolean bCreate,
                        Boolean bAll)
{
  MapStrRec::iterator iter = m_mpRecords.find(strRecordName);
  if(iter == m_mpRecords.end())
    {
      if(!bCreate) 
      {
      	return FALSE;
      }
      m_mpRecords[strRecordName] = recSrc.Map();			// !!! not right
    }
  else
    {
      MapStringToField &mpDest = iter->second;

      if(bAll)
        {
          Record::const_iterator it = recSrc.begin();
          while(it != recSrc.end()) { mpDest.insert(*it); it++; }
        }
      else
        {
          SetOfStrings::const_iterator itDirty;
          itDirty = recSrc.GetFirstDirty();
          while(itDirty != recSrc.GetLastDirty())
            {
              String str = *itDirty;
              Record::const_iterator itSrcField = recSrc.find(*itDirty);
              if(itSrcField != recSrc.end())
              {
                  mpDest.insert(*itSrcField);
              }
              else
                {
                  MapStringToField::iterator it =
                    mpDest.find(*itDirty);
                  mpDest.erase(it);
                }
              ++itDirty;
            }
        }
    }

  return TRUE;
}

Boolean IMDB::RemoveRecord(const String &strRecordName)
{
  m_mpRecords.erase(strRecordName);
  return TRUE;
}

Boolean IMDB::GetChildren(const String &strRecordName,
                          StringArray &astrChildren)
{
  ACE_UNUSED_ARG(strRecordName);
  ACE_UNUSED_ARG(astrChildren);
	/*
  MapStrRec::const_iterator iter = m_mpRecords.begin();
  String strRec = strRecordName + CDB_HIERARCHY_SEPARATOR;
  while(iter != m_mpRecords.end())
    {
#if defined(__GNUG__)
      if(iter->first.compare(strRec, 0, strRec.length()) == 0 &&
         iter->first.size() > strRec.size())
#else
		 if(iter->first.compare(0, strRec.length(), strRec) == 0 &&
         iter->first.size() > strRec.size())
#endif
        astrChildren.push_back(iter->first);
      ++iter;
    }
  return TRUE;
*/
	return FALSE;
}

 }; 

// ************************************************************************
//
// REVISION HISTORY:
//
//   $Log: cdbIMDB.cpp,v $
//   Revision 1.28  2011/10/28 15:05:05  hsommer
//   Manually fixed "no LGPL license text" issue reported by addCopyright.py
//
//   Revision 1.27  2006/09/01 02:20:54  cparedes
//   small change, NAMESPACE_BEGIN / NAMESPACE_END / NAMESPACE_USE macross to clean up a little the cpp code
//
//   Revision 1.26  2005/02/14 10:39:36  acaproni
//   Some changes to reduce the coding standards number of errors
//
//   Revision 1.25  2003/08/18 12:36:00  rgeorgie
//   LGPL header added
//
//   Revision 1.24  2003/01/28 16:44:04  vltsccm
//   gchiozzi: patch for cdb module to create lib/endorsed directory, since CVS cannot restore empty directories
//
//   Revision 1.23  2003/01/24 10:44:27  vltsccm
//   cdb1.23
//
//   Revision 1.22  2003/01/20 15:12:33  vltsccm
//   cdb1.22
//
//   Revision 1.21  2003/01/20 10:46:07  vltsccm
//   cdb1.21
//
//   Revision 1.20  2002/12/05 16:04:12  vltsccm
//   cdb1.20
//
//   Revision 1.19  2002/11/25 16:05:05  vltsccm
//   cdb1.19
//
//   Revision 1.18  2002/11/13 14:53:18  vltsccm
//   cdb1.18
//
//   Revision 1.17  2002/11/13 10:22:44  vltsccm
//   cdb1.17
//
//   Revision 1.16  2002/11/06 08:37:40  vltsccm
//   cdb1.16
//
//   Revision 1.15.1.23  2002/11/05 16:05:27  vltsccm
//   cdb1.15.1.23
//
//   Revision 1.15.1.22  2002/11/05 13:46:46  vltsccm
//   cdb1.15.1.22
//
//   Revision 1.15.1.21  2002/11/05 10:41:27  vltsccm
//   cdb1.15.1.21
//
//   Revision 1.15.1.20  2002/11/01 12:49:16  vltsccm
//   cdb1.15.1.20
//
//   Revision 1.15.1.19  2002/10/30 07:56:57  vltsccm
//   cdb1.15.1.19
//
//   Revision 1.15.1.18  2002/10/25 12:44:40  vltsccm
//   cdb1.15.1.18
//
//   Revision 1.15.1.17  2002/10/24 13:08:57  vltsccm
//   cdb1.15.1.17
//
//   Revision 1.15.1.16  2002/10/16 11:44:29  vltsccm
//   cdb1.15.1.16
//
//   Revision 1.15.1.15  2002/10/14 22:26:34  vltsccm
//   cdb1.15.1.15
//
//   Revision 1.15.1.14  2002/10/14 12:18:58  vltsccm
//   cdb1.15.1.14
//
//   Revision 1.15.1.13  2002/10/04 16:20:40  vltsccm
//   cdb1.15.1.13
//
//   Revision 1.15.1.12  2002/10/02 12:54:26  vltsccm
//   cdb1.15.1.12
//
//   Revision 1.15.1.11  2002/10/01 10:33:38  vltsccm
//   cdb1.15.1.11
//
//   Revision 1.15.1.10  2002/09/30 13:57:46  vltsccm
//   cdb1.15.1.10
//
//   Revision 1.15.1.9  2002/09/26 14:13:16  vltsccm
//   cdb1.15.1.9
//
//   Revision 1.15.1.8  2002/09/26 07:47:07  vltsccm
//   cdb1.15.1.8
//
//   Revision 1.15.1.7  2002/09/17 16:19:26  vltsccm
//   cdb1.15.1.7
//
//   Revision 1.15.1.6  2002/09/17 11:15:51  vltsccm
//   cdb1.15.1.6
//
//   Revision 1.15.1.5  2002/09/02 09:37:09  vltsccm
//   cdb1.15.1.5
//
//   Revision 1.15.1.4  2002/08/09 09:35:27  vltsccm
//   cdb1.15.1.4
//
//   Revision 1.15.1.3  2002/07/24 07:29:13  vltsccm
//   cdb1.15.1.3
//
//   Revision 1.15.1.2  2002/07/12 09:58:22  vltsccm
//   cdb1.15.1.2
//
//   Revision 1.15+.1.1  2002/07/09 09:40:33  vltsccm
//   cdb1.15.1
//
//   Revision 1.15  2002/02/05 17:50:10  vltsccm
//   cdb1.15
//
//   Revision 1.14  2002/01/14 21:14:20  vltsccm
//   cdb1.14
//
//   Revision 1.13  2001/10/19 09:56:25  vltsccm
//   cdb1.13
//
//   Revision 1.12  2001/09/18 10:07:14  vltsccm
//   cdb1.12
//
//   Revision 1.11  2001/07/12 07:48:30  vltsccm
//   cdb1.11
//
//   Revision 1.10  2001/07/11 09:16:28  vltsccm
//   cdb1.10
//
//   Revision 1.6  2000/12/07 18:00:42  vltsccm
//   cdb1.6
//
//   Revision 1.5  2000/11/17 13:15:00  vltsccm
//   cdb1.5
//
//   Revision 1.4  2000/10/20 13:51:35  vltsccm
//   cdb1.4
//
//   Revision 1.3  2000/10/20 13:51:34  vltsccm
//   cdb1.3
//
//   Revision 1.2  2000/10/20 13:51:34  vltsccm
//   cdb1.2
//
//   Revision 1.1  2000/10/20 13:51:34  vltsccm
//   cdb1.1
//
//   Revision 1.0  2000/10/20 13:51:33  vltsccm
//   cdb1.0
//
//   Revision 1.3  2000/10/13 16:03:03  vltsccm
//   cdb1.3
//
//   Revision 1.2  2000/09/13 14:49:30  vltsccm
//   cdb1.2
//
//   Revision 1.1  2000/09/06 15:42:12  vltsccm
//   cdb1.1
//
//   Revision 1.1  2000/08/12 12:21:27  matej
//   *** empty log message ***
//
//   Revision 1.2  2000/07/05 12:14:21  matej
//   *** empty log message ***
//
//   Revision 1.1  2000/06/13 07:26:24  kzagar
//   CDB, initial commit. Documentation not yet finished.
//
// ************************************************************************
