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
* "@(#) $Id: cdb.i,v 1.26 2011/10/28 15:05:05 hsommer Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* almadev  2011-10-28  created
*/


inline Boolean Record::Commit()
{
  if(m_pTable)
  {
    Boolean rv = m_pTable->SetRecord(m_strRecord, *this, TRUE, FALSE);
    m_setDirty.clear();
    return rv;
  }
  else
    return FALSE;
}

inline const Field& Record::operator[](const String &strName) const
{
  const_iterator iter = MapStringToField::find(strName);
  if(iter == MapStringToField::end())
  {
	  return Field::Nonexistent;
	  /*Field fld;
	  return fld; // "tole" resi problem*/
  }
  else
	return iter->second;
}

inline const Field &
Record::GetField(const String &strName,
                 const Field &fldDefault) const
{
  const_iterator iter = find(strName);
  if(iter == end())
    return fldDefault;
  else
    return iter->second;
}

inline Boolean
Record::SetField(const String &strName,
                 const Field &fldValue,
                 Boolean bCreate)
{
  iterator iter = MapStringToField::find(strName);
  if(iter == end())
    {
      if(!bCreate) return FALSE;
      insert(std::make_pair(strName, fldValue));
    }
  else
    {
      if(iter->second.GetType() != fldValue.GetType()) return FALSE;
      iter->second = fldValue;
    }
  m_setDirty.insert(strName);
  return TRUE;
}

inline void
Record::Clear()
{
  const_iterator it = begin();
  while(it != end())
    m_setDirty.insert(it->first);
  clear();
}

inline Boolean Record::RemoveField(const String &strName)
{
  iterator iter = MapStringToField::find(strName);
  if(iter != end()) 
    { 
      erase(iter); 
      m_setDirty.insert(strName);
      return TRUE; 
    }
  return FALSE;
}

inline Boolean Table::Lock(Boolean bExclusiveWrite)
{
  if(bExclusiveWrite)
    {
      if(m_bWriteLock) return FALSE;
      m_bWriteLock = TRUE;
    }
  ++m_nRefCount;
  return TRUE;
}

inline Boolean Table::Unlock(Boolean bExclusiveWrite)
{
  if(bExclusiveWrite)
    {
      // ASSERT: m_bWriteLock == TRUE
      m_bWriteLock = FALSE;
    }
  --m_nRefCount;
  if(m_nRefCount == 0)
    delete this;
  return TRUE;
}

// ************************************************************************
//
// REVISION HISTORY:
//
//   $Log: cdb.i,v $
//   Revision 1.26  2011/10/28 15:05:05  hsommer
//   Manually fixed "no LGPL license text" issue reported by addCopyright.py
//
//   Revision 1.25  2003/07/09 08:07:35  bjeram
//   ported to gcc 3.2
//
//   Revision 1.24  2003/01/28 16:43:49  vltsccm
//   gchiozzi: patch for cdb module to create lib/endorsed directory, since CVS cannot restore empty directories
//
//   Revision 1.23  2003/01/24 10:44:02  vltsccm
//   cdb1.23
//
//   Revision 1.22  2003/01/20 15:12:18  vltsccm
//   cdb1.22
//
//   Revision 1.21  2003/01/20 10:45:52  vltsccm
//   cdb1.21
//
//   Revision 1.20  2002/12/05 16:03:57  vltsccm
//   cdb1.20
//
//   Revision 1.19  2002/11/25 16:04:49  vltsccm
//   cdb1.19
//
//   Revision 1.18  2002/11/13 14:53:03  vltsccm
//   cdb1.18
//
//   Revision 1.17  2002/11/13 10:22:30  vltsccm
//   cdb1.17
//
//   Revision 1.16  2002/11/06 08:37:03  vltsccm
//   cdb1.16
//
//   Revision 1.15.1.23  2002/11/05 16:05:12  vltsccm
//   cdb1.15.1.23
//
//   Revision 1.15.1.22  2002/11/05 13:46:30  vltsccm
//   cdb1.15.1.22
//
//   Revision 1.15.1.21  2002/11/05 10:41:13  vltsccm
//   cdb1.15.1.21
//
//   Revision 1.15.1.20  2002/11/01 12:49:02  vltsccm
//   cdb1.15.1.20
//
//   Revision 1.15.1.19  2002/10/30 07:56:43  vltsccm
//   cdb1.15.1.19
//
//   Revision 1.15.1.18  2002/10/25 12:44:23  vltsccm
//   cdb1.15.1.18
//
//   Revision 1.15.1.17  2002/10/24 13:08:43  vltsccm
//   cdb1.15.1.17
//
//   Revision 1.15.1.16  2002/10/16 11:43:44  vltsccm
//   cdb1.15.1.16
//
//   Revision 1.15.1.15  2002/10/14 22:26:08  vltsccm
//   cdb1.15.1.15
//
//   Revision 1.15.1.14  2002/10/14 12:18:31  vltsccm
//   cdb1.15.1.14
//
//   Revision 1.15.1.13  2002/10/04 16:20:22  vltsccm
//   cdb1.15.1.13
//
//   Revision 1.15.1.12  2002/10/02 12:54:13  vltsccm
//   cdb1.15.1.12
//
//   Revision 1.15.1.11  2002/10/01 10:33:24  vltsccm
//   cdb1.15.1.11
//
//   Revision 1.15.1.10  2002/09/30 13:56:51  vltsccm
//   cdb1.15.1.10
//
//   Revision 1.15.1.9  2002/09/26 14:13:09  vltsccm
//   cdb1.15.1.9
//
//   Revision 1.15.1.8  2002/09/26 07:45:46  vltsccm
//   cdb1.15.1.8
//
//   Revision 1.15.1.7  2002/09/17 16:19:21  vltsccm
//   cdb1.15.1.7
//
//   Revision 1.15.1.6  2002/09/17 11:15:46  vltsccm
//   cdb1.15.1.6
//
//   Revision 1.15.1.5  2002/09/02 09:37:06  vltsccm
//   cdb1.15.1.5
//
//   Revision 1.15.1.4  2002/08/09 09:35:22  vltsccm
//   cdb1.15.1.4
//
//   Revision 1.15.1.3  2002/07/24 07:29:10  vltsccm
//   cdb1.15.1.3
//
//   Revision 1.15.1.2  2002/07/12 09:58:16  vltsccm
//   cdb1.15.1.2
//
//   Revision 1.15+.1.1  2002/07/09 09:40:08  vltsccm
//   cdb1.15.1
//
//   Revision 1.15  2002/02/05 17:50:07  vltsccm
//   cdb1.15
//
//   Revision 1.14  2002/01/14 21:14:17  vltsccm
//   cdb1.14
//
//   Revision 1.13  2001/10/19 09:56:22  vltsccm
//   cdb1.13
//
//   Revision 1.12  2001/09/18 10:07:11  vltsccm
//   cdb1.12
//
//   Revision 1.11  2001/07/12 07:48:25  vltsccm
//   cdb1.11
//
//   Revision 1.10  2001/07/11 09:16:12  vltsccm
//   cdb1.10
//
//   Revision 1.9  2001/07/11 09:16:12  vltsccm
//   cdb1.9
//
//   Revision 1.8  2001/07/11 09:16:12  vltsccm
//   cdb1.8
//
//   Revision 1.7  2001/07/11 09:16:12  vltsccm
//   cdb1.7
//
//   Revision 1.6  2001/07/11 09:16:11  vltsccm
//   cdb1.6
//
//   Revision 1.5  2001/07/11 09:16:11  vltsccm
//   cdb1.5
//
//   Revision 1.4  2001/07/11 09:16:10  vltsccm
//   cdb1.4
//
//   Revision 1.3  2001/07/11 09:16:10  vltsccm
//   cdb1.3
//
//   Revision 1.2  2001/07/11 09:16:10  vltsccm
//   cdb1.2
//
//   Revision 1.1  2001/07/11 09:16:09  vltsccm
//   cdb1.1
//
//   Revision 1.0  2001/07/11 09:16:09  vltsccm
//   cdb1.0
//
//   Revision 1.6  2000/12/07 18:00:40  vltsccm
//   cdb1.6
//
//   Revision 1.5  2000/11/17 13:14:57  vltsccm
//   cdb1.5
//
//   Revision 1.4  2000/10/20 13:51:17  vltsccm
//   cdb1.4
//
//   Revision 1.3  2000/10/20 13:51:17  vltsccm
//   cdb1.3
//
//   Revision 1.2  2000/10/20 13:51:17  vltsccm
//   cdb1.2
//
//   Revision 1.1  2000/10/20 13:51:16  vltsccm
//   cdb1.1
//
//   Revision 1.0  2000/10/20 13:51:16  vltsccm
//   cdb1.0
//
//   Revision 1.3  2000/10/13 16:03:01  vltsccm
//   cdb1.3
//
//   Revision 1.2  2000/09/13 14:49:28  vltsccm
//   cdb1.2
//
//   Revision 1.1  2000/09/06 15:42:11  vltsccm
//   cdb1.1
//
//   Revision 1.1  2000/06/13 07:26:24  kzagar
//   CDB, initial commit. Documentation not yet finished.
//
// ************************************************************************
