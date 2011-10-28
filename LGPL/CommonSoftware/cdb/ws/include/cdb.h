#ifndef __cdb__CDB_h__
#define __cdb__CDB_h__
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
* "@(#) $Id: cdb.h,v 1.27 2011/10/28 15:05:05 hsommer Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* almadev  2011-10-28  created
*/

#include "cdbField.h"

#include <map>
#include <set>

#include "acsutil.h"
#include "cdbExport.h"
#include "ace/Singleton.h"

namespace cdb {
class cdb_EXPORT Table;

//
// DESCRIPTION: A record in a configuration database.
//
// A record stores a plethora of fields, each of them identified by a name.
// The fields are stored in memory as a STL map, which guarantees fast
// O(log N) 
//
typedef std::map<String, Field> MapStringToField;
typedef std::set<String> SetOfStrings;
typedef Table* (*TableFactory)(int, char**, CORBA::ORB_ptr);

class  cdb_EXPORT Record : protected MapStringToField
{
  friend class Table; 
 
  Record(const Record&);
  Record &operator=(const Record&);

 public:  


  typedef MapStringToField::const_iterator const_iterator;
  const_iterator begin() const { return MapStringToField::begin(); }
  const_iterator end() const { return MapStringToField::end(); }
  const_iterator find(const String &str) const
    { return MapStringToField::find(str); } 

  void Clear();

  Record(const String &strRecord = "", Table *pTable = 0);
  ~Record();

  void SetOrigin(const String &strRecord, Table *pTable);

  void CommitOnClose(Boolean b) { m_bCommitOnClose = b; };
  Boolean Commit();

  //
  //
  const Field& operator[](const String &strName) const;

  //
  // DESCRIPTION:
  //   Get a field from the record. If it doesn't exist, use a default
  //   value.
  //
  // PARAMETERS:
  //   strName      Name of the field.
  //   fldDefault   Default value which is returned if the field does not
  //                exist yet.
  //
  // RETURN VALUE:
  //   Returns the requested field or default value if it does not exist.
  //
  const Field& GetField(const String &strName,
                        const Field &fldDefault) const;

  //
  // DESCRIPTION:
  //   Set a value of the field.
  //
  Boolean SetField(const String &strName,
                   const Field &fldValue,
                   Boolean bCreate = TRUE);



  Boolean RemoveField(const String &strName);

  const SetOfStrings::const_iterator GetFirstDirty() const
    { return m_setDirty.begin(); }
  const SetOfStrings::const_iterator GetLastDirty() const
    { return m_setDirty.end(); }

  SetOfStrings &Dirty() { return m_setDirty; }
  const SetOfStrings &Dirty() const { return m_setDirty; }

  MapStringToField &Map() { return *this; }
  const MapStringToField &Map() const { return *this; }

 protected:
  iterator begin() { return MapStringToField::begin(); }
  iterator end() { return MapStringToField::end(); }

 private:

  Table *m_pTable;
  String m_strRecord;

  Boolean m_bCommitOnClose;

  SetOfStrings m_setDirty;
};

//
// Separator between name components of the record, so that the name can
// imply a hierarchy.
//
// Alternatives:
//    /
//    :   VLT CCS database
//    \   Windows
//

#define CDB_HIERARCHY_SEPARATOR ':'

#define CDB_RECORD_READABLE  1
#define CDB_RECORD_WRITABLE  2
#define CDB_RECORD_REMOVABLE 4

//
// DESCRIPTION: A table in the configuration database.
//
class cdb_EXPORT Table
{
  int     m_nRefCount;
  Boolean m_bWriteLock;
public:
  int _add_ref(void) { return ++m_nRefCount; }
  int _rem_ref(void) { return --m_nRefCount; }

  typedef std::pair<String, Field> NamedField;
  typedef std::vector<Field> NamedFieldArray;

  Table();
  virtual ~Table();

  virtual Boolean isInitialized() = 0;


  //
  // DESCRIPTION: Read/write lock the table.
  //
  // Before using a table, a read/write lock must be put in place to:
  //
  //   1. Prevent the table from being deleted while still in use.
  //   2. Prevent several processes from simultaneously modifying the
  //      table.
  //
  // To gain an exclusive write lock, set bExclusiveWrite to a TRUE
  // value. Such an operation might block, waiting for the current
  // exclusive write lock to be released. To prevent blocking in such
  // cases, set bNonBlocking to TRUE as well.
  //
  // EXAMPLE:
  //
  //         SomeTable tbl;
  //         tbl.Lock(TRUE, FALSE);
  //         // ...
  //         tbl.Unlock(TRUE); // don't forget to unlock!
  //
  Boolean Lock(Boolean bExclusiveWrite = 0);

  Boolean Unlock(Boolean bExclusiveWrite = 0);

  //
  // DESCRIPTION: Create a record in the table.
  //
  // Create an empty record in the table.
  //
  // PARAMETERS:
  //   strRecordName - The name by which the newly created record
  //                   will be identified.
  //
  // RETURN VALUE: Returns TRUE if the record can be used, and FALSE if the
  // creation had failed.
  //   
  virtual Boolean CreateRecord(const String &strRecordName,
                               Boolean bTruncate = FALSE) = 0;

  //
  // Returns TRUE if the record already exists.
  //
  virtual ULong GetRecordState(const String &strRecordName) = 0;

  virtual Boolean GetField(const String &strRecordName,
                           const String &strFieldName,
                           Field &fld) = 0;
  virtual Boolean SetField(const String &strRecordName,
                           const String &strFieldName,
                           const Field &fld,
                           Boolean bCreate = TRUE) = 0;
  virtual Boolean RemoveField(const String &strRecordName,
                              const String &strFieldName) = 0;

  //
  // DESCRIPTION: Get a record from the database.
  //
  // PARAMETERS:
  //   strRecordName   - The name of the record to retrieve from the database.
  //   rec             - Reference of the record to fill-in.
  virtual Boolean GetRecord(const String &strRecordName,
                            Record &rec,
                            Boolean bCreate = FALSE,
                            Boolean bAppend = FALSE) = 0;
  virtual Boolean SetRecord(const String &strRecordName,
                            const Record &rec,
                            Boolean bCreate = TRUE,
                            Boolean bAll = TRUE) = 0;
  virtual Boolean RemoveRecord(const String &strRecordName) = 0;

  // ----------------------------------------------------------------------
  // GROUP = Navigation
  // ----------------------------------------------------------------------

  //
  // DESCRIPTION: Get the name of the root record.
  //
  // PARAMETERS:
  //   strRoot -  Reference of the string where the root record's name will
  //              be placed. This name can be later used with other methods
  //              of the table.
  //
  // RETURN VALUE:
  //   FALSE if the table does not have any support for hierarchical
  //   ordering of its records, TRUE otherwise.
  //
  virtual Boolean GetRoot(String &strRoot)
  { strRoot = CDB_HIERARCHY_SEPARATOR; return TRUE; }

  //
  // DESCRIPTION: Get all child-records of a given record.
  //
  // PARAMETERS:
  //   strRecordName
  //
  virtual Boolean GetChildren(const String &strRecordName,
                              StringArray &astrChildren) = 0;

  virtual Boolean GetParent(const String &strRecordName,
                            String &strParent);
};

typedef struct _tagTableEntry
{
  String  name;
  Table*  table;
} TableEntry;

class TableStorage
{
public:
  Table* find(const char* name );  
  Table* first(void);
  Boolean   bind(const char* name, Table* table );
  Boolean   unbind( Table* table );
  const char*  getDefault( void );
  void   setDefault( const char* name );

  // registered types
  Boolean  bindType(const char* name, TableFactory pTf );
  TableFactory findType(const char* name);

protected:
  TableStorage();
  ~TableStorage();

 	typedef std::map<String, TableFactory> DBTypes;

  std::vector<TableEntry> m_dbMap;
  DBTypes m_dbTypes;
  ACE_CString m_defaultTable;
  friend class ACE_Singleton<TableStorage, ACE_Null_Mutex>;
};

cdb_EXPORT Table* getDatabase( int argc = 0, char** argv = NULL, CORBA::ORB_ptr orb = CORBA::ORB::_nil(), const char* defaultTable= NULL, int forceNew = 0 );
cdb_EXPORT void destroyDatabase( Table* table );
cdb_EXPORT void registerTable( const char* name, TableFactory pTf );

#include "cdb.i"

 }; 



#endif // __cdb__CDB_h__

// ************************************************************************
//
// REVISION HISTORY:
//
//   $Log: cdb.h,v $
//   Revision 1.27  2011/10/28 15:05:05  hsommer
//   Manually fixed "no LGPL license text" issue reported by addCopyright.py
//
//   Revision 1.26  2006/09/01 02:20:54  cparedes
//   small change, NAMESPACE_BEGIN / NAMESPACE_END / NAMESPACE_USE macross to clean up a little the cpp code
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
//   Revision 1.19  2002/11/25 16:04:48  vltsccm
//   cdb1.19
//
//   Revision 1.18  2002/11/13 14:53:03  vltsccm
//   cdb1.18
//
//   Revision 1.17  2002/11/13 10:22:29  vltsccm
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
//   Revision 1.15.1.20  2002/11/01 12:49:01  vltsccm
//   cdb1.15.1.20
//
//   Revision 1.15.1.19  2002/10/30 07:56:43  vltsccm
//   cdb1.15.1.19
//
//   Revision 1.15.1.18  2002/10/25 12:44:22  vltsccm
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
//   Revision 1.15.1.10  2002/09/30 13:56:50  vltsccm
//   cdb1.15.1.10
//
//   Revision 1.15.1.9  2002/09/26 14:13:09  vltsccm
//   cdb1.15.1.9
//
//   Revision 1.15.1.8  2002/09/26 07:45:45  vltsccm
//   cdb1.15.1.8
//
//   Revision 1.15.1.7  2002/09/17 16:19:20  vltsccm
//   cdb1.15.1.7
//
//   Revision 1.15.1.6  2002/09/17 11:15:46  vltsccm
//   cdb1.15.1.6
//
//   Revision 1.15.1.5  2002/09/02 09:37:05  vltsccm
//   cdb1.15.1.5
//
//   Revision 1.15.1.4  2002/08/09 09:35:22  vltsccm
//   cdb1.15.1.4
//
//   Revision 1.15.1.3  2002/07/24 07:29:09  vltsccm
//   cdb1.15.1.3
//
//   Revision 1.15.1.2  2002/07/12 09:58:15  vltsccm
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
//   Revision 1.13  2001/10/19 09:56:21  vltsccm
//   cdb1.13
//
//   Revision 1.12  2001/09/18 10:07:11  vltsccm
//   cdb1.12
//
//   Revision 1.11  2001/07/12 07:48:25  vltsccm
//   cdb1.11
//
//   Revision 1.10  2001/07/11 09:16:09  vltsccm
//   cdb1.10
//
//   Revision 1.6  2000/12/07 18:00:40  vltsccm
//   cdb1.6
//
//   Revision 1.5  2000/11/17 13:14:57  vltsccm
//   cdb1.5
//
//   Revision 1.4  2000/10/20 13:51:19  vltsccm
//   cdb1.4
//
//   Revision 1.3  2000/10/20 13:51:19  vltsccm
//   cdb1.3
//
//   Revision 1.2  2000/10/20 13:51:18  vltsccm
//   cdb1.2
//
//   Revision 1.1  2000/10/20 13:51:18  vltsccm
//   cdb1.1
//
//   Revision 1.0  2000/10/20 13:51:18  vltsccm
//   cdb1.0
//
//   Revision 1.3  2000/10/13 16:03:01  vltsccm
//   cdb1.3
//
//   Revision 1.2  2000/09/13 14:49:28  vltsccm
//   cdb1.2
//
//   Revision 1.1  2000/09/06 15:42:10  vltsccm
//   cdb1.1
//
//   Revision 1.1  2000/06/13 07:26:24  kzagar
//   CDB, initial commit. Documentation not yet finished.
//
// ************************************************************************
