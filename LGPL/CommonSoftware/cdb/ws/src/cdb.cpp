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
* "@(#) $Id: cdb.cpp,v 1.33 2012/01/20 23:18:16 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* almadev  2011-10-28  created
*/

#include <vltPort.h>

#include "cdb.h"

// GCH: CCS database not used any more for the time being
// #include <cdbCCS.h>

#include <cdbIMDB.h>
#include <cdbDALaccess.h>
#include <cdbINIReader.h>
#include <ace/Null_Mutex.h>

 using namespace cdb;

Record::Record(const String &strRecord, Table *pTable) :
  m_pTable(pTable),
  m_strRecord(strRecord),
  m_bCommitOnClose(TRUE)
{
  if(m_pTable != 0)
  {
    m_pTable->Lock();
  }
}

Record::~Record()
{
  if(m_bCommitOnClose)
  {
    Commit();
  }
  if(m_pTable != 0)
  {
    m_pTable->Unlock();
  }
}

void Record::SetOrigin(const String &strRecord, Table *pTable)
{
  if(m_pTable)
  {
    m_pTable->Unlock();
  }
  m_pTable = pTable;
  if(pTable)
  {
    pTable->Lock();
  }
  m_strRecord = strRecord;
}

Table::Table()
{
  m_nRefCount  = 1;
  m_bWriteLock = FALSE;
}

Table::~Table()
{
}

Boolean Table::GetParent(const String &strRecordName,
                         String &strParent)
{
	int pos = strRecordName.rfind(CDB_HIERARCHY_SEPARATOR);
	if(pos == String::npos) 
	{ 
		strParent = ""; 
		return FALSE; 
	}
	strParent = strRecordName.substr(0,pos);
	return TRUE;
}

////////////////////////////////////////////////////////////////////////////////////
// TableStorage and static functions

typedef ACE_Singleton<TableStorage, ACE_Null_Mutex> TableStorageS;

template ACE_Singleton<TableStorage, ACE_Null_Mutex> * ACE_Singleton<TableStorage, ACE_Null_Mutex>::singleton_;

TableStorage::TableStorage()
{
  //register default tables
  bindType( "DAL", DALaccess::createTable );
  bindType( "IMDB", IMDB::createTable  );
  bindType( "INI", INIReader::createTable  );

  // GCH: CCS database not used any more for the time being
  // bindType( "CCS", CCS::createTable  );

}

TableStorage::~TableStorage()
{
  // delete all tables that remain in 
  for( unsigned int i=0; i<m_dbMap.size(); i++ ) {
    delete m_dbMap[i].table;
  }
}

Table* TableStorage::first(void)
{
  if( m_dbMap.size() > 0 ) 
  {
    return m_dbMap[0].table;
  }
  return NULL;
}

Table* TableStorage::find(const char* name)
{
  for( unsigned int i=0; i<m_dbMap.size(); i++ ) 
  {
    if( strcmp(m_dbMap[i].name.c_str(), name) == 0 )
    {
      return m_dbMap[i].table;
    }
  }
  return NULL;
}

Boolean TableStorage::bind(const char* name, Table* table )
{
  TableEntry entry;
  entry.name = name;
  entry.table = table;
  m_dbMap.push_back(entry);
  return TRUE;
}

Boolean TableStorage::unbind( Table* table )
{
//  for( unsigned int i=0; i<m_dbMap.size(); i++ ) {
//    if( m_dbMap[i].table == table ) {
//      m_dbMap.erase(&m_dbMap[i]);
// replacet gor gcc 3.2
    for( std::vector<TableEntry>::iterator i=m_dbMap.begin(); i != m_dbMap.end(); ++i ) 
 	{
    	if( i->table == table ) {
    		m_dbMap.erase(i);
    		return TRUE;
    	}
    }
  return FALSE;
}

Boolean TableStorage::bindType(const char* name, TableFactory pTf )
{
  m_dbTypes[name] = pTf;
  return TRUE;
}

TableFactory TableStorage::findType(const char* name)
{
	DBTypes::const_iterator iter = m_dbTypes.find( name );
	if(iter != m_dbTypes.end())
	{
		return iter->second;
	}
  return NULL;
}

const char* TableStorage::getDefault( void ) 
{
  if( m_defaultTable.length() == 0 ) 
  {
    return NULL;
  }
  return m_defaultTable.c_str();
}

void TableStorage::setDefault( const char* name ) 
{
  m_defaultTable = name;
}

cdb_EXPORT Table* cdb::getDatabase( int argc, char** argv, CORBA::ORB_ptr orb, const char* defaultTable, int forceNew)
{
  const char *pDBName = defaultTable;
  Table* pDataBase;
  TableFactory pTf;

  for(int i=0; i<argc; i++ ) {
    if( ACE_OS::strcmp( argv[i], "-ACS_CDB") == 0 ) {
      if( i<argc-1 )
      {
        pDBName = argv[i+1];
      }
    }
  }
  
  TableStorage* ts = TableStorageS::instance();

  if( !pDBName ) {
    pDBName = ts->getDefault();
    if( !pDBName )
    {
      pDBName = "DAL";
    }
  }

  pDataBase = ts->find(pDBName);
  if( !forceNew && pDataBase ) {
    pDataBase->_add_ref();
    return pDataBase;
  }

  pTf = ts->findType(pDBName);
  if( !pTf ) 
  {
    return NULL;
  }
  
  pDataBase = pTf(argc, argv, orb );
  if( !ts->getDefault() )
  {
    ts->setDefault(pDBName);
  }
  ts->bind( pDBName, pDataBase );
  
  return pDataBase;

}

cdb_EXPORT void cdb::destroyDatabase( Table* table )
{
  int count = table->_rem_ref();
  if( count == 0 ) { // it is safe to delete this object
    TableStorage* ts = TableStorageS::instance();
    ts->unbind( table );
    delete table;
    if( ts->first() == NULL )
    {
      ts->setDefault("");
    }
  }
}

cdb_EXPORT void cdb::registerTable( const char* name, TableFactory pTf )
{
  TableStorage* ts = TableStorageS::instance();
  ts->bindType( name, pTf );
}


// ************************************************************************
//
// REVISION HISTORY:
//
//   $Log: cdb.cpp,v $
//   Revision 1.33  2012/01/20 23:18:16  tstaig
//   Backport from branches ACS-9_0_0-windows-B and ACS-9_1_0-windows-B to support
//   ACS on Windows under Cygwin. This commit corresponds to the folowing
//   CommonSoftware modules:
//   acsthread acscomponentidl cdbidl maciidl baciidl acsncidl acsjlog repeatGuard
//   loggingts loggingtsTypes jacsutil2 cdb cdbChecker codegen cdb_rdb
//
//   Revision 1.32  2011/10/28 15:05:11  hsommer
//   Manually fixed "no LGPL license text" issue reported by addCopyright.py
//
//   Revision 1.31  2006/09/25 08:36:59  cparedes
//   cdbErrType extended to complete the others exceptions not covered on cdbDAL.idl
//   Removed all the exceptions from cdbDAL.idl, now they are in cdbErrType.
//   The list of changes is:
//
//   FieldIsReadOnly  ----> CDBFieldIsReadOnlyEx
//   RecordIsReadOnly  ----> CDBReadoOnlyDataEx
//   WrongDataType  ----> WrongCDBDataTypeEx
//   RecordAlreadyExists  ----> CDBRecordAlreadyExistsEx
//   CDBException  ----> CDBExceptionEx
//   FieldDoesNotExist  ----> CDBFieldDoesNotExistEx
//   RecordDoesNotExist  ----> CDBRecordDoesNotExistEx
//   XMLerror  ----> CDBXMLErrorEx
//
//   All the source code and the reference files on the test directories on the HEAD were changed according the above.
//
//   Revision 1.30  2006/09/01 02:20:54  cparedes
//   small change, NAMESPACE_BEGIN / NAMESPACE_END / NAMESPACE_USE macross to clean up a little the cpp code
//
//   Revision 1.29  2005/02/14 10:39:36  acaproni
//   Some changes to reduce the coding standards number of errors
//
//   Revision 1.28  2004/03/17 07:39:28  bjeram
//   ported to ACE 5.4 and TAO 1.4
//
//   Revision 1.27  2003/08/18 12:36:00  rgeorgie
//   LGPL header added
//
//   Revision 1.26  2003/07/09 08:07:35  bjeram
//   ported to gcc 3.2
//
//   Revision 1.25  2003/05/06 13:32:01  bjeram
//   port to Tornado 2.2
//
//   Revision 1.24  2003/01/28 16:44:03  vltsccm
//   gchiozzi: patch for cdb module to create lib/endorsed directory, since CVS cannot restore empty directories
//
//   Revision 1.23  2003/01/24 10:44:25  vltsccm
//   cdb1.23
//
//   Revision 1.22  2003/01/20 15:12:32  vltsccm
//   cdb1.22
//
//   Revision 1.21  2003/01/20 10:46:06  vltsccm
//   cdb1.21
//
//   Revision 1.20  2002/12/05 16:04:11  vltsccm
//   cdb1.20
//
//   Revision 1.19  2002/11/25 16:05:04  vltsccm
//   cdb1.19
//
//   Revision 1.18  2002/11/13 14:53:17  vltsccm
//   cdb1.18
//
//   Revision 1.17  2002/11/13 10:22:43  vltsccm
//   cdb1.17
//
//   Revision 1.16  2002/11/06 08:37:39  vltsccm
//   cdb1.16
//
//   Revision 1.15.1.23  2002/11/05 16:05:26  vltsccm
//   cdb1.15.1.23
//
//   Revision 1.15.1.22  2002/11/05 13:46:45  vltsccm
//   cdb1.15.1.22
//
//   Revision 1.15.1.21  2002/11/05 10:41:26  vltsccm
//   cdb1.15.1.21
//
//   Revision 1.15.1.20  2002/11/01 12:49:16  vltsccm
//   cdb1.15.1.20
//
//   Revision 1.15.1.19  2002/10/30 07:56:57  vltsccm
//   cdb1.15.1.19
//
//   Revision 1.15.1.18  2002/10/25 12:44:39  vltsccm
//   cdb1.15.1.18
//
//   Revision 1.15.1.17  2002/10/24 13:08:56  vltsccm
//   cdb1.15.1.17
//
//   Revision 1.15.1.16  2002/10/16 11:44:28  vltsccm
//   cdb1.15.1.16
//
//   Revision 1.15.1.15  2002/10/14 22:26:33  vltsccm
//   cdb1.15.1.15
//
//   Revision 1.15.1.14  2002/10/14 12:18:57  vltsccm
//   cdb1.15.1.14
//
//   Revision 1.15.1.13  2002/10/04 16:20:39  vltsccm
//   cdb1.15.1.13
//
//   Revision 1.15.1.12  2002/10/02 12:54:25  vltsccm
//   cdb1.15.1.12
//
//   Revision 1.15.1.11  2002/10/01 10:33:37  vltsccm
//   cdb1.15.1.11
//
//   Revision 1.15.1.10  2002/09/30 13:57:45  vltsccm
//   cdb1.15.1.10
//
//   Revision 1.15.1.9  2002/09/26 14:13:16  vltsccm
//   cdb1.15.1.9
//
//   Revision 1.15.1.8  2002/09/26 07:47:06  vltsccm
//   cdb1.15.1.8
//
//   Revision 1.15.1.7  2002/09/17 16:19:25  vltsccm
//   cdb1.15.1.7
//
//   Revision 1.15.1.6  2002/09/17 11:15:50  vltsccm
//   cdb1.15.1.6
//
//   Revision 1.15.1.5  2002/09/02 09:37:08  vltsccm
//   cdb1.15.1.5
//
//   Revision 1.15.1.4  2002/08/09 09:35:26  vltsccm
//   cdb1.15.1.4
//
//   Revision 1.15.1.3  2002/07/24 07:29:13  vltsccm
//   cdb1.15.1.3
//
//   Revision 1.15.1.2  2002/07/12 09:58:21  vltsccm
//   cdb1.15.1.2
//
//   Revision 1.15+.1.1  2002/07/09 09:40:33  vltsccm
//   cdb1.15.1
//
//   Revision 1.15  2002/02/05 17:50:09  vltsccm
//   cdb1.15
//
//   Revision 1.14  2002/01/14 21:14:19  vltsccm
//   cdb1.14
//
//   Revision 1.13  2001/10/19 09:56:24  vltsccm
//   cdb1.13
//
//   Revision 1.12  2001/09/18 10:07:13  vltsccm
//   cdb1.12
//
//   Revision 1.11  2001/07/12 07:48:29  vltsccm
//   cdb1.11
//
//   Revision 1.10  2001/07/11 09:16:28  vltsccm
//   cdb1.10
//
//   Revision 1.6  2000/12/07 18:00:42  vltsccm
//   cdb1.6
//
//   Revision 1.5  2000/11/17 13:14:59  vltsccm
//   cdb1.5
//
//   Revision 1.4  2000/10/20 13:51:31  vltsccm
//   cdb1.4
//
//   Revision 1.3  2000/10/20 13:51:31  vltsccm
//   cdb1.3
//
//   Revision 1.2  2000/10/20 13:51:30  vltsccm
//   cdb1.2
//
//   Revision 1.1  2000/10/20 13:51:30  vltsccm
//   cdb1.1
//
//   Revision 1.0  2000/10/20 13:51:30  vltsccm
//   cdb1.0
//
//   Revision 1.3  2000/10/13 16:03:04  vltsccm
//   cdb1.3
//
//   Revision 1.2  2000/09/13 14:49:30  vltsccm
//   cdb1.2
//
//   Revision 1.1  2000/09/06 15:42:13  vltsccm
//   cdb1.1
//
//   Revision 1.1  2000/08/12 12:21:28  matej
//   *** empty log message ***
//
//   Revision 1.2  2000/07/05 12:14:21  matej
//   *** empty log message ***
//
//   Revision 1.1  2000/06/13 07:26:24  kzagar
//   CDB, initial commit. Documentation not yet finished.
//
// ************************************************************************
