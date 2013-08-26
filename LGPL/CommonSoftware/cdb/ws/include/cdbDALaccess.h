/*******************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: cdbDALaccess.h,v 1.33 2008/09/29 09:51:19 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* dvitas    2002/07/01  created
*/

#ifndef __cdb_DALaccess_h__
#define __cdb_DALaccess_h__


#include <acsutil.h>
#include <cdbExport.h>

#include <cdb.h>
#include <cdbDALC.h>
#include <cdbDALS.h>
#include <cdbDAOImpl.h>


namespace cdb {

class DALChangeListenerImpl;

class cdb_EXPORT DALaccess : public Table
{
public:
        DALaccess( int argc, char *argv[], CORBA::ORB_ptr orb = CORBA::ORB::_nil() );
	virtual ~DALaccess();
	static  Table* createTable( int argc, char** argv, CORBA::ORB_ptr orb );

// Overrides
	virtual Boolean isInitialized() { return m_initialized; }

	Boolean CreateRecord(const String &strRecordName,
		Boolean bTruncate = FALSE);
	
	ULong		GetRecordState(const String &strRecordName);
	
	Boolean GetField(const String &strRecordName,
		const String &strFieldName,
		Field &fld);
	
	Boolean SetField(const String &strRecordName,
		const String &strFieldName,
		const Field &fld,
		Boolean bCreate = TRUE);
	
	Boolean RemoveField(const String &strRecordName,
		const String &strFieldName);
	
	Boolean GetRecord(const String &strRecordName,
		Record &rec,
		Boolean bCreate = FALSE,
		Boolean bAppend = FALSE);
	
	Boolean SetRecord(const String &strRecordName,
		const Record &rec,
		Boolean bCreate = TRUE,
		Boolean bAll = TRUE);
	Boolean RemoveRecord(const String &strRecordName);
	
	virtual Boolean GetChildren(const String &strRecordName,
		StringArray &astrChildren);
//
	void	UseLocalDAOs( int localDAOs = 1 ) { m_useLocalDAO = localDAOs; }

	static void forceDAL(CDB::DAL_ptr dal) { m_forcedDAL = CDB::DAL::_duplicate(dal); }
        static void exitFunction() { exitStarts = 1; }
protected:
// Operations
	char*				resolveDALserverIOR( int argc, char *argv[] );
	DAOImpl*		getDAO( const String &strRecordName );

// Implementation
	CORBA::ORB_var	m_orb;
	CDB::DAL_var	m_dal;
  Boolean	m_initialized;
  int		m_useLocalDAO;
  Boolean       m_destroyORB;
  int		m_useCacheListener;

  typedef std::map<String, DAOImpl*> MapStrRec;
  MapStrRec m_mpRecords;

  CDB::DALChangeListener_var changeListenerObj;
  long changeListenerID;

  friend class DALChangeListenerImpl;
// avoid deregistration of change listener when stub objects are destroyed
  static int exitStarts;

  static CDB::DAL_var m_forcedDAL;

};

class DALChangeListenerImpl : public POA_CDB::DALChangeListener
{
public:
    virtual void object_changed (
        const char * curl
         
      );

      DALaccess::MapStrRec* pMap;

};

 }; 

#endif // __cdb_DALaccess_h__



