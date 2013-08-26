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
*
* "@(#) $Id: cdbDALaccess.cpp,v 1.45 2008/09/29 09:51:19 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* dvitas    2002/07/01  created
*/

#include <vltPort.h>

#include <cdbDALaccess.h>

#include <algorithm>
#include <logging.h>

#include <acsutilPorts.h>
#include <cdbErrType.h>

// string table
#define MSG_XML_ERROR   "cdbErrType::CDBXMLerrorEx '%s' '%s'\n"

using namespace cdbErrType;

namespace cdb {

CDB::DAL_var DALaccess::m_forcedDAL = CDB::DAL::_nil();

int DALaccess::exitStarts = 0;

void DALChangeListenerImpl::object_changed( const char * curl) 
{
	DALaccess::MapStrRec::iterator iter = pMap->find(curl);
	if(iter != pMap->end()) {
		delete iter->second;
		pMap->erase( iter );
		ACS_LOG(0, "cdb::DALaccess::resolveDALserverIOR",
			(LM_INFO, "DALaccess: DAO object removed: '%s'", curl));
	}
}

// env variable for DAL_REFERENCE
#define DAL_REFERENCE "DAL_REFERENCE"

Table* DALaccess::createTable( int argc, char** argv, CORBA::ORB_ptr orb)
{
  return new DALaccess( argc, argv, orb );
}

DALaccess:: DALaccess( int argc, char *argv[], CORBA::ORB_ptr orb ) : m_orb(CORBA::ORB::_duplicate(orb)),
    m_initialized(FALSE), m_useLocalDAO(1), m_destroyORB(FALSE), m_useCacheListener(1)
{
	char *DALior = NULL;
	const char *orbId = "DALaccess";
	atexit(exitFunction);
  for(int i=0; i<argc; i++ ) {
    if( ACE_OS::strcmp( argv[i], "-DALORBid") == 0 ) {
      if( i<argc-1 )
      {
        orbId = argv[i+1];
      }
    }
    if( ACE_OS::strcmp( argv[i], "-DAOremote") == 0 ) {
      m_useLocalDAO = 0;
    }
    if( ACE_OS::strcmp( argv[i], "-DALNoCacheListener") == 0 ) {
      m_useCacheListener = 0;
    }
  }
	// init DAL client
	
	try{
		if( orb == CORBA::ORB::_nil() ){
			// init ORB
			m_orb = CORBA::ORB_init (argc, argv, orbId);
			
			m_destroyORB = TRUE;
		}
		// get the ior for DAL
		DALior = resolveDALserverIOR( argc, argv );
		// Get the object reference DAL
		CORBA::Object_var object = m_orb->string_to_object (DALior);
		

		// narrow object
		m_dal = CDB::DAL::_narrow (object.in ());

		
	}catch(CORBA::Exception &ex){
		ACE_PRINT_EXCEPTION(ACE_ANY_EXCEPTION, "cdb::DALaccess::DALaccess()");
	}

	if( DALior )
	{
		ACE_OS::free (DALior);
	}

	// check if we succeed
	if( !CORBA::is_nil(m_dal.in()) ) {
		m_initialized = TRUE;
 
		if( m_useCacheListener ) {
			DALChangeListenerImpl* changeListener = new DALChangeListenerImpl;
			changeListenerObj = changeListener->_this();
			changeListener->_remove_ref(); // decrement ref for one (to 1) that changeListener is deleted 
			changeListenerID = m_dal->add_change_listener( changeListenerObj.ptr() );
			changeListener->pMap = &m_mpRecords;
		}
	}


}

DALaccess::~DALaccess()
{
   
	if( m_useCacheListener && !exitStarts ) {
		
		try{
			if( !CORBA::is_nil(m_dal.in()) ) 
			{
				m_dal->remove_change_listener( changeListenerID );
				
			}
		}catch(CORBA::Exception &ex){
			// don't worry server will handle itself
		}
	}
	MapStrRec::const_iterator iter = m_mpRecords.begin();
	while(iter != m_mpRecords.end()) {
		delete iter->second;
		++iter;
	}
	if (m_destroyORB)
	{
	    m_orb->destroy();
	}
}

DAOImpl* DALaccess::getDAO( const String &strRecordName )
{
	DAOImpl* pDAO = NULL;

	
	try{
		// in the case when we create local object
		if( m_useLocalDAO ) {
			// remote call
			CORBA::String_var xml = m_dal->get_DAO( strRecordName.c_str() );
			
			// create local object for reord
			pDAO = new DAOImpl( xml.in() );
			if( !pDAO )
			{
				return NULL;
			}

			if( !pDAO->isInitialized() ) {
				ACE_OS::printf( MSG_XML_ERROR, strRecordName.c_str(), pDAO->m_errorMessage.c_str() );
				delete pDAO;
				return NULL;
			}
			pDAO->m_name = strRecordName;
		}
		else {
			// we have to use remote DAO instantiated by DAL server
			CDB::DAO_var dao = m_dal->get_DAO_Servant( strRecordName.c_str() );
      
			pDAO = new DAOImpl( dao.in() );
		}
	}
	catch( CDBXMLErrorEx ex ) {
		CDBXMLErrorExImpl ex_impl(ex); 
		// a = ex_impl.getCurl();
		ACE_OS::printf( MSG_XML_ERROR, strRecordName.c_str(), ex_impl.getCurl().c_str());
		if( pDAO )
		{
			delete pDAO;
		}
		return NULL;
	}
	catch(CORBA::Exception &ex){
	/**
         * TODO GCH Commented out for the time being
         */
        //	ACE_PRINT_EXCEPTION(ACE_ANY_EXCEPTION, "cdb::DALaccess::getDAO()");
		if( pDAO )
		{
			delete pDAO;
		}
		return NULL;
	}

	// seems all OK
	return pDAO;
}

Boolean DALaccess::GetField(const String &strRecordName,
			    const String &strFieldName,
			    Field &fld)
{
    if (!m_initialized)
    {
		return FALSE;
    }
    
    /*
     * replace all ":" chars with "/"
     */
    char *cpath = (char*)strRecordName.c_str();
    if (ACE_OS::strncmp(cpath, ":Appl_data:", 11)==0)
    {
		cpath += 11;    // skip
    }
    
    for(char* p = cpath; *p; p++)
	{
	if (*p==':')
	    {
	    *p = '/';
	    }
	}

    /***********************************/
    /* do we have already the record ? */
    /***********************************/
    DAOImpl *pDAO = NULL;
    ACE_CString usedRecordName = cpath;
    ACE_CString usedFieldName  = strFieldName;

    MapStrRec::iterator iter;

    /** TODO
     * GCH workaround to handle hiearchies.
     * Now we have two good cases:
     *  - a parent record is already loaded
     *  - a parent record exist
     * The bad case is when a parent record does not
     * esist, and therefore there is nothing we can do!
     * This handling is due to a fundamental limitation
     * of jDAL: is can return only records that are
     * corresponding to XML files, not to sub-parts of
     * XML files.
     * Therefore whan reading a record fails, we have to see
     * if the data we are looking for is contained in a parent
     * record.
     */ 
        
    /*
     * Loops up the hierarchy
     * until it finds a record already loaded 
     * or until it can get it from the CDB
     * Hopefully it finds it at the first loop.
     */
    int pos = ACE_CString::npos;
    int searchComp = TRUE;
    do
	{
	/**
	 * TODO
	 * Tries to reuse (cache) only if in the "alma" branch????
	 * Does not seem ok.
	 * I would say that then the list of records
	 * would be overwritten every time with a new entry.
	 * What about memory leaks?
	 * Please chek. For the time being I comment out.
	 */ 
	/*
	 * Now searches if parent DAO
	 * already loaded
	 */
	iter = m_mpRecords.find(usedRecordName);
	if(iter != m_mpRecords.end()) 
	    {
	    pDAO = iter->second;
	    }

        /*
	 * GCH: We have a very special common case: Properties.
	 *      For a property, data is in the XML of the parent, so it is worth to test 
	 *      if this is already loaded and try with that.
	 *      But only at the first iteration.
	 *      This needs to be checked: what if the parent is loaded
	 *      but it is not a Componenet for a property?
	 *      I.E. if it is a hierarchical Component?
	 */
	else if ( searchComp == TRUE &&
		  ACE_OS::strncmp(usedRecordName.c_str(), "alma", 4)==0 ) 
	    {
	    searchComp = FALSE;
	    pos = usedRecordName.rfind('/');
	    /*
	     * If we are on top, give up
	     */
	    if(pos != ACE_CString::npos)
		{
		ACE_CString anotherFieldName  = 
		    usedRecordName.substr(pos+1) + "/" + usedFieldName;
		ACE_CString anotherRecordName = 
		    usedRecordName.substr(0, pos);
		iter = m_mpRecords.find(anotherRecordName);
		if(iter != m_mpRecords.end()) 
	    {
		    usedFieldName = anotherFieldName;
		    pDAO = iter->second;
	    }
		}
	    }
	/*
	 * Otherwise tries to load it
	 */
	if(pDAO == NULL) 
	    {
	    pDAO = getDAO( usedRecordName );
	    /*
	     * If it can get it, registers it in the table
	     * of loaded records.
	     */
	    if( pDAO )
		{
		m_mpRecords[usedRecordName] = pDAO;
		if( m_useCacheListener )
		{
		    m_dal->listen_for_changes( usedRecordName.c_str(), 
					       changeListenerID );
		}
		}
	    }
	// } /* Here would finish the if "alma" for cache handling

	/* 
	 * If it is still NULL, I navigate up the hiearchy
	 */
	if(pDAO == NULL)
	    {
	    pos = usedRecordName.rfind('/');
	    /*
	     * If we are on top, give up
	     */
	    if(pos == ACE_CString::npos)
	    {
			break;
	    }

	    usedFieldName  = usedRecordName.substr(pos+1) + "/" + usedFieldName;
	    usedRecordName = usedRecordName.substr(0, pos);
	    }

	} while(pDAO==NULL); /* end do loop over hierarchy */

    /*
     * Hopefully we have now a DAO!
     */
    if(pDAO == NULL)
	{
	ACS_LOG(0, "cdb::DAOImpl", 
		(LM_INFO, "Unable to create DAL for record: '%s' (field: '%s')", 
		 strRecordName.c_str(), strFieldName.c_str()));
	return FALSE;
	}

    /*
     * Now try to read the field from the record
     */
    if(!pDAO->get_field(usedFieldName, fld ))
	{
	ACS_LOG(0, "cdb::DAOImpl", 
		(LM_INFO, "DAO:'%s' Unable to return field: '%s'", 
		 pDAO->m_name.c_str(), usedFieldName.c_str()));
	return FALSE;
	}
    return TRUE;
}

Boolean DALaccess::SetField(const String &strRecordName,
			    const String &strFieldName,
			    const Field &fld,
			    Boolean bCreate)
{
    return FALSE;
}

Boolean DALaccess::CreateRecord(const String &strRecordName,
				Boolean bTruncate)
{
    return FALSE;
}

ULong DALaccess::GetRecordState(const String &strRecordName)
{
    return 0;
}

Boolean DALaccess::RemoveField(const String &/*strRecordName*/,
			       const String &/*strFieldName*/)
{
    return FALSE;
}

Boolean DALaccess::GetRecord(const String &strRecordName,
			     Record &rec,
			     Boolean bCreate,
			     Boolean bAppend)
{
    return FALSE;
}

Boolean DALaccess::SetRecord(const String &strRecordName,
			     const Record &recSrc,
			     Boolean bCreate,
			     Boolean bAll)
{
	return FALSE;
}

Boolean DALaccess::RemoveRecord(const String &strRecordName)
{
	return FALSE;
}

Boolean DALaccess::GetChildren(const String &strRecordName,
			       StringArray &astrChildren)
{
	ACE_UNUSED_ARG(strRecordName);
	ACE_UNUSED_ARG(astrChildren);
	return FALSE;
}

char* DALaccess::resolveDALserverIOR( int argc, char *argv[] )
{
	// take forced DAL reference first
	if (!CORBA::is_nil(m_forcedDAL.in()))
	{
		CORBA::String_var ior = m_orb->object_to_string(m_forcedDAL.in());
		return ACE_OS::strdup(ior.in());
	}

	// 1. Command line option -d or -DALReference
	for (int pos = 1; pos < argc-1; pos++) {
		if (ACE_OS::strcmp(argv[pos], "-d")==0 || ACE_OS::strcmp(argv[pos], "-DALReference")==0) 
		{
			// increase pos to point to the DAL's reference
			pos++;
			
			ACS_LOG(0, "cdb::DALaccess::resolveDALserverIOR",
				(LM_INFO, "DALReference obtained via command line: '%s'", argv[pos]));
			return ACE_OS::strdup( argv[pos] );
		}
	}
		
	// 2. Environment variable DAL_REFERENCE
	ACE_TCHAR * envRef = ACE_OS::getenv (DAL_REFERENCE);
	if (envRef && *envRef)
	{
		ACS_LOG(0, "cdb::DALaccess::resolveDALserverIOR",
			(LM_INFO, "DALReference obtained via environment: '%s'", envRef));
		return ACE_OS::strdup( envRef );
	}
		
	// 3. corbaloc::<hostname>:<port>/CDB
	const char* hostname = 0;
	hostname = ACSPorts::getIP();
	if (hostname==0)
	{
		ACS_LOG(LM_RUNTIME_CONTEXT, "cdb::DALaccess::resolveDALserverIOR",
			(LM_ERROR, "Failed to obtain localhost address!"));
		
		return NULL;
	}
	
	ACE_TCHAR corbalocRef[230];
	ACE_OS::sprintf(corbalocRef, "corbaloc::%s:%s/CDB", hostname, ACSPorts::getCDBPort().c_str());

	ACS_LOG(0, "cdb::DALaccess::resolveDALserverIOR",
		(LM_INFO, "DALReference generated using localhost address: '%s'", corbalocRef));
	// return reference
	return ACE_OS::strdup( corbalocRef );
}


 }; 

/*___oOo___*/

