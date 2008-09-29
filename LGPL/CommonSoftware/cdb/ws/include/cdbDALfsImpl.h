/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: cdbDALfsImpl.h,v 1.26 2008/09/29 09:51:19 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* dvitas    2002/07/01  created
*/


#ifndef cdbDALfsImpl_H_
#define cdbDALfsImpl_H_

#include <cdbDALS.h>

#if !defined (ACE_LACKS_PRAGMA_ONCE)
#pragma once
#endif /* ACE_LACKS_PRAGMA_ONCE */


//Class DALfsImpl
class  DALfsImpl : public virtual POA_CDB::DAL
{
public:
  //Constructor 
  DALfsImpl (CORBA::ORB_ptr orb, PortableServer::POA_ptr poa, CORBA::Environment &);
  
  //Destructor 
  virtual ~DALfsImpl (void);
  
virtual char * get_DAO (
    const char * curl,
    
  );
/*
* @throw CDB::RecordDoesNotExist
* @throw CDB::XMLerror
*/
virtual ::CDB::DAO_ptr get_DAO_Servant (
    const char * curl
    
  );

protected:
/*
* @throw CDB::RecordDoesNotExist
*/
	char*		LoadRecord( const char * curl );

	bool		GetRecordPath( const char * curl, ACE_CString& path );

	CORBA::ORB_var			m_orb;
	PortableServer::POA_var m_poa;
	ACE_CString				m_root;
};


#endif /* cdbDALfsImpl_H_  */




