/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: taskStaticContainerServices.cpp,v 1.8 2011/10/14 21:05:01 javarias Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram  yyyy-mm-dd  created 
*/

static char *rcsId="@(#) $Id: taskStaticContainerServices.cpp,v 1.8 2011/10/14 21:05:01 javarias Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#define _POSIX_SOURCE 1
#include "vltPort.h"
#include <acsutilPorts.h>
#include "taskStaticContainerServices.h"

using namespace acsErrTypeContainerServices;

StaticContainerServices::StaticContainerServices(const maci::Handle componentHandle, 
						ACE_CString& name,
						ACE_CString& type,
						 PortableServer::POA_ptr poa,
						 CORBA::ORB_ptr orb)
    :  maci::ContainerServices::ContainerServices(name, poa), 
/*       orb_m(orb),*/
       componentStateManager_m(name)
{
    ACS_TRACE("StaticContainerServices:StaticContainerServices");
    m_componentType = ACE_CString(type);
}//StaticContainerServices

CDB::DAL_ptr StaticContainerServices::getCDB()
{

    ACE_TCHAR corbalocRef[230];
    ACE_TCHAR * envRef = ACE_OS::getenv ("DAL_REFERENCE");
    ACS_TRACE("StaticContainerServices::getCDB");
    
    if (envRef && *envRef)
	{
	ACS_LOG(0, "TestContainerServices::getCDB",
		(LM_INFO, "CDB obtained via environment: '%s'", envRef));
	strcpy(corbalocRef, envRef);
	}
    else
	{
	// corbaloc::<hostname>:<port>/CDB
	const char* hostname = 0;
	hostname = ACSPorts::getIP();
	if (hostname==0)
	    {
	    ACSErrTypeCommon::NullPointerExImpl ex(__FILE__, __LINE__, "TestContainerServices::getCDB()");
	    ex.setVariable("hostname");
	    throw CanNotGetCDBExImpl(ex, __FILE__, __LINE__, 
				     "StaticContainerServices::getCDB()");
	    }//if

	
	
	ACE_OS::sprintf(corbalocRef, "corbaloc::%s:%s/CDB", hostname, ACSPorts::getCDBPort().c_str());
	
	ACS_LOG(0, "TestContainerServices::getCDB",
		(LM_INFO, "CDB reference generated using localhost address: '%s'", corbalocRef));
	}//if-than
    
    CDB::DAL_var dalObj = CDB::DAL::_nil();
    CORBA::Object_var obj = orb_m->string_to_object(corbalocRef);
    
    if (!CORBA::is_nil(obj.in()))
	{
	dalObj = CDB::DAL::_narrow(obj.in());
	if (CORBA::is_nil(dalObj.in())) 
	    {
	    throw CanNotGetCDBExImpl(__FILE__, __LINE__, 
				     "StaticContainerServices::getCDB()");
	    }
	}
    else
	{
	throw CanNotGetCDBExImpl(__FILE__, __LINE__, 
				 "StaticContainerServices::getCDB()");
	}
    
    return dalObj._retn();
}//getCDB

acsalarm::AlarmSource* StaticContainerServices::getAlarmSource() {
    return NULL;
}
/*___oOo___*/
