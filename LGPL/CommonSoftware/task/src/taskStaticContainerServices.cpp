/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: taskStaticContainerServices.cpp,v 1.4 2005/10/06 15:04:40 bjeram Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram  yyyy-mm-dd  created 
*/

#define _POSIX_SOURCE 1
#include "vltPort.h"
#include <acsutilPorts.h>

static char *rcsId="@(#) $Id: taskStaticContainerServices.cpp,v 1.4 2005/10/06 15:04:40 bjeram Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);


#include "taskStaticContainerServices.h"

StaticContainerServices::StaticContainerServices(const maci::Handle componentHandle, 
						ACE_CString& name,
						 PortableServer::POA_ptr poa,
						 CORBA::ORB_ptr orb)
    :  maci::ContainerServices::ContainerServices(name, poa), 
/*       orb_m(orb),*/
       componentStateManager_m(name)
{
    ACS_TRACE("StaticContainerServices:StaticContainerServices");
}//StaticContainerServices

CDB::DAL_ptr StaticContainerServices::getCDB()
{

    ACE_TCHAR corbalocRef[230];
    ACE_TCHAR * envRef = ACE_OS::getenv ("DAL_REFERENCE");
    
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
	    return (CDB::DAL *)0;
	
	
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
	    ACS_SHORT_LOG((LM_INFO, "TestContainerServices::getCDB() - Failed to narrow CDB"));
	    return (CDB::DAL *)0;
	    }
	}
    
    return dalObj._retn();
}


/*___oOo___*/
