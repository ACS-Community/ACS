#include <MockManager.h>
#include <orbsvcs/CosNamingC.h>
#include <acsutilPorts.h>

namespace maci 
{
	/** 
    Get a service, activating it if necessary (components). 
    The client represented by id (the handle) 
    must have adequate access rights to access 
    the service. 
    NOTE: a component is also a service, i.e. a service activated by a container.
    
    @return Reference to the service. 
    If the service could not be activated, a nil 
    reference is returned, and the status contains 
    an error code detailing the cause of failure 
    (one of the COMPONENT_* constants).
 	*/
	CORBA::Object_ptr MockManager::get_service (maci::Handle id, const char * curl, CORBA::Boolean activate)
        throw (CORBA::SystemException, maciErrType::CannotGetComponentEx, maciErrType::ComponentNotAlreadyActivatedEx, 
               maciErrType::ComponentConfigurationNotFoundEx) 

	{
		// corbaloc::<hostname>:<port>/CDB
		const char* hostname = 0;
		hostname = ACSPorts::getIP();
		if (hostname == 0)
		{
			return CORBA::Object::_nil();
		}

		ACE_TCHAR corbalocRef[240];
		ACE_OS::sprintf(corbalocRef, "corbaloc::%s:%s/CDB", hostname, ACSPorts::getCDBPort().c_str());
		int  nargc = 0;
		char **nargv = 0;
		CORBA::ORB_var orb = CORBA::ORB_init (nargc, nargv, "");
		CORBA::Object_var object = orb->string_to_object(corbalocRef);
		return object._retn();
	}
}
