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

/*
	CORBA::Object_ptr MockManager::resolveNameService(CORBA::ORB_ptr orb, int retries, unsigned int secTimeout)
	{
		if (CORBA::is_nil(orb))
			return CosNaming::NamingContext::_nil();

		// use CORBA::ORB::resolve_intial_references
		try
		{
			CORBA::Object_var naming_obj = orb->resolve_initial_references ("NameService");
			if (!CORBA::is_nil (naming_obj.in ()))
			{
				CosNaming::NamingContext_var naming_context = CosNaming::NamingContext::_narrow (naming_obj.in ());
				if (naming_context.ptr() != CosNaming::NamingContext::_nil())
					return naming_context._retn();
			}
		}
		catch(...)
		{
			ACS_SHORT_LOG((LM_ERROR, "(logging::LoggingHelper::resolveNameService) CORBA exception caught!"));
		}

		// Environment variable LOG_NAMESERVICE_REFERENCE
		ACE_TCHAR * envRef = ACE_OS::getenv (LOG_NAMESERVICE_REFERENCE);
		if (envRef && *envRef)
		{
			//ACS_LOG(0, "logging::LoggingHelper::resolveNameService",
			//      (LM_INFO, "NameService reference obtained via environment: '%s'", envRef));
			// return reference
			return resolveNameService(orb, envRef, retries, secTimeout);
		}

		// corbaloc::<hostname>:<port>/NameService
		const char* hostname = 0;
		hostname = ACSPorts::getIP();
		if (hostname==0)
		{
			return CosNaming::NamingContext::_nil();
		}

		ACE_TCHAR corbalocRef[240];
		ACE_OS::sprintf(corbalocRef, "corbaloc::%s:%s/NameService", hostname, ACSPorts::getNamingServicePort().c_str());

		// return reference
		return resolveNameService(orb, corbalocRef, retries, secTimeout);
	}

	CORBA::Object_ptr MockManager::resolveNameService(CORBA::ORB_ptr orb, const ACE_TCHAR * reference, int retries, unsigned int secTimeout)
	{
		if (!reference || CORBA::is_nil(orb))
			return CosNaming::NamingContext::_nil();
 
		unsigned int secsToWait = 3, secsWaited = 0;
		int retried = 0;
		CosNaming::NamingContext_var ref = CosNaming::NamingContext::_nil();
      
		while (!m_terminate)
		{
			try
			{
				CORBA::Object_var obj = orb->string_to_object(reference);
				ref = CosNaming::NamingContext::_narrow(obj.in());
				return ref._retn(); 
			}
			catch(...)
			{
				ref = CosNaming::NamingContext::_nil();
			}
         
			if ( ((secTimeout != 0) && (secsWaited >= secTimeout)) || ((retries > 0) && (retried >= retries)))
				break;
			else
				ACE_OS::sleep(secsToWait);
			secsWaited += secsToWait;
			retried++;
		}
		return CosNaming::NamingContext::_nil();
	}
	*/
}

#if 0
		CosNaming::NamingContext_var naming_context;
		int  nargc=0;
		char **nargv=0;
		const char *hn=ACSPorts::getIP();
		ACE_CString iorFile;

		CORBA::Object_var obj = orb->string_to_object(reference);

//ORBOPTS="-ORBDottedDecimalAddresses 1" 
// ORBOPTS="-ORBDottedDecimalAddresses 1"
// -ORBEndpoint iiop://$HOST:$NAMING_SERVICE_PORT -o $NAME_IOR $ORBOPTS &

		if (argStr.find ("-ORBEndpoint")==ACE_CString::npos)
		{
			argStr = argStr + "-ORBEndpoint iiop://" + hn + ":" + ACSPorts::getLogPort().c_str();
		}

		ACS_SHORT_LOG((LM_INFO, "New command line is: %s", argStr.c_str()));

		ACE_OS::string_to_argv ((ACE_TCHAR*)argStr.c_str(), nargc, nargv);
 
		ACE_OS::signal(SIGINT,  TerminationSignalHandler);  // Ctrl+C
		ACE_OS::signal(SIGTERM, TerminationSignalHandler);  // termination request

		CORBA::ORB_var orb;

		try
		{
			// Initialize the ORB
			ACE_OS::printf ("Initialising ORB ... \n"); 
			orb = CORBA::ORB_init (nargc, nargv, 0);
			ACE_OS::printf ("ORB initialsed !\n");
		}
		catch( CORBA::Exception &ex )
		{
			ACE_PRINT_EXCEPTION (ex, "Failed to initalise ORB");
			return -1;
		}


		if (!ACSError::init(orb.in()))
		{
			ACS_SHORT_LOG ((LM_ERROR, "Failed to initalise the ACS Error System"));
			return -1;
		}

		// resolve naming service
		try
		{
			ACS_SHORT_LOG((LM_INFO, "Trying to connect to the Naming Service ...."));
			CORBA::Object_var naming_obj = orb->resolve_initial_references ("NameService");
	
			if (!CORBA::is_nil (naming_obj.in ()))
			{
				naming_context = CosNaming::NamingContext::_narrow (naming_obj.in ());
				ACS_SHORT_LOG((LM_INFO, "Connected to the Name Service"));
			}
			else
			{
				ACS_SHORT_LOG((LM_ERROR, "Could not connect the Name Service!"));
				return -1;
			}
		}
		catch( CORBA::Exception &_ex )
		{
			ACS_SHORT_LOG((LM_ERROR, "Could not connect the Name Service!"));
			return -1;
		}

		// adding ACSLog to NamingService
		if (!CORBA::is_nil (naming_context.in ()))
		{
			// register cdb server in Naming service
			CosNaming::Name name (1);
			name.length (1);
			name[0].id = CORBA::string_dup ("ACSLogSvc");
			naming_context->rebind (name, obj.in ());

			ACS_SHORT_LOG((LM_INFO, "ACSLogSvc service registered with Naming Services")); 
		}

		return 0;
#endif
