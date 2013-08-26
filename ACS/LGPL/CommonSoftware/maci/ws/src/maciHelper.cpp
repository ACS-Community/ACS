/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: maciHelper.cpp,v 1.93 2007/09/07 13:38:13 hsommer Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran 2003-05-23 removed exponential backoff
* msekoran 2002-02-09 ACE_DECLARE_NEW_CORBA_ENV moved out of while block
* gchiozzi 2002-01-17 Replaced wrong ACE_TRY_CHECK with ACE_TRY_CHECK_EX
* msekoran 2001-08-29 created 
*/

#include <vltPort.h>
#include <maciHelper.h>
#include <maciContainerImpl.h>

#include <ace/SString.h>

#include <logging.h>

#include <acsutilPorts.h>

#define MANAGER_REFERENCE  "MANAGER_REFERENCE"
#define NAMESERVICE_REFERENCE  "NAMESERVICE_REFERENCE"

 using namespace maci;
 using namespace cdb;


bool MACIHelper::m_terminate = false;

void 
MACIHelper::terminateResolving(bool terminate)
{
  m_terminate = terminate;
}

ACE_CString
MACIHelper::extractHostnameFromCorbaloc(const ACE_TCHAR *corbaloc)
{

  ACE_CString result = corbaloc;

  // corbaloc reference
  int pos = result.find("corbaloc:");
  if (pos!=ACE_CString::npos)
    {
      // second ':'
      pos = result.find(':', 9);
      if (pos!=ACE_CString::npos)
	{
	  int pos2 = result.find(':', pos+1);
	  // if no port specified
	  if (pos2==ACE_CString::npos)
	    pos2 = result.find('/', pos+1);
	  
	  // is not invalid address
	  if (pos2!=ACE_CString::npos)
	    {
	      result = result.substr(pos+1, pos2-pos-1);
	      return result;
	    }
	}
    }

  return "";
}

ACE_CString
MACIHelper::getManagerHostname(int argc, ACE_TCHAR **argv)
{
  ACS_TRACE("maci::MACIHelper::getManagerHostname");

  // Command line option -m or -managerReference
  for (int pos = 1; pos < argc-1; pos++)
    if (ACE_OS::strcmp(argv[pos], "-m")==0 ||
	ACE_OS::strcmp(argv[pos], "-managerReference")==0)
      {
	// increase pos to point to the Manager's reference
	pos++;
	
	ACE_CString result = extractHostnameFromCorbaloc(argv[pos]);
	if (result.length()>0)
	  {
	    ACS_LOG(0, "maci::MACIHelper::getManagerHostname",
		    (LM_INFO, "Manager hostname obtained via command line: '%s'", result.c_str()));
	    return result;
	  }

      }

  // Environment variable MANAGER_REFERENCE
  ACE_TCHAR * envRef = ACE_OS::getenv (MANAGER_REFERENCE);
  if (envRef && *envRef)
    {
      ACE_CString result = extractHostnameFromCorbaloc(envRef);
      if (result.length()>0)
	{
	  ACS_LOG(0, "maci::MACIHelper::getManagerHostname",
		  (LM_INFO, "Manager hostname obtained via environment: '%s'", result.c_str()));
	  return result;
	}
    }


  // corbaloc::<hostname>:<port>/Manager
  const char* hostname = 0;
  hostname = ACSPorts::getIP();
  if (hostname==0)
    {
      ACS_LOG(LM_RUNTIME_CONTEXT, "maci::MACIHelper::getManagerHostname",
	      (LM_ERROR, "Failed to obtain localhost address!"));

      return "localhost";
    }

  ACS_LOG(0, "maci::MACIHelper::getManagerHostname",
	  (LM_INFO, "Manager hostname generated using localhost address: '%s'", hostname));

  return const_cast<char *>(hostname);
}


maci::Manager_ptr
MACIHelper::resolveManager(CORBA::ORB_ptr orb,
			   int argc, ACE_TCHAR **argv,
			   int retries, unsigned int secTimeout)
{
  ACS_TRACE("maci::MACIHelper::resolveManager");

  if (CORBA::is_nil(orb))
    return maci::Manager::_nil();

  // Command line option -m or -managerReference
  for (int pos = 1; pos < argc-1; pos++)
    if (ACE_OS::strcmp(argv[pos], "-m")==0 ||
	ACE_OS::strcmp(argv[pos], "-managerReference")==0)
      {
	// increase pos to point to the Manager's reference
	pos++;
	
	ACS_LOG(0, "maci::MACIHelper::resolveManager",
		(LM_INFO, "ManagerReference obtained via command line: '%s'", argv[pos]));
	
	// return reference
	return resolveManager(orb, argv[pos], retries, secTimeout);
      }

  // Environment variable MANAGER_REFERENCE
  ACE_TCHAR * envRef = ACE_OS::getenv (MANAGER_REFERENCE);
  if (envRef && *envRef)
    {
      ACS_LOG(0, "maci::MACIHelper::resolveManager",
	      (LM_INFO, "ManagerReference obtained via environment: '%s'", envRef));
      
      // return reference
      return resolveManager(orb, envRef, retries, secTimeout);
    }


  // corbaloc::<hostname>:<port>/Manager
  const char* hostname = 0;
  hostname = ACSPorts::getIP();
  if (hostname==0)
    {
      ACS_LOG(LM_RUNTIME_CONTEXT, "maci::MACIHelper::resolveManager",
	      (LM_ERROR, "Failed to obtain localhost address!"));

      return maci::Manager::_nil();
    }

  ACE_TCHAR corbalocRef[230];
  ACE_OS::sprintf(corbalocRef, "corbaloc::%s:%s/Manager", hostname, ACSPorts::getManagerPort().c_str());
 
  ACS_LOG(0, "maci::MACIHelper::resolveManager",
	  (LM_INFO, "ManagerReference generated using localhost address: '%s'", corbalocRef));

  // return reference
  return resolveManager(orb, corbalocRef, retries, secTimeout);
 
}


maci::Manager_ptr
MACIHelper::resolveManager(CORBA::ORB_ptr orb, 
	       const ACE_TCHAR * reference, 
	       int retries, unsigned int secTimeout)
{
  ACS_TRACE("maci::MACIHelper::resolveManager");
  
  if (!reference || CORBA::is_nil(orb))
    return maci::Manager::_nil();
 
  ACS_DEBUG_PARAM("maci::MACIHelper::resolveManager", "Resolving reference: '%s'", reference);

  unsigned int secsToWait = 3, secsWaited = 0;
  int retried = 0;
  maci::Manager_var ref = maci::Manager::_nil();

  
 
  while (!m_terminate)
    {

      try
	{
	  
	  CORBA::Object_var obj = orb->string_to_object(reference);
	  
	      
	  ref = maci::Manager::_narrow(obj.in());
	  

	  ACS_DEBUG("maci::MACIHelper::resolveManager", "Manager reference narrowed.");

	  return ref._retn(); 
	}
      catch( CORBA::Exception &ex )
	{
	  ACE_PRINT_EXCEPTION(ex, "maci::MACIHelper::resolveManager");
	  ref = maci::Manager::_nil();
	}
      catch(...)
	{
	  ref = maci::Manager::_nil();
	}
         
      if (
	  ((secTimeout != 0) && (secsWaited >= secTimeout)) || 
	  ((retries > 0) && (retried >= retries))
	  )
	break;
      else
	ACE_OS::sleep(secsToWait);
      
      secsWaited += secsToWait;

      retried++;
      
    }

  return maci::Manager::_nil();
}

CosNaming::NamingContext_ptr
MACIHelper::resolveNameService(CORBA::ORB_ptr orb,
			       int retries, unsigned int secTimeout)
{
  ACS_TRACE("maci::MACIHelper::resolveNameService");

  if (CORBA::is_nil(orb))
    return CosNaming::NamingContext::_nil();

  // use CORBA::ORB::resolve_intial_references
  
  try
    {
      CORBA::Object_var naming_obj =
        orb->resolve_initial_references ("NameService");
      

      if (!CORBA::is_nil (naming_obj.in ()))
	{
	  CosNaming::NamingContext_var naming_context =
	    CosNaming::NamingContext::_narrow (naming_obj.in ()
					       );
	  

	  if (naming_context.ptr() != CosNaming::NamingContext::_nil())
	    return naming_context._retn();
	}
    }
  catch( CORBA::Exception &ex )
    {
      //ACE_PRINT_EXCEPTION (ex, "(maci::MACIHelper::resolveNameService) CORBA exception caught!");
    }

  ACS_LOG(LM_RUNTIME_CONTEXT, "maci::MACIHelper::resolveNameService",
	  (LM_DEBUG,"Unable to resolve NameService reference using CORBA::ORB::resolve_intial_references()."));

  // Environment variable NAMESERVICE_REFERENCE
  ACE_TCHAR * envRef = ACE_OS::getenv (NAMESERVICE_REFERENCE);
  if (envRef && *envRef)
    {
      ACS_LOG(0, "maci::MACIHelper::resolveNameService",
	      (LM_INFO, "NameService reference obtained via environment: '%s'", envRef));
      
      // return reference
      return resolveNameService(orb, envRef, retries, secTimeout);
    }


  // corbaloc::<hostname>:<port>/NameService
  const char* hostname = 0;
  hostname = ACSPorts::getIP();
  if (hostname==0)
    {
      ACS_LOG(LM_RUNTIME_CONTEXT, "maci::MACIHelper::resolveNameService",
	      (LM_ERROR, "Failed to obtain localhost address!"));

      return CosNaming::NamingContext::_nil();
    }

  ACE_TCHAR corbalocRef[240];
  ACE_OS::sprintf(corbalocRef, "corbaloc::%s:%s/NameService", hostname, ACSPorts::getNamingServicePort().c_str());
 
  ACS_LOG(0, "maci::MACIHelper::resolveNameService",
	  (LM_INFO, "NameService reference generated using localhost address: '%s'", corbalocRef));

  // return reference
  return resolveNameService(orb, corbalocRef, retries, secTimeout);
 
}

CosNaming::NamingContext_ptr
MACIHelper::resolveNameService(CORBA::ORB_ptr orb, 
			       const ACE_TCHAR * reference, 
			       int retries, unsigned int secTimeout)
{
  ACS_TRACE("maci::MACIHelper::resolveNameService");
  
  if (!reference || CORBA::is_nil(orb))
    return CosNaming::NamingContext::_nil();
 
  ACS_DEBUG_PARAM("maci::MACIHelper::resolveNameService", "Resolving reference: '%s'", reference);

  unsigned int secsToWait = 3, secsWaited = 0;
  int retried = 0;
  CosNaming::NamingContext_var ref = CosNaming::NamingContext::_nil();
      
  
  while (!m_terminate)
    {

      try
	{
	  
	  CORBA::Object_var obj = orb->string_to_object(reference);
	  
	      
	  ref = CosNaming::NamingContext::_narrow(obj.in());
	  

	  ACS_DEBUG("maci::MACIHelper::resolveNameService", "NameService reference narrowed.");

	  return ref._retn(); 
	}
      catch( CORBA::Exception &ex )
	{
	  //ACE_PRINT_EXCEPTION(ACE_ANY_EXCEPTION, "maci::MACIHelper::resolveNameService");
	  ACS_LOG(0, "maci::MACIHelper::resolveNameService",
		  (LM_DEBUG, "Failed to resolve naming service using reference: '%s'.", reference));
	  ref = CosNaming::NamingContext::_nil();
	}
      catch(...)
	{
	  ref = CosNaming::NamingContext::_nil();
	}
         
      if (
	  ((secTimeout != 0) && (secsWaited >= secTimeout)) || 
	  ((retries > 0) && (retried >= retries))
	  )
	break;
      else
	ACE_OS::sleep(secsToWait);
      
      secsWaited += secsToWait;

      retried++;
      
    }

  return CosNaming::NamingContext::_nil();
}



CORBA::Repository_ptr
MACIHelper::resolveInterfaceRepository(CORBA::ORB_ptr orb, maci::Manager_ptr manager,
				       int argc, ACE_TCHAR **argv,
				       int retries, unsigned int secTimeout)
{
  ACS_TRACE("maci::MACIHelper::resolveInterfaceRepository");

  if (CORBA::is_nil(orb))
    return CORBA::Repository::_nil();

  // use CORBA::ORB::resolve_intial_references
  
  try
    {
      CORBA::Object_var ifr_obj =
        orb->resolve_initial_references ("InterfaceRepository");
      

      if (!CORBA::is_nil (ifr_obj.in ()))
	{
	  CORBA::Repository_var ifr =
	    CORBA::Repository::_narrow (ifr_obj.in ()
					       );
	  

	  if (ifr.ptr() != CORBA::Repository::_nil())
	    return ifr._retn();
	}
    }
  catch( CORBA::Exception &ex )
    {
      ACE_PRINT_EXCEPTION (ex, "(maci::MACIHelper::resolveInterfaceRepository) CORBA exception caught!");
    }

  ACS_LOG(LM_RUNTIME_CONTEXT, "maci::MACIHelper::resolveInterfaceRepository",
	  (LM_DEBUG,"Unable to resolve InterfaceRepository reference using CORBA::ORB::resolve_intial_references()."));

  // Manager get_service("InterfaceRepository")
  if (manager != maci::Manager::_nil())
  {
    try
      {
	CORBA::Object_var ifr_obj =
	    orb->resolve_initial_references ("InterfaceRepository");

	if (!CORBA::is_nil (ifr_obj.in ()))
	  {
	    CORBA::Repository_var ifr =
		CORBA::Repository::_narrow (ifr_obj.in ()
					   );
	    if (ifr.ptr() != CORBA::Repository::_nil())
		return ifr._retn();
	  }
      }
    catch( CORBA::Exception &ex )
      {
	ACE_PRINT_EXCEPTION (ex, "(maci::MACIHelper::resolveInterfaceRepository) CORBA exception caught!");
      }
  }

  ACS_LOG(LM_RUNTIME_CONTEXT, "maci::MACIHelper::resolveInterfaceRepository",
	  (LM_DEBUG,"Unable to resolve InterfaceRepository reference using maci::Manager::get_service(\"InterfaceRepository\") method."));


  ACE_CString managerHostname = MACIHelper::getManagerHostname(argc, argv);
  if (managerHostname.length()>0)
  {
      ACE_TCHAR corbalocRef[240];
      ACE_OS::sprintf(corbalocRef, "corbaloc::%s:%s/InterfaceRepository", managerHostname.c_str(), ACSPorts::getIRPort().c_str());     
      
      ACS_LOG(0, "maci::MACIHelper::resolveInterfaceRepository",
	      (LM_INFO, "InterfaceRepository reference generated using Manager's host address: '%s'", corbalocRef));
      
      // return reference
      CORBA::Repository_var ifr = resolveInterfaceRepository(orb, corbalocRef, retries, secTimeout);
      if (ifr.ptr() != CORBA::Repository::_nil())
	  return ifr._retn();
  }

  ACS_LOG(LM_RUNTIME_CONTEXT, "maci::MACIHelper::resolveInterfaceRepository",
	  (LM_DEBUG,"Unable to resolve InterfaceRepository reference using Manager's host address."));


  // corbaloc::<hostname>:<port>/InterfaceRepository
  const char* hostname = 0;
  hostname = ACSPorts::getIP();
  if (ACE_OS::strcmp(managerHostname.c_str(), hostname)==0)
    {
      return CORBA::Repository::_nil();
    }
  else if (hostname==0)
    {
      ACS_LOG(LM_RUNTIME_CONTEXT, "maci::MACIHelper::resolveInterfaceRepository",
	      (LM_ERROR, "Failed to obtain localhost address!"));

      return CORBA::Repository::_nil();
    }

  ACE_TCHAR corbalocRef[240];
  ACE_OS::sprintf(corbalocRef, "corbaloc::%s:%s/InterfaceRepository", hostname, ACSPorts::getIRPort().c_str());
 
  ACS_LOG(0, "maci::MACIHelper::resolveInterfaceRepository",
	  (LM_INFO, "InterfaceRepository reference generated using localhost address: '%s'", corbalocRef));
  
  // return reference
  return resolveInterfaceRepository(orb, corbalocRef, retries, secTimeout);
 
}

CORBA::Repository_ptr
MACIHelper::resolveInterfaceRepository(CORBA::ORB_ptr orb, 
			       const ACE_TCHAR * reference, 
			       int retries, unsigned int secTimeout)
{
  ACS_TRACE("maci::MACIHelper::resolveInterfaceRepository");
  
  if (!reference || CORBA::is_nil(orb))
    return CORBA::Repository::_nil();
 
  ACS_DEBUG_PARAM("maci::MACIHelper::resolveInterfaceRepository", "Resolving reference: '%s'", reference);

  unsigned int secsToWait = 3, secsWaited = 0;
  int retried = 0;
  CORBA::Repository_var ref = CORBA::Repository::_nil();
      
  
  while (!m_terminate)
    {

      try
	{
	  
	  CORBA::Object_var obj = orb->string_to_object(reference);
	  
	      
	  ref = CORBA::Repository::_narrow(obj.in());
	  

	  ACS_DEBUG("maci::MACIHelper::resolveInterfaceRepository", "InterfaceRepository reference narrowed.");

	  return ref._retn(); 
	}
      catch( CORBA::Exception &ex )
	{
	  ACE_PRINT_EXCEPTION(ex, "maci::MACIHelper::resolveInterfaceRepository");
	  ref = CORBA::Repository::_nil();
	}
      catch(...)
	{
	  ref = CORBA::Repository::_nil();
	}
         
      if (
	  ((secTimeout != 0) && (secsWaited >= secTimeout)) || 
	  ((retries > 0) && (retried >= retries))
	  )
	break;
      else
	ACE_OS::sleep(secsToWait);
      
      secsWaited += secsToWait;

      retried++;
      
    }

  return CORBA::Repository::_nil();
}






