/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciSimpleClient.cpp,v 1.91 2006/01/09 21:15:17 dfugate Exp $"
*
* who       when        what
* --------  --------    ----------------------------------------------
* msekoran  2002/09/06  logging proxy initialization moved to init() method
* bjeram    2002/04/10  added ACSError::done and LoggingProxy::done in destroy
* bjeram    2002/04/10  added that simeple client at its initalization makes connection to the centralized logger
* msekoran  2001/12/24  cleaned
* bjeram    2001-11-20  Improved initialization and initialysed checks.
* msekoran  2001/06/22  redesigned to new maci
* msekoran  2001/03/14  created 
*/


/** @file maciSimpleClient.cpp
 *  SimpleClient is used to manipulate distributed objects.  There are many different
 *  parameters which can be passed to SimpleClient, but none are required.
 *  @htmlonly
 *  <br><br>
 *  @endhtmlonly

 *  @param "-ORBEndpoint iiop://yyy:xxxx" Use this optional parameter to specify which host/port SimpleClient
 *  should run on.
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly

 *  @param "-ORBInitRef NameService=corbaloc::yyy:xxxx/NameService" Use this optional parameter to specify which 
 *  host/port SimpleClient should get a reference to the naming service from.
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly

 *  @param "-ORBInitRef NotifyEventChannelFactory=corbaloc::yyy:xxxx/NotifyEventChannelFactory" Use this optional 
 *  parameter to specify which host/port SimpleClient should get a reference to the notification service from.
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly
 */  

#include <vltPort.h>

#include <maciSimpleClient.h>

#include <ace/ARGV.h>
#include <ace/Arg_Shifter.h>
#include <ace/INET_Addr.h>

#include <maciHelper.h>
#include <acserr.h>
#include <acsQoS.h>
#include <orbsvcs/orbsvcs/DsLogAdminC.h>

#include <acsutilORBHelper.h>

#include <baciCORBA.h>
#include <baciThread.h>
#include "maciContainerImpl.h"


NAMESPACE_BEGIN(maci);
using namespace baci;

// Init the static logger
LoggingProxy* SimpleClient::m_logger=0;
ACE_CString SimpleClient::m_processName("");

SimpleClient::SimpleClient (): 
    m_handle(0),
    m_initialized(false)
{
  if (m_logger==0) {
	m_logger = new LoggingProxy(0,0,31);
  }

  m_manager = maci::Manager::_nil();
  m_orb = CORBA::ORB::_nil();
  m_poaRoot = m_poaPersistent = PortableServer::POA::_nil();

  BACIThread::setInitializers(SimpleClient::initThread, SimpleClient::doneThread);
}

SimpleClient::~SimpleClient ()
{
  destroy();
  
}

int
SimpleClient::destroy ()
{
  int result = 0;

  //if (m_initialized)
  {
    /**
     * Gets done with CORBA
     */
    ACS_SHORT_LOG ((LM_DEBUG, "Destroying CORBA..."));
	
    result = doneCORBA();
  }

  acsQoS::done();

  ACSError::done();

  ACS_SHORT_LOG ((LM_DEBUG, "Client destroyed."));

  LoggingProxy::done();

  delete m_logger;
  return result;
}


int
SimpleClient::doneCORBA()
{
  
  try
    {
      
      if(m_poaRoot.ptr() != PortableServer::POA::_nil())
	{
	  // this also destroys other POAs
	  m_poaRoot->destroy(1, 1);
	  
	  m_poaRoot = PortableServer::POA::_nil();
	}
      
      if(m_orb.ptr() != CORBA::ORB::_nil())
	{
	  m_orb->destroy();
	  
	  m_orb = CORBA::ORB::_nil();
	}

      return 1;
      
    }
  catch( CORBA::Exception &ex )
    {
      ACE_PRINT_EXCEPTION(ex, "Unexpected exception occure while destroying CORBA.");
    }
  catch(...) {
    /*		ACS_LOG(LM_RUNTIME_CONTEXT, "maci::SimpleClient::doneCORBA",
		(LM_ERROR, "Unexpected exception occure while destroying CORBA"));
    */
  }

  return 0;
}

int
SimpleClient::initCORBA(int argc, char * argv[]) 
{
    if(m_poaRoot.ptr() != PortableServer::POA::_nil() &&
       m_orb.ptr() != CORBA::ORB::_nil())
    {
	return 0;
    }

    
    try
	{
      
	// Initialize the ORB.
	m_orb = CORBA::ORB_init(argc, argv, "TAO");
	
	//set the global ORB in simpleclient
	ORBHelper::setORB(m_orb);
      
	if(m_orb.ptr() == CORBA::ORB::_nil())
	    return false;
      
	//
	// Initialize POAs.
	//
      
	// Get the Root POA.
	CORBA::Object_var objRootPOA =
	    m_orb->resolve_initial_references("RootPOA");
	
      
	m_poaRoot = PortableServer::POA::_narrow(objRootPOA.in());
	
      
	if(m_poaRoot.ptr() == PortableServer::POA::_nil())
	    return false;
      
	// Get the manager of the root POA to apply to the child POAs.
	PortableServer::POAManager_var poaManager =
	    m_poaRoot->the_POAManager();
	
      
	//
	// Prepare policies our POAs will be using.
	//
	PortableServer::IdAssignmentPolicy_var user_id_policy =
	    m_poaRoot->create_id_assignment_policy(PortableServer::USER_ID);
	
      
	PortableServer::LifespanPolicy_var persistent_policy =
	    m_poaRoot->create_lifespan_policy(PortableServer::PERSISTENT);
	
      
	PortableServer::ServantRetentionPolicy_var servant_retention_policy  =
	    m_poaRoot->create_servant_retention_policy (PortableServer::RETAIN);
	
      
     
	CORBA::PolicyList policies;
	m_poaTransient = m_poaRoot->create_POA("TransientPOA", 
					       poaManager.in(),
					       policies);



	policies.length(3);
      
	policies[0] = PortableServer::LifespanPolicy::_duplicate(persistent_policy.in());
	policies[1] = PortableServer::IdAssignmentPolicy::_duplicate(user_id_policy.in());
	policies[2] = PortableServer::ServantRetentionPolicy::_duplicate(servant_retention_policy.in());
      
	//
	// Persistent POA
	//
      
	m_poaPersistent = m_poaRoot->create_POA("PersistentPOA", 
						poaManager.in(),
						policies);
	
      
	if (m_poaPersistent.ptr() == PortableServer::POA::_nil())
	    return 0;
      
      
	// We're done using the policies.
	user_id_policy->destroy();
	persistent_policy->destroy();
	servant_retention_policy->destroy();
      
	// Initialize BACI if needed
	if(BACI_CORBA::getInstance()==0)
	    {
	    BACI_CORBA::createInstance(m_orb.ptr(), poaManager.ptr(),
				       m_poaRoot.ptr(), m_poaPersistent.ptr(), m_poaTransient.ptr());
	    }

	// POA Manager can start processing incoming requests.
	poaManager->activate();
	

	return 1;

	}
    catch( CORBA::Exception &ex )
	{
	ACE_PRINT_EXCEPTION(ex,
			    "maci::SimpleClient::initCORBA");
	}
    catch(...)
	{
	/*		ACS_LOG(LM_RUNTIME_CONTEXT, "maci::SimpleClient::initCORBA",
			(LM_ERROR, "Unexpected exception occure while destroying CORBA"));
	*/
	}
  
    return 0;
}

CORBA::ORB_ptr
SimpleClient::getORB()
{
  return m_orb.ptr();
}


int
SimpleClient::login() 
{

  if (!m_initialized)
    return 0;

  
  
  try
    {
      /**
       * Login
       */
      maci::Client_var c = _this();
      

      if (CORBA::is_nil(c.in())) 
	{
	  ACS_SHORT_LOG((LM_DEBUG, "Failed to get client"));
	  return 0;
	}
      
      maci::ClientInfo_var clientInfo = m_manager->login(c.in());
      

      if (clientInfo.ptr() == 0)
	{
	  return 0;
	}

      m_handle = clientInfo->h;

      ACS_SHORT_LOG((LM_DEBUG, "Logged in."));

      return 1;
    }
  catch( CORBA::Exception &ex )
    {
      ACE_PRINT_EXCEPTION(ex, "A CORBA Exception occurred.");
    }
  
  return 0;
}
  

int
SimpleClient::logout() 
{
  if (!m_initialized || !m_handle)
    return 0;
  
  
  
  try
    {
      /**
       * Logout from Manager
       */
      ACS_SHORT_LOG ((LM_DEBUG, "Logging out..."));

      m_manager->logout(m_handle);
      

      return 1;
    }    
  catch( CORBA::Exception &ex )
    {
      ACE_PRINT_EXCEPTION(ex, "A CORBA Exception occurred.");
    }
  
  return 0;
}


int
SimpleClient::init(int argc, char *argv[])
{
    
  // initialize ACE logger instance
  m_processName=argv[0];
  ACE_TCHAR hostname[33];
  ACE_OS::hostname (hostname, sizeof(hostname));
  ACE_Log_Msg::instance()->local_host(hostname);

  // initialize the rest of LoggingProxy
  if (argv>0)
      LoggingProxy::ProcessName(argv[0]);
  LoggingProxy::ThreadName("main");

  LoggingProxy::init (m_logger); 
  ACS_SHORT_LOG((LM_INFO, "Logging proxy successfully created."));

  /**
   * Start the TRY block
   */
  //
  
  //ACE_TRY
    {
      /**
       * Copies the command line arguments in an ACE_ARGV
       * object to manipilate them
       */
      
      ACE_ARGV editArgv(1);
      
      for(int i=0; i<argc; i++)
	editArgv.add(argv[i]);
      
      /**
       * The "-ORBDottedDecimalAddresses 1" argument 
       * must always be there, so we just add it
       */
      editArgv.add("-ORBDottedDecimalAddresses");
      editArgv.add("1");
      
      /**
       * Now get back again a standard (argc,argv)
       */
      argc = editArgv.argc();
      argv = editArgv.argv();
      
      /**
       * Initialyze CORBA with the new command line
       */
      if(!initCORBA(argc, argv))
	{
	  ACS_SHORT_LOG((LM_DEBUG, "Failed to init CORBA."));
	  return 0;
	}

      /**
       * Initalize ACS error system
       */
      if (!ACSError::init (getORB()))
	  {
	   ACS_SHORT_LOG((LM_DEBUG, "Failed to init ACS error system."));
	   return 0;
	  }

      /**
       * Initalize ACS QoS
       */
      if (!acsQoS::init (getORB()))
	  {
	   ACS_SHORT_LOG((LM_DEBUG, "Failed to init ACS QoS."));
	   return 0;
	  }


      m_manager = MACIHelper::resolveManager(getORB(), argc, argv, 0, 0);
      if (m_manager.ptr() == maci::Manager::_nil())
	{
	  ACS_SHORT_LOG((LM_DEBUG, "Failed to resolve Manager reference!"));
	  return 0;
 	}

      /**
       * Make connection to the Centralized Logger
       */
      

      try
	  {
	  ACE_CString centralizedLogger("Log");	
	  CORBA::ULong status;
	  CORBA::Object_var log_obj = m_manager->get_service(m_handle, centralizedLogger.c_str(), true, status);
	  

	  if (log_obj.ptr() != CORBA::Object::_nil())
	      {
	      DsLogAdmin::Log_var logger = DsLogAdmin::Log::_narrow(log_obj.in());
	      
	  
	      if (logger.ptr() != DsLogAdmin::Log::_nil())
		  {
		  m_logger->setCentralizedLogger(logger.in());
		  }
	      else
		  {
		  ACS_SHORT_LOG((LM_DEBUG, "Simple client are not going to use Centralized Logger (narrow problem)."));
		  }
	      }
	  else
	      {
	      ACS_SHORT_LOG((LM_DEBUG, "Simple client are not going to use Centralized Logger (nil problem)."));
	      }//if-else
	  }
      catch( CORBA::Exception &ex )
	  {
	  ACS_SHORT_LOG((LM_DEBUG, "Simple client are not going to use Centralized Logger (exception occured)."));
	  ACE_PRINT_EXCEPTION(ex, "maciSimpleClient::init");
	  } 

      m_initialized = true;
      return 1;
      
    }
    /*
  catch( CORBA::Exception &_ex )
    {
      ACE_PRINT_EXCEPTION(_ex, "A CORBA Exception occurred.");
    }
  */

  return 0;
}

int
SimpleClient::run(ACE_Time_Value &tv
		  )
{
  getORB()->run(tv);
  return 1;
}

int
SimpleClient::run()
{
  getORB()->run();
  return 1;
}

maci::Manager_ptr
SimpleClient::manager() 
{ 
  if(!m_initialized)
    {
      ACS_SHORT_LOG((LM_DEBUG, "Client not initialized."));
      return maci::Manager::_nil();
    }
  
  return m_manager.ptr(); 
}

maci::Handle
SimpleClient::handle() 
{
  if(!m_initialized)
    {
      ACS_SHORT_LOG((LM_DEBUG, "Client not initialized."));
      return 0;
    }
    
  return m_handle; 
}

CORBA::Object_ptr
SimpleClient::get_object(const char *name,
			 const char *domain, 
			 bool activate
			 )
{
  /**
   * Check first if the client is initialized
   */
  if(!m_initialized)
    {
      ACS_SHORT_LOG((LM_DEBUG, "Client not initialized."));
      return CORBA::Object::_nil();
    }

  /**
   * Check if <name> is null
   */
  if(!name)
    {
      ACS_SHORT_LOG((LM_DEBUG, "Name parameter is null."));
      return CORBA::Object::_nil();
    }
  
  /**
   * Get reference of device object
   */
  
  /**
   * First creates the CURL, if not already a CURL,
   * and query the Manager for the component
   */
  char *curl_str = "curl://";

  ACE_CString curl = "";
  if(strncmp(name, curl_str, strlen(curl_str)) != 0 )
      {
      curl += curl_str;
      if (domain)
	  curl += domain;
  
      curl += ACE_CString("/");
      }
  curl += name;

  ACS_SHORT_LOG((LM_DEBUG, "Getting device: '%s'. Creating it...",  curl.c_str()));
  
  try
    {
      CORBA::ULong status;
      CORBA::Object_var obj = manager()->get_service(m_handle, curl.c_str(), activate, status);
      

      if (CORBA::is_nil(obj.in()) || status!=maci::Manager::COMPONENT_ACTIVATED)
	{
	  ACS_SHORT_LOG((LM_DEBUG, "Failed to create '%s', status: %d.",  curl.c_str(), status));
	  return 0;
	}
  
      return obj._retn();

    }
  catch( CORBA::Exception &ex )
    {
      ACE_PRINT_EXCEPTION(ex,
			  "maci::SimpleClient::get_object");
      return CORBA::Object::_nil();
    }
  catch(...)
    {
      /*		ACS_LOG(LM_RUNTIME_CONTEXT, "maci::SimpleClient::initCORBA",
			(LM_ERROR, "Unexpected exception occure while destroying CORBA"));
      */
      return CORBA::Object::_nil();
    }

  return CORBA::Object::_nil();
}


/* ----------------------------------------------------------------*/
/* ------------------ [ CORBA Client interface ] ------------------*/
/* ----------------------------------------------------------------*/

char * 
SimpleClient::name ()
  throw (CORBA::SystemException)
{ 
  return CORBA::string_dup("Simple MACI Client"); 
}

void
SimpleClient::disconnect ()
  throw (CORBA::SystemException)
{ 
  // override this implementation in case of special handling
  ACS_SHORT_LOG((LM_DEBUG, "Manager requested that I should log off or I will be disconneced from the Manager."));
}

char * 
SimpleClient::authenticate (const char * question
			    )
  throw (CORBA::SystemException)
{ 
  // first character indicated "C" => Client
  return CORBA::string_dup("CSimple MACI Client"); 
}

void
SimpleClient::message (CORBA::Short type,
		       const char * message
		       )
  throw (CORBA::SystemException)
{
  ACS_SHORT_LOG((LM_DEBUG, "Got message from the Manager: '%s'.", message));
}

void
SimpleClient::components_available (const maci::ComponentInfoSeq & cobs
			      )
  throw (CORBA::SystemException)
{

  // this is the default implementation
  CORBA::ULong len = cobs.length (); 
  
  for (unsigned int i=0; i < len; i++) {
    ACS_SHORT_LOG((LM_DEBUG, "Available component: '%s'.", cobs[i].name.in()));
  }
}

void
SimpleClient::components_unavailable (const maci::stringSeq & cob_names
				)
  throw (CORBA::SystemException)
{

  // this is the default implementation
  CORBA::ULong len = cob_names.length (); 
  
  for (unsigned int i=0; i < len; i++) {
    ACS_SHORT_LOG((LM_DEBUG, "Unavailable component: '%s'.", cob_names[i].in()));
  }
}

void
SimpleClient::initThread(const char * threadName)
{
  ACS_TRACE("maci::SimpleClient::initThread");

  if (m_logger)
    LoggingProxy::init(m_logger);
  LoggingProxy::ProcessName(m_processName.c_str());
  LoggingProxy::ThreadName(threadName);

  if (threadName && ACE_OS::strlen(threadName))
    {
      ACS_LOG(LM_RUNTIME_CONTEXT, "maci::SimpleClient::initThread", (LM_INFO, "Thread name: '%s'", threadName));
    }
}

void
SimpleClient::doneThread()
{
  ACS_TRACE("maci::SimpleClient::doneThread");
  // LoggingProxy::done();
}

CORBA::Boolean
SimpleClient::ping ()
      throw (CORBA::SystemException)
{
    return true;
}

NAMESPACE_END(maci);


/*___oOo___*/








