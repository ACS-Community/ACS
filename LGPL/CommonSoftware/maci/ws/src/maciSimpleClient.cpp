/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciSimpleClient.cpp,v 1.115 2011/04/26 16:53:11 javarias Exp $"
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

#include <acsutilTimeStamp.h>
#include <loggingLog4cpp.h>

namespace maci {
using namespace baci;

// Init the static logger
LoggingProxy* SimpleClient::m_logger=0;
ACE_CString SimpleClient::m_processName("");

SimpleClient::SimpleClient ():
    m_handle(0),
    m_initialized(false),
    m_loggedin(false),
    m_executionId(0),
    m_startTime(::getTimeStamp()),
	 m_ContServ(0)
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
  if (m_loggedin)
  	logout();
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

  delete m_ContServ;
  delete m_logger;
  m_logger =0;
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

	if(CORBA::is_nil(m_orb.ptr()))
	    return false;

	//
	// Initialize POAs.
	//

	// Get the Root POA.
	CORBA::Object_var objRootPOA =
	    m_orb->resolve_initial_references("RootPOA");


	m_poaRoot = PortableServer::POA::_narrow(objRootPOA.in());


	if(CORBA::is_nil(m_poaRoot.ptr()))
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


	if (CORBA::is_nil(m_poaPersistent.ptr()))
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
      m_loggedin=true;


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
      m_loggedin=false;

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
    if (CORBA::is_nil(m_manager.ptr()))
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
	  CORBA::Object_var log_obj = m_manager->get_service(m_handle, centralizedLogger.c_str(), true);


	  if (log_obj.ptr() != CORBA::Object::_nil())
	      {
	      Logging::AcsLogService_var logger = Logging::AcsLogService::_narrow(log_obj.in());


	      if (logger.ptr() != Logging::AcsLogService::_nil())
		  {
		  m_logger->setCentralizedLogger(logger.in());
		  char * disable_log4cpp = getenv("ACS_DISABLE_LOG4CPP");
		  if (disable_log4cpp == NULL)
		     LOGGER_FACTORY->enableRemoteAppender(100, 3, logger.ptr());
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
SimpleClient::getDynamicComponent(maci::ComponentSpec compSpec, bool markAsDefault)
{
    return getDynamicComponent<CORBA::Object>(compSpec, markAsDefault);
}

CORBA::Object_ptr
SimpleClient::getComponent(const char *name,
			   const char *domain,
			   bool activate)
{
    return getComponent<CORBA::Object>(name, domain, activate);
}//getComponent

CORBA::Object_ptr
SimpleClient::getComponentNonSticky(const char *name)
{
    return getComponentNonSticky<CORBA::Object>(name);
}//getComponentNonSticky

long  SimpleClient::releaseComponent(const char* name)
{
    ACS_SHORT_LOG((LM_DEBUG, "Releasing component: '%s'.",  name));
    try
	{
	return manager()->release_component(m_handle, name);
	}
    catch (maciErrType::NoPermissionEx &_ex)
	{
	maciErrType::CannotReleaseComponentExImpl ex(_ex, __FILE__, __LINE__,
						     "maci::SimpleCleint::releaseComponent");
	ex.setCURL(name);
	throw ex;
	}
    catch( CORBA::SystemException &_ex )
	{
	ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__,
							    "maci::SimpleCleint::releaseComponent");
	corbaProblemEx.setMinor(_ex.minor());
	corbaProblemEx.setCompletionStatus(_ex.completed());
	corbaProblemEx.setInfo(_ex._info().c_str());
	maciErrType::CannotReleaseComponentExImpl ex(corbaProblemEx, __FILE__, __LINE__,
						 "maci::SimpleCleint::releaseComponent");
	ex.setCURL(name);
	throw ex;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
							"maci::SimpleCleint::releaseComponent");
	maciErrType::CannotReleaseComponentExImpl ex(uex, __FILE__, __LINE__,
						 "maci::SimpleCleint::releaseComponent");
	ex.setCURL(name);
	throw ex;
	}//try-catch					}
}//releaseComponent

CORBA::Object* SimpleClient::getCORBADefaultComponent(const char* idlType)
{
	ComponentInfo_var cInfo;
	ACS_TRACE("maci::SimpleClient::getCORBADefaultComponent");
	try{
		cInfo  = m_manager->get_default_component(m_handle, idlType);
		CORBA::Object_var obj = cInfo->reference;
		if (CORBA::is_nil(obj.in())){
			ACSErrTypeCORBA::CORBAReferenceNilExImpl ex(__FILE__, __LINE__,
					"maci::SimpleClient::getCORBADefaultComponent");
			ex.setVariable("cInfo->reference");
			throw ex;
		}
		return CORBA::Object::_narrow(obj.in());
	}
	catch (maciErrType::NoPermissionEx &ex){
		throw maciErrType::NoPermissionExImpl (ex, __FILE__, __LINE__,
				"maci::SimpleClient::getCORBADefaultComponent");
	}
	catch (maciErrType::NoDefaultComponentEx &ex){
		throw maciErrType::NoDefaultComponentExImpl (ex, __FILE__, __LINE__,
				"maci::SimpleClient::getCORBADefaultComponent");
	}
	catch (maciErrType::CannotGetComponentEx &ex){
		maciErrType::CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
				"maci::SimpleClient::getCORBADefaultComponent");
		lex.setCURL("IDL type:"+ACE_CString(idlType));
		throw lex;
	}
	catch(ACSErr::ACSbaseExImpl &ex){
		maciErrType::CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
				"maci::SimpleClient::getCORBADefaultComponent");
		lex.setCURL("IDL type:"+ACE_CString(idlType));
		throw lex;
	}
	catch( CORBA::SystemException &ex ){
		ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__,
				"maci::SimpleClient::getCORBADefaultComponent");
		corbaProblemEx.setMinor(ex.minor());
		corbaProblemEx.setCompletionStatus(ex.completed());
		corbaProblemEx.setInfo(ex._info().c_str());

		maciErrType::CannotGetComponentExImpl lex(corbaProblemEx, __FILE__, __LINE__,
				"maci::SimpleClient::getCORBADefaultComponent");
		lex.setCURL("IDL type:"+ACE_CString(idlType));
		throw lex;
	}
	catch(...){
		ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
				"maci::SimpleClient::getCORBADefaultComponent");
		maciErrType::CannotGetComponentExImpl lex(uex, __FILE__, __LINE__,
				"maci::SimpleClient::getCORBADefaultComponent");
		lex.setCURL("IDL type:"+ACE_CString(idlType));
		throw lex;
	}
}

CORBA::Object* SimpleClient::getCORBACollocatedComponent(
		maci::ComponentSpec compSpec, bool markAsDefault, 
		const char* targetComponent)
{
	ComponentInfo_var cInfo;
	ACS_TRACE("maci::SimpleClient::getCORBACollocatedComponent");
	try{
		cInfo = m_manager->get_collocated_component(m_handle, compSpec,
				markAsDefault, targetComponent);
		CORBA::Object_var obj = cInfo->reference;
		if (CORBA::is_nil(obj.in())) {
			ACSErrTypeCORBA::CORBAReferenceNilExImpl ex(__FILE__, __LINE__,
					"maci::SimpleClient::getCORBACollocatedComponent");
			ex.setVariable("cInfo->reference");
			throw ex;
		}
		return CORBA::Object::_narrow(obj.in());
	}
	catch (maciErrType::NoPermissionEx &_ex){
		maciErrType::NoPermissionExImpl ex(__FILE__, __LINE__,
				"maci::SimpleClient::getCORBACollocatedComponent");
		throw ex;
	}
	catch (maciErrType::IncompleteComponentSpecEx &ex){
		maciErrType::IncompleteComponentSpecExImpl lex(ex, __FILE__, __LINE__,
				"maci::SimpleClient::getCORBACollocatedComponent");
		lex.setCURL(compSpec.component_name.in());
		throw lex;
	}
	catch (maciErrType::InvalidComponentSpecEx &ex){
		maciErrType::InvalidComponentSpecExImpl lex(ex, __FILE__, __LINE__,
				"maci::SimpleClient::getCORBACollocatedComponent");
		throw lex;
	}
	catch (maciErrType::ComponentSpecIncompatibleWithActiveComponentEx &ex){
		maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl lex(ex,
			  	__FILE__, __LINE__,
				"maci::SimpleClient::getCORBACollocatedComponent");
		lex.setCURL(compSpec.component_name.in());
		throw lex;
	}
	catch (maciErrType::CannotGetComponentEx &ex){
		maciErrType::CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
				"maci::SimpleClient::getCORBACollocatedComponent");
		lex.setCURL(compSpec.component_name.in());
		throw lex;
	}
	catch(ACSErr::ACSbaseExImpl &ex){
		maciErrType::CannotGetComponentExImpl lex(ex, __FILE__, __LINE__,
				"maci::SimpleClient::getCORBACollocatedComponent");
		lex.setCURL(compSpec.component_name.in());
		throw lex;
	}
	catch( CORBA::SystemException &ex ){
		ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__,
				"maci::SimpleClient::getCORBACollocatedComponent");
		corbaProblemEx.setMinor(ex.minor());
		corbaProblemEx.setCompletionStatus(ex.completed());
		corbaProblemEx.setInfo(ex._info().c_str());

		maciErrType::CannotGetComponentExImpl lex(corbaProblemEx,
				__FILE__, __LINE__,
				"maci::SimpleClient::getCORBACollocatedComponent");
		lex.setCURL(compSpec.component_name.in());
		throw lex;
	}
	catch (...){
		ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
				"maci::SimpleClient::getCORBACollocatedComponent");
		maciErrType::CannotGetComponentExImpl lex(uex, __FILE__, __LINE__,
				"maci::SimpleClient::getCORBACollocatedComponent");
		lex.setCURL(compSpec.component_name.in());
		throw lex;
	}
}

ACE_CString_Vector SimpleClient::findComponents(const char *nameWildcard,
		const char *typeWildcard)
{
	ACE_CString_Vector names;

	if(nameWildcard == NULL)
	{
		nameWildcard = "*";
	}

	if(typeWildcard == NULL)
	{
		typeWildcard = "*";
	}

	maci::HandleSeq seq;
	maci::ComponentInfoSeq_var devs = m_manager->get_component_info(
			m_handle,seq,nameWildcard,typeWildcard,false);

	CORBA::ULong len = devs->length ();

	for (CORBA::ULong i=0; i < len; i++){
		names.push_back(devs[i].name.in());
	}
	return names;
}

maci::ComponentInfo SimpleClient::getComponentDescriptor(
		const char* componentName)
{
	maci::HandleSeq seq;
	ComponentInfoSeq_var compInfoSeq =
		m_manager->get_component_info(m_handle,seq,componentName,"*",false);

	if (compInfoSeq!=NULL && compInfoSeq->length()==1){
		return (*compInfoSeq)[0];
	}
	else{
		acsErrTypeContainerServices::GettingCompInfoExImpl
			ex(__FILE__,__LINE__,"maci::SimpleClient::getComponentDescriptor");
		ex.setCURL(componentName);
		throw ex;
	}
}

ContainerServices* SimpleClient::getContainerServices()
{
	if (m_ContServ == NULL){
		ACE_CString clientName(name());
		m_ContServ =  new MACIContainerServices(m_handle, 
				clientName, m_poaRoot.in(), m_manager.in());
	}
	return m_ContServ;
}


/* ----------------------------------------------------------------*/
/* ------------------ [ CORBA Client interface ] ------------------*/
/* ----------------------------------------------------------------*/

char *
SimpleClient::name ()
{
  return CORBA::string_dup("Simple MACI Client");
}

void
SimpleClient::disconnect ()
{
  // override this implementation in case of special handling
  ACS_SHORT_LOG((LM_DEBUG, "Manager requested that I should log off or I will be disconneced from the Manager."));
}

::maci::AuthenticationData *
SimpleClient::authenticate (
        maci::ExecutionId execution_id,
        const char * question
        )
{
  ACE_UNUSED_ARG(question);

  maci::AuthenticationData_var data = new AuthenticationData();
  data->answer = CORBA::string_dup("");
  data->client_type = maci::CLIENT_TYPE;
  data->impl_lang = maci::CPP;
  data->recover = true;
  data->timestamp = m_startTime;

  if (m_executionId == 0)
    m_executionId = execution_id;
  data->execution_id = m_executionId;

  return data._retn();
}

void
SimpleClient::message (CORBA::Short type,
		       const char * message
		       )
{
  ACS_SHORT_LOG((LM_DEBUG, "Got message from the Manager: '%s'.", message));
}

void
SimpleClient::taggedmessage (CORBA::Short type,
		       CORBA::Short tag,
		       const char * message
		       )
{
  ACS_SHORT_LOG((LM_DEBUG, "Got tagged message from the Manager: (%d) '%s'.", tag, message));
}

void
SimpleClient::components_available (const maci::ComponentInfoSeq & cobs
			      )
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
{
    return true;
}

 };


/*___oOo___*/








