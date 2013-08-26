/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciReleaseComponent.cpp,v 1.6 2008/10/01 02:40:28 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran  2005-10-19  created
*/


/** @file maciReleaseComponent.cpp
 *  maciReleaseComponent is used to (forcefully, regardeless of clients) release component.
 *  @htmlonly
 *  <br><br>
 *  @endhtmlonly

 *  @param container This is simply the name of the component you want to release.
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly

 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly
 */   



#include <vltPort.h>
#include <acsutil.h>

#include <logging.h>
#include <maciHelper.h>
#include <maciContainerImpl.h>
#include <acsutilTimeStamp.h>

 using namespace maci;


//Class AdministratorImpl
class  AdministratorImpl :
    public virtual POA_maci::Administrator
{
  public:
    
    ACE_CString m_name;
    
    //Constructor
    AdministratorImpl (ACE_CString name) : m_name(name) {}
    
    //Destructor
    virtual ~AdministratorImpl (void) {}

    virtual char * name ()
    {
	return CORBA::string_dup(m_name.c_str());
    }

    virtual void disconnect ()
    {
	// noop
    }

    virtual maci::AuthenticationData * authenticate (
	maci::ExecutionId execution_id,
	const char * question
	)
    {
	ACE_UNUSED_ARG(question);

	maci::AuthenticationData_var data = new AuthenticationData();
	data->answer = CORBA::string_dup("");
	data->client_type = maci::CLIENT_TYPE;
	data->impl_lang = maci::CPP;
	data->recover = false;
 	data->timestamp = ::getTimeStamp();
 	data->execution_id = execution_id;

 	return data._retn();
    }

    virtual void message (
	CORBA::Short type,
	const char * message
	)
    {
	// noop
    }


    virtual void taggedmessage (
	CORBA::Short type,
	CORBA::Short tag,
	const char * message
	)
    {
	// noop
    }


    virtual CORBA::Boolean ping ()
    {
	return true;
    }

    virtual void components_available (
	const maci::ComponentInfoSeq & cobs
    
	    ) {}

    virtual void components_unavailable (
	const maci::stringSeq & cob_names
    
	    ) {}

    virtual void client_logged_in (
        const ::maci::ClientInfo & info,
        ::ACS::Time timestamp,
        ::maci::ExecutionId execution_id) {}

    virtual void client_logged_out (
        ::maci::Handle h,
        ::ACS::Time timestamp) {}

    virtual void container_logged_in (
        const ::maci::ContainerInfo & info,
        ::ACS::Time timestamp,
        ::maci::ExecutionId execution_id) {}

    virtual void container_logged_out (
        ::maci::Handle h,
        ::ACS::Time timestamp) {}

    virtual void components_requested (
        const ::maci::HandleSeq & clients,
        const ::maci::HandleSeq & components,
        ::ACS::Time timestamp) {}

    virtual void components_released (
        const ::maci::HandleSeq & clients,
        const ::maci::HandleSeq & components,
        ::ACS::Time timestamp) {}

    virtual void component_activated (
        const ::maci::ComponentInfo & info,
        ::ACS::Time timestamp,
        ::maci::ExecutionId execution_id) {}

    virtual void component_deactivated (
        ::maci::Handle h,
        ::ACS::Time timestamp) {}


};

int
main (int argc, char *argv[])
{

    if (argc < 2)
    {
	ACE_OS::printf("\n\tusage: %s <component name> [<ORB options>]\n\n", argv[0]);
	return -1;
    }

    LoggingProxy * logger = new LoggingProxy(0, 0, 31);
    if (logger)
    {
	LoggingProxy::init(logger);
	LoggingProxy::ProcessName(argv[0]);
	LoggingProxy::ThreadName("main");
    }
    else
	ACS_SHORT_LOG((LM_INFO, "Failed to initialize logging."));

    
    try
    {
	// Initialize the ORB.
	CORBA::ORB_var orb = CORBA::ORB_init (argc,
					      argv,
					      "TAO"
					      );
	
	// Get the Root POA.
	CORBA::Object_var objRootPOA =
	    orb->resolve_initial_references("RootPOA");
	
	PortableServer::POA_var m_poaRoot = PortableServer::POA::_narrow(objRootPOA.in());
      
	// Get the manager of the root POA to apply to the child POAs.
	PortableServer::POAManager_var poaManager =
	    m_poaRoot->the_POAManager();
	
	// POA Manager can start processing incoming requests.
	poaManager->activate();
	

	maci::Manager_var mgr = MACIHelper::resolveManager(orb.ptr(), argc, argv, 0, 0);
        if (mgr.ptr() == maci::Manager::_nil())
	{
	    ACS_SHORT_LOG((LM_ERROR, "Failed to resolve Manager reference."));
	    return -1;
	}


	AdministratorImpl * admin = new AdministratorImpl("MACI Release Component client");
        maci::Client_var c = admin->_this();
	
	if (CORBA::is_nil(c.in())) 
	    {
	    ACS_SHORT_LOG((LM_DEBUG, "Failed to get client"));
	    return -1;
	    }
	
        maci::ClientInfo_var clientInfo = mgr->login(c.in());
      
	if (clientInfo.ptr() == 0)
	  return -1;

	ACS_SHORT_LOG((LM_DEBUG, "Logged in."));

        CORBA::Long owners = mgr->force_release_component(clientInfo->h, argv[1]);
	
	ACS_SHORT_LOG((LM_INFO, "Component released (component has %d clients left).", owners));

	mgr->logout(clientInfo->h);
      
	ACS_SHORT_LOG((LM_DEBUG, "Logged out."));

	ACS_SHORT_LOG((LM_INFO, "Done."));
      
    }
  catch( CORBA::Exception &ex )
  {
      ACS_SHORT_LOG((LM_INFO, "Failed."));
      ACE_PRINT_EXCEPTION (ex,
                           ACE_TEXT ("Caught unexpected exception:"));
      
      return -1;
  }
  
  return 0;
}





