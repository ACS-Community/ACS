/*
 * "@(#) $Id: acstestcompTestClient.cpp,v 1.3 2008/10/01 05:33:43 cparedes Exp $"
 *
 * $Log: acstestcompTestClient.cpp,v $
 * Revision 1.3  2008/10/01 05:33:43  cparedes
 * Removing exception declaration from cpp implementation
 *
 * Revision 1.2  2006/10/11 15:03:43  gchiozzi
 * Improved error handling.
 *
 * Revision 1.1  2006/09/14 14:54:34  gchiozzi
 * First checkin of new module with CPP test components.
 *
 *
 */

#include <acsutil.h>
#include <maciSimpleClient.h>
#include <baciC.h>
#include <baciS.h>
#include <logging.h>

#include <acstestcompC.h>

using namespace maci;


char* cobname;

/*
 * This is the callback class to retrieve the 
 * periodic replies for the async methods.
 */
class ClientCBvoid: public virtual POA_ACS::CBvoid
{   
  public:
    ClientCBvoid() {}
    ~ClientCBvoid() {}
    void working (const ACSErr::Completion &c, const ACS::CBDescOut &desc) 
	{ 
	    ACS_SHORT_LOG ((LM_INFO, "CBvoid::working")); 
	}
    void done (const ACSErr::Completion &c, const ACS::CBDescOut &desc) 
	{ 
	    ACS_SHORT_LOG ((LM_INFO, "CBvoid::done"));
	    ACS_SHORT_LOG ((LM_INFO, "Error code returned: %d", c.code));
	}
    CORBA::Boolean negotiate (ACS::TimeInterval time_to_transmit, 
			      const ACS::CBDescOut &desc) 
	{
	    return true;
	}
};

int main(int argc, char* argv[])
{
    // Check command line arguments
    if (argc < 2)
	{
	cobname = "TimingExplorer";
	}
    else
	{
	cobname = argv[1];
	}

    // Creates and initializes the SimpleClient object
    SimpleClient client;
    
    if(client.init(argc,argv) == 0)
	{
	ACS_SHORT_LOG((LM_ERROR,"Cannot init client"));
	return -1;
	}

    ACS_SHORT_LOG((LM_INFO, "Welcome to acstestcompTestClient!"));
    ACS_SHORT_LOG((LM_INFO, "Login in maciManager..."));
    client.login();

    try
	{
	// List all components the Manager knows of our type.
	ACS_SHORT_LOG((LM_INFO, "Listing all componentes of type *TimingExplorer*"));
	maci::HandleSeq seq;
	maci::ComponentInfoSeq_var components = 
	    client.manager()->get_component_info(client.handle(), seq, "*", 
						 "*TimingExplorer*", false);
	
	for (CORBA::ULong i = 0; i < components->length(); i++)
	    {
	    ACS_SHORT_LOG((LM_INFO,"%s (%s)", components[i].name.in(), 
			   components[i].type.in()));
	    }
	
	// Now get the specific component we have requested on the command line.
	ACS_SHORT_LOG((LM_INFO, "Getting component %s...", cobname));
	acstestcomp::TimingExplorer_var component = 
	    client.get_object<acstestcomp::TimingExplorer>(cobname, 0, true);
	
	if (!CORBA::is_nil(component.in()))
	    {
	    ACS_SHORT_LOG((LM_INFO, "... got component %s", cobname));

	    /*
	     * Calls the methods of the component
	     */
	    ACS_SHORT_LOG((LM_INFO, "Call waitToReply..."));
	    component->waitToReply(1);
	    
	    // Create the CBvoid callback
	    ClientCBvoid myCallback;
	    // Activate it as a CORBA object
	    ACS::CBvoid_var cb = myCallback._this(); 
	      
	    // Invoke the asynchronous method and give it time to complete
	    // in the callbacks
	    ACS_SHORT_LOG((LM_INFO, "Call multipleReplies..."));
	    ACS::CBDescIn desc;
	    component->multipleReplies(10, 1, cb.in(), desc);
	    component->multipleReplies(5, 2, cb.in(), desc);

	    //Enter main loop and stays there for a fixed amount of time (1s)
	    //This is done to give the asynchronous method a chance to finish.
	    ACE_Time_Value tv(14);
	    client.run(tv);
	    
	    } /* end if component reference OK */
	else
	    {
	    ACS_SHORT_LOG((LM_INFO, "Component %s is nil !!!", cobname));
	    }
	} /* end main try */
    catch (...)
	{
	ACS_SHORT_LOG((LM_ERROR, "Error in TestClient::main!"));
	}
    
    /* Another try section where we release our component
     * and logout from the Manager.
     */
    try
	{
	ACS_SHORT_LOG((LM_INFO, "Releasing..."));
	client.manager()->release_component(client.handle(), cobname);
	client.logout();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_ERROR, "Error in TestClient::main!"));
	}
    
    /*
     * sleep for 3 sec to allow everytihng to cleanup and stabilize
     * so that the tests can be determinitstic.
     */
    ACE_OS::sleep(3);
    return 0;
}

/* __oOo__ */
