/*
 * "@(#) $Id: acstestcompTestComponentActivationErrors.cpp,v 1.1 2006/10/11 15:04:40 gchiozzi Exp $"
 *
 * $Log: acstestcompTestComponentActivationErrors.cpp,v $
 * Revision 1.1  2006/10/11 15:04:40  gchiozzi
 * Archived first version.
 * Still get_object returns null on error.
 * The test will have to be changed once we have exceptions instead.
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
#include <ACSErrTypeCommon.h>

using namespace maci;


char* cobname;

/**
 * We pass on the command line the name of a component.
 * The program tries to get the reference to it and logs the exceptions
 * in case of failures.
 *
 * Calling components that produce diffrent kind of failures we can
 * test what exceptions are thrown and what the stack trace contains.
 */
int main(int argc, char* argv[])
{
    // Check command line arguments
    // If no component name, uses a default.
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

    ACS_SHORT_LOG((LM_INFO, "Welcome to acstestcompTestComponentActivationErrors!"));
    ACS_SHORT_LOG((LM_INFO, "Login in maciManager..."));
    client.login();

    try
	{
	// Now get the specific component we have requested on the command line.
	ACS_SHORT_LOG((LM_INFO, "Getting component %s...", cobname));
	acstestcomp::TimingExplorer_var component = 
	    client.get_object<acstestcomp::TimingExplorer>(cobname, 0, true);

	/* 
	 * This should never happen.
	 * With ACS 6.0, a failure in getting the object
	 * shall throw an exception and not a NULL
	 */
	if (CORBA::is_nil(component.in()))
	    {
	    ACSErrTypeCommon::GenericErrorExImpl ex(__FILE__, __LINE__,
						    "main");
	    ex.setErrorDesc("Component is NULL. This should never happen!!!");
	    throw ex;
	    } /* end if component reference OK */

	} /* end main try */
    /*
     * As soon as they are available, we should catch here
     * any ACS exception thrown by the get_object().
     */
    catch(CORBA::SystemException &ex)
	{
	ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(
				   __FILE__, __LINE__,
				   "main");
	corbaProblemEx.setMinor(ex.minor());
	corbaProblemEx.setCompletionStatus(ex.completed());
	corbaProblemEx.setInfo(ex._info().c_str());
	corbaProblemEx.log();
	}
    catch(ACSErrTypeCommon::GenericErrorExImpl &ex)
	{
	ACSErrTypeCommon::UnexpectedExceptionExImpl ex2(ex, __FILE__, __LINE__,
						"main");
	ex2.log();
	}
    catch(...)
	{
	ACSErrTypeCommon::UnexpectedExceptionExImpl ex(__FILE__, __LINE__,
						"main");
	ex.log();
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
	ACSErrTypeCommon::GenericErrorExImpl ex(__FILE__, __LINE__,
						"main");
	ex.setErrorDesc("Client has catched an UNEXPECTED exception");
	ex.log();
	}
    
    /*
     * sleep for 3 sec to allow everytihng to cleanup and stabilize
     * so that the tests can be determinitstic.
     */
    ACE_OS::sleep(3);
    return 0;
}

/* __oOo__ */
