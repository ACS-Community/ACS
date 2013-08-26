/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: maciLifeCycleClient.cpp,v 1.6 2006/10/13 10:43:13 bjeram Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* acaproni  2005-02-28  created 
*/

/************************************************************************
*   NAME
*   
* 
*   SYNOPSIS
*   
* 
*   DESCRIPTION
*
*   FILES
*
*   ENVIRONMENT
*
*   COMMANDS
*
*   RETURN VALUES
*
*   CAUTIONS 
*
*   EXAMPLES
*
*   SEE ALSO
*
*   BUGS   
* 
*------------------------------------------------------------------------
*/

#define _POSIX_SOURCE 1
#include "vltPort.h"

static char *rcsId="@(#) $Id: maciLifeCycleClient.cpp,v 1.6 2006/10/13 10:43:13 bjeram Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <maciTestC.h>
#include <maciSimpleClient.h>
#include <logging.h>
#include <string>
#include <iostream>

using namespace maci;



/**
 * The test checks the methods of the LifeCycle.
 * The purpose is to check the robustness of the Container
 * more then the life cycle methods
 * 
 * It is a client that try to load some components that launch an exception
 * in the constructor or in a life clycle method.
 * 
 */
int main (int argc, char **argv)
{
	/// The simple client to connect to the component to test
	SimpleClient client;
	
	if (client.init(argc,argv) == 0)
	{
		return -1;
	} 
    else
	{
		// Log into the manager before doing anything
		client.login();
	}
	
	try
	    {
	// Get the components that will throw exception
	    ACS_SHORT_LOG((LM_INFO,"Getting MACI_EXCEPTION_COSTR"));
	    MACI_TEST::DynamicTestClass_var costr = client.getComponent<MACI_TEST::DynamicTestClass>("MACI_EXCEPTION_COSTR", 0, true);
	    client.releaseComponent( "MACI_EXCEPTION_COSTR");
	    }
	catch(maciErrType::CannotGetComponentExImpl &_ex)
	    {
	    // This is correct because the component launched an exception in
	    // the constructor          
	    ACS_SHORT_LOG((LM_INFO,"Error getting %s (right behaviour)","MACI_EXCEPTION_COSTR"));
	    _ex.log();
	    }
	catch(maciErrType::CannotReleaseComponentExImpl &_ex)
	    {
	    _ex.log();
	    }//try-catch
	
    
	try
	    {
	    ACS_SHORT_LOG((LM_INFO,"Getting MACI_EXCEPTION_INIT"));
	    MACI_TEST::DynamicTestClass_var ini = client.getComponent<MACI_TEST::DynamicTestClass>("MACI_EXCEPTION_INIT", 0, true);
	    client.releaseComponent("MACI_EXCEPTION_INIT");
	    }
	catch(maciErrType::CannotGetComponentExImpl &_ex)
	    {
	    // This is correct because the component launched an exception in
	    // the initialize          
	    ACS_SHORT_LOG((LM_INFO,"Error getting %s (right behaviour)","MACI_EXCEPTION_INIT"));
	    _ex.log();
	    }
	catch(maciErrType::CannotReleaseComponentExImpl &_ex)
	    {
	    _ex.log();
	    }//try-catch

	try
	    {
	    ACS_SHORT_LOG((LM_INFO,"Getting MACI_EXCEPTION_EXE"));
	    MACI_TEST::DynamicTestClass_var exe = client.getComponent<MACI_TEST::DynamicTestClass>("MACI_EXCEPTION_EXE", 0, true);
	    client.releaseComponent("MACI_EXCEPTION_EXE");
	    }
	catch(maciErrType::CannotGetComponentExImpl &_ex)
	    {
	    // This is correct because the component launched an exception in
	    // the execute          
	    ACS_SHORT_LOG((LM_INFO,"Error getting %s (right behaviour)","MACI_EXCEPTION_EXE"));
	    _ex.log();
	    }
	catch(maciErrType::CannotReleaseComponentExImpl &_ex)
	    {
	    _ex.log();
	    }//try-catch
    
	try
	    {
	    ACS_SHORT_LOG((LM_INFO,"Getting MACI_EXCEPTION_CLEAN"));
	    MACI_TEST::DynamicTestClass_var clean = client.getComponent<MACI_TEST::DynamicTestClass>("MACI_EXCEPTION_CLEAN", 0, true); 
	    // Release the component (will trigger the component to launch an exception
	    ACS_SHORT_LOG((LM_INFO,"Releasing MACI_EXCEPTION_CLEAN"));
	    client.releaseComponent("MACI_EXCEPTION_CLEAN");
	    }
	catch(maciErrType::CannotGetComponentExImpl &_ex)
	    {
	    // This is an error because the exception is launched deactivating the component          
	    ACS_SHORT_LOG((LM_ERROR,"Error getting %s (wrong behaviour)","MACI_EXCEPTION_CLEAN"));
	    _ex.log();
	    }
	catch(maciErrType::CannotReleaseComponentExImpl &_ex)
	    {
	    _ex.log();
	    }//try-catch
	
	try
	    {
	    ACS_SHORT_LOG((LM_INFO,"Getting MACI_EXCEPTION_ABORT"));
	    MACI_TEST::DynamicTestClass_var abort = client.getComponent<MACI_TEST::DynamicTestClass>("MACI_EXCEPTION_ABORT", 0, true);
	    client.releaseComponent("MACI_EXCEPTION_ABORT");
	    }
	catch(maciErrType::CannotGetComponentExImpl &_ex)
	    {
	    _ex.log();
	    }
	catch(maciErrType::CannotReleaseComponentExImpl &_ex)
	    {
	    // This is an error 
	    // At the present the aboutToAbort is never called
	    ACS_SHORT_LOG((LM_INFO,"Releasing MACI_EXCEPTION_ABORT"));
	    ACS_SHORT_LOG((LM_ERROR,"Error getting %s (wrong behaviour)","MACI_EXCEPTION_ABORT"));
	    _ex.log();
	    }//try-catch
		
	// Logout from manager
	client.logout();
}

