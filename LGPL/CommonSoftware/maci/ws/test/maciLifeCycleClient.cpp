/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: maciLifeCycleClient.cpp,v 1.4 2005/04/25 16:28:44 acaproni Exp $"
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

static char *rcsId="@(#) $Id: maciLifeCycleClient.cpp,v 1.4 2005/04/25 16:28:44 acaproni Exp $"; 
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
	
	// Get the components that will throw exception
    ACS_SHORT_LOG((LM_INFO,"Getting MACI_EXCEPTION_COSTR"));
    MACI_TEST::DynamicTestClass_var costr = client.get_object<MACI_TEST::DynamicTestClass>("MACI_EXCEPTION_COSTR", 0, true);
    if (CORBA::is_nil(costr.in()))
    {
        // This is correct because the component launched an exception in
        // the constructor          
        ACS_SHORT_LOG((LM_INFO,"Error getting %s (right behaviour)","MACI_EXCEPTION_COSTR"));
    }
    else 
    {
      client.manager()->release_component(client.handle(), "MACI_EXCEPTION_COSTR");
    }
    
    ACS_SHORT_LOG((LM_INFO,"Getting MACI_EXCEPTION_INIT"));
    MACI_TEST::DynamicTestClass_var ini = client.get_object<MACI_TEST::DynamicTestClass>("MACI_EXCEPTION_INIT", 0, true);
    if (CORBA::is_nil(ini.in()))
    {
        // This is correct because the component launched an exception in
        // the initialize          
        ACS_SHORT_LOG((LM_INFO,"Error getting %s (right behaviour)","MACI_EXCEPTION_INIT"));
    }
    else 
    {
      client.manager()->release_component(client.handle(), "MACI_EXCEPTION_INIT");
    }
    
    ACS_SHORT_LOG((LM_INFO,"Getting MACI_EXCEPTION_EXE"));
    MACI_TEST::DynamicTestClass_var exe = client.get_object<MACI_TEST::DynamicTestClass>("MACI_EXCEPTION_EXE", 0, true);
    if (CORBA::is_nil(exe.in()))
    {
        // This is correct because the component launched an exception in
        // the execute          
        ACS_SHORT_LOG((LM_INFO,"Error getting %s (right behaviour)","MACI_EXCEPTION_EXE"));
    }
    else
    {
      client.manager()->release_component(client.handle(), "MACI_EXCEPTION_EXE");
    }
    
    ACS_SHORT_LOG((LM_INFO,"Getting MACI_EXCEPTION_CLEAN"));
	MACI_TEST::DynamicTestClass_var clean = client.get_object<MACI_TEST::DynamicTestClass>("MACI_EXCEPTION_CLEAN", 0, true); 
    if (CORBA::is_nil(clean.in()))
    {
        // This is an error because the exception is launched deactivating the component          
        ACS_SHORT_LOG((LM_ERROR,"Error getting %s (wrong behaviour)","MACI_EXCEPTION_CLEAN"));
    }
    else
    {
        // Release the component (will trigger the component to launch an exception
        ACS_SHORT_LOG((LM_INFO,"Releasing MACI_EXCEPTION_CLEAN"));
        client.manager()->release_component(client.handle(), "MACI_EXCEPTION_CLEAN");
    }
  
    ACS_SHORT_LOG((LM_INFO,"Getting MACI_EXCEPTION_ABORT"));
    MACI_TEST::DynamicTestClass_var abort = client.get_object<MACI_TEST::DynamicTestClass>("MACI_EXCEPTION_ABORT", 0, true);
    if (CORBA::is_nil(abort.in()))
    {
        // This is an error 
        // At the present the aboutToAbort is never called
        ACS_SHORT_LOG((LM_INFO,"Releasing MACI_EXCEPTION_ABORT"));
        ACS_SHORT_LOG((LM_ERROR,"Error getting %s (wrong behaviour)","MACI_EXCEPTION_ABORT"));
    }
    else
    {
        client.manager()->release_component(client.handle(), "MACI_EXCEPTION_ABORT");
    }
	
	// Logout from manager
    client.logout();
}

