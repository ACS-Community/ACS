/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: maciContainerServicesClient.cpp,v 1.5 2005/04/25 07:29:24 acaproni Exp $"
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

static char *rcsId="@(#) $Id: maciContainerServicesClient.cpp,v 1.5 2005/04/25 07:29:24 acaproni Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <maciTestC.h>
#include <maciSimpleClient.h>
#include <logging.h>
#include <string>
#include <iostream>

using namespace maci;

/**
 * The test checks the methods of the ContainerServices.
 * 
 * It is a client that connects to the component (the name is in the command line) 
 * and executes some of its methods. The component uses its ContainerSerrvices
 * object to activate and deactivate other components (MACI_DYN_TESTn)
 * 
 * The test exercises the getComponent, getDynamicComponent,
 * getDefaultComponent, releaseComponent and releaseAllComponent methods.
 */
int main (int argc, char **argv)
{
	if (argc!=2) 
	{
		ACS_SHORT_LOG((LM_ERROR,"Wrong command line"));
		return -1;
	}
	
	std::string componentName=argv[1];
	
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
	
	// Get the component
	MACI_TEST::ContainerServicesTestClass_var comp = client.get_object<MACI_TEST::ContainerServicesTestClass>(componentName.c_str(), 0, true);
	if (CORBA::is_nil(comp.in()))
	    {
	    	ACS_SHORT_LOG((LM_ERROR,"Error getting %s",componentName.c_str()));
	    	// Logout from manager
		    client.logout();
		    // Teriminate
		    return -1;
	    }
	 
	// Ask the remote component to get a component (it will execute
	// the getComponent and the releaseComponent of the ContainerServices)   
    ACS_SHORT_LOG((LM_INFO,"Testing getComponent..."));
	comp->getComponentTest();
	
	// Ask the remote component to get the default component for the given
	// IDL interface (it will execute the getDefaultComponent and the 
	// releaseComponent of the ContainerServices)
    ACS_SHORT_LOG((LM_INFO,"Testing getDefaultComponent..."));
	comp->defaultComponentTest();
	
	// Ask the remote component to get a dynamic component for the given
	// IDL interface (it will execute the getDynamicComponent and the 
	// releaseComponent of the ContainerServices)
    ACS_SHORT_LOG((LM_INFO,"Testing getDynamicComponent..."));
	comp->dynamicComponentTest();
	
	// The remote component uses the ContainerServices to test the 
	// releaseAllComponents method (it basically gets two components and
	// release both of them with that method)
    ACS_SHORT_LOG((LM_INFO,"Testing realeaseAllComponents..."));
	comp->releaseResourcesTest();
    
    // The remote component uses the ContainerServices to test the 
    // getComponentDescriptor method
    ACS_SHORT_LOG((LM_INFO,"Testing getComponentDescriptor..."));
    comp->componentDescriptorTest();
    
    ACS_SHORT_LOG((LM_INFO,"All tests done."));
	
	// Release the component
	client.manager()->release_component(client.handle(), componentName.c_str());
	
	// Logout from manager
    client.logout();
}

