/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: maciTestSimpleClient.cpp,v 1.1 2006/10/12 15:33:11 bjeram Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
*/


#define _POSIX_SOURCE 1
#include "vltPort.h"

static char *rcsId="@(#) $Id: maciTestSimpleClient.cpp,v 1.1 2006/10/12 15:33:11 bjeram Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <maciTestC.h>
#include <maciSimpleClient.h>
#include <logging.h>
#include <string>
#include <iostream>

using namespace maci;



/**
 * The test checks the methods of the SimpleClient
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
	    // first we test old way
	    ACS_SHORT_LOG((LM_INFO,"Getting MACI01 (deprecated way)"));
	    MACI_TEST::MaciTestClass_var costr = client.get_object<MACI_TEST::MaciTestClass>("MACI01", 0, true);
	    client.manager()->release_component(client.handle(), "MACI01");
	    
	    // .. and here new way
	    ACS_SHORT_LOG((LM_INFO,"Getting MACI04"));
	    costr = client.getComponent<MACI_TEST::MaciTestClass>("MACI04", 0, true);
	    client.manager()->release_component(client.handle(), "MACI04");
	    }
	catch(maciErrType::CannotGetComponentExImpl &_ex)
	    {
	    // Here we catch eventual exception
	    _ex.log();
	    }
	catch( CORBA::SystemException &_ex )
	    {
	    ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__,
								"main");
	    corbaProblemEx.setMinor(_ex.minor());
	    corbaProblemEx.setCompletionStatus(_ex.completed());
	    corbaProblemEx.setInfo(_ex._info().c_str());
	    corbaProblemEx.log();
	}//try-catch

	client.logout();
}//main

