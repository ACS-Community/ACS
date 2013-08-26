/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: maciTestSimpleClient.cpp,v 1.6 2008/08/26 03:18:59 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
*/


#define _POSIX_SOURCE 1
#include "vltPort.h"

static char *rcsId="@(#) $Id: maciTestSimpleClient.cpp,v 1.6 2008/08/26 03:18:59 cparedes Exp $"; 
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
	    // first we test old way of getting and releasing a component
		ACS_SHORT_LOG((LM_INFO,"Getting MACI01 (deprecated way)"));
	    MACI_TEST::MaciTestClass_var costr = client.get_object<MACI_TEST::MaciTestClass>("MACI01", 0, true);
	    ACS_SHORT_LOG((LM_INFO,"Releasing MACI01 (calling release_componen on manger)"));
	    client.manager()->release_component(client.handle(), "MACI01");
	    
	    // .. and here new way
	    ACS_SHORT_LOG((LM_INFO,"Getting MACI04"));
	    costr = client.getComponent<MACI_TEST::MaciTestClass>("MACI04", 0, true);
	  

	    // test of getComponentNonSticky (it has to be activated be4)
	    ACS_SHORT_LOG((LM_INFO,"Getting MACI04"));
	    costr = client.getComponentNonSticky<MACI_TEST::MaciTestClass>("MACI04");

	    ACS_SHORT_LOG((LM_INFO,"Releasing MACI04"));
	    client.releaseComponent("MACI04");
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

    // test dynamic component
	ACS_SHORT_LOG((LM_INFO,"Testing dynamic component"));
    try{

	 // Prepare the ComponentSpec struct
	 ComponentSpec cSpec;
	 cSpec.component_name="*";		
	 cSpec.component_code="*";	
	 cSpec.container_name="*";
	 cSpec.component_type="IDL:alma/MACI_TEST/DynamicTestClass:1.0";
	 
	 // Get the default component for the given IDL interface
	 MACI_TEST::DynamicTestClass_var comp = 
	 	client.getDynamicComponent<MACI_TEST::DynamicTestClass>(cSpec,false);
	 if (CORBA::is_nil(comp.in())) {
	 	ACS_SHORT_LOG((LM_ERROR,"getDynamicComponent: Error getting type %s",cSpec.component_type.in()));
	 }
	 // Execute a method on the remote component
	 comp->whoami();
	 // Release the component
	 client.releaseComponent(comp->name());
	
     //Testing getDynamicComponentSmartPtr 
	 ComponentSmartPtr<MACI_TEST::DynamicTestClass> compSmrt = 
	 	client.getDynamicComponentSmartPtr<MACI_TEST::DynamicTestClass>(cSpec,false);
	// if (CORBA::is_nil(compSmrt)) {
	// 	ACS_SHORT_LOG((LM_ERROR,"getDynamicComponent: Error getting type %s",cSpec.component_type.in()));
	 //}
	 // Execute a method on the remote component
	 compSmrt->whoami();
	 // Release the component
	 client.releaseComponent(compSmrt->name());
    
	 // here we test some error handling
	 try
	 {
		 // we try to narrow to the wrong type (MaciTestClass)!
	 MACI_TEST::MaciTestClass_var comp = 
	 	 	client.getDynamicComponent<MACI_TEST::MaciTestClass>(cSpec,false);
	 }
	 catch(ACSErr::ACSbaseExImpl &ex)
	 {
		 ex.log();
	 }//try-catch

    }catch(...){
        ACS_SHORT_LOG((LM_ERROR,"Testing dynamic component Exception"));
    }
	// test error situation
	ACS_SHORT_LOG((LM_INFO,"Testing error handling"));
	try
	    {
	    ACS_SHORT_LOG((LM_INFO,"Getting MACI07"));
	    MACI_TEST::MaciTestClass_var co =
		client.getComponentNonSticky<MACI_TEST::MaciTestClass>("MACI04");
	    }
	catch(maciErrType::CannotGetComponentExImpl &_ex)
	    {
	    ACS_SHORT_LOG((LM_INFO, "Right behaviour: Got exception!"));
	    _ex.log();
	    }//try-catch
	
	try
	{
		ACS_SHORT_LOG((LM_INFO,"Testing error handling: getComponent (Narrow problem)"));
		// here we try to narrow to wrong type (DynamicTestClass) !
		MACI_TEST::DynamicTestClass_var co = client.getComponent<MACI_TEST::DynamicTestClass>("MACI04", 0, true);
	}catch(ACSErr::ACSbaseExImpl &ex)
	{
		ex.log();
	}//try-catch

	try
	{
		ACS_SHORT_LOG((LM_INFO,"Testing error handling: getComponentNonSticky (Narrow problem)"));
		// 1st we have to activate the component!
		MACI_TEST::MaciTestClass_var costr = client.getComponent<MACI_TEST::MaciTestClass>("MACI04", 0, true);
		// here we try to narrow to wrong type (DynamicTestClass) !
		MACI_TEST::DynamicTestClass_var co = client.getComponentNonSticky<MACI_TEST::DynamicTestClass>("MACI04");
	}catch(ACSErr::ACSbaseExImpl &ex)
	{
		ex.log();
	}//try-catch
	client.releaseComponent("MACI04");
	client.logout();
}//main

