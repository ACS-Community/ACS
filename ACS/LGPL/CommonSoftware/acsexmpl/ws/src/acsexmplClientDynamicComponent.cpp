/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration)
*    and Cosylab 2002, All rights reserved
*
*    This library is free software; you can redistribute it and/or
*    modify it under the terms of the GNU Lesser General Public
*    License as published by the Free Software Foundation; either
*    version 2.1 of the License, or (at your option) any later version.
*
*    This library is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*    Lesser General Public License for more details.
*
*    You should have received a copy of the GNU Lesser General Public
*    License along with this library; if not, write to the Free Software
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
*
* "@(#) $Id: acsexmplClientDynamicComponent.cpp,v 1.9 2007/02/01 05:14:26 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2004/01/25  created 
*/

/** @file acsexmplClientDynamicComponent.cpp
 *  @htmlonly
 *  <br><br>
    @endhtmlonly
 *  @param "container name" Use this required parameter to specify which container
 *  should activate the dynamic component.
 *  @htmlonly
    <br><hr>
    @endhtmlonly
 *  @param "-ORBEndpoint iiop://yyy:xxxx" Use this optional parameter to specify which host/port SimpleClient
 *  should run on.
 *  @htmlonly
    <br><hr>
    @endhtmlonly
 *  @param "-m corbaloc::yyy:xxxx/Manager" Use this optional parameter to specify where
 *  manager is.
 *  @htmlonly
    <br><hr>
    @endhtmlonly
 */

/** @addtogroup ACSEXMPLTOC
*/
/*@{
*/

/** @addtogroup ACSEXMPLTOCCLIENTS
*/
/*@{
*/

/** @defgroup ACSEXMPLCLIENTDYNAMICCOMPONENTDOC Client Dynamic Component
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
This trivial client uses SimpleClient's manager reference to retrieve a dynamic component.  In this case,
it's a <a href="group__ACSEXMPLHWDOC.html">Hello World</a> component.
<br>
<br>
<h2>What can I gain from this example?</h2>
<ul>
  <li>SimpleClient usage.</li>
  <li>Using manager directly through SimpleClient.</li>
  <li>ACS logging mechanisms.</li>
  <li>Accessing (dynamic) components through manager methods.</li>
</ul>
<br>
<br>
<h2>Links</h2>
<ul>
  <li><a href="acsexmplClientDynamicComponent_8cpp.html">Client Dynamic Component File Reference</a></li>
</ul>
</div>
   @endhtmlonly
 * @}
 */

/* @}*/
/* @}*/

#include <maciSimpleClient.h>
#include "acsexmplHelloWorldC.h"

ACE_RCSID(acsexmpl, acsexmpClient, "$Id: acsexmplClientDynamicComponent.cpp,v 1.9 2007/02/01 05:14:26 cparedes Exp $")
using namespace maci;
       
/*******************************************************************************/
/** @cond
*/    
    
int main(int argc, char *argv[])
{
    ComponentInfo_var cInfo;

    // Checks command-line arguments.
    if (argc < 2)
	{
	ACS_SHORT_LOG((LM_INFO, "Usage: %s <container name> <options>", argv[0]));
	return -1;
	}
    else
	{
	ACS_SHORT_LOG((LM_INFO, "Welcome to %s!", argv[0]));
	}
    
    //Creates and initializes the SimpleClient object
    SimpleClient client;
    if (client.init(argc,argv) == 0)
	{
	ACE_DEBUG((LM_DEBUG,"Cannot init client"));
	return -1;
	}
    else
	{
	//Must log into manager before we can really do anything
	client.login();
	}

    try
	{
	//In order for us to obtain a reference to a so-called dynamic component,
	//we must first create an instance of an IDL ComponentSpec structure.  This 
	//structure defines the various specififications for the component and is equivalent
	//to the entries found in $ACS_CDB/MACI/Components/Components.xml.  Specific options
	//for the values can be found in the MACI Specifications document (or maci.idl if 
	//you prefer).
	ComponentSpec_var cSpec = new ComponentSpec();    //use _var type for automatic memory management
	cSpec->component_name = CORBA::string_dup(COMPONENT_SPEC_ANY);    //name of the component
	cSpec->component_type = CORBA::string_dup("IDL:alma/acsexmplHelloWorld/HelloWorld:1.0");    //IDL interface implemented by the component
	cSpec->component_code = CORBA::string_dup("acsexmplHelloWorldImpl");     //executable code for the component (e.g. DLL)
	cSpec->container_name = CORBA::string_dup(argv[1]);     //container where the component is deployed

	//The IDL ComponentInfo structure returned by the get_dynamic_component method
	//contains tons of information about the newly created component and the most important
	//field is "reference" (i.e., the unnarrowed dynamic component).
	cInfo  = client.manager()->get_dynamic_component(client.handle(),    //Must pass the client's handle
									   cSpec.in(),    //Pass the component specifications
									   false);    //Inform manager this component is NOT the default for it's type!
 
	//As always, the reference must be CORBA casted to it's correct type.
	acsexmplHelloWorld::HelloWorld_var hwRef = acsexmplHelloWorld::HelloWorld::_narrow(cInfo->reference.in());

	//Ensure it's a valid reference
	if (CORBA::is_nil(hwRef.in()) == false)
	    {
	    ACS_SHORT_LOG((LM_INFO, "Retrieved valid reference from manager."));
	    //Do something useful with the component...
	    hwRef->displayMessage();
	    ACS_SHORT_LOG((LM_INFO, "Method of dynamic component successfully invoked. Have a nice day!"));
	    }
	else
	    {
	    ACS_SHORT_LOG((LM_ERROR, "Bad reference retrieved from manager"));
	    }
	}
// first we catch CORBA user exception ...
    catch( maciErrType::IncompleteComponentSpecEx &_ex)  // can be thrown by get_dynamic_component
	{
	// we convert CORBA exception to C++ local exception
	maciErrType::IncompleteComponentSpecExImpl ex(_ex);
	ex.log();
	}
    catch( maciErrType::InvalidComponentSpecEx &_ex) // can be thrown by get_dynamic_component
	{
	// we convert CORBA exception to C++ local exception
	maciErrType::InvalidComponentSpecExImpl ex(_ex);
	ex.log();
	}
    catch( maciErrType::ComponentSpecIncompatibleWithActiveComponentEx &_ex) // can be thrown by get_dynamic_component
	{
	// we convert CORBA exception to C++ local exception
	maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl ex(_ex);
	ex.log();
	}
    catch( maciErrType::CannotGetComponentEx &_ex) // can be thrown by get_dynamic_component
	{
	// we convert CORBA exception to C++ local exception
	maciErrType::CannotGetComponentExImpl ex(_ex);
	ex.log();
	}
    catch( CORBA::SystemException &_ex )  // ... than CORBA system exception
	{
	ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__,
							    "main");
	corbaProblemEx.setMinor(_ex.minor());
	corbaProblemEx.setCompletionStatus(_ex.completed());
	corbaProblemEx.setInfo(_ex._info().c_str());
	corbaProblemEx.log();
	return -1;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__, 
							"main");
	uex.log();
	return -1;
	}//try-catch

	try
	    {
	    //Must remember to release the dynamic component!
	    client.releaseComponent( cInfo->name);	
	
	    //Cleanly log out of manager.
	    client.logout();
	}
	catch(maciErrType::CannotReleaseComponentExImpl &_ex) // this can be thrown by releaseComponent
	    {
	    _ex.log();
	    return -1;
	    }
	catch(...)
	    {
	    ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__, 
							    "main");
	    uex.log();
	    return -1;
	    }//try-catch
  
    //sleep for 3 sec to allow everytihng to cleanup and stabilize
    //so that the tests can be determinitstic.
    ACE_OS::sleep(3);   
    return 0;
}
/** @endcond
*/    

/*___oOo___*/



