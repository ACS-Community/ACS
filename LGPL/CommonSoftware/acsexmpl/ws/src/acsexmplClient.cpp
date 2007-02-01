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
* "@(#) $Id: acsexmplClient.cpp,v 1.98 2007/02/01 05:14:26 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* oat 2002-12-17 templatization of the client.get_object method
* david 2002-06-17 changed client.init(argc,argv) for improved error checking
* gchiozzi 2002-02-13 cleane up
* msekoran  2001/07/13  created 
*/

/** @file acsexmplClient.cpp
 *  @htmlonly
 *  <br><br>
    @endhtmlonly
 *  @param "component name" Use this required parameter to specify which component
 *  should be activated.
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

/** @defgroup ACSEXMPLCLIENTDOC Client
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
Client is a pretty simple example of SimpleClient usage. &nbsp;It logs
into Manager and utilizes the ACS logging service to record all Mount
components manager knows about. &nbsp;Next, it gets a reference to one of those Mount
instances (where the component's name is passed from the command-line).
&nbsp;From this reference, the actAz read-only double property is
obtained asynchronously and it's value is logged.
<br>
<br>
<h2>What can I gain from this example?</h2>
<ul>
  <li>SimpleClient usage.</li>
  <li>Using manager directly through SimpleClient.</li>
  <li>Using manager to dynamically determine what components are available.</li>
  <li>ACS logging mechanisms.</li>
  <li>Accessing (remote) components.</li>
  <li>Invoking asynchronous methods using callbacks.</li>
</ul>
<br>
<br>
<h2>Links</h2>
<ul>
  <li><a href="acsexmplClient_8cpp.html">Client File Reference</a></li>
</ul>
</div>
   @endhtmlonly
 * @}
 */

/* @}*/
/* @}*/

#include <maciSimpleClient.h>
#include "acsexmplMountC.h"

#include "acsexmplCallbacks.h"

ACE_RCSID(acsexmpl, acsexmpClient, "$Id: acsexmplClient.cpp,v 1.98 2007/02/01 05:14:26 cparedes Exp $")
using namespace maci;
       
/*******************************************************************************/
/** @cond
*/    
    
int main(int argc, char *argv[])
{
    // Checks command-line arguments.
    if (argc < 2)
	{
	ACS_SHORT_LOG((LM_INFO, "Usage: %s <component name> <options>", argv[0]));
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
	//List all components of type "*Mount*" the Manager knows of. 
	ACS_SHORT_LOG((LM_INFO, "Listing all components of type *Mount*"));
	maci::HandleSeq seq;
	//See the doxygen documentation for maci.idl to understand what these parameters
	//are.
	maci::ComponentInfoSeq_var components = client.manager()->get_component_info(client.handle(), 
										     seq, 
										     "*", 
										     "*Mount*", 
										     false);
	
	for (CORBA::ULong i = static_cast<CORBA::ULong>(0); i < components->length(); i++)
	    {
	    //just print out all known mount components
	    ACS_SHORT_LOG((LM_INFO,"%s (%s)", components[i].name.in(), components[i].type.in()));
	    }
	
	// Now get the specific component we have requested from the command-line
	ACS_SHORT_LOG((LM_INFO, "Getting component: %s", argv[1]));

	//getComponent can throw an exception if it fails
	MOUNT_ACS::Mount_var mount = client.getComponent<MOUNT_ACS::Mount>(argv[1], 0, true);
	
	
	//Prints the descriptor of the requested component
	ACS_SHORT_LOG((LM_DEBUG, "Requesting descriptor()... "));
	ACS::CharacteristicComponentDesc_var descriptor = mount->descriptor();
	ACS_SHORT_LOG((LM_DEBUG, "Got descriptor()."));
	ACS_SHORT_LOG((LM_INFO,"Descriptor:"));
	ACS_SHORT_LOG((LM_INFO,"\tname: %s", descriptor->name.in()));
	
	//Get the reference to the  actAz double property
	ACS_SHORT_LOG((LM_INFO, "Getting component property: %s:actAz", argv[1]));
	ACS::ROdouble_var actAz = mount->actAz();
	    
	if (actAz.ptr() != ACS::ROdouble::_nil())
	    {
	    //Get the current value of the property synchronously
	    ACSErr::Completion_var completion;
	    CORBA::Double val = actAz->get_sync(completion.out());
	    ACS_SHORT_LOG((LM_INFO,"Value: %f", val));
	    
	    
	    //Create the CBdouble property
	    ACS_SHORT_LOG((LM_INFO, "Trying to narrow CB for actAz... "));
	    MyCBdouble myCallback("actAz");
	    //Activate it as a CORBA object
	    ACS::CBdouble_var cb = myCallback._this(); 
	    ACS_SHORT_LOG((LM_INFO, "OK"));
	    
	    //Invoke the asynchronous method.
	    ACS_SHORT_LOG((LM_INFO, "Call get_async for actAz..."));
	    ACS::CBDescIn desc;
	    actAz->get_async(cb.in(), desc);    //returns control immediately
	    
	    //Here some other useful things should be done
	    //while the asyncrhonous reply comes
	    //...
	    //...
	    //...
	    
	    //Enter main loop and stays there for a fixed amount of time (1s)
	    //This is done to give the asynchronous method a chance to finish.
	    ACE_Time_Value tv(1);
	    client.run(tv);
	    }//if
	}
    catch(maciErrType::CannotGetComponentExImpl &_ex) // can be thrown by getComponent<..>(...)
	{
	_ex.log();
	return -1;
	}
    catch( CORBA::SystemException &_ex ) // can be thrown by get_component_info
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
  
    //Another try section where we release our component and logout from the Manager
    try
	{
	ACS_SHORT_LOG((LM_INFO,"Releasing..."));
	client.releaseComponent( argv[1]);	
	client.logout();
	}
    catch(maciErrType::CannotReleaseComponentExImpl &_ex)
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




