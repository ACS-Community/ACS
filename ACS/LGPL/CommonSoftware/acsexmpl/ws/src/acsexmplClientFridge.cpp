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
* "@(#) $Id: acsexmplClientFridge.cpp,v 1.9 2007/02/01 05:14:26 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-06-17 fixed client.init(argc,argv)
* gchiozzi 2002-03-18 Replaced includes of fridge*.* with acsexmplFridge*.*
* gchiozzi 2002-03-18 created 
*/

/** @file acsexmplClientFridge.cpp
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

/** @defgroup ACSEXMPLCLIENTFRIDGEDOC Client Fridge
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
This example shows a client that:
<ul>
  <li>logs into manager</li>
  <li>gets a fridge component specified from the command-line</li>
  <li>retrieves the value of a BACI property synchronously</li>
  <li>creates a monitor using the callback pattern for the property</li>
  <li>releases the component</li>
  <li>logs out of manager</li>
</ul>
<br>
<br>
<h2>What can I gain from this example?</h2>
<ul>
  <li>SimpleClient usage.</li>
  <li>Using manager directly through SimpleClient.</li>
  <li>ACS logging mechanisms.</li>
  <li>Accessing (remote) components.</li>
  <li>Using callback classes for BACI monitors.</li>
</ul>
<br>
<br>
<h2>Links</h2>
<ul>
  <li><a href="acsexmplClientFridge_8cpp.html">Fridge Client File Reference</a></li>
</ul>
</div>
   @endhtmlonly
 * @}
 */


/* @}*/
/* @}*/

#include <maciSimpleClient.h>
#include <acsexmplFridgeC.h>
#include "acsexmplCallbacks.h"

ACE_RCSID(acsexmpl, acsexmplFridgeClient, "$Id: acsexmplClientFridge.cpp,v 1.9 2007/02/01 05:14:26 cparedes Exp $")
using namespace maci;
    
/*******************************************************************************/
/** @cond
*/    

/*
 * Main procedure
 */
int main(int argc, char *argv[])
{
    //Checks command-line arguments.
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

    //Create an instance of our user-defined callback class
    MyCBdouble myCallback("refTemp");

    try
	{
	//Get the specific component we have requested on the command-line
	FRIDGE::FridgeControl_var fridge = client.getComponent<FRIDGE::FridgeControl>(argv[1], 0, true);
	    
	//Get one of the component's BACI properties
	ACS::RWdouble_var refTemperature = fridge->refTemperature();
	
	if (refTemperature.ptr() != ACS::RWdouble::_nil())
	    {
	    ACSErr::Completion_var completion;
	    
	    //Just synchronously reading the value of refTemp
	    CORBA::Double val = refTemperature->get_sync(completion.out());
	    ACS_SHORT_LOG((LM_INFO,"Value: %f", val));
	    
	    //Activate the callback as a CORBA object
	    ACS::CBdouble_var cb = myCallback._this();
	    ACS_SHORT_LOG((LM_INFO, "OK"));
	    
	    ACS::CBDescIn desc;
	    desc.id_tag = 2;
	    ACS_SHORT_LOG((LM_INFO, "Trying to create monitor for refTemperature..."));

	    //Create the actual monitor
	    ACS::Monitordouble_var md = refTemperature->create_monitor(cb.in(), desc);
	    if (md.ptr() != ACS::Monitordouble::_nil())
		{
		ACS_SHORT_LOG((LM_INFO, "OK"));
		//Set the timer trigger to one second.
		md->set_timer_trigger(10000000);
		}
	    else
		{
		ACS_SHORT_LOG((LM_INFO, "Failed"));
		}

	    //Give the callback some time to run.
	    ACE_Time_Value time(20);
	    client.run(time);
	    
	    //Must explicitly destroy the callback before exiting
	    md->destroy();
	    //Give the callback time to be really destroyed
	    ACE_OS::sleep(15);
	    }
	}
    catch(maciErrType::CannotGetComponentExImpl &_ex)
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
        
    try
	{
	//Must release components and logout from manager	
	ACS_SHORT_LOG((LM_INFO,"Releasing..."));
	client.releaseComponent(argv[1]);
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
  
    // sleep for 3 sec.
    ACE_OS::sleep(3);
    return 0;
}

/** @endcond
*/    

/*___oOo___*/




