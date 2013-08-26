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
* "@(#) $Id: acsexmplClientFridgeCmd.cpp,v 1.8 2007/02/01 05:14:26 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-06-17 fixed client.init(argc,argv)
* gchiozzi 2002-03-18 created 
*/

/** @file acsexmplClientFridgeCmd.cpp
 *  @htmlonly
 *  <br><br>
    @endhtmlonly
 *  @param "component name" Use this required parameter to specify which component
 *  should be activated.
 *  @param "command" Use this required parameter to specify what action the fridge should 
 *  perform: 
 *    ON - turns the fridge on
 *    OFF - turns the fridge off
 *    OPEN - opens the door
 *    CLOSE  - closes the door
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

/** @defgroup ACSEXMPLCLIENTFRIDGECMDDOC Client Fridge Commands
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
This example shows a client that:
<ul>
  <li>logs into manager</li>
  <li>gets a fridge component specified from the command-line</li>
  <li>invokes a command on the component specified from the command-line</li>
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
</ul>
<br>
<br>
<h2>Links</h2>
<ul>
   <li><a href="acsexmplClientFridgeCmd_8cpp.html">Fridge Client Commands File Reference</a></li>
</ul>
</div>
   @endhtmlonly
 * @}
 */

/* @}*/
/* @}*/

#include <maciSimpleClient.h>
#include <acsexmplFridgeC.h>

ACE_RCSID(acsexmpl, acsexmplFridgeClientCmd, "$Id: acsexmplClientFridgeCmd.cpp,v 1.8 2007/02/01 05:14:26 cparedes Exp $")
using namespace maci;
/*******************************************************************************/
/** @cond
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

    try
	{
	//Get the specific component we have requested on the command-line
	FRIDGE::FridgeControl_var fridge = client.getComponent<FRIDGE::FridgeControl>(argv[1], 0, true);
	
	//Run whatever supported command the end-user has specified
	//from the command-line
	if(strcmp(argv[2],"ON") == 0)    // Command ON
	    {
	    ACS_SHORT_LOG((LM_INFO, "ON"));
	    fridge->on();
	    }
	else if(strcmp(argv[2],"OFF") == 0)    // Command OFF
	    {
	    ACS_SHORT_LOG((LM_INFO, "OFF"));
	    fridge->off();
	    }
	else if(strcmp(argv[2],"OPEN") == 0)    // Command OPEN
	    {
	    ACS_SHORT_LOG((LM_INFO, "OPEN"));
	    fridge->open();
	    }
	else if(strcmp(argv[2],"CLOSE") == 0)    // Command CLOSE
	    {
	    ACS_SHORT_LOG((LM_INFO, "CLOSE"));
	    fridge->close();
	    }
	else
	    {
	    // User specified some non-existant command
	    ACS_SHORT_LOG((LM_INFO, "Unknown command"));
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
	}
    
    try
	{
	//Release the component and log out from manager.
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
	}//try-catch
       
    // sleep for 3 sec.
    ACE_OS::sleep(3);
    return 0;
}
/** @endcond
*/    

/*___oOo___*/



