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
*
* "@(#) $Id: acsexmplClientHelloWorld.cpp,v 1.7 2007/02/01 05:14:26 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-10-31 created
*/

/** @file acsexmplClientHelloWorld.cpp
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

/** @defgroup ACSEXMPLTOCCLIENTS Client Examples
*/
/*@{
*/

/** @defgroup ACSEXMPLCLIENTHWDOC Client Hello World
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
This example shows a client that:
<ul>
  <li>logs into manager via SimpleClient</li>
  <li>activates the HELLOWORLD component specified from the command-line</li>
  <li>calls the displayMessage() method</li>
  <li>releases the component</li>
  <li>logs out of manager</li>
</ul>
<br>
<br>
<h2>What can I gain from this example?</h2>
<ul>
  <li>SimpleClient usage.</li>
  <li>Using manager directly through SimpleClient.</li>
  <li>Accessing (remote) components.</li>
</ul>
<br>
<br>
<h2>Links</h2>
<ul>
  <li><a href="acsexmplClientHelloWorld_8cpp.html">Hello World Client File Reference</a></li>
</ul>
</div>
   @endhtmlonly
 * @}
 */

/* @}*/
/* @}*/

#include <maciSimpleClient.h>
#include <acsexmplHelloWorldC.h>
#include <ACSErrTypeCommon.h>
#include <acsutilTimeStamp.h>

ACE_RCSID(acsexmpl, acsexmplHelloWorldClient, "$Id: acsexmplClientHelloWorld.cpp,v 1.7 2007/02/01 05:14:26 cparedes Exp $")
using namespace maci;

/*******************************************************************************/
/** @cond
*/    
int main(int argc, char *argv[])
{
    SimpleClient client;
    acsexmplHelloWorld::HelloWorld_var foo;

// Creates and initializes the SimpleClient object

    if (client.init(argc,argv) == 0)
	{
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
	foo = client.getComponent<acsexmplHelloWorld::HelloWorld>(argv[1], 0, true);
	}
    catch(maciErrType::CannotGetComponentExImpl &_ex)
	{
	_ex.log();
	return -1;
	}
    
    
    //Call the displayMessage() method existing in the interface for HelloWorld
    foo->displayMessage();
    
    try
	{
	foo->badMethod();
	}
    catch(ACSErrTypeCommon::UnknownEx &ex)
	{
	ACSErrTypeCommon::UnknownExImpl badMethodEx(ex);
	badMethodEx.log();
	ACS::Time timeStamp = badMethodEx.getTimeStamp();
	ACE_CString tString = getStringifiedUTC(timeStamp);
	ACS_DEBUG_PARAM(argv[0], "Time of the exception: %s\n", tString.c_str());
	}

    //We release our component and logout from manager
    try
	{
	client.releaseComponent(argv[1]);
	}
    catch(maciErrType::CannotReleaseComponentExImpl &_ex)
	{
	_ex.log();
	return -1;
	}
    
    client.logout();
    
    //Sleep for 3 sec to allow everytihng to cleanup and stablize
    ACE_OS::sleep(3);
    return 0;
}
/** @endcond
*/
/*___oOo___*/



