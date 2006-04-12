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
* "@(#) $Id: acsexmplClientErrorComponent.cpp,v 1.1 2006/04/12 16:35:05 gchiozzi Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-10-31 created
*/

/** @file acsexmplClientErrorComponent.cpp
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
  <li><a href="acsexmplClientErrorComponent_8cpp.html">Hello World Client File Reference</a></li>
</ul>
</div>
   @endhtmlonly
 * @}
 */


#include <maciSimpleClient.h>
#include <acsexmplErrorComponentC.h>
#include <ACSErrTypeCommon.h>
#include <acsutilTimeStamp.h>

ACE_RCSID(acsexmpl, acsexmplErrorComponentClient, "$Id: acsexmplClientErrorComponent.cpp,v 1.1 2006/04/12 16:35:05 gchiozzi Exp $")
using namespace maci;

/*******************************************************************************/
    
int main(int argc, char *argv[])
{
    // Creates and initializes the SimpleClient object
    SimpleClient client;
    if (client.init(argc,argv) == 0)
	{
	return -1;
	}
    else
	{
	//Must log into manager before we can really do anything
	client.login();
	}
   
    //Get the specific component we have requested on the command-line
    acsexmplErrorComponent::ErrorComponent_var foo = client.get_object<acsexmplErrorComponent::ErrorComponent>(argv[1], 0, true);
    
    //Call the displayMessage() method existing in the interface for ErrorComponent
    foo->displayMessage();

    /********************************************************
     * Example 1: Calls a method that throws an exception
     *            with a stack trace..
     * - Catches the exception, 
     * - Adds context information
     * - sends it to the logging system
     */
    ACS_SHORT_LOG((LM_INFO, "Example 1: Calls a method that throws an exception."));
    try
	{
	foo->badMethod(3);
	}
    catch(ACSErrTypeCommon::GenericErrorEx &ex)
	{
	ACSErrTypeCommon::GenericErrorExImpl badMethodEx(ex,
				   __FILE__, __LINE__,
				   "main");
	badMethodEx.setErrorDesc("badMethod has thrown the expected exception");
	badMethodEx.log();

	ACS::Time timeStamp = badMethodEx.getTimeStamp();
	ACE_CString tString = getStringifiedUTC(timeStamp);
	ACS_DEBUG_PARAM(argv[0], "Time of the exception: %s\n", tString.c_str());
	}
    catch(CORBA::SystemException &ex)
	{
	// Map......
	ACSErrTypeCommon::GenericErrorExImpl badMethodEx(
				   __FILE__, __LINE__,
				   "main");
	badMethodEx.setErrorDesc("badMethod has thrown a CORBA exception");
	badMethodEx.log();

	ACS::Time timeStamp = badMethodEx.getTimeStamp();
	ACE_CString tString = getStringifiedUTC(timeStamp);
	ACS_DEBUG_PARAM(argv[0], "Time of the CORBA exception: %s\n", tString.c_str());
	}
    catch(...)
	{
	ACSErrTypeCommon::GenericErrorExImpl badMethodEx(__FILE__, __LINE__,
							 "main");
	badMethodEx.setErrorDesc("badMethod has thrown an UNEXPECTED exception");
	badMethodEx.log();

	ACS::Time timeStamp = badMethodEx.getTimeStamp();
	ACE_CString tString = getStringifiedUTC(timeStamp);
	ACS_DEBUG_PARAM(argv[0], "Time of the unexpected exception: %s\n", tString.c_str());
	}

    /********************************************************
     * Example 2: Calls a method that returns a completion
     *            If the completion contains an error, then
     * - Catches the exception, 
     * - prints it locally 
     * - sends it to the logging system
     */
    try
	{
	// OK Completion
	ACS_SHORT_LOG((LM_INFO, "Example 2a: Calls a method that returns an OK completion."));
	CompletionImpl comp = foo->returnCompletion(0);
	comp.log();

	// ERROR completion with an error trace inside.
	ACS_SHORT_LOG((LM_INFO, "Example 2b: Calls a method that returns an Error completion."));
	CompletionImpl comp2 = foo->returnCompletion(3);
	comp2.log();
	}
    catch(...)
	{
	ACSErrTypeCommon::GenericErrorExImpl badMethodEx(__FILE__, __LINE__,
							 "main");
	badMethodEx.setErrorDesc("returnCompletion has thrown an UNEXPECTED exception");
	badMethodEx.log();
	}

    /****************************************************
     * We release our component and logout from manager
     */
    client.manager()->release_component(client.handle(), argv[1]);
    client.logout();
    
    //Sleep for 3 sec to allow everything to cleanup and stablize
    ACE_OS::sleep(3);
    return 0;
}

/*___oOo___*/



