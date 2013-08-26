/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) NRAO, 2003
*    Copyright by NRAO (in the framework of the ALMA collaboration)
*    All rights reserved
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
* "@(#) $Id: acsexmplClientFridgeNC.cpp,v 1.12 2008/07/27 15:10:49 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2003-02-13   created
*/
/** @file acsexmplClientFridgeNC.cpp
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

/** @defgroup ACSEXMPLCLIENTFRIDGENCDOC Client Fridge Channel Event Consumer
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
ClientFridgeNC provides developers with a very simple example on
creating an event consumer for a given channel using the SimpleConsumer
class.&nbsp; In this particular example, only
FRIDGE::temperatureDataBlockEvent events are processed (and only five are
printed to standard out).  After 50 seconds, the consumer disconnects
from the channel.
<br>
<br>
<h2>What can I gain from this example?</h2>
<ul>
  <li>The ability to process a certain type of event on a given notification channel without the need for subclassing.</li>
  <li>Usage of the ACS_NEW_SIMPLE_CONSUMER macro which is mandatory for events defined in ICDs.</li>
  <li>The implementation of a so-called "handler" function.</li>
</ul>
<br>
<br>
<h2>Links</h2>
<ul>
  <li><a href="acsexmplClientFridgeNC_8cpp.html">Fridge Event Channel Client File Reference</a></li>
</ul>
</div>
   @endhtmlonly
 * @}
 */

/* @}*/
/* @}*/
/////////////////////////////////////////////////////////////////////////
#include <maciSimpleClient.h>
#include <acsncSimpleConsumer.h>
#include "acsexmplFridgeC.h"
/////////////////////////////////////////////////////////////////////////  
ACE_RCSID(acsexmpl, acsexmplFridgeNCClient, "$Id: acsexmplClientFridgeNC.cpp,v 1.12 2008/07/27 15:10:49 cparedes Exp $")
//-----------------------------------------------------------------------------

/** Function designed to do something useful with temperatureDataBlockEvents
 *  received from the "fridge" channel.  Prints only the first five events
 *  so the modular test will be deterministic.
 *  @param joe A temperatureDataBlockEvent describing the current status of a fridge device
 *  @return void
 *  @htmlonly
 *  <br><br>
    @endhtmlonly
 */
void myHandlerFunction(FRIDGE::temperatureDataBlockEvent joe, void *handlerParam)
{
    int *eventCount = (int *)handlerParam;

    if (*eventCount < 5)
	{
	ACS_SHORT_LOG((LM_INFO, "::myHandlerFunction(...): %f is the tempdiff.",
		       joe.absoluteDiff));
	}
    (*eventCount)++;
}
/*******************************************************************************/
/** @cond
*/    
int main(int argc, char *argv[])
{
    ACS_SHORT_LOG((LM_INFO, "Welcome to %s!", argv[0]));
        
    //Creates and initializes the SimpleClient object
    maci::SimpleClient client;    
    
    int eventCount = 0;

    try
	{
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

	// Create consumer
	//First initialize a SimpleConsumer pointer to 0.
	nc::SimpleConsumer<FRIDGE::temperatureDataBlockEvent> *simpConsumer_p=0;
	//Pass the pointer, event data type, channel name, and a handler function
	//to an ACS macro which initializes the consumer.
	ACS_NEW_SIMPLE_CONSUMER(simpConsumer_p, FRIDGE::temperatureDataBlockEvent, FRIDGE::CHANNELNAME_FRIDGE, myHandlerFunction, (void *)&eventCount);

	std::cout << "created a consumer" << std::endl;
	//Let the channel know we are ready to begin processing events
	simpConsumer_p->consumerReady();

	//Give the Consumer 50 seconds to process as many events as it can.
	ACE_Time_Value tv(50);
	client.run(tv);
	
	// Delete the objects or the TAO Notification Service will become 
	// corrupted.
	simpConsumer_p->disconnect(); 
	simpConsumer_p=0;
	ACS_SHORT_LOG((LM_INFO,"Releasing..."));
	} 
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR, "main"));
	}
    
    //Sleep so the test will be deterministic.
    client.logout();
    ACE_OS::sleep(5);
    return 0;
}
/** @endcond
*/    

/*___oOo___*/



