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
 * "@(#) $Id: bulkDataSendRecvEx.cpp,v 1.5 2005/04/14 06:19:30 rcirami Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * oat       2003-07-18 created
 */

/**
 * This test program is used to demonstrate/test the ACS 
 * bulk data transfer mechanism
 */

#include <maciSimpleClient.h>
#include <baci.h>

#include "ace/Get_Opt.h"
#include "orbsvcs/AV/AVStreams_i.h"

#include "bulkDataSenderC.h"
#include "bulkDataReceiverC.h"
#include "bulkDataSendRecvC.h"
#include "ACSBulkDataError.h"


using namespace maci;
using namespace ACSBulkDataError;


int main(int argc, char *argv[])
{
    // parsing parameters

    int c;
    size_t size = 0;

    ACE_Get_Opt opts (argc,argv,"s:");

    while ((c= opts ()) != -1)
	{
	switch (c)
	    {
	    case 's':
		int dim;
		dim = ACE_OS::atoi (opts.opt_arg ());
		size=dim*1;
		break;
	    default:
		ACS_SHORT_LOG((LM_INFO,"Unknown option."));
		ACS_SHORT_LOG((LM_INFO, "USAGE: bulkDataSendRecvEx [-s size]"));
		return -1;
	    }
	}

    if ( size == 0 )
	{
	size=10000;
	}



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
	
    //CORBA::ORB_var orb = BACI_CORBA::getORB();


    try
	{

	// Get the specific component we have requested on the command-line
	bulkdata::BulkDataReceiver_var receiver = client.get_object<bulkdata::BulkDataReceiver>("BulkDataReceiver", 0, true);
	if (CORBA::is_nil (receiver.in ()))
	    {
	    ACS_SHORT_LOG((LM_INFO,"Could not retrieve BulkDataReceiver Component."));
	    return -1;
	    }

	//receiver->open();

	bulkdata::BulkDataSender_var sender = client.get_object<bulkdata::BulkDataSender>("BulkDataSenderEx1", 0, true);
	if (CORBA::is_nil (sender.in ()))
	    {
	    ACS_SHORT_LOG((LM_INFO,"Could not retrieve BulkDataSender Component."));
	    return -1;
	    }


	bulkdata::BulkDataSendRecv_var sendrecv = client.get_object<bulkdata::BulkDataSendRecv>("BulkDataSendRecv", 0, true);
	if (CORBA::is_nil (sendrecv.in ()))
	    {
	    ACS_SHORT_LOG((LM_INFO,"Could not retrieve BulkDataSendRecv Component."));
	    return -1;
	    }

	/******************** BulkDataSendRecv -> BulkDataReceiver **************************/

	sendrecv->connect(receiver.in());

	sendrecv->startSend();

	sendrecv->paceData();

	sendrecv->stopSend();

	sendrecv->disconnect();

	//receiver->closeReceiver();
	// just as an example

	ACE_OS::sleep(3);


	/******************** BulkDataSender -> BulkDataSendRecv **************************/

	sender->connect(sendrecv.in());
      
	sender->startSend();

	sender->paceData();

	sender->stopSend();

	sender->disconnect();

	sendrecv->closeReceiver();
	
	receiver->closeReceiver();
	}

    catch (AVConnectErrorEx & ex)
	{   
	ACS_SHORT_LOG((LM_INFO, "AVConnectErrorEx exception catched !"));
	AVConnectErrorExImpl ex1(ex);
	ex1.log();
	}
    catch (AVStartSendErrorEx & ex)
	{   
	ACS_SHORT_LOG((LM_INFO, "AVStartSendErrorEx exception catched !"));
	AVStartSendErrorExImpl ex1(ex);
	ex1.log();
	}
    catch (AVPaceDataErrorEx & ex)
	{   
	ACS_SHORT_LOG((LM_INFO, "AVPaceDataErrorEx exception catched !"));
	AVPaceDataErrorExImpl ex1(ex);
	ex1.log();
	}
    catch (AVStopSendErrorEx & ex)
	{   
	ACS_SHORT_LOG((LM_INFO, "AVStopSendErrorEx exception catched !"));
	AVStopSendErrorExImpl ex1(ex);
	ex1.log();
	}
    catch (AVDisconnectErrorEx & ex)
	{   
	ACS_SHORT_LOG((LM_INFO, "AVDisconnectErrorEx exception catched !"));
	AVDisconnectErrorExImpl ex1(ex);
	ex1.log();
	}
    catch (AVCloseReceiverErrorEx & ex)
	{   
	ACS_SHORT_LOG((LM_INFO, "AVCloseReceiverErrorEx exception catched !"));
	AVCloseReceiverErrorExImpl ex1(ex);
	ex1.log();
	}

    catch(...)
	{
	ACS_SHORT_LOG((LM_INFO,"UNKNOWN exception catched!"));
	} 

    //We release our component and logout from manager
    client.manager()->release_component(client.handle(), "BulkDataSendRecv");
    client.manager()->release_component(client.handle(), "BulkDataSenderEx1");
    client.manager()->release_component(client.handle(), "BulkDataReceiver");

    client.logout();

    ACS_SHORT_LOG((LM_INFO,"Sleeping 3 sec to allow everything to cleanup and stabilize"));  
    
    ACE_OS::sleep(3);
    
    return 0;
}


/*___oOo___*/
