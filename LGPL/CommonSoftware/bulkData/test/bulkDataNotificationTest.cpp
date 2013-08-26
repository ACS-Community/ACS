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
 * "@(#) $Id: bulkDataNotificationTest.cpp,v 1.2 2007/11/12 13:36:43 rcirami Exp $"
 *
 * who       when        what
 * --------  --------    ----------------------------------------------
 * oat       2007-09-25  created
 */

/**
 * This test program is used to demonstrate/test the ACS 
 * bulk data transfer notification mechanism
 */

#include <maciSimpleClient.h>
#include <baci.h>

#include "ace/Get_Opt.h"
#include "orbsvcs/AV/AVStreams_i.h"

#include "bulkDataSenderC.h"
#include "bulkDataDistributerC.h"
#include "bulkDataReceiverC.h"

#include "ACSBulkDataError.h"

#include "bulkDataTestNotificationCb.h"

using namespace maci;
using namespace ACSBulkDataError;


int main(int argc, char *argv[])
{
    SimpleClient client;

    if (client.init(argc,argv) == 0)
	{
	return -1;
	}
    else
	{
	client.login();
	}
	
    try
	{
	bulkdata::BulkDataSender_var sender = client.get_object<bulkdata::BulkDataSender>("BulkDataNotifSender", 0, true);
	if (CORBA::is_nil(sender.in()))
	    {
	    ACS_SHORT_LOG((LM_ERROR,"Could not retrieve BulkDataNotifSender component"));
	    return -1;
	    }

	bulkdata::BulkDataDistributer_var distributer = client.get_object<bulkdata::BulkDataDistributer>("BulkDataNotifDistributer", 0, true);
	if (CORBA::is_nil(distributer.in()))
	    {
	    ACS_SHORT_LOG((LM_ERROR,"Could not retrieve BulkDataNotifDistributer component"));
	    return -1;
	    }

	bulkdata::BulkDataReceiver_var receiver = client.get_object<bulkdata::BulkDataReceiver>("BulkDataNotifReceiver", 0, true);
	if (CORBA::is_nil(receiver.in()))
	    {
	    ACS_SHORT_LOG((LM_ERROR,"Could not retrieve BulkDataNotifReceiver component"));
	    return -1;
	    }

	bulkdata::BulkDataReceiver_var receiver1 = client.get_object<bulkdata::BulkDataReceiver>("BulkDataNotifReceiver1", 0, true);
	if (CORBA::is_nil(receiver1.in()))
	    {
	    ACS_SHORT_LOG((LM_ERROR,"Could not retrieve BulkDataNotifReceiver1 component"));
	    return -1;
	    }


// Receiver connected to the Distributor

	sender->connect(distributer.in());

	distributer->multiConnect(receiver.in());

	// instantiate and activate user callbacks for the notification
	BulkDataTestNotificationCb *notifCb = new BulkDataTestNotificationCb();
	ACS::CBvoid_var cb = notifCb->_this();

	// subscribe to the notification mechanism
	distributer->subscribeNotification(cb);


	sender->startSend();

	sender->paceData();

	sender->stopSend();


	sender->disconnect();

	distributer->closeReceiver();
	    
	distributer->multiDisconnect(receiver.in());

	notifCb->_remove_ref();



// Receiver 1 connected directly to the Sender

	sender->connect(receiver1.in());

	// instantiate and activate user callbacks for the notification
	BulkDataTestNotificationCb *notifCb1 = new BulkDataTestNotificationCb();
	ACS::CBvoid_var cb1 = notifCb1->_this();

	// subscribe to the notification mechanism
	receiver1->subscribeNotification(cb1);


	sender->startSend();

	sender->paceData();

	sender->stopSend();


	sender->disconnect();

	receiver1->closeReceiver();

	notifCb1->_remove_ref();
	}

    catch (AVConnectErrorEx & ex)
	{   
	ACS_SHORT_LOG((LM_ERROR, "AVConnectErrorEx exception catched !"));
	AVConnectErrorExImpl ex1(ex);
	ex1.log();
	}
    catch (AVStartSendErrorEx & ex)
	{   
	ACS_SHORT_LOG((LM_ERROR, "AVStartSendErrorEx exception catched !"));
	AVStartSendErrorExImpl ex1(ex);
	ex1.log();
	}
    catch (AVPaceDataErrorEx & ex)
	{   
	ACS_SHORT_LOG((LM_ERROR, "AVPaceDataErrorEx exception catched !"));
	AVPaceDataErrorExImpl ex1(ex);
	ex1.log();
	}
    catch (AVStopSendErrorEx & ex)
	{   
	ACS_SHORT_LOG((LM_ERROR, "AVStopSendErrorEx exception catched !"));
	AVStopSendErrorExImpl ex1(ex);
	ex1.log();
	}
    catch (AVDisconnectErrorEx & ex)
	{   
	ACS_SHORT_LOG((LM_ERROR, "AVDisconnectErrorEx exception catched !"));
	AVDisconnectErrorExImpl ex1(ex);
	ex1.log();
	}
    catch (AVCloseReceiverErrorEx & ex)
	{   
	ACS_SHORT_LOG((LM_ERROR, "AVCloseReceiverErrorEx exception catched !"));
	AVCloseReceiverErrorExImpl ex1(ex);
	ex1.log();
	}
    catch (ACSErrTypeCommon::CORBAProblemEx & ex)    
	{   
	ACS_SHORT_LOG((LM_ERROR, "CORBAProblemEx exception catched !"));
	ACSErrTypeCommon::CORBAProblemExImpl ex1(ex);
	ex1.log();
	}
    catch (AVNotificationMechanismErrorEx & ex)
	{   
	ACS_SHORT_LOG((LM_ERROR, "AVNotificationMechanismErrorEx exception catched !"));
	AVNotificationMechanismErrorExImpl ex1(ex);
	ex1.log();
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"UNKNOWN exception catched!"));
	}
    
    //We release our component and logout from manager
    client.manager()->release_component(client.handle(), "BulkDataNotifReceiver1");
    client.manager()->release_component(client.handle(), "BulkDataNotifReceiver");
    client.manager()->release_component(client.handle(), "BulkDataDistributer");
    client.manager()->release_component(client.handle(), "BulkDataNotifSender");
    
    client.logout();

    ACS_SHORT_LOG((LM_INFO,"Sleeping 3 sec to allow everything to cleanup and stabilize"));  
    
    ACE_OS::sleep(3);
    
    return 0;
}
