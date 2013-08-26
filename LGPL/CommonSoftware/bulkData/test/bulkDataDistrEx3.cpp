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
 * "@(#) $Id: bulkDataDistrEx3.cpp,v 1.2 2006/05/10 13:03:59 rcirami Exp $"
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

#include "orbsvcs/AV/AVStreams_i.h"

#include "bulkDataSenderEx3C.h"
#include "bulkDataReceiverEx3C.h"
#include "bulkDataDistributerC.h"
#include "ACSBulkDataError.h"


using namespace maci;
using namespace ACSBulkDataError;


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
	
    try
	{

	// Get the specific component we have requested on the command-line
	bulkdata::BulkDataReceiverEx3_var receiver = client.get_object<bulkdata::BulkDataReceiverEx3>("BulkDataReceiverEx3", 0, true);
	if (CORBA::is_nil (receiver.in ()))
	    {
	    ACS_SHORT_LOG((LM_INFO,"Could not retrieve BulkDataReceiverEx3 Component."));
	    return -1;
	    }

	bulkdata::BulkDataDistributer_var distributer = client.get_object<bulkdata::BulkDataDistributer>("BulkDataDistributer", 0, true);
	if (CORBA::is_nil(distributer.in ()))
	    {
	    ACS_SHORT_LOG((LM_INFO,"Could not retrieve BulkDataDistributer Component."));
	    return -1;
	    }

	bulkdata::BulkDataSenderEx3_var sender = client.get_object<bulkdata::BulkDataSenderEx3>("BulkDataSenderEx3", 0, true);
	if (CORBA::is_nil (sender.in ()))
	    {
	    ACS_SHORT_LOG((LM_INFO,"Could not retrieve BulkDataSenderEx3 Component."));
	    return -1;
	    }


	sender->connect(distributer.in());

	distributer->multiConnect(receiver.in());

// we simulate several causes of errors:
//
// 1a. user throws exception in the receiver cbStart
// 1b. the exception is catched, the error is retrieved and logged
//
// 2a. user throws exception in the receiver cbReceive
// 2b. the exception is catched, the error is retrieved and logged
//
// 3a. a timeout is triggered on the sender side 
// 3b. the status of the callback is retrieved
// 3c. the disconnection does not occur until the cbReceive is busy


	ACS_SHORT_LOG((LM_INFO, "###### SIMULATING ERROR WHEN SENDING PARAMETER ######"));
	try
	    {
	    //Error simulation in start send (1.a)
	    sender->startSendErr();
	    }
	catch (AVStartSendErrorEx & ex)
	    {   
	    ACS_SHORT_LOG((LM_INFO, "### Start Send Exception catched !"));

	    // exception catched (1.b)
	    AVStartSendErrorExImpl ex1(ex);
	    ex1.log();

	    // error retrieved (1.b)
	    CompletionImpl comp = receiver->getCbStatus(1);
	    if(comp.getCode() == ACSBulkDataStatus::AVCbError)
		{
		ACS_SHORT_LOG((LM_INFO,"### Retrieving the cause of error from the receiver:"));
		comp.log();
		}

	    }
	ACS_SHORT_LOG((LM_INFO, "################################################"));



	ACS_SHORT_LOG((LM_INFO, "****** SIMULATING ERROR WHEN SENDING DATA ******"));
	
	try
	    {
	    //No error in start send
	    sender->startSend();

	    //Error simulation in pace data (2.a)
	    sender->paceDataErr();
	    }
	catch (AVPaceDataErrorEx & ex)
	    {   
	    ACS_SHORT_LOG((LM_INFO, "*** Pace data Exception catched !"));
	    // exception catched (2.a)
	    AVPaceDataErrorExImpl ex1(ex);
	    ex1.log();

	    // error retrieved (2.b)
	    CompletionImpl comp1 = receiver->getCbStatus(1);
	    if(comp1.getCode() == ACSBulkDataStatus::AVCbError)
		{

		ACS_SHORT_LOG((LM_INFO,"*** Retrieving the cause of error from the receiver:"));
		comp1.log();
		}

	    }

	ACS_SHORT_LOG((LM_INFO, "************************************************"));

/*

	ACS_SHORT_LOG((LM_INFO, "@@@@@@ SIMULATING TIMEOUT ON SENDER SIDE @@@@@@"));
	try
	    {
	    //Timeout simulation (3.a)
	    sender->startSendTimeout();

	    // a timeout of 10 s is set on the sender
	    sender->paceDataTimeout(10000);
	    }
	catch (AVPaceDataErrorEx & ex)
	    { 
	    ACS_SHORT_LOG((LM_INFO, "@@@@ Pace data Exception catched !"));

	    AVPaceDataErrorExImpl ex1(ex);
	    ex1.log();

	    // status retrieved (3.b)
	    ACS_SHORT_LOG((LM_INFO,"@@@@ Retrieving the status of the receiver:"));
	    CompletionImpl comp1 = receiver->getCbStatus(1);
	    comp1.log();
	    
	    }

	ACS_SHORT_LOG((LM_INFO, "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"));

//	sender->stopSend();

*/
	// disconnect does not occur until the cbReceive has returned (3.c)
	sender->disconnect();

	distributer->closeReceiver();

	distributer->multiDisconnect(receiver.in());
	}

    catch (AVConnectErrorEx & ex)
	{   
	ACS_SHORT_LOG((LM_INFO, "AVConnectErrorEx exception catched !"));
	AVConnectErrorExImpl ex1(ex);
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
    client.manager()->release_component(client.handle(), "BulkDataSenderEx3");
    client.manager()->release_component(client.handle(), "BulkDataDistributer");
    client.manager()->release_component(client.handle(), "BulkDataReceiverEx3");
    
    client.logout();

    ACS_SHORT_LOG((LM_INFO,"Sleeping 3 sec to allow everything to cleanup and stabilize"));  
    
    ACE_OS::sleep(3);
    
    return 0;
}


/*___oOo___*/
