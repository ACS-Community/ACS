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
 * "@(#) $Id: bdNTTEST1.cpp,v 1.6 2013/02/06 15:30:38 bjeram Exp $"
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
#include "ACSBulkDataError.h"

using namespace maci;
using namespace ACSBulkDataError;


int main(int argc, char *argv[])
{
    // Creates and initializes the SimpleClient object
    SimpleClient client;
    //ACE_Time_Value start_time; // for performances test
    //ACE_Time_Value elapsed_time;
    //double dtime;

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
	bulkdata::BulkDataReceiver_var receiver1 = client.get_object<bulkdata::BulkDataReceiver>(argv[2], 0, true);
	if (CORBA::is_nil (receiver1.in ()))
	{
		ACS_SHORT_LOG((LM_ERROR,"Could not retrieve BulkDataReceiverTEST_1 Component."));
		return -1;
	}

	bulkdata::BulkDataReceiver_var receiver2 = client.get_object<bulkdata::BulkDataReceiver>(argv[3], 0, true);
	if (CORBA::is_nil (receiver2.in ()))
	{
		ACS_SHORT_LOG((LM_ERROR,"Could not retrieve BulkDataReceiverTEST_2 Component."));
		return -1;
	}


	bulkdata::BulkDataSender_var sender = client.get_object<bulkdata::BulkDataSender>(argv[1], 0, true);
	if (CORBA::is_nil (sender.in ()))
	{
		ACS_SHORT_LOG((LM_ERROR,"Could not retrieve BulkDataSenderTEST Component."));
		return -1;
	}

	receiver1->openReceiver();
	receiver2->openReceiver();



	sender->connect(receiver1.in()); //!! the receiver does not matter

	sleep(4);
//getchar();
	ACS_SHORT_LOG((LM_INFO,"Let's first disconnect sender and close all receivers  (so we destroy all streams/flows)...."));
	sender->disconnect();
	receiver1->closeReceiver();
	receiver2->closeReceiver();


	sleep(4);
	ACS_SHORT_LOG((LM_INFO,".... and after open all receivers and connect sender (we create again all streams/flows"));

	receiver1->openReceiver();
	receiver2->openReceiver();
	sender->connect(receiver1.in()); //!! the receiver does not matter

	sleep(4);
	ACS_SHORT_LOG((LM_INFO,".... and and the end let's send data"));
	//start_time = ACE_OS::gettimeofday(); // for performances test
	sender->startSend();

	sender->paceData();

	sender->stopSend();
	//elapsed_time = ACE_OS::gettimeofday() - start_time; // for performances test
	//dtime = elapsed_time.sec() + ( elapsed_time.usec() / 1000000. );
	//cout << "dtime: " << dtime << endl;

//	getchar();
	sleep(2);

	sender->disconnect();
	sleep(1);

	receiver1->closeReceiver();
	sleep(1);

	receiver2->closeReceiver();
	sleep(1);
	}

    catch (ACSErr::ACSbaseExImpl& ex)
	{   
	ex.log();
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"UNKNOWN exception catched!"));
	}

    //We release our component and logout from manager
    client.manager()->release_component(client.handle(), argv[1]);
    sleep(1);

    client.manager()->release_component(client.handle(), argv[2]);
    sleep(1);
    client.manager()->release_component(client.handle(), argv[3]);
    sleep(1);
    
    client.logout();
    sleep(1);

    //   ACS_SHORT_LOG((LM_INFO,"Sleeping 3 sec to allow everything to cleanup and stabilize"));
    //   ACE_OS::sleep(3);
    
    return 0;
}


/*___oOo___*/
