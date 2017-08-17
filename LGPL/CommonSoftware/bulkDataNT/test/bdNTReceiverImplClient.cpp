/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2011
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: bdNTReceiverImplClient.cpp,v 1.3 2011/12/15 11:50:00 rtobar Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/
#include <iostream>
#include <maciSimpleClient.h>
#include <bulkDataReceiverC.h>
#include <ace/OS_NS_unistd.h>
#include <maciSimpleClient.h>

using namespace maci;
using namespace std;

//Thread to receive events and avoid stuck
void* tr_sc(void *param){
    maci::SimpleClient *myclient = static_cast<maci::SimpleClient*>(param);
    cout << "Thread to run client" << endl;
    myclient->run();
    return NULL;
}

int main(int argc, char *argv[]) {

	SimpleClient client;
	if( client.init(argc, argv) == 0 ) {
		cerr << "Cannot initialize client, not continuing" << endl;
		return 1;
	}
	client.login();
	pthread_t t1;
	if (pthread_create(&t1, NULL, tr_sc, (void *) &client) != 0){
		cout << "client->run(), getting events" << std::endl;
	}

	try {

		ACS_SHORT_LOG((LM_INFO,"Obtaining reference to NEWCONFIG_RECEIVER"));
		bulkdata::BulkDataReceiver_var receiver = client.getComponent<bulkdata::BulkDataReceiver>("NEWCONFIG_RECEIVER", 0, true);
	
		// This stream is not configuredon the CDB, will use a default configuration
		ACS_SHORT_LOG((LM_INFO,"Opening stream 'no_existing_stream' (not in CDB)"));
		receiver->openReceiverStream("no_existing_stream");
	
		// This is configured on the CDB, cool
		ACS_SHORT_LOG((LM_INFO,"Opening stream 'Name1' (in CDB)"));
		receiver->openReceiverStream("Name1");
		sleep(2);

		// Open the rest of the receivers
		ACS_SHORT_LOG((LM_INFO,"Opening all remaining streams (namely, Name7)"));
		receiver->openReceiver();
	
		// now sleep a little bit
		ACS_SHORT_LOG((LM_INFO,"Sleeping 10 seconds"));
		ACE_OS::sleep(10);
	
		// and close the receivers
	
		// woops, this doesn't exist
		ACS_SHORT_LOG((LM_INFO,"Closing stream 'name12'"));
		receiver->closeReceiverStream("name12");
	
		// This was the one we wanted to close before
		ACS_SHORT_LOG((LM_INFO,"Closing stream 'Name1'"));
		receiver->closeReceiverStream("Name1");
		ACS_SHORT_LOG((LM_INFO,"Closing stream 'no_existing_stream' (but now it does exist)"));
		receiver->closeReceiverStream("no_existing_stream");
	
		// close the rest
		ACS_SHORT_LOG((LM_INFO,"Closing remaining streams"));
		receiver->closeReceiver();

		// Close receiver
		client.releaseComponent("NEWCONFIG_RECEIVER");

	} catch(maciErrType::CannotGetComponentExImpl &ex) {
		cerr << "Cannot get component '" << ex.getCURL() << "'. Reason: " << ex.getReason() << endl;
	} catch(...) {
		cerr << "Unexpected exception while running test code" << endl;
	}
	client.getORB()->shutdown(true);
	pthread_join(t1, NULL);
	client.logout();
}
