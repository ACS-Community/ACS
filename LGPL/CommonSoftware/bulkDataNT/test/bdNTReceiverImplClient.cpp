#include <iostream>
#include <maciSimpleClient.h>
#include <bulkDataReceiverC.h>
#include <ace/OS_NS_unistd.h>

using namespace maci;
using namespace std;

int main(int argc, char *argv[]) {

	SimpleClient client;
	if( client.init(argc, argv) == 0 ) {
		cerr << "Cannot initialize client, not continuing" << endl;
		return 1;
	}
	client.login();

	try {

		ACS_SHORT_LOG((LM_INFO,"Obtaining reference to NEWCONFIG_RECEIVER"));
		bulkdata::BulkDataReceiver_var receiver = client.getComponent<bulkdata::BulkDataReceiver>("NEWCONFIG_RECEIVER", 0, true);
	
		// This stream is not configuredon the CDB, will use a default configuration
		ACS_SHORT_LOG((LM_INFO,"Opening stream 'no_existing_stream' (not in CDB)"));
		receiver->openReceiverStream("no_existing_stream");
	
		// This is configured on the CDB, cool
		ACS_SHORT_LOG((LM_INFO,"Opening stream 'Name1' (in CDB)"));
		receiver->openReceiverStream("Name1");
	
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
	} catch(maciErrType::CannotGetComponentExImpl &ex) {
		cerr << "Cannot get component '" << ex.getCURL() << "'. Reason: " << ex.getReason() << endl;
	} catch(...) {
		cerr << "Unexpected exception while running test code" << endl;
		client.logout();
	}
}
