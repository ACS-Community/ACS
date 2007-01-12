#include <maciSimpleClient.h>
#include <baci.h>

#include "ace/Get_Opt.h"
#include "orbsvcs/AV/AVStreams_i.h"

#include <bulkDataSenderC.h>
#include <receiverPTC.h>
#include <senderPTC.h>

#include "ACSBulkDataError.h"

using namespace maci;
using namespace ACSBulkDataError;


int main(int argc, char *argv[])
{
  bulkdata::BulkDataReceiver_var receiver;
  bulkdata::BulkDataSender_var sender;
  // Creates and initializes the SimpleClient object
  SimpleClient client;

  if (argc<4)
    {
      printf ("Usage: senderReceiverPerfTest sender receiver1 test_name \n");
      return -1;
    }
  
  ACS_SHORT_LOG((LM_INFO, "Running Sender-Receiver test: %s", argv[argc-1])); 

    if (client.init(argc,argv) == 0)
	{
	return -1;
	}
    else
	{
	//Must log into manager before we can really do anything
	client.login();
	}
	
	TEST_M::ReceiverPT_var rpt;
	TEST_M::SenderPT_var spt;

    try
      {
	spt = client.getComponent<TEST_M::SenderPT>(argv[1], 0, true);
	if (CORBA::is_nil(spt.in()))
	    {
	    ACS_SHORT_LOG((LM_INFO, "!!!! Error activating sender component !!!"));
	    throw;
	    }
	
	receiver = client.getComponent<bulkdata::BulkDataReceiver>(argv[2], 0, true);
	rpt = TEST_M::ReceiverPT::_narrow(receiver.in());
	if (!CORBA::is_nil(rpt))
	  {
	    rpt->setTestName(argv[argc-1]);
	  }

      }
    catch(ACSErr::ACSbaseExImpl &ex)
      {
	ex.log();
	return -1;
      }
    
    ACS_SHORT_LOG((LM_INFO, "All components have been retrieven !"));
		  
    try
      {
	
	ACS_SHORT_LOG((LM_INFO, "Connecting receiver to the sender."));
	spt->connect(rpt.in());
			      
	ACS_SHORT_LOG((LM_INFO, "Start sending the data"));		      

	//spt->startSendNew(1,100000);
	spt->paceDataNew(1,1200000);
		      
	ACS_SHORT_LOG((LM_INFO, "Stop sending the data"));		      
	spt->stopSendNew(1);

	ACS_SHORT_LOG((LM_INFO, "Disconnect the sender"));		      
	spt->disconnect();
	
	rpt->closeReceiver();
      }
    catch (AVConnectErrorEx & ex)
      {   
	ACS_SHORT_LOG((LM_INFO, "AVConnectErrorEx exception catched !"));
	AVConnectErrorExImpl ex1(ex);
	ex1.log();
	spt->disconnect();	
	rpt->closeReceiver();
	}
    catch (AVStartSendErrorEx & ex)
	{   
	ACS_SHORT_LOG((LM_INFO, "AVStartSendErrorEx exception catched !"));
	AVStartSendErrorExImpl ex1(ex);
	ex1.log();
	spt->disconnect();	
	rpt->closeReceiver();
	}
    catch (AVPaceDataErrorEx & ex)
	{   
	ACS_SHORT_LOG((LM_INFO, "AVPaceDataErrorEx exception catched !"));
	AVPaceDataErrorExImpl ex1(ex);
	ex1.log();
	spt->disconnect();	
	rpt->closeReceiver();
	}
    catch (AVStopSendErrorEx & ex)
	{   
	ACS_SHORT_LOG((LM_INFO, "AVStopSendErrorEx exception catched !"));
	AVStopSendErrorExImpl ex1(ex);
	ex1.log();
	spt->disconnect();	
	rpt->closeReceiver();
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

    try
      {
	//We release our component and logout from manager
	client.releaseComponent(argv[1]); //release sender
	
	client.releaseComponent(argv[2]); // release receiver
      }
    catch(ACSErr::ACSbaseExImpl &ex)
      {
	ex.log();
      }
	
    client.logout();

    return 0;
}


/*___oOo___*/
