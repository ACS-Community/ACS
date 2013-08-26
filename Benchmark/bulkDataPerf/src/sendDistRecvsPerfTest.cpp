#include <maciSimpleClient.h>
#include <baci.h>

#include "ace/Get_Opt.h"
#include "orbsvcs/AV/AVStreams_i.h"

#include <bulkDataSenderC.h>
#include <receiverPTC.h>
#include <senderPTC.h>
#include <bulkDataDistributerC.h>

#include "ACSBulkDataError.h"

using namespace maci;
using namespace ACSBulkDataError;


int main(int argc, char *argv[])
{
  bulkdata::BulkDataReceiver_var  receivers[10];
  bulkdata::BulkDataSender_var sender;
  bulkdata::BulkDataDistributer_var distributor;
  short i, recvsNum=0;
  // Creates and initializes the SimpleClient object
  SimpleClient client;

  if (argc<3)
    {
      printf ("Usage: sendDistRecvsPerfTest sender distributor receiver1 [receiver_n] test_name \n");
      return -1;
    }
  if (argc>12)
    {
      printf("Maximal number of recivers is 10!\n");
      return -1;
    }
  
  ACS_SHORT_LOG((LM_INFO, "Running test: %s", argv[argc-1])); 

    if (client.init(argc,argv) == 0)
	{
	return -1;
	}
    else
	{
	//Must log into manager before we can really do anything
	client.login();
	}


    TEST_M::SenderPT_var spt;
    TEST_M::ReceiverPT_var rpt[10];
	
    try
      {
      //TEST_M::ReceiverPT_var rpt;
      //sender = client.getComponent<bulkdata::BulkDataSender>(argv[1], 0, true);
      spt = client.getComponent<TEST_M::SenderPT>(argv[1], 0, true);
      //spt = TEST_M::SenderPT::_narrow(sender.in());
	    if (CORBA::is_nil(spt.in()))
	      {
	      ACS_SHORT_LOG((LM_INFO, "!!!! Error activating sender component !!!"));
	      throw;
	      }
	
	distributor = client.getComponent<bulkdata::BulkDataDistributer>(argv[2], 0, true);

	for (;recvsNum < (argc-4); recvsNum++)
	  {
	  receivers[recvsNum] = client.getComponent<bulkdata::BulkDataReceiver>(argv[recvsNum+3], 0, true);
	  rpt[recvsNum] = TEST_M::ReceiverPT::_narrow(receivers[recvsNum].in());
	  //receivers[recvsNum] = client.getComponent<bulkdata::BulkDataReceiver>(argv[recvsNum+3], 0, true);
	  //rpt[recvsNum] = client.getComponent<TEST_M::ReceiverPT>(argv[recvsNum+3], 0, true);
	    if (!CORBA::is_nil(rpt[recvsNum].in()))
	      {
	      rpt[recvsNum]->setTestName(argv[argc-1]);
	      }//if
	  }//for
      }
    catch(ACSErr::ACSbaseExImpl &ex)
      {
	ex.log();
	for (--recvsNum; recvsNum >=0;  recvsNum--)
	  {
	    client.releaseComponent(argv[recvsNum+3]);
	  }//for
	// release also sender and distributor
	return -1;
      }
    
    ACS_SHORT_LOG((LM_INFO, "All components have been retrieven !"));
		  
    try
      {	
	ACS_SHORT_LOG((LM_INFO, "Connecting to the sender."));
	spt->connect(distributor.in());
	
	ACS_SHORT_LOG((LM_INFO, "Connecting receivers to the distributor"));		      
	for (i=0; i<recvsNum; i++)
	  {
	    distributor->multiConnect(rpt[i].in());
	  } 
		      
	ACS_SHORT_LOG((LM_INFO, "Start sending the data"));		      
	//spt->startSendNew(1,100000);
	//spt->startSendNew(2,200000);
	//spt->startSendNew(3,300000);
	//spt->startSendNew(4,400000);

	spt->paceDataNew(1,1000000);
	//spt->paceDataNew(2,2000000);
	//spt->paceDataNew(3,3000000);
	//spt->paceDataNew(4,4000000);
		      
	ACS_SHORT_LOG((LM_INFO, "Stop sending the data"));		      
	spt->stopSendNew(1);
	//spt->stopSendNew(2);
	//spt->stopSendNew(3);
	//spt->stopSendNew(4);

	ACS_SHORT_LOG((LM_INFO, "Disconnect the sender"));		      
	spt->disconnect();
	
	ACS_SHORT_LOG((LM_INFO, "Close the receivers"));	      
	/*for (i=0; i<recvsNum; i++)
	  {
	    rpt[i]->closeReceiver();
	    }*/

	distributor->closeReceiver();
	
	ACS_SHORT_LOG((LM_INFO, "Disconnect the receivers from the distributer"));	      
	for (i=0; i<recvsNum; i++)
	  {
	  distributor->multiDisconnect(rpt[i]);
	  } 
		
      }
    catch (AVConnectErrorEx & ex)
      {   
	ACS_SHORT_LOG((LM_ERROR, "AVConnectErrorEx exception catched !"));
	AVConnectErrorExImpl ex1(ex);
	ex1.log();
	distributor->closeReceiver();
	
	ACS_SHORT_LOG((LM_INFO, "Disconnect the receivers from the distributer"));	      
	for (i=0; i<recvsNum; i++)
	  {
	  distributor->multiDisconnect(rpt[i]);
	  } 
	}
    catch (AVStartSendErrorEx & ex)
	{   
	ACS_SHORT_LOG((LM_ERROR, "AVStartSendErrorEx exception catched !"));
	AVStartSendErrorExImpl ex1(ex);
	ex1.log();
	distributor->closeReceiver();
	
	ACS_SHORT_LOG((LM_INFO, "Disconnect the receivers from the distributer"));	      
	for (i=0; i<recvsNum; i++)
	  {
	  distributor->multiDisconnect(rpt[i]);
	  } 
	}
    catch (AVPaceDataErrorEx & ex)
	{   
	ACS_SHORT_LOG((LM_ERROR, "AVPaceDataErrorEx exception catched !"));
	AVPaceDataErrorExImpl ex1(ex);
	ex1.log();
	distributor->closeReceiver();
	
	ACS_SHORT_LOG((LM_INFO, "Disconnect the receivers from the distributer"));	      
	for (i=0; i<recvsNum; i++)
	  {
	  distributor->multiDisconnect(rpt[i]);
	  } 
	}
    catch (AVStopSendErrorEx & ex)
	{   
	ACS_SHORT_LOG((LM_ERROR, "AVStopSendErrorEx exception catched !"));
	AVStopSendErrorExImpl ex1(ex);
	ex1.log();
	distributor->closeReceiver();
	
	ACS_SHORT_LOG((LM_INFO, "Disconnect the receivers from the distributer"));	      
	for (i=0; i<recvsNum; i++)
	  {
	  distributor->multiDisconnect(rpt[i]);
	  } 
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
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"UNKNOWN exception catched!"));
	}

    try
      {
	//We release our component and logout from manager
	client.releaseComponent(argv[1]); //release sender
	client.releaseComponent(argv[2]); //release distributor
	
	for (i=0; i<recvsNum; i++)
	  {
	    client.releaseComponent(argv[i+3]); // release receiver(s)
	  }
      }
    catch(ACSErr::ACSbaseExImpl &ex)
      {
	ex.log();
      }
	
    client.logout();

    ACS_SHORT_LOG((LM_INFO, "Test: %s ended", argv[argc-1]));

    return 0;
}


/*___oOo___*/
