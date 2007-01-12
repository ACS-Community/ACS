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


ACE_Barrier *barrier_p;
pthread_t tasks[4];

TEST_M::SenderPT_var spt;

void *senderTask(void *_ptr)
{
    int id = (int)_ptr;

    id++;

    LoggingProxy m_logger(0, 0, 31);
    LoggingProxy::init (&m_logger); 
 
    pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);

    ACS_SHORT_LOG((LM_INFO, "sender task %d started...", id));

    try
    {

    unsigned long loop = 1;

    for (unsigned long k=0;k<loop;k++ )
	{


//	barrier_p->wait();
      
	ACS_SHORT_LOG((LM_INFO, "Start sending the data"));		      
	spt->startSendNew(id,100000);
	
//	barrier_p->wait();

	if ( id == 1 )
	    spt->paceDataNew(id,1000000);
	else if ( id == 2 )
	    spt->paceDataNew(id,2000000);
	else if ( id == 3 )
	    spt->paceDataNew(id,3000000);
	else if ( id == 4 )
	    spt->paceDataNew(id,4000000);
	
//	barrier_p->wait();
	
	ACS_SHORT_LOG((LM_INFO, "Stop sending the data"));		      
	spt->stopSendNew(id);
 
	barrier_p->wait();    

	}

    }
    catch(...)
    {
	ACS_SHORT_LOG((LM_INFO,"UNKNOWN exception catched in thread %d!", id));
    }
 
    ACS_SHORT_LOG((LM_INFO, "sender task %d completed", id));

    pthread_exit(0);
}



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


    TEST_M::ReceiverPT_var rpt[10];
	
    try
      {
      //TEST_M::ReceiverPT_var rpt;
      //sender = client.getComponent<bulkdata::BulkDataSender>(argv[1], 0, true);
      //spt = TEST_M::SenderPT::_narrow(sender.in());

      spt = client.getComponent<TEST_M::SenderPT>(argv[1], 0, true);

      if (CORBA::is_nil(spt.in()))
	  {
	  ACS_SHORT_LOG((LM_INFO, "!!!! Error activating sender component !!!"));
	  return -1;
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

/*
	      char buffer[256];
	      ACE_OS::sprintf(buffer,"-LOOP%ld",globalLoop);
	      ACE_CString strToPass = ACE_CString(argv[argc-1])+ACE_CString(buffer);
	      rpt[recvsNum]->setTestName(strToPass.c_str());
*/
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
    
    ACS_SHORT_LOG((LM_INFO, "All components have been retreiven !"));
    
    ACE_Barrier barrier(4);
    barrier_p = &barrier;
    
    try
	{
	
	ACS_SHORT_LOG((LM_INFO, "Connecting to the sender."));
	spt->connect(distributor.in());
	
	ACS_SHORT_LOG((LM_INFO, "Connecting receivers to the distributor"));		      
	for (i=0; i<recvsNum; i++)
	  {
	    distributor->multiConnect(rpt[i].in());
	  } 

// ACTIVATING THREAD

	//
	// spawn the senders
	//
	for ( int ii = 0; ii < 4; ii++ )
            {
	    int stat;
	    if ( (stat = pthread_create(&tasks[ii],
					0,
					senderTask,
					(void *)ii))
		 != 0 )
                {
		ACS_SHORT_LOG((LM_ERROR,
			       "failed to spawn sender task %d (err=%d)!",
			       ii, stat));
		
		return -1;
                }
            }

	//
	// join the senders
	//
	for ( int jj = 0; jj < 4; jj++ )
	    {
	    ACS_SHORT_LOG((LM_INFO, "joining sender task %d...", jj));
	    
	    pthread_join(tasks[jj], 0);
	    
	    ACS_SHORT_LOG((LM_INFO, "joined with sender task %d", jj));
            }
	

// END OF THREAD
		      

	ACS_SHORT_LOG((LM_INFO, "Disconnect the sender"));		      
	spt->disconnect();
	
	ACS_SHORT_LOG((LM_INFO, "Close the receivers"));	      

	distributor->closeReceiver();
	
	ACS_SHORT_LOG((LM_INFO, "Disconnect the receivers from the distributer"));	      
	for (i=0; i<recvsNum; i++)
	  {
	  distributor->multiDisconnect(rpt[i]);
	  } 
	
	
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
