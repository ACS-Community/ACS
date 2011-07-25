
#include "bulkDataNTReaderListener.h"
#include <iostream>
#include <iterator>

int BulkDataNTReaderListener::sleep_period=0;

// Implementation skeleton constructor
BulkDataNTReaderListener::BulkDataNTReaderListener(const char* n, BulkDataCallback* cb)
  : num_reads_(-1),
  lost_packs(0),
  listName(n),
  data_length(0),
  callback_m (cb)
{
	next_sample=0;
	itera = 0;
	message_dr=0;
}

// Implementation skeleton destructor
BulkDataNTReaderListener::~BulkDataNTReaderListener ()
{
}

void BulkDataNTReaderListener::on_data_available(DDS::DataReader* reader)
 // throw (CORBA::SystemException)
{
 /*cout << ">---------------------------------------------" << endl;
 cout << "BDOSPLListenerImpl(" << listName << ")::on_data_available" << endl;
*/
	num_reads_ ++;
  try {


	  if (message_dr==NULL)
		  message_dr = ACSBulkData::BulkDataNTFrameDataReader::narrow(reader);

    if  (  message_dr==NULL) {
      cerr << "read: _narrow failed." << endl;
      exit(1);
    }

    ACSBulkData::BulkDataNTFrame message;


    DDS::ReturnCode_t status;
    DDS::SampleInfo si ;
    DDS::SampleInfoSeq infoSeq;
    ACSBulkData::BulkDataNTFramePtrSeq msgList;


      status = message_dr->take(&msgList, &infoSeq, DDS::LENGTH_UNLIMITED,
        DDS::ANY_SAMPLE_STATE, DDS::ANY_VIEW_STATE, DDS::ANY_INSTANCE_STATE);

    //status = message_dr->take_next_sample(message, si) ;

/*
      cout << "SampleInfo.sample_rank = " << si.sample_rank << endl;
      cout << "SampleInfo.instance_state = " << si.instance_state << endl;
      */
//      cout << "Taken " << msgList.length() << " samples. STATUS: " << status << endl;

for(unsigned i=0; i<msgList.size()/*length()*/; i++)
{

	si = *(infoSeq[i]);
	message = *(msgList[i]);
//	cout << "SampleInfo.instance_state = " << si.instance_state << endl;

    if (si.valid_data == 1)
    {

    	if (status == DDS::RETCODE_OK)
    	{

    		if (message.dataType==ACSBulkData::BD_PARAM)
    		{
    			cout << listName << " startSend: parameter size: " << message.data.size() << endl;
    			data_length = 0;
    			start_time = ACE_OS::gettimeofday();
    			// we still call the old cbReceive !!
    			ACE_Message_Block mb((const char*)&message.data[0], message.data.size()); // dirty ????
    			mb.length(message.data.size());
    			callback_m->cbStart(&mb);
    		}
//    		std::ostream_iterator<unsigned char> out_it(std::cout, "");

    		if (message.dataType==ACSBulkData::BD_DATA)
    		{
    			if (data_length==0)
    			{
    				std::cout << " *************************   New sendData @ " << listName << " *******************************" << std::endl;
    			}

    			cout << listName << " got " << message.data.size() << " data on " << endl;
    		//	std::cout << message.data[0];
//    			std::copy(message.data.begin(), message.data.end(), out_it);
// std::cout << std::endl;
    			unsigned int sum=0;
    			for(unsigned int j=0; j<message.data.size(); j++)
    				sum+=message.data[j];
// std::cout << "Sum: " << sum << " " << message.data.size() << std::endl;

    			  if (sleep_period!=0)
    			  {
    				  	  cout << listName << " Rest:" << message.restDataLength << " Going sleep for: " << sleep_period << endl;
    				  	  //cout <<  message.data.length() << endl;
    				  	 //cout <<  message.restDataLength << endl;
    				  	 usleep(sleep_period);
    			  }
    			data_length += message.data.size();
/* simulate seg fault
    			if (data_length>100000)
    			{
    			char *tt=0;
    			printf("XXX %s\n", tt);
    			printf("crash\n");
    			ACE_Time_Value *t=0;
    			t->sec();
    			DDS::DataReaderQos *ddr_qos=0;
    			ddr_qos->reliability.kind = 0;
    			printf("after crash\n");
    			}
*/

    			if ( message.restDataLength>0)
    			{
    				if (next_sample!=0 && next_sample!=message.restDataLength)
    				{
    					cerr << "#" << itera << "    " << ">>>> missed sample #: " << message.restDataLength << " for " << listName << endl;
    				}

    				next_sample=message.restDataLength-1;
    			}
    			else //message.restDataLength==0
    			{
    				ACE_Time_Value elapsed_time = ACE_OS::gettimeofday() - start_time;
    				cout <<	listName << " Received all data from sendData: " << data_length << " Bytes in ";
    				cout <<(elapsed_time.sec()+( elapsed_time.usec() / 1000000.0 ));
    				cout << "secs. => Rate: ";
    				cout << ((data_length/(1024.0*1024.0))/(elapsed_time.sec()+( elapsed_time.usec() / 1000000.0 ))) << "MBytes/sec" << endl;

    				DDS::SampleLostStatus s;
    				reader->get_sample_lost_status(&s);
    				cerr << listName << " LOST samples: \t\t total_count: " << s.total_count << " total_count_change: " << s.total_count_change << endl;
    				itera++;
    				data_length = 0;
    			}
    			ACE_Message_Block mb ((char*)&message.data[0], message.data.size()); // dirty ????
    			mb.length(message.data.size());
    			callback_m->cbReceive(&mb);
    		} else if (message.dataType==ACSBulkData::BD_STOP)
    		{
    			cout << "===============================================================" << endl;
    			itera =0;
    			callback_m->cbStop();
    		}

    		/*
        cout << "Message: length      = " << message.data.length() << endl
        << " transactionnum " << message.transactionnum << endl;

        if (num_reads_ != message.transactionnum)
        {
        	cerr << "We lost message #" << num_reads_ << endl;
        	num_reads_ = message.transactionnum;
        	lost_packs ++;
        }//if
        if (num_reads_>=99)
        {
        	cout << "We lost in total: " << lost_packs << endl;
        }
        */
      }
      else if (si.instance_state == DDS::NOT_ALIVE_DISPOSED_INSTANCE_STATE)
      {
        cout << "instance is disposed" << endl;
      }
      else if (si.instance_state == DDS::NOT_ALIVE_NO_WRITERS_INSTANCE_STATE)
      {
        cout << "instance is unregistered" << endl;
      }
      else
      {
    	  cout << "BulkDataNTReaderListener(" << listName << ")::on_data_available:";
    	  cout << " received unknown instance state " << si.instance_state;
    	  cout << endl;
      }

    } else if (status == DDS::RETCODE_NO_DATA) {
      cerr << "ERROR: reader received DDS::RETCODE_NO_DATA!" << endl;
    } else {
      cerr << "ERROR: read Message: Error: " <<  status << endl;
    }

}//for
	status = message_dr->return_loan(&msgList, &infoSeq);
// cout << "<---------------------------------------------" << endl;

  } catch (.../*CORBA::Exception& e*/) {
    cerr << "Exception caught in read:" << endl;// << e << endl;
    exit(1);
  }

}

void BulkDataNTReaderListener::on_requested_deadline_missed (
    DDS::DataReader*,
    DDS::RequestedDeadlineMissedStatus )
 // throw (CORBA::SystemException)
{
  cerr << "BulkDataNTReaderListener(" << listName << ")::on_requested_deadline_missed" << endl;
}

void BulkDataNTReaderListener::on_requested_incompatible_qos (
    DDS::DataReader*,
    DDS::RequestedIncompatibleQosStatus)
 // throw (CORBA::SystemException)
{
  cerr << "BulkDataNTReaderListener(" << listName << ")::on_requested_incompatible_qos" << endl;
}

void BulkDataNTReaderListener::on_liveliness_changed (
    DDS::DataReader*,
    DDS::LivelinessChangedStatus lcs)
// throw (CORBA::SystemException)
{
  cerr << "BulkDataNTReaderListener(" << listName << ")::on_liveliness_changed:" << endl;
  cerr << "    alive_count: " << lcs.alive_count << endl;
  cerr << "    not_alive_count: " << lcs.not_alive_count << endl;
  cerr << "    alive_count_change: " << lcs.alive_count_change << endl;
  cerr << "    not_alive_count_change: " << lcs.not_alive_count_change << endl;


  cout << "Received:"  << data_length << endl;
}

void BulkDataNTReaderListener::on_subscription_matched (
    DDS::DataReader*,
    DDS::SubscriptionMatchedStatus )
 // throw (CORBA::SystemException)
{
  cerr << "BulkDataNTReaderListener(" << listName << ")::on_subscription_match" << endl;
  num_reads_ = -1;
   lost_packs = 0;
}

void BulkDataNTReaderListener::on_sample_rejected(
    DDS::DataReader*,
    DDS::SampleRejectedStatus srs)
//  throw (CORBA::SystemException)
{
  cerr << "BulkDataNTReaderListener(" << listName << "::on_sample_rejected SampleRejectedStatus.last_reason: ";
  cerr << srs.last_reason << " SampleRejectedStatus.total_count_change: " << srs.total_count_change;
  cerr << " SampleRejectedStatus.total_count: " << srs.total_count << endl;
}

void BulkDataNTReaderListener::on_sample_lost(
  DDS::DataReader*,
  DDS::SampleLostStatus s)
 // throw (CORBA::SystemException)
{
  cerr << endl << endl << "BulkDataNTReaderListener(" << listName << "::on_sample_lost: ";
  cerr << "total_count: " << s.total_count << " total_count_change: " << s.total_count_change << endl << endl;
}
