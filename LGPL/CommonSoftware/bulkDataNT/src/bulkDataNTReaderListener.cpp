
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
{
	DDS::ReturnCode_t status;
	DDS::SampleInfo si ;
	ACSBulkData::BulkDataNTFrame message;
	unsigned char tmpArray[ACSBulkData::FRAME_MAX_LEN];

	num_reads_ ++;

	if (message_dr==NULL)	message_dr = ACSBulkData::BulkDataNTFrameDataReader::narrow(reader);

	if  (  message_dr==NULL) {
		cerr << "read: _narrow failed." << endl;
		exit(1);
	}

	message.data.maximum(ACSBulkData::FRAME_MAX_LEN); //TBD constant from
	status = message_dr->take_next_sample(message, si) ;

	if (si.valid_data == 1)
	{
		if (status == DDS::RETCODE_OK)
		{
			switch(message.dataType)
			{
			case ACSBulkData::BD_PARAM:
			{
				cout << listName << " startSend: parameter size: " << message.data.length() << endl;
				data_length = 0;
				start_time = ACE_OS::gettimeofday();
				message.data.to_array(tmpArray, message.data.length());
				callback_m->cbStart(tmpArray, message.data.length());
				break;
			}//	case ACSBulkData::BD_PARAM:
			case ACSBulkData::BD_DATA:
			{
				if (data_length==0)
				{
					std::cout << " *************************   New sendData @ " << listName << " *******************************" << std::endl;
				}

				cout << listName << " got " << message.data.length() << " data on " << endl;
				//	std::cout << message.data[0];
				//    			std::copy(message.data.begin(), message.data.end(), out_it);
				// std::cout << std::endl;
				unsigned int sum=0;
				for(int j=0; j<message.data.length(); j++)
					sum+=message.data[j];
				// std::cout << "Sum: " << sum << " " << message.data.size() << std::endl;

				if (sleep_period!=0)
				{
					cout << listName << " Rest:" << message.restDataLength << " Going sleep for: " << sleep_period << endl;
					//cout <<  message.data.length() << endl;
					//cout <<  message.restDataLength << endl;
					usleep(sleep_period);
				}
				data_length += message.data.length();
				/* simulate seg fault
    			if (data_length>100000) {
    			char *tt=0;
    			printf("XXX %s\n", tt);
    			printf("crash\n");
    			ACE_Time_Value *t=0;
    			t->sec();
    			DDS::DataReaderQos *ddr_qos=0;
    			ddr_qos->reliability.kind = 0;
    			printf("after crash\n");
    			} */
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
					reader->get_sample_lost_status(s);
					cerr << listName << " LOST samples: \t\t total_count: " << s.total_count << " total_count_change: " << s.total_count_change << endl;
					itera++;
					data_length = 0;
				}

				message.data.to_array(tmpArray, message.data.length());
				callback_m->cbReceive(tmpArray, message.data.length());
				break;
			}//case ACSBulkData::BD_DATA
			case ACSBulkData::BD_STOP:
			{	cout << "===============================================================" << endl;
				itera =0;
				callback_m->cbStop();
				break;
			}//case ACSBulkData::BD_STOP
			default:
				cerr << "Unknown message.dataType: " << message.dataType << endl;
			}//switch
		}else if (status == DDS::RETCODE_NO_DATA) {
			cerr << "ERROR: reader received DDS::RETCODE_NO_DATA!" << endl;
		}
	}else if (si.instance_state == DDS::NOT_ALIVE_DISPOSED_INSTANCE_STATE)
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
}//on_data_available

void BulkDataNTReaderListener::on_requested_deadline_missed (
		DDS::DataReader*,
		const DDS::RequestedDeadlineMissedStatus& )
{
	cerr << "BulkDataNTReaderListener(" << listName << ")::on_requested_deadline_missed" << endl;
}

void BulkDataNTReaderListener::on_requested_incompatible_qos (
		DDS::DataReader*,
		const DDS::RequestedIncompatibleQosStatus&)
{
	cerr << "BulkDataNTReaderListener(" << listName << ")::on_requested_incompatible_qos" << endl;
}

void BulkDataNTReaderListener::on_liveliness_changed (
		DDS::DataReader*,
		const DDS::LivelinessChangedStatus& lcs)
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
		const DDS::SubscriptionMatchedStatus& )
{
	cerr << "BulkDataNTReaderListener(" << listName << ")::on_subscription_match" << endl;
	num_reads_ = -1;
	lost_packs = 0;
}

void BulkDataNTReaderListener::on_sample_rejected(
		DDS::DataReader*,
		const DDS::SampleRejectedStatus& srs)
{
	cerr << "BulkDataNTReaderListener(" << listName << "::on_sample_rejected SampleRejectedStatus.last_reason: ";
	cerr << srs.last_reason << " SampleRejectedStatus.total_count_change: " << srs.total_count_change;
	cerr << " SampleRejectedStatus.total_count: " << srs.total_count << endl;
}

void BulkDataNTReaderListener::on_sample_lost(
		DDS::DataReader*,
		const DDS::SampleLostStatus& s)
{
	cerr << endl << endl << "BulkDataNTReaderListener(" << listName << "::on_sample_lost: ";
	cerr << "total_count: " << s.total_count << " total_count_change: " << s.total_count_change << endl << endl;
}
