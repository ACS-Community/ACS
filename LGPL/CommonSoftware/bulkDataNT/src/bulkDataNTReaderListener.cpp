
#include "bulkDataNTReaderListener.h"
#include <iostream>
#include <iterator>

int BulkDataNTReaderListener::sleep_period=0;


BulkDataNTReaderListener::BulkDataNTReaderListener(const char* name, BulkDataCallback* cb)
: Logging::Loggable("BulkDataNT:"+string(name)),
				currentState_m(StartState),
				lost_packs(0),
				flowName_m(name),
				data_length(0),
				logger_mp(0),
				callback_m (cb)
{
	next_sample=0;
	message_dr=0;
}//BulkDataNTReaderListener


BulkDataNTReaderListener::~BulkDataNTReaderListener ()
{
	if(logger_mp)
	{
		LoggingProxy::done();
		delete logger_mp;
	}
}//~BulkDataNTReaderListener

void BulkDataNTReaderListener::on_data_available(DDS::DataReader* reader)
{
	DDS::ReturnCode_t status;
	DDS::SampleInfo si ;
	ACSBulkData::BulkDataNTFrame message;
	unsigned char tmpArray[ACSBulkData::FRAME_MAX_LEN];


	if (message_dr==NULL)	message_dr = ACSBulkData::BulkDataNTFrameDataReader::narrow(reader);

	if  (  message_dr==NULL) {
		cerr << "read: _narrow failed." << endl;
		exit(1);
	}

	message.data.maximum(ACSBulkData::FRAME_MAX_LEN); //TBD constant from
	status = message_dr->take_next_sample(message, si) ;

	if (status == DDS::RETCODE_OK)
	{
		if (si.valid_data == 1)
		{
			switch(message.dataType)
			{
			case ACSBulkData::BD_PARAM:
			{
				cout << flowName_m << " startSend: parameter size: " << message.data.length() << endl;
				if (currentState_m==StartState || currentState_m==StopState)
				{
					data_length = 0;
					currentState_m = DataRcvState;
					message.data.to_array(tmpArray, message.data.length());
					callback_m->cbStart(tmpArray, message.data.length());
				}
				else
				{
					std::cerr << "ERROR: Parameter of length " << message.data.length();
					std::cerr << " has arrived (BD_PARAM) in state: " << currentState_m << " on flow: " << flowName_m << std::endl;
				}
				break;
			}//	case ACSBulkData::BD_PARAM:
			case ACSBulkData::BD_DATA:
			{
				if (currentState_m==DataRcvState)
				{
					if (data_length==0)
					{
						std::cout << " *************************   New sendData @ " << flowName_m << " *******************************" << std::endl;
						start_time = ACE_OS::gettimeofday();
					}

					data_length += message.data.length();

					if ( message.restDataLength>0)
					{
						if (next_sample!=0 && next_sample!=message.restDataLength)
						{
							cerr << "ERROR: " << flowName_m << "    " << ">>>> missed sample #: " << message.restDataLength << " for " << flowName_m << endl;
						}
						next_sample=message.restDataLength-1;
					}
					else //message.restDataLength==0
					{
						ACE_Time_Value elapsed_time = ACE_OS::gettimeofday() - start_time;
						cout <<	flowName_m << " Received all data from sendData: " << data_length << " Bytes in ";
						cout <<(elapsed_time.sec()+( elapsed_time.usec() / 1000000.0 ));
						cout << "secs. => Rate: ";
						cout << ((data_length/(1024.0*1024.0))/(elapsed_time.sec()+( elapsed_time.usec() / 1000000.0 ))) << "MBytes/sec" << endl;

						DDS::SampleLostStatus s;
						reader->get_sample_lost_status(s);
						cerr << flowName_m << " LOST samples: \t\t total_count: " << s.total_count << " total_count_change: " << s.total_count_change << endl;
						data_length = 0;
					}

					message.data.to_array(tmpArray, message.data.length());
					callback_m->cbReceive(tmpArray, message.data.length());
				}
				else
				{
					std::cerr << "ERROR: Data of length " << message.data.length();
					std::cerr << " arrived (BD_DATA) in state: " << currentState_m << " on flow: " << flowName_m << std::endl;
				}
				break;
			}//case ACSBulkData::BD_DATA
			case ACSBulkData::BD_STOP:
			{
				if (currentState_m==DataRcvState)
				{
					currentState_m = StopState;
					cout << "===============================================================" << endl;
					callback_m->cbStop();
				}else
					if (currentState_m==StopState)
					{
						std::cerr << "WARNING: Stop (BD_STOP) arrived in stop state - ignored!" << std::endl;
					}
					else //StartState
					{
						std::cerr << "WARNING: Stop (BD_STOP) arrived in start state: no data has been sent after start!" << std::endl;
					}
				break;
			}//case ACSBulkData::BD_STOP
			default:
				cerr << "Unknown message.dataType: " << message.dataType << endl;
			}//switch
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
			cout << "BulkDataNTReaderListener(" << flowName_m << ")::on_data_available:";
			cout << " received unknown instance state " << si.instance_state;
			cout << endl;
		}
	}else if (status == DDS::RETCODE_NO_DATA) {
		cerr << "ERROR: on_data_available DDS::RETCODE_NO_DATA!" << endl;
	}
	else
	{
		cerr << "ERROR: on_data_available status: " << status << endl;
	}//if(status)
}//on_data_available

void BulkDataNTReaderListener::on_requested_deadline_missed (
		DDS::DataReader*,
		const DDS::RequestedDeadlineMissedStatus& )
{
	cerr << "BulkDataNTReaderListener(" << flowName_m << ")::on_requested_deadline_missed" << endl;
}

void BulkDataNTReaderListener::on_requested_incompatible_qos (
		DDS::DataReader*,
	const DDS::RequestedIncompatibleQosStatus&)
{
	cerr << "BulkDataNTReaderListener(" << flowName_m << ")::on_requested_incompatible_qos" << endl;
}

void BulkDataNTReaderListener::on_liveliness_changed (
		DDS::DataReader*, const DDS::LivelinessChangedStatus& lcs)
{
	if (lcs.alive_count_change>0)
	{
		for(int i=0; i<lcs.alive_count_change; i++)
		{
			ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
					(LM_INFO, "A new sender has connected to flow: %s of the stream: %s. Total alive connection(s): %d",
							callback_m->getFlowName(), callback_m->getStreamName(),
							lcs.alive_count));
			callback_m->onSenderConnect();
		}//for
	}else
	{
		for(int i=lcs.alive_count_change; i<0; i++)
		{
			ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
					(LM_INFO, "A sender has disconnected to flow: %s of the stream: %s. Total alive connection(s): %d",
							callback_m->getFlowName(), callback_m->getStreamName(),
							lcs.alive_count));
			callback_m->onSenderDisconnect();
		}//for
	}//if-else
}//on_liveliness_changed

void BulkDataNTReaderListener::on_subscription_matched (
		DDS::DataReader*, const DDS::SubscriptionMatchedStatus& )
{
	ACS_TRACE(__FUNCTION__);
}//on_subscription_matched

void BulkDataNTReaderListener::on_sample_rejected(
		DDS::DataReader*, const DDS::SampleRejectedStatus& srs)
{
	cerr << "BulkDataNTReaderListener(" << flowName_m << "::on_sample_rejected SampleRejectedStatus.last_reason: ";
	cerr << srs.last_reason << " SampleRejectedStatus.total_count_change: " << srs.total_count_change;
	cerr << " SampleRejectedStatus.total_count: " << srs.total_count << endl;
}//on_sample_rejected

void BulkDataNTReaderListener::on_sample_lost(
		DDS::DataReader*, const DDS::SampleLostStatus& s)
{
	cerr << endl << endl << "BulkDataNTReaderListener(" << flowName_m << "::on_sample_lost: ";
	cerr << "total_count: " << s.total_count << " total_count_change: " << s.total_count_change << endl << endl;
}//on_sample_lost


Logging::Logger::LoggerSmartPtr BulkDataNTReaderListener::getLogger ()
{
	if (logger_mp==0)
	{
		//TBD here we have to set centralized loggger as well, but we need some support from logging
		logger_mp = new LoggingProxy(0, 0, 31);
		LoggingProxy::init(logger_mp);
	}
	return Logging::Loggable::getLogger();
}//getLogger
