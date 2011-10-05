
#include "bulkDataNTReaderListener.h"
#include "ACS_BD_Errors.h"
#include <iostream>
#include <iterator>

int BulkDataNTReaderListener::sleep_period=0;


BulkDataNTReaderListener::BulkDataNTReaderListener(const char* name, BulkDataCallback* cb)
: Logging::Loggable("BulkDataNT:"+string(name)),
				currentState_m(StartState),
				lost_packs(0),
				topicName_m(name),
				dataLength_m(0),
				frameCounter_m(0),
				totalFrames_m(0),
				logger_mp(0),
				loggerInitCount_m(0),
				callback_mp (cb)
{
	ACS_TRACE(__FUNCTION__);
	nestSample_m=0;
	frameDataReader_mp=0;
}//BulkDataNTReaderListener


BulkDataNTReaderListener::~BulkDataNTReaderListener ()
{
	ACS_TRACE(__FUNCTION__);
	if(logger_mp)
	{
		// we have to call doen as many times as we call init !!
		for(unsigned int i=0; i<loggerInitCount_m; i++)
			LoggingProxy::done();
		delete logger_mp; // ...  but we have just one proxy object for all DDS reader threads
	}
}//~BulkDataNTReaderListener

void BulkDataNTReaderListener::on_data_available(DDS::DataReader* reader)
{
	DDS::ReturnCode_t status;
	DDS::SampleInfo si ;
	ACSBulkData::BulkDataNTFrame message;
	unsigned char tmpArray[ACSBulkData::FRAME_MAX_LEN];

	if (frameDataReader_mp==NULL)
	{
		frameDataReader_mp = ACSBulkData::BulkDataNTFrameDataReader::narrow(reader);
		if  (  frameDataReader_mp==NULL) {
			cerr << "read: _narrow failed." << endl;
			exit(1);
		}//if
	}//if

	message.data.maximum(ACSBulkData::FRAME_MAX_LEN); //TBD constant from
	status = frameDataReader_mp->take_next_sample(message, si) ;

	if (status == DDS::RETCODE_OK)
	{
		if (si.valid_data == true)
		{
			switch(message.dataType)
			{
			case ACSBulkData::BD_PARAM:
			{
				cout << topicName_m << " startSend: parameter size: " << message.data.length() << endl;
				if (currentState_m==StartState || currentState_m==StopState)
				{
					dataLength_m = 0;
					frameCounter_m = 0;
					currentState_m = DataRcvState;
					message.data.to_array(tmpArray, message.data.length());
					callback_mp->cbStart(tmpArray, message.data.length());
				}
				else //error
				{
					ACS_BD_Errors::WrongFrameOrderCompletion wfo(__FILE__, __LINE__, __FUNCTION__);
					wfo.setDataType("BD_PARAM");
					wfo.setState(currentState_m);
					wfo.setFlow(topicName_m.c_str());
					wfo.setFrameCount(frameCounter_m);
					wfo.setTotalFrameCount(totalFrames_m);
					wfo.setFrameLength(message.data.length());
					getLogger(); //force initalization of logging sys TBD changed
					callback_mp->onError(wfo);
				}
				break;
			}//	case ACSBulkData::BD_PARAM:
			case ACSBulkData::BD_DATA:
			{
				if (currentState_m==DataRcvState)
				{
					if (dataLength_m==0) // we get first data frame
					{
						std::cout << " *************************   New sendData @ " << topicName_m << " *******************************" << std::endl;
						start_time = ACE_OS::gettimeofday();
						totalFrames_m = message.restDataLength+1;
						frameCounter_m = 0;
					}

					dataLength_m += message.data.length();
					frameCounter_m ++;

					if ( message.restDataLength>0)
					{
						if (nestSample_m!=0 && nestSample_m!=message.restDataLength)
						{
							cerr << "ERROR: " << topicName_m << "    " << ">>>> missed sample #: " << message.restDataLength << " for " << topicName_m << endl;
						}
						nestSample_m=message.restDataLength-1;
					}
					else //message.restDataLength==0 what means the last frame
					{
						ACE_Time_Value elapsed_time = ACE_OS::gettimeofday() - start_time;
						cout <<	topicName_m << " Received all data from sendData: " << dataLength_m << " Bytes in ";
						cout <<(elapsed_time.sec()+( elapsed_time.usec() / 1000000.0 ));
						cout << "secs. => Rate: ";
						cout << ((dataLength_m/(1024.0*1024.0))/(elapsed_time.sec()+( elapsed_time.usec() / 1000000.0 ))) << "MBytes/sec" << endl;

						DDS::SampleLostStatus s;
						reader->get_sample_lost_status(s);
						cerr << topicName_m << " LOST samples: \t\t total_count: " << s.total_count << " total_count_change: " << s.total_count_change << endl;
						dataLength_m = 0;
					}

					message.data.to_array(tmpArray, message.data.length());
					callback_mp->cbReceive(tmpArray, message.data.length());
				}
				else //error
				{
					ACS_BD_Errors::WrongFrameOrderCompletion wfo(__FILE__, __LINE__, __FUNCTION__);
					wfo.setDataType("BD_DATA");
					wfo.setState(currentState_m);
					wfo.setFlow(topicName_m.c_str());
					wfo.setFrameCount(frameCounter_m);
					wfo.setTotalFrameCount(totalFrames_m);
					wfo.setFrameLength(message.data.length());
					getLogger(); //force initalization of logging sys TBD changed
					callback_mp->onError(wfo);
				}
				break;
			}//case ACSBulkData::BD_DATA
			case ACSBulkData::BD_STOP:
			{
				if (currentState_m==DataRcvState)
				{
					currentState_m = StopState;
					cout << "===============================================================" << endl;
					if (dataLength_m==0)
					{
						ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
								(LM_WARNING, "Got stop (BD_STOP) before any data (sendData)!"));
					}else
					{
						if (frameCounter_m != totalFrames_m)
						{
							std::cerr << "Not all data frames arrived: " << frameCounter_m << "/" << totalFrames_m << endl;
						}
					}
//					std::cout << "Arrived: " << frameCounter_m << "/" << totalFrames_m << endl;
					callback_mp->cbStop();
				}else
				{
					if (currentState_m==StopState)
					{
						ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
								(LM_WARNING, "Stop (BD_STOP) arrived in stop state - will be ignored!"));
						//std::cerr << "WARNING: Stop (BD_STOP) arrived in stop state - ignored!" << std::endl;
					}
					else //StartState
					{
						ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
								(LM_WARNING, "Stop (BD_STOP) arrived in start state: no parameter data (startSend) has arrived!"));
						//std::cerr << "WARNING: Stop (BD_STOP) arrived in start state: no data has been sent after start!" << std::endl;
					}//if-else
				}
				break;
			}//case ACSBulkData::BD_STOP
			default:
				ACS_BD_Errors::UnknownDataTypeCompletion udt(__FILE__, __LINE__, __FUNCTION__);
				udt.setDataType(message.dataType);
				udt.setFrameCount(frameCounter_m);
				udt.setTotalFrameCount(totalFrames_m);
				getLogger(); //force initalization of logging sys TBD
				callback_mp->onError(udt);
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
			cout << "BulkDataNTReaderListener(" << topicName_m << ")::on_data_available:";
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
	cerr << "BulkDataNTReaderListener(" << topicName_m << ")::on_requested_deadline_missed" << endl;
}

void BulkDataNTReaderListener::on_requested_incompatible_qos (
		DDS::DataReader*,
	const DDS::RequestedIncompatibleQosStatus&)
{
	cerr << "BulkDataNTReaderListener(" << topicName_m << ")::on_requested_incompatible_qos" << endl;
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
							callback_mp->getFlowName(), callback_mp->getStreamName(),
							lcs.alive_count));
			callback_mp->onSenderConnect();
		}//for
	}else
	{
		for(int i=lcs.alive_count_change; i<0; i++)
		{
			ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
					(LM_INFO, "A sender has disconnected to flow: %s of the stream: %s. Total alive connection(s): %d",
							callback_mp->getFlowName(), callback_mp->getStreamName(),
							lcs.alive_count));
			callback_mp->onSenderDisconnect();
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
	cerr << "BulkDataNTReaderListener(" << topicName_m << "::on_sample_rejected SampleRejectedStatus.last_reason: ";
	cerr << srs.last_reason << " SampleRejectedStatus.total_count_change: " << srs.total_count_change;
	cerr << " SampleRejectedStatus.total_count: " << srs.total_count << endl;
}//on_sample_rejected

void BulkDataNTReaderListener::on_sample_lost(
		DDS::DataReader*, const DDS::SampleLostStatus& s)
{
	cerr << endl << endl << "BulkDataNTReaderListener(" << topicName_m << "::on_sample_lost: ";
	cerr << "total_count: " << s.total_count << " total_count_change: " << s.total_count_change << endl << endl;
}//on_sample_lost


Logging::Logger::LoggerSmartPtr BulkDataNTReaderListener::getLogger ()
{
	// this code is a bit dirty, but wee need to initialize loggerproxy per thread
	//isThreadInit return 0 if it is initialized !!!
	if (LoggingProxy::isInitThread() )
	{
		//TBD here we have to set centralized loggger as well, but we need some support from logging
		if (logger_mp==0) //if we do not have a logger we create one for all DDS threads
			logger_mp = new LoggingProxy(0, 0, 31);
		LoggingProxy::init(logger_mp);
		loggerInitCount_m++; // we initialized Proxy another time
	}
	return Logging::Loggable::getLogger();
}//getLogger
