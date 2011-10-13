
#include "bulkDataNTReaderListener.h"
#include "ACS_BD_Errors.h"
#include <ACS_DDS_Errors.h>
#include <iostream>
#include <iterator>

using namespace ACS_BD_Errors;
using namespace ACS_DDS_Errors;

BulkDataNTReaderListener::BulkDataNTReaderListener(const char* name, BulkDataCallback* cb)
: Logging::Loggable("BulkDataNT:"+string(name)),
				currentState_m(StartState),
				topicName_m(name),
				dataLength_m(0),
				frameCounter_m(0),
				totalFrames_m(0),
				logger_mp(0),
				loggerInitCount_m(0),
				callback_mp (cb)
{
	ACS_TRACE(__FUNCTION__);
	nextFrame_m=0;
	frameDataReader_mp=0;
	conseqErrorCount_m=0;
	maxConseqErrorCount_m = 4; //TBD now hardcoded, to be get from somewhere else
}//BulkDataNTReaderListener


BulkDataNTReaderListener::~BulkDataNTReaderListener ()
{
	ACS_TRACE(__FUNCTION__);
	if(logger_mp)
	{
		// we have to call "done" as many times as we call "init" !!
		for(unsigned int i=0; i<loggerInitCount_m; i++)
			LoggingProxy::done();
		delete logger_mp; // ...  but we have just one proxy object for all DDS reader threads
	}
}//~BulkDataNTReaderListener

void BulkDataNTReaderListener::on_data_available(DDS::DataReader* reader)
{
	DDS::ReturnCode_t retCode;
	DDS::SampleInfo si ;
	ACSBulkData::BulkDataNTFrame message;
	unsigned char tmpArray[ACSBulkData::FRAME_MAX_LEN];

	if (frameDataReader_mp==NULL)
	{
		frameDataReader_mp = ACSBulkData::BulkDataNTFrameDataReader::narrow(reader);
		if  (  frameDataReader_mp==NULL) {
			ACS_DDS_Errors::DDSNarrowFailedCompletion nerr(__FILE__, __LINE__, __FUNCTION__);
			nerr.setVariable("frameDataReader_mp");
			nerr.setNarrowType("ACSBulkData::BulkDataNTFrameDataReader");
			getLogger(); //force initialization of logging sys TBD changed
			callback_mp->onError(nerr);
			return;
		}//if
	}//if

	message.data.maximum(ACSBulkData::FRAME_MAX_LEN); //TBD constant from
	retCode = frameDataReader_mp->take_next_sample(message, si) ;

	if (retCode == DDS::RETCODE_OK)
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
					conseqErrorCount_m=0;
				}
				else //error
				{
					WrongFrameOrderCompletion wfo(__FILE__, __LINE__, __FUNCTION__);
					wfo.setDataType("BD_PARAM"); wfo.setState(currentState_m);
					wfo.setFlow(topicName_m.c_str()); wfo.setFrameCount(frameCounter_m);
					wfo.setTotalFrameCount(totalFrames_m); wfo.setFrameLength(message.data.length());
					getLogger(); //force initialization of logging sys TBD changed
					callback_mp->onError(wfo);
					increasConseqErrorCount();
				}//if-else
				break;
			}//	case ACSBulkData::BD_PARAM:
			case ACSBulkData::BD_DATA:
			{
				if (currentState_m==DataRcvState)
				{
					if (dataLength_m==0) // we get the first data frame
					{
						std::cout << " *************************   New sendData @ " << topicName_m << " *******************************" << std::endl;
						start_time = ACE_OS::gettimeofday();
						totalFrames_m = message.restDataLength+1;
						frameCounter_m = 0;
					}//if

					dataLength_m += message.data.length();
					frameCounter_m ++;

					if ( message.restDataLength>0)
					{
						// do we miss a frame ?
						if (nextFrame_m!=0 && nextFrame_m!=message.restDataLength)
						{
							FrameLostCompletion lde(__FILE__, __LINE__, __FUNCTION__);
							lde.setNextDataFrame(nextFrame_m);
							lde.setFrameCount(frameCounter_m);
							lde.setRestFrames(message.restDataLength);
							lde.setFrameLength(message.data.length());
							lde.setFlow(topicName_m.c_str());
							getLogger(); //force initialization of logging sys TBD changed
							callback_mp->onError(lde);
							increasConseqErrorCount();
							return; // ??
						}
						nextFrame_m = message.restDataLength-1;
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
					conseqErrorCount_m=0;
				}
				else //error
				{
					WrongFrameOrderCompletion wfo(__FILE__, __LINE__, __FUNCTION__);
					wfo.setDataType("BD_DATA"); wfo.setState(currentState_m);
					wfo.setFlow(topicName_m.c_str()); wfo.setFrameCount(frameCounter_m);
					wfo.setTotalFrameCount(totalFrames_m); wfo.setFrameLength(message.data.length());
					getLogger(); //force initialization of logging sys TBD changed
					callback_mp->onError(wfo);
					increasConseqErrorCount();
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
							ACS_BD_Errors::FrameLostCompletion lde(__FILE__, __LINE__, __FUNCTION__);
							lde.setNextDataFrame(nextFrame_m);
							lde.setFrameCount(frameCounter_m);
							lde.setRestFrames(message.restDataLength); // should be ??
							lde.setFrameLength(message.data.length()); // should be 0
							lde.setFlow(topicName_m.c_str());
							getLogger(); //force initialization of logging sys TBD changed
							callback_mp->onError(lde);
							increasConseqErrorCount();
						}//if
					}//if-else
				}else
				{
					if (currentState_m==StopState)
					{
						ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
								(LM_WARNING, "Stop (BD_STOP) arrived in stop state - will be ignored!"));
					}
					else //StartState
					{
						ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
								(LM_WARNING, "Stop (BD_STOP) arrived in start state: no parameter data (startSend) has arrived!"));
					}//if-else
				}
				// in all above warning/error case we call  user's cbStop()
				callback_mp->cbStop();
				conseqErrorCount_m=0;
				break;
			}//case ACSBulkData::BD_STOP
			default:
				conseqErrorCount_m++;
				UnknownDataTypeCompletion udt(__FILE__, __LINE__, __FUNCTION__);
				udt.setDataType(message.dataType);
				udt.setFrameCount(frameCounter_m);
				udt.setTotalFrameCount(totalFrames_m);
				getLogger(); //force initialization of logging sys TBD
				callback_mp->onError(udt);
			}//switch
		}else { //si.valie_data == false
			conseqErrorCount_m++;
			DDSSampleStateErrorCompletion ssErr(__FILE__, __LINE__, __FUNCTION__);
			ssErr.setInstanceState(si.instance_state);  //would be good if we can give also string value
			ssErr.setViewState(si.view_state);  //would be good if we can give also string value
			ssErr.setSampleState(si.sample_state);  //would be good if we can give also string value
			getLogger(); //force initialization of logging sys TBD
			callback_mp->onError(ssErr);
		}//if-else (si.valid_data)
	}
	else
	{
		conseqErrorCount_m++;
		DDSReturnErrorCompletion retErr(__FILE__, __LINE__, __FUNCTION__);
		retErr.setRetCode(retCode);  //would be good if we can give also string value
		getLogger(); //force initialization of logging sys TBD
		callback_mp->onError(retErr);
	}//if(status)
}//on_data_available

void BulkDataNTReaderListener::on_requested_deadline_missed(DDS::DataReader*, const DDS::RequestedDeadlineMissedStatus& )
{
	ACS_DDS_Errors::DDSDeadlineMissedCompletion dmerr(__FILE__, __LINE__, __FUNCTION__);
	getLogger(); //force initialization of logging sys TBD changed
	callback_mp->onError(dmerr);
}//on_requested_deadline_missed

void BulkDataNTReaderListener::on_requested_incompatible_qos(DDS::DataReader*, const DDS::RequestedIncompatibleQosStatus&)
{
	ACS_DDS_Errors::DDSIncompatibleQoSCompletion iqerr(__FILE__, __LINE__, __FUNCTION__);
	getLogger(); //force initialization of logging sys TBD changed
	callback_mp->onError(iqerr);
}//on_requested_incompatible_qos

void BulkDataNTReaderListener::on_liveliness_changed(DDS::DataReader*, const DDS::LivelinessChangedStatus& lcs)
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

void BulkDataNTReaderListener::on_subscription_matched(DDS::DataReader*, const DDS::SubscriptionMatchedStatus&)
{
	ACS_TRACE(__FUNCTION__);
}//on_subscription_matched

void BulkDataNTReaderListener::on_sample_rejected( DDS::DataReader*, const DDS::SampleRejectedStatus& srs)
{
	ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
			(LM_WARNING, "Sample Rejected: reason %d, change: %d, total: %d!",
			srs.last_reason, srs.total_count_change, srs.total_count));
}//on_sample_rejected

void BulkDataNTReaderListener::on_sample_lost(DDS::DataReader*, const DDS::SampleLostStatus& s)
{
	ACS_BD_Errors::SampleLostCompletion sle(__FILE__, __LINE__, __FUNCTION__);
	sle.setLostSamples(s.total_count_change);
	sle.setNextDataFrame(nextFrame_m);
	sle.setFrameCount(frameCounter_m);
	sle.setFlow(topicName_m.c_str());
	getLogger(); //force initialization of logging sys TBD changed
	callback_mp->onError(sle);
}//on_sample_lost

void BulkDataNTReaderListener::increasConseqErrorCount()
{
	conseqErrorCount_m++;
	if (conseqErrorCount_m>=maxConseqErrorCount_m)
	{

		ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
			(LM_ALERT, "Too many consequente errors: %d/%d", conseqErrorCount_m, maxConseqErrorCount_m));
		//TBD: disconnect
	}
}//increasConseqErroCount

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
