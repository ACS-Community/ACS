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
* "@(#) $Id: bulkDataNTSenderFlow.cpp,v 1.57 2013/03/05 15:19:33 gchiozzi Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/

#include "bulkDataNTSenderFlow.h"
#include <iostream>
#include "ACS_BD_Errors.h"

using namespace AcsBulkdata;
using namespace std;
using namespace ACS_DDS_Errors;
using namespace ACS_BD_Errors;

class HangingFinder
{
  public:
    HangingFinder(unsigned long  thresholdMSec, char *location)
	{
	    thresholdMSec_m  = thresholdMSec;
	    startTime_m      = ACE_OS::gettimeofday();  

	    strncpy(location_m, location, 19);
	    location_m[19]='\0';
	}

    ~HangingFinder() { Evaluate(); }

    void Evaluate()
	{
	    ACE_Time_Value elapsedTime_m = ACE_OS::gettimeofday() - startTime_m;
	    if( elapsedTime_m.msec() > thresholdMSec_m)
		{
		counterNo++;
		ACS_SHORT_LOG((LM_DEBUG, "XXX %s - Detected hanging time of %ld msec, consec bad: %d, ok: %d", 
			       location_m, elapsedTime_m.msec(),counterNo, counterYes));
		counterYes=0;
		}
	    else
		{
		counterYes++;
		counterNo=0;
		}
	}

  private:
    ACE_Time_Value startTime_m;
    unsigned long  thresholdMSec_m;
    char           location_m[20];
    static int     counterNo;        
    static int     counterYes;        
};

int HangingFinder::counterNo  = 0;
int HangingFinder::counterYes = 0;


const char *BulkDataNTSenderFlow::state2String[] = {"StartState", "DataSendState", "StopState" };

BulkDataNTSenderFlow::BulkDataNTSenderFlow(BulkDataNTSenderStream *senderStream,
    const char* flowName,
    const SenderFlowConfiguration &sndCfg,
    BulkDataNTSenderFlowCallback *cb, bool releaseCB) :
    BulkDataNTFlow(flowName),
    currentState_m(StartState),
    senderStream_m(senderStream),
    senderFlowCfg_m(sndCfg),
    callback_m(cb), releaseCB_m(releaseCB),
    ddsPublisher_m(0), ddsTopic_m(0), writerReaderListener_m(0), ddsDataWriter_m(0), frame_m(0)
{
  AUTO_TRACE(__PRETTY_FUNCTION__);
  std::string streamName, topicName;
  streamName = senderStream_m->getName();
  topicName =  streamName + "#" + flowName_m;
  ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__, (LM_INFO, "Going to create Sender Flow: %s @ stream: %s ...", flowName_m.c_str(), streamName.c_str()));

  callback_m->setStreamName(streamName.c_str());
  callback_m->setFlowName(flowName);

  senderStream_m->addDDSQoSProfile(senderFlowCfg_m, topicName.c_str());

  // should be reactor to have just one object for communication !! DDSDataWriter or similar
  ddsPublisher_m = new BulkDataNTDDSPublisher(senderStream_m->getDDSParticipant(), senderFlowCfg_m);

  ddsTopic_m = ddsPublisher_m->createDDSTopic(topicName.c_str());

  writerReaderListener_m = new BulkDataNTWriterListener(topicName.c_str(), callback_m);
  ddsDataWriter_m= ddsPublisher_m->createDDSWriter(ddsTopic_m, writerReaderListener_m);

  //RTI probably is enough to create frame once
  frame_m = ACSBulkData::BulkDataNTFrameTypeSupport::create_data();
  if (frame_m == 0)
    {
      //TBD delete dw/topic/publisher
      DDSCreateDataProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
      ex.setDataType("ACSBulkData::BulkDataNTFrameTypeSupport");
      throw ex;
    }//if

  frame_m->data.maximum(0); //we need if we want to use loaning

  setACKsTimeout(senderFlowCfg_m.getACKsTimeout());
  setThrottling(senderFlowCfg_m.getThrottling());
  ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__, (LM_INFO, "Sender Flow: %s @ stream: %s has been created.", flowName_m.c_str(), streamName.c_str()));
}//BulkDataNTSenderFlow


BulkDataNTSenderFlow::~BulkDataNTSenderFlow()
{
	AUTO_TRACE(__PRETTY_FUNCTION__);

	// Ensure the stopSend has been invoked to left the receiver in the
	// proper state to get another block of data
	if (currentState_m==DataRcvState) {
		ACS_LOG(LM_RUNTIME_CONTEXT, __PRETTY_FUNCTION__, (LM_WARNING, "BulkDataNTSenderFlow %s did not send stopData to the receiver",flowName_m.c_str()));
		try {
			this->stopSend();
		} catch (StopSendErrorExImpl &ssEx) {
			ACS_LOG(LM_RUNTIME_CONTEXT, __PRETTY_FUNCTION__, (LM_ERROR, "Error in flow %s while trying to force a stopSend",flowName_m.c_str()));
			ssEx.log();
		} catch (...) {
			ACS_LOG(LM_RUNTIME_CONTEXT, __PRETTY_FUNCTION__, (LM_ERROR, "Generic error in flow %s while trying to force a stopSend",flowName_m.c_str()));
		}
	}

	DDS::ReturnCode_t ret;
	std::string streamName = senderStream_m->getName();
	// no matter what happen we remove flow from the map
	senderStream_m->removeFlowFromMap(flowName_m.c_str());

	// remove QoS from DDS factory if any
	senderStream_m->removeDDSQoSProfile(senderFlowCfg_m);

	ret = ACSBulkData::BulkDataNTFrameTypeSupport::delete_data(frame_m);
	if (ret != DDS::RETCODE_OK) {
		ACS_LOG(LM_RUNTIME_CONTEXT, __PRETTY_FUNCTION__, (LM_ERROR, "BulkDataNTFrameTypeSupport::delete_data failed"));
	}//if

	// this part can go to BulkDataNTDDSPublisher, anyway we need to refactor
	DDS::DomainParticipant *participant = senderStream_m->getDDSParticipant();
	if (participant!=0)
	{
		ddsPublisher_m->destroyDDSWriter(ddsDataWriter_m);
		ddsDataWriter_m = 0;
		delete writerReaderListener_m;
		writerReaderListener_m = 0;

		ddsPublisher_m->destroyDDSTopic(ddsTopic_m);
		ddsTopic_m=0;
	}
	else
	{
		ACS_LOG(LM_RUNTIME_CONTEXT, __PRETTY_FUNCTION__, (LM_ERROR, "Problem deleting data write and topic because participant is NULL"));
	}
	delete ddsPublisher_m;
	ddsPublisher_m=0;
	if (releaseCB_m) delete callback_m;

	ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__, (LM_INFO, "Sender Flow: %s @ stream: %s has been destroyed.", flowName_m.c_str(), streamName.c_str()));
}//~BulkDataNTSenderFlow


unsigned int BulkDataNTSenderFlow::getNumberOfReceivers()
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	DDS::ReliableReaderActivityChangedStatus status;

	ddsDataWriter_m->get_reliable_reader_activity_changed_status(status);
	return status.active_count;
}//getNumberOfReceivers

void BulkDataNTSenderFlow::setACKsTimeout(double ACKsTimeout)
{
	DDS::Long ackTOSec = static_cast<DDS::Long>(ACKsTimeout);
	DDS::Long ackTONanosec = 1000000 * static_cast<DDS::Long>(ACKsTimeout - ackTOSec);

	ackTimeout_m.sec = ackTOSec;
	ackTimeout_m.nanosec = ackTONanosec;
	ACS_LOG(LM_RUNTIME_CONTEXT, __PRETTY_FUNCTION__, (LM_DEBUG, "ACKsTimeout set to: %d sec %d nanosec",
			ackTOSec, ackTONanosec));
}//setACKsTimeout


void BulkDataNTSenderFlow::setThrottling(double throttling)
{
	throttling_m = throttling; // unit is MBytes/sec
	// we have to calculate how long should in min case takes to send one frame of size ACSBulkData::FRAME_MAX_LEN
	// unit is usec
	if (throttling_m!=0.0)
	{
		throttlingMinFrameTime_m = ACSBulkData::FRAME_MAX_LEN / (throttling_m*1.024*1.024)-50; //50us is overhead of the rest code in sendData (set empirically)
		ACS_LOG(LM_RUNTIME_CONTEXT, __PRETTY_FUNCTION__, (LM_DEBUG, "throttling set to: %f MBytes/sec (%f usec)",
			throttling_m, throttlingMinFrameTime_m));
	}
	else
	{
		throttlingMinFrameTime_m = 0.0;
	}
}//setThrottling

void BulkDataNTSenderFlow::startSend(ACE_Message_Block *param)
{
	startSend((unsigned char*)(param->rd_ptr()), param->length());
}

void BulkDataNTSenderFlow::startSend(const unsigned char *param, size_t len)
{
	// Clean statistictics
	//getStatistics(false);
	//getStatistics(true);
	getDelayedStatistics(true, 0);

	// Check the number of receivers
	int receivers = getNumberOfReceivers();
	if (receivers==0) {
		ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__, (LM_ERROR, "startSend invoked without connected listeners in Sender Flow: %s @ Stream: %s", flowName_m.c_str(), senderStream_m->getName().c_str()));
	} else {
		ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__, (LM_INFO, "startSend will send data to %d connected listeners in Sender Flow: %s @ Stream: %s", receivers, flowName_m.c_str(), senderStream_m->getName().c_str()));
	}

	try
	{
		if (currentState_m==StartState || currentState_m==StopState)
		{
			writeFrame(ACSBulkData::BD_PARAM, param, len);
			currentState_m = DataRcvState;
		}
		else
		{
			SenderWrongCmdOrderExImpl swco(__FILE__, __LINE__, __FUNCTION__);
			swco.setSenderName(senderStream_m->getName().c_str()); swco.setFlowName(flowName_m.c_str());
			swco.setCommand("startSend");
			swco.setState(state2String[currentState_m]);
			throw swco;
		}
	}catch(const ACSErr::ACSbaseExImpl &ex)
	{
		StartSendErrorExImpl ssEx(ex, __FILE__, __LINE__, __FUNCTION__);
		ssEx.setSenderName(senderStream_m->getName().c_str()); ssEx.setFlowName(flowName_m.c_str());
		throw ssEx;
	}
}//startSend

void BulkDataNTSenderFlow::sendData(const unsigned char *buffer, size_t len)
{
	// Clean statistics
	//getStatistics(false);
	//getStatistics(true);
	getDelayedStatistics(true, 1);

	// Check the number of receivers
	int receivers = getNumberOfReceivers();
	if (receivers==0) {
		ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__, (LM_ERROR, "sendData invoked without connected listeners in Sender Flow: %s @ Stream: %s", flowName_m.c_str(), senderStream_m->getName().c_str()));
	} else {
		ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__, (LM_INFO, "sendData will send data to %d connected listeners in Sender Flow: %s @ Stream: %s", receivers, flowName_m.c_str(), senderStream_m->getName().c_str()));
	}

	//double send_time;
	ACE_Time_Value start_time, elapsed_time;


	unsigned int iteration=0;
	unsigned int sizeOfFrame = ACSBulkData::FRAME_MAX_LEN;  //TBD: should be configurable ?

	unsigned int numOfFrames = len / sizeOfFrame; // how many frames of size sizeOfFrame do we have to send
	unsigned int restFrameSize = len % sizeOfFrame; // what is the rest

	if (DDSConfiguration::debugLevel>0)
	{
		// the message can cause performance penalty for small data sizes
		ACS_SHORT_LOG((LM_DEBUG, "Going to send: %zd Bytes = %d*%d(=%d) + %d on flow: %s to %d receiver(s).",
			len, numOfFrames, sizeOfFrame, numOfFrames*sizeOfFrame, restFrameSize, flowName_m.c_str(), getNumberOfReceivers()));
	}//if
	unsigned int numOfIter = (restFrameSize>0) ? numOfFrames+1 : numOfFrames;

	try{
		if (currentState_m!=DataRcvState)
		{
			SenderWrongCmdOrderExImpl swco(__FILE__, __LINE__, __FUNCTION__);
			swco.setSenderName(senderStream_m->getName().c_str()); swco.setFlowName(flowName_m.c_str());
			swco.setCommand("sendData");
			swco.setState(state2String[currentState_m]);
			throw swco;
		}

		for(; iteration<numOfIter; iteration++)
		{

			if (iteration==(numOfIter-1) && restFrameSize>0)
			{
				// last frame
				writeFrame(ACSBulkData::BD_DATA, (buffer+(iteration*sizeOfFrame)), restFrameSize, numOfIter-1-iteration, true/*=0*/);
			}else
			{
				if (throttling_m!=0.0) // is throttling "enabled"
				{
					start_time = ACE_OS::gettimeofday();
				}
				// if we wait for ACKs for example after: (iteration%50==0) then we have more NACKS than if we do not wait !!
				writeFrame(ACSBulkData::BD_DATA, (buffer+(iteration*sizeOfFrame)), sizeOfFrame, numOfIter-1-iteration, 0/*we do not ask for ACKs*/);
				if (throttling_m!=0.0) // is throttling "enabled"
				{
					// here we wait for less than msec
					elapsed_time = ACE_OS::gettimeofday() - start_time;
					while (elapsed_time.usec() <  throttlingMinFrameTime_m)
					{
						elapsed_time = ACE_OS::gettimeofday() - start_time;
					}
				}
			}
/*
			elapsed_time = ACE_OS::gettimeofday() - start_time;
			//send_time = (elapsed_time.sec()+( elapsed_time.usec() / 1000000. ));
			//std::cout << "elapsed : " << elapsed_time.usec() << " " << std::endl;

			while (elapsed_time.usec() <  800)
			{
				//std::cout << "sleep for: " << elapsed_time.usec()<< std::endl;
				elapsed_time = ACE_OS::gettimeofday() - start_time;
			}
*/
/*
			if ( elapsed_time.usec() < 670 )
			{
				timespec t,tr;
				t.tv_sec =0;
				t.tv_nsec = 1000;//(670-elapsed_time.usec()) * 100000;
				int a = nanosleep(&t, 0);
				//std::cout << "sleep for: " << elapsed_time.usec()-670 << " " << std::endl;
				//int a =usleep(670-elapsed_time.usec());

				if (a!=0)
					std::cout << "sleep return: " << a << std::endl;

			}
*/
		}//for
		// at this point we have sent all frames, we could wait for ACKs, but it is done in writeFrame
	}catch(const ACSErr::ACSbaseExImpl &ex)
	{
		SendDataErrorExImpl sfEx(ex, __FILE__, __LINE__, __FUNCTION__);
		sfEx.setSenderName(senderStream_m->getName().c_str()); sfEx.setFlowName(flowName_m.c_str());
		sfEx.setFrameCount(iteration+1); sfEx.setTotalFrameCount(numOfIter);
		throw sfEx;
	}//try-catch
}//sendData

void BulkDataNTSenderFlow::stopSend()
{
	// Clean statistics
	//getStatistics(false);
	//getStatistics(true);
	getDelayedStatistics(true, 2);

	// Check the number of receivers
	int receivers = getNumberOfReceivers();
	if (receivers==0) {
		ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__, (LM_ERROR, "stopSend invoked without connected listeners in Sender Flow: %s @ Stream: %s", flowName_m.c_str(), senderStream_m->getName().c_str()));
	} else {
		ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__, (LM_INFO, "stopSend will send data to %d connected listeners in Sender Flow: %s @ Stream: %s", receivers, flowName_m.c_str(), senderStream_m->getName().c_str()));
	}

	try
	{
		if (currentState_m==DataRcvState)
		{
			writeFrame(ACSBulkData::BD_STOP);
			if (DDSConfiguration::debugLevel>0) getStatistics(true);
		}
		else // here we can be in stop or start state which is not so problematic, and we just log the error
		{
			SenderWrongCmdOrderExImpl swco(__FILE__, __LINE__, __FUNCTION__);
			swco.setSenderName(senderStream_m->getName().c_str()); swco.setFlowName(flowName_m.c_str());
			swco.setCommand("stopSend");
			swco.setState(state2String[currentState_m]);
			swco.log(LM_WARNING);
		}
		currentState_m = StopState;
	}catch(const ACSErr::ACSbaseExImpl &ex)
	{
		StopSendErrorExImpl ssEx(ex, __FILE__, __LINE__, __FUNCTION__);
		ssEx.setSenderName(senderStream_m->getName().c_str()); ssEx.setFlowName(flowName_m.c_str());
		throw ssEx;
	}
}//stopSend

void BulkDataNTSenderFlow::writeFrame(ACSBulkData::DataType dataType,  const unsigned char *param, size_t len, unsigned int restFrameCount, bool waitForACKs)
{
	DDS::ReturnCode_t ret;
	DDS::ReliableWriterCacheChangedStatus status; //RTI

	if (len>ACSBulkData::FRAME_MAX_LEN){
		FrameTooLongExImpl ftlEx(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ftlEx.setFrameLength(len);
		ftlEx.setMaxFrameLength(ACSBulkData::FRAME_MAX_LEN);
		throw ftlEx;
	}//if

	frame_m->typeOfdata = dataType;
//	frame_m->data.length(len);
	frame_m->restDataLength = restFrameCount; //we need it just in some cases, but we can always set to 0
	if (param!=0 && len!=0)
		frame_m->data.loan_contiguous(const_cast<DDS_Octet*>(param), len, len);
//		frame_m->data.from_array(param, len);

	ret = ddsDataWriter_m->write(*frame_m, DDS::HANDLE_NIL);
	if (param!=0 && len!=0)
		frame_m->data.unloan();

	if( ret != DDS::RETCODE_OK)
	{
		getStatistics(true);
		if (ret==DDS::RETCODE_TIMEOUT)
		{
			SendFrameTimeoutExImpl toEx(__FILE__, __LINE__, __PRETTY_FUNCTION__);
			toEx.setSenderName(senderStream_m->getName().c_str()); 
			toEx.setFlowName(flowName_m.c_str());
			
			toEx.setTimeout(senderFlowCfg_m.getSendFrameTimeout());
			toEx.setFrameCount(restFrameCount);
			throw toEx;
		}else
		{
			SendFrameGenericErrorExImpl sfEx(__FILE__, __LINE__, __PRETTY_FUNCTION__);
			sfEx.setSenderName(senderStream_m->getName().c_str()); 
			sfEx.setFlowName(flowName_m.c_str());

			sfEx.setFrameCount(restFrameCount);
			sfEx.setRetCode(ret);
			throw sfEx;
		}//if-else
	}//if (ret != DDS::RETCODE_OK)


	//we wait for ACKs if it is explicitly asked or if it was the last frame (only frame)
	if (waitForACKs || restFrameCount==0)
	{
		if (DDSConfiguration::debugLevel>0)
		{
			// the message can cause performance penalty for small data sizes
			ddsDataWriter_m->get_reliable_writer_cache_changed_status(status); //RTI
			ACS_SHORT_LOG((LM_DEBUG, "unacknowledged_sample_count (%s) for flow: %s before waiting for ACKs: %d", dataType2String[dataType], flowName_m.c_str(), status.unacknowledged_sample_count)); //RTI
		}
		ret = ddsDataWriter_m->wait_for_acknowledgments(ackTimeout_m);

		if( ret != DDS::RETCODE_OK)
		{
			getStatistics(true);
			FrameAckTimeoutExImpl ackToEx(__FILE__, __LINE__, __PRETTY_FUNCTION__);
			ackToEx.setSenderName(senderStream_m->getName().c_str()); ackToEx.setFlowName(flowName_m.c_str());
			ackToEx.setTimeout(ackTimeout_m.sec + ackTimeout_m.nanosec/1000000.0);
			throw ackToEx; //	ackToEx.log(LM_WARNING);
		}//if
		if (DDSConfiguration::debugLevel>1)
		{
			ddsDataWriter_m->get_reliable_writer_cache_changed_status(status); //RTI
			ACS_SHORT_LOG((LM_DEBUG, "unacknowledged_sample_count (%s) for flow: %s after waiting for ACKs: %d", dataType2String[dataType], flowName_m.c_str(), status.unacknowledged_sample_count)); //RTI
		}
	}//if (waitForACKs || restFrameCount==0)
}//writeFrame

void BulkDataNTSenderFlow::getStatistics(bool log)
{
	DDS::DataWriterProtocolStatus dwps;
	DDS::DataWriterCacheStatus dwcs;

	// This must be read even without logging as it performs a kind of "clean"

	ddsDataWriter_m->get_datawriter_protocol_status(dwps);
	if (log) {
		ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
		(LM_DEBUG, "TEST DataWriter protocol status for flow: %s [# samples: %lld (bytes %lld); # HBs count: %lld (bytes %lld) # ACKs: %lld (bytes %lld) # NACK counts: %lld (bytes %lld) # rejected: %lld]",
					flowName_m.c_str(),
					dwps.pushed_sample_count_change, dwps.pushed_sample_bytes_change,
					dwps.sent_heartbeat_count_change, dwps.sent_heartbeat_bytes_change,
					dwps.received_ack_count_change, dwps.received_ack_bytes_change,
					dwps.received_nack_count_change, dwps.received_nack_bytes_change,
					dwps.rejected_sample_count_change));

		// This does not perform a clean so it is useful to call only when data needs to be logged
		ddsDataWriter_m->get_datawriter_cache_status(dwcs);
		ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
			(LM_DEBUG, "DataWriter cache Status: sample count in queue: %lld (highest peak since lifetime %lld)", dwcs.sample_count, dwcs.sample_count_peak));
	}
}//void dumpStatistics()

void BulkDataNTSenderFlow::getDelayedStatistics(bool log, int flowMethod)
{
	if(log){

		if(flowMethod == 0) {
			ddsDataWriter_m->get_datawriter_protocol_status(this->delayedStatistics.startSendDwps);
			ddsDataWriter_m->get_datawriter_cache_status(this->delayedStatistics.startSendDwcs);
		}
		else if(flowMethod == 1){
			DDS::DataWriterProtocolStatus dwps;
			DDS::DataWriterCacheStatus dwcs;

			ddsDataWriter_m->get_datawriter_protocol_status(dwps);
			ddsDataWriter_m->get_datawriter_cache_status(dwcs);

			this->delayedStatistics.sendDataDwps.push_back(dwps);
			this->delayedStatistics.sendDataDwcs.push_back(dwcs);
		}
		else if(flowMethod == 1){
			ddsDataWriter_m->get_datawriter_protocol_status(this->delayedStatistics.stopSendDwps);
			ddsDataWriter_m->get_datawriter_cache_status(this->delayedStatistics.stopSendDwcs);
		}
	}
}//void dumpStatistics()

void BulkDataNTSenderFlow::statisticsLogs()
{
	// START SEND
	ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
	(LM_DEBUG, "START SEND: DataWriter protocol status for flow: %s [# samples: %lld (bytes %lld); # HBs count: %lld (bytes %lld) # ACKs: %lld (bytes %lld) # NACK counts: %lld (bytes %lld) # rejected: %lld]",
				flowName_m.c_str(),
				this->delayedStatistics.startSendDwps.pushed_sample_count_change, this->delayedStatistics.startSendDwps.pushed_sample_bytes_change,
				this->delayedStatistics.startSendDwps.sent_heartbeat_count_change, this->delayedStatistics.startSendDwps.sent_heartbeat_bytes_change,
				this->delayedStatistics.startSendDwps.received_ack_count_change, this->delayedStatistics.startSendDwps.received_ack_bytes_change,
				this->delayedStatistics.startSendDwps.received_nack_count_change, this->delayedStatistics.startSendDwps.received_nack_bytes_change,
				this->delayedStatistics.startSendDwps.rejected_sample_count_change));
	ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
		(LM_DEBUG, "START SEND: DataWriter cache Status: sample count in queue: %lld (highest peak since lifetime %lld)", this->delayedStatistics.startSendDwcs.sample_count, this->delayedStatistics.startSendDwcs.sample_count_peak));

	// SEND DATA
	for(std::vector<DDS::DataWriterProtocolStatus>::iterator dwps = this->delayedStatistics.sendDataDwps.begin(); dwps != this->delayedStatistics.sendDataDwps.end(); ++dwps){
		ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
		(LM_DEBUG, "SEND DATA: DataWriter protocol status for flow: %s [# samples: %lld (bytes %lld); # HBs count: %lld (bytes %lld) # ACKs: %lld (bytes %lld) # NACK counts: %lld (bytes %lld) # rejected: %lld]",
					flowName_m.c_str(),
					dwps->pushed_sample_count_change,   dwps->pushed_sample_bytes_change,
					dwps->sent_heartbeat_count_change,  dwps->sent_heartbeat_bytes_change,
					dwps->received_ack_count_change,    dwps->received_ack_bytes_change,
					dwps->received_nack_count_change,   dwps->received_nack_bytes_change,
					dwps->rejected_sample_count_change));
	}
	for(std::vector<DDS::DataWriterCacheStatus>::iterator dwcs = this->delayedStatistics.sendDataDwcs.begin(); dwcs != this->delayedStatistics.sendDataDwcs.end(); ++dwcs){
		ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
			(LM_DEBUG, "SEND DATA: DataWriter cache Status: sample count in queue: %lld (highest peak since lifetime %lld)", dwcs->sample_count, dwcs->sample_count_peak));
	}

	// STOP SEND
	ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
	(LM_DEBUG, "STOP SEND: DataWriter protocol status for flow: %s [# samples: %lld (bytes %lld); # HBs count: %lld (bytes %lld) # ACKs: %lld (bytes %lld) # NACK counts: %lld (bytes %lld) # rejected: %lld]",
				flowName_m.c_str(),
				this->delayedStatistics.stopSendDwps.pushed_sample_count_change, this->delayedStatistics.stopSendDwps.pushed_sample_bytes_change,
				this->delayedStatistics.stopSendDwps.sent_heartbeat_count_change, this->delayedStatistics.stopSendDwps.sent_heartbeat_bytes_change,
				this->delayedStatistics.stopSendDwps.received_ack_count_change, this->delayedStatistics.stopSendDwps.received_ack_bytes_change,
				this->delayedStatistics.stopSendDwps.received_nack_count_change, this->delayedStatistics.stopSendDwps.received_nack_bytes_change,
				this->delayedStatistics.stopSendDwps.rejected_sample_count_change));
	ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
		(LM_DEBUG, "STOP SEND: DataWriter cache Status: sample count in queue: %lld (highest peak since lifetime %lld)", this->delayedStatistics.stopSendDwcs.sample_count, this->delayedStatistics.stopSendDwcs.sample_count_peak));
}//void dumpStatistics()



/*___oOo___*/
