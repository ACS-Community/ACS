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
* "@(#) $Id: bulkDataNTSenderFlow.cpp,v 1.40 2012/03/06 16:22:21 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/

static char *rcsId="@(#) $Id: bulkDataNTSenderFlow.cpp,v 1.40 2012/03/06 16:22:21 bjeram Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "bulkDataNTSenderFlow.h"
#include <iostream>
#include "ACS_BD_Errors.h"

using namespace AcsBulkdata;
using namespace std;
using namespace ACS_DDS_Errors;
using namespace ACS_BD_Errors;

const char *BulkDataNTSenderFlow::state2String[] = {"StartState", "DataRcvState", "StopState" };

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
  ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__, (LM_DEBUG, "Going to create Sender Flow: %s @ stream: %s ...", flowName_m.c_str(), streamName.c_str()));

  callback_m->setStreamName(streamName.c_str());
  callback_m->setFlowName(flowName);

  senderStream->addDDSQoSProfile(sndCfg);

  // should be reactor to have just one object for communication !! DDSDataWriter or similar
  ddsPublisher_m = new BulkDataNTDDSPublisher(senderStream_m->getDDSParticipant(), sndCfg);

  topicName =  streamName + "#" + flowName_m;
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

  setACKsTimeout(senderFlowCfg_m.getACKsTimeout());
  ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__, (LM_DEBUG, "Sender Flow: %s @ stream: %s has been created.", flowName_m.c_str(), streamName.c_str()));
}//BulkDataNTSenderFlow


BulkDataNTSenderFlow::~BulkDataNTSenderFlow()
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	DDS::ReturnCode_t ret;

	// no matter what happen we remove flow from the map
	senderStream_m->removeFlowFromMap(flowName_m.c_str());

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
		ret = participant->delete_topic(ddsTopic_m);
		if (ret!=DDS::RETCODE_OK)
		{
			ACS_SHORT_LOG((LM_ERROR, "Problem deleting topic (%d)", ret));
		}
		ddsTopic_m=0;
	}
	else
	{
		ACS_LOG(LM_RUNTIME_CONTEXT, __PRETTY_FUNCTION__, (LM_ERROR, "Problem deleting data write and topic participant is NULL"));
	}
	delete ddsPublisher_m;
	ddsPublisher_m=0;
	if (releaseCB_m) delete callback_m;
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



void BulkDataNTSenderFlow::startSend(ACE_Message_Block *param)
{
	startSend((unsigned char*)(param->rd_ptr()), param->length());
}

void BulkDataNTSenderFlow::startSend(const unsigned char *param, size_t len)
{
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
	unsigned int iteration=0;
	unsigned int sizeOfFrame = ACSBulkData::FRAME_MAX_LEN;  //TBD: should be configurable ?

	unsigned int numOfFrames = len / sizeOfFrame; // how many frames of size sizeOfFrame do we have to send
	unsigned int restFrameSize = len % sizeOfFrame; // what is the rest

	if (DDSConfiguration::debugLevel>0)
	{
		// the message can cause perfomance penality for small data sizes
		ACS_SHORT_LOG((LM_DEBUG, "Going to send: %zd Bytes = %d*%d(=%d) + %d to flow: %s",
			len, numOfFrames, sizeOfFrame, numOfFrames*sizeOfFrame, restFrameSize, flowName_m.c_str()));
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

		//	start_time = ACE_OS::gettimeofday();
		for(; iteration<numOfIter; iteration++)
		{
			if (iteration==(numOfIter-1) && restFrameSize>0)
			{
				// last frame
				writeFrame(ACSBulkData::BD_DATA, (buffer+(iteration*sizeOfFrame)), restFrameSize, numOfIter-1-iteration);
			}else
			{
				writeFrame(ACSBulkData::BD_DATA, (buffer+(iteration*sizeOfFrame)), sizeOfFrame, numOfIter-1-iteration/*, (iteration%100==0)*/);
			}
		}//for
		// at this point we have sent all frames, we could wait for ACKs, but it is done in writeFrame
	}catch(const ACSErr::ACSbaseExImpl &ex)
	{
		SendDataErrorExImpl sfEx(ex, __FILE__, __LINE__, __FUNCTION__);
		sfEx.setSenderName(senderStream_m->getName().c_str()); sfEx.setFlowName(flowName_m.c_str());
		sfEx.setFrameCount(iteration); sfEx.setTotalFrameCount(numOfIter);
		throw sfEx;
	}//try-catch
}//sendData

void BulkDataNTSenderFlow::stopSend()
{
	try
	{
		if (currentState_m==DataRcvState)
		{
			writeFrame(ACSBulkData::BD_STOP);
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

	//frame
	frame_m->dataType = dataType;
	frame_m->data.length(len);
	frame_m->restDataLength = restFrameCount; //we need it just in some cases, but we can always set to 0
	if (param!=0 && len!=0)
		frame_m->data.from_array(param, len);

	ret = ddsDataWriter_m->write(*frame_m, DDS::HANDLE_NIL);

	if( ret != DDS::RETCODE_OK)
	{
		ddsDataWriter_m->get_reliable_writer_cache_changed_status(status); //RTI
		if (ret==DDS::RETCODE_TIMEOUT)
		{
			SendFrameTimeoutExImpl toEx(__FILE__, __LINE__, __PRETTY_FUNCTION__);
			toEx.setSenderName(senderStream_m->getName().c_str()); toEx.setFlowName(flowName_m.c_str());
			toEx.setTimeout(senderFlowCfg_m.getSendFrameTimeout());
			toEx.setFrameCount(restFrameCount);
			throw toEx;
		}else
		{
			SendFrameGenericErrorExImpl sfEx(__FILE__, __LINE__, __PRETTY_FUNCTION__);
			sfEx.setSenderName(senderStream_m->getName().c_str()); sfEx.setFlowName(flowName_m.c_str());
			sfEx.setFrameCount(restFrameCount);
			sfEx.setRetCode(ret);
			throw sfEx;
		}//if-else

		ddsDataWriter_m->get_reliable_writer_cache_changed_status(status); //RTI
		ACS_SHORT_LOG((LM_DEBUG, "unacknowledged_sample_count (%s) %d for flow: %s", dataType2String[dataType], status.unacknowledged_sample_count, flowName_m.c_str())); //RTI
		// RTI			cout << "\t\t Int unacknowledged_sample_count_peak: " << status.unacknowledged_sample_count_peak << endl;
		ret = ddsDataWriter_m->wait_for_acknowledgments(ackTimeout_m);
		if( ret != DDS::RETCODE_OK)
		{
			FrameAckTimeoutExImpl ackToEx(__FILE__, __LINE__, __PRETTY_FUNCTION__);
			ackToEx.setSenderName(senderStream_m->getName().c_str()); ackToEx.setFlowName(flowName_m.c_str());
			ackToEx.setTimeout(ackTimeout_m.sec + ackTimeout_m.nanosec/1000000.0);
			throw ackToEx; //	ackToEx.log(LM_WARNING);
		}//if

		ddsDataWriter_m->get_reliable_writer_cache_changed_status(status); //RTI
		ACS_SHORT_LOG((LM_DEBUG, "unacknowledged_sample_count (%s) after waiting: %d", dataType2String[dataType], status.unacknowledged_sample_count)); //RTI
		//cout << "\t\t Int1 unacknowledged_sample_count_peak: " << status.unacknowledged_sample_count_peak << endl;
	}//if (ret != DDS::RETCODE_OK)


	//we wait for ACKs if it is explecitly asked or if it was the last frame (only frame)
	if (waitForACKs || restFrameCount==0)
	{
		ddsDataWriter_m->get_reliable_writer_cache_changed_status(status); //RTI
		if (DDSConfiguration::debugLevel>0)
		{
			// the message can cause perfomance penality for small data sizes
			ACS_SHORT_LOG((LM_DEBUG, "unacknowledged_sample_count (%s) before waiting: %d", dataType2String[dataType], status.unacknowledged_sample_count)); //RTI
		}
		ret = ddsDataWriter_m->wait_for_acknowledgments(ackTimeout_m);
		if( ret != DDS::RETCODE_OK)
		{
			FrameAckTimeoutExImpl ackToEx(__FILE__, __LINE__, __PRETTY_FUNCTION__);
			ackToEx.setSenderName(senderStream_m->getName().c_str()); ackToEx.setFlowName(flowName_m.c_str());
			ackToEx.setTimeout(ackTimeout_m.sec + ackTimeout_m.nanosec/1000000.0);
			throw ackToEx; //	ackToEx.log(LM_WARNING);
		}//if
	}//if (waitForACKs || restFrameCount==0)
}//writeFrame

/*___oOo___*/
