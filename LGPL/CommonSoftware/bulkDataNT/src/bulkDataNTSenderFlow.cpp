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
* "@(#) $Id: bulkDataNTSenderFlow.cpp,v 1.8 2011/07/28 10:28:57 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/

#include "bulkDataNTSenderFlow.h"
#include <iostream>

#include <AV/FlowSpec_Entry.h>  // we need it for TAO_Tokenizer ??

static char *rcsId="@(#) $Id: bulkDataNTSenderFlow.cpp,v 1.8 2011/07/28 10:28:57 bjeram Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

using namespace AcsBulkdata;
using namespace std;

BulkDataNTSenderFlow::BulkDataNTSenderFlow(BulkDataNTSenderStream *senderStream, const char* flowName/*, cb*/) :
		senderStream_m(senderStream)
{
	AUTO_TRACE(__PRETTY_FUNCTION__);

	std::string topicName;

	flowName_m = flowName;

	// should be reactor to have just one object for comunication !! DDSDataWriter or similar
	ddsPublisher_m = new BulkDataNTDDSPublisher(senderStream_m->getDDSParticipant());

	topicName = senderStream_m->getName() + "#" + flowName_m;
	ddsTopic_m = ddsPublisher_m->createDDSTopic(topicName.c_str());

	ddsDataWriter_m= ddsPublisher_m->createDDSWriter(ddsTopic_m);
}




BulkDataNTSenderFlow::~BulkDataNTSenderFlow()
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	DDS::ReturnCode_t ret;

	senderStream_m->removeFlowFromMap(flowName_m.c_str());

	// this part can go to BulkDataNTDDSPublisher, anyway we need to refactor
	DDS::DomainParticipant *participant = senderStream_m->getDDSParticipant();
	if (participant!=0)
	{
		ddsPublisher_m->destroyDDSWriter(ddsDataWriter_m);
		ddsDataWriter_m = 0;
		ret = participant->delete_topic(ddsTopic_m);
		if (ret!=DDS::RETCODE_OK)
		{
			ACS_SHORT_LOG((LM_ERROR, "Problem deleting topic (%d)", ret));
		}
	}
	else
	{
		ACS_SHORT_LOG((LM_ERROR, "Problem deleting data write and topic participant is NULL"));
	}
	delete ddsPublisher_m;
}//~BulkDataNTSenderFlow



void BulkDataNTSenderFlow::startSend(ACE_Message_Block *param)
{
 startSend((unsigned char*)(param->rd_ptr()), param->length());
}

void BulkDataNTSenderFlow::startSend(const unsigned char *param, size_t len)
{
	writeFrame(ACSBulkData::BD_PARAM, param, len);
}//startSend


void BulkDataNTSenderFlow::sendData(ACE_Message_Block *buffer)
{
	 sendData((unsigned char*)(buffer->rd_ptr()), buffer->length());
}//sendData

void BulkDataNTSenderFlow::sendData(const unsigned char *buffer, size_t len)
{
	DDS::ReturnCode_t ret;
// RTI	DDS_ReliableWriterCacheChangedStatus status;
	DDS::InstanceHandle_t handle = DDS::HANDLE_NIL;

	//ACSBulkData::BulkDataNTFrame *frame;
	unsigned int sizeOfFrame = ACSBulkData::FRAME_MAX_LEN;  //TBD: tmp should be configurable

	unsigned int numOfFrames = len / sizeOfFrame; // how many frames of size sizeOfDataChunk do we have to send
	unsigned int restFrameSize = len % sizeOfFrame; // what rests ?


	// should we wait for all ACKs? timeout should be configurable
	DDS::Duration_t ack_timeout_delay = {1, 0};//1s


	//TBD  RTI do we have to create the frame each time or  just once in the constructor ... ?
	 frame =  ACSBulkData::BulkDataNTFrameTypeSupport::create_data();
	if (frame == NULL) {
		printf("BD_DDS::BDdataMessageTypeSupport::create_data error - frame null\n");
		// delete
	//TBD:: error handling
	}


	frame->dataType = ACSBulkData::BD_DATA;  //we are going to send data
	frame->data.length(sizeOfFrame); // frame.data.resize(sizeOfFrame);; // do we actually need resize ?

	ACS_SHORT_LOG((LM_DEBUG, "Going to send: %d Bytes = %d*%d(=%d) + %d",
			len, numOfFrames, sizeOfFrame, numOfFrames*sizeOfFrame, restFrameSize));

//	start_time = ACE_OS::gettimeofday();

	unsigned int numOfIter = (restFrameSize>0) ? numOfFrames+1 : numOfFrames;

	for(unsigned int i=0; i<numOfIter; i++)
	{
		if (i==(numOfIter-1) && restFrameSize>0)
		{
			// last frame
			frame->data.length(sizeOfFrame); //frame.data.resize(restFrameSize);
			frame->data.from_array((buffer+(i*sizeOfFrame)), restFrameSize);
			//std::copy ((buffer+(i*sizeOfFrame)), (buffer+(i*sizeOfFrame)+restFrameSize), frame.data.begin() );
		}else
		{
			frame->data.from_array((buffer+(i*sizeOfFrame)), sizeOfFrame);
			//std::copy ((buffer+(i*sizeOfFrame)), (buffer+(i*sizeOfFrame)+sizeOfFrame), frame.data.begin() );
		}


		frame->restDataLength = numOfIter-1-i;


		ret = ddsDataWriter_m->write(*frame, handle);
		if( ret != DDS::RETCODE_OK) {

// RTI			senderFlows_m[flownumber].dataWriter->get_reliable_writer_cache_changed_status(status);

			if (ret==DDS::RETCODE_TIMEOUT)
			{
				std::cerr << "Timeout while sending " <<  i << " data" << std::endl;
			}else
			{
				std::cerr << "Failed to send " <<  i << " data return code: " << ret << std::endl;
			}

/* RTI			cout << "\t\t Int unacknowledged_sample_count: " << status.unacknowledged_sample_count;
			cout << "\t\t Int unacknowledged_sample_count_peak: " << status.unacknowledged_sample_count_peak << endl;
*/
			ret = ddsDataWriter_m->wait_for_acknowledgments(ack_timeout_delay);
							if( ret != DDS::RETCODE_OK) {
								std::cerr << " !Failed while waiting for acknowledgment of "
										<< "data being received by subscriptions, some data "
										<< "may not have been delivered." << std::endl;
							}//if

/* RTI			cout << "\t\t Int1 unacknowledged_sample_count: " << status.unacknowledged_sample_count;
			cout << "\t\t Int1 unacknowledged_sample_count_peak: " << status.unacknowledged_sample_count_peak << endl;
			*/
		}//if
	}//for

// do we need to wait for ACK after each frame is sent or at the edn or not at all, configurable ?
	ret = ddsDataWriter_m->wait_for_acknowledgments(ack_timeout_delay);
	if( ret != DDS::RETCODE_OK) {
		std::cerr << " !Failed while waiting for acknowledgment of "
				<< "data being received by subscriptions, some data "
				<< "may not have been delivered." << std::endl;
	}//if


}//sendData

void BulkDataNTSenderFlow::stopSend()
{
	writeFrame(ACSBulkData::BD_STOP);
}//stopSend

void BulkDataNTSenderFlow::writeFrame(ACSBulkData::DataType dataType,  const unsigned char *param, size_t len)
{

	DDS::ReturnCode_t ret;
	DDS::InstanceHandle_t handle = DDS::HANDLE_NIL;
	//ACSBulkData::BulkDataNTFrame *frame;

	if (len>ACSBulkData::FRAME_MAX_LEN){
		printf("parameter too long !!\n");
		// delete
		//TBD:: error handling
	}

	//RTI do we have to create the frame each time or ... ?
	frame = ACSBulkData::BulkDataNTFrameTypeSupport::create_data();
	if (frame == NULL) {
		printf("BD_DDS::BDdataMessageTypeSupport::create_data error - frame null\n");
		// delete
		//TBD:: error handling
	}

	//frame
	frame->dataType = dataType;
	frame->data./*resize*/length(len);
	if (param!=0 && len!=0)
		frame->data.from_array(param, len); //frame.data.assign(len, *param);   //1st parameter  is of type const unsigned char !!

	ret = ddsDataWriter_m->write(*frame, handle);
	if( ret != DDS::RETCODE_OK)
	{
		//TBD EH  special for timeout
		std::cerr << "Failed to send parameter. Return code: " << ret << std::endl;
	}//if

	// should we wait for all ACKs? timeout should be configurable
	DDS::Duration_t ack_timeout_delay = {1, 0};//1s
	ret = ddsDataWriter_m->wait_for_acknowledgments(ack_timeout_delay);
	if( ret != DDS::RETCODE_OK) {
		std::cerr << " !Failed while waiting for acknowledgment of "
				<< "data being received by subscriptions, some data "
				<< "may not have been delivered." << std::endl;
		//TBD error handling
	}//if

}//writeFrame

/*___oOo___*/
