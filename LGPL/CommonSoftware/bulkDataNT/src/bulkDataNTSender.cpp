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
* "@(#) $Id: bulkDataNTSender.cpp,v 1.1 2011/05/20 13:39:23 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created 
*/

#include "bulkDataNTSender.h"
#include <iostream>


static char *rcsId="@(#) $Id: bulkDataNTSender.cpp,v 1.1 2011/05/20 13:39:23 bjeram Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

using namespace AcsBulkdata;
using namespace std;

BulkDataNTSender::BulkDataNTSender() :
		senderFlows_m(0)
{

}



BulkDataNTSender::~BulkDataNTSender()
{
	destroyFlows(); //if flows have not been deleted
}




void BulkDataNTSender::startSend(FlowNumberType flownumber, ACE_Message_Block *param)
{
 startSend(flownumber, (unsigned char*)(param->rd_ptr()), param->length());
}

void BulkDataNTSender::startSend(FlowNumberType flownumber, const unsigned char *param, size_t len)
{
	writeFrame(flownumber, ACSBulkData::BD_PARAM, param, len);
}//startSend


void BulkDataNTSender::sendData(FlowNumberType flownumber, ACE_Message_Block *buffer)
{
	 sendData(flownumber, (unsigned char*)(buffer->rd_ptr()), buffer->length());
}//sendData

void BulkDataNTSender::sendData(FlowNumberType flownumber, const unsigned char *buffer, size_t len)
{
	DDS::ReturnCode_t ret;
	DDS_ReliableWriterCacheChangedStatus status;
	DDS::InstanceHandle_t handle = DDS::HANDLE_NIL;

	ACSBulkData::BulkDataNTFrame *frame;
	unsigned int sizeOfFrame = ACSBulkData::FRAME_MAX_LEN;  //TBD: tmp should be configurable

	unsigned int numOfFrames = (len*1024) / sizeOfFrame; // how many frames of size sizeOfDataChunk do we have to send
	unsigned int restFrameSize = (len*1024) % sizeOfFrame; // what rests ?

	// should we wait for all ACKs? timeout should be configurable
	DDS::Duration_t ack_timeout_delay = {1, 0};//1s

	// do we have to create the frame each time or ... ?
	frame = ACSBulkData::BulkDataNTFrameTypeSupport::create_data();
	if (frame == NULL) {
		printf("BD_DDS::BDdataMessageTypeSupport::create_data error - frame null\n");
		// delete
	//TBD:: error handling
	}

	frame->dataType = ACSBulkData::BD_DATA;  //we are going to send data
	frame->data.length(sizeOfFrame);

/*	std::cout <<  " Going to send: " << dataSize << "kBytes";
	std::cout << " = (" << numOfChunks << " times chunks of size: " << sizeOfDataChunk << " bytes => ";
	std::cout << numOfChunks*sizeOfDataChunk/1024.0 << "kBytes ) + " <<  restDataSize/1024.0 << " kBytes"<< std::endl;
*/
//	start_time = ACE_OS::gettimeofday();

	unsigned int numOfIter = (restFrameSize>0) ? numOfFrames+1 : numOfFrames;

	for(unsigned int i=0; i<numOfIter; i++)
	{
		if (i==(numOfIter-1) && restFrameSize>0)
			frame->data.length(restFrameSize);

		frame->restDataLength = numOfIter-1-i;
		frame->data.from_array(buffer+(i*sizeOfFrame), sizeOfFrame);

		ret = senderFlows_m[flownumber].dataWriter->write(*frame, handle);
		if( ret != DDS::RETCODE_OK) {

			senderFlows_m[flownumber].dataWriter->get_reliable_writer_cache_changed_status(status);

			if (ret==DDS::RETCODE_TIMEOUT)
			{
				std::cerr << "Timeout while sending " <<  i << " data" << std::endl;
			}else
			{
				std::cerr << "Failed to send " <<  i << " data return code: " << ret << std::endl;
			}

			cout << "\t\t Int unacknowledged_sample_count: " << status.unacknowledged_sample_count;
			cout << "\t\t Int unacknowledged_sample_count_peak: " << status.unacknowledged_sample_count_peak << endl;

			ret = senderFlows_m[flownumber].dataWriter->wait_for_acknowledgments(ack_timeout_delay);
							if( ret != DDS::RETCODE_OK) {
								std::cerr << " !Failed while waiting for acknowledgment of "
										<< "data being received by subscriptions, some data "
										<< "may not have been delivered." << std::endl;
							}//if

			cout << "\t\t Int1 unacknowledged_sample_count: " << status.unacknowledged_sample_count;
			cout << "\t\t Int1 unacknowledged_sample_count_peak: " << status.unacknowledged_sample_count_peak << endl;
		}//if
	}//for

// do we need to wait for ACK after each fram is sent or at the edn or not at all, configurable ?
	ret = senderFlows_m[flownumber].dataWriter->wait_for_acknowledgments(ack_timeout_delay);
	if( ret != DDS::RETCODE_OK) {
		std::cerr << " !Failed while waiting for acknowledgment of "
				<< "data being received by subscriptions, some data "
				<< "may not have been delivered." << std::endl;
	}//if



}//sendData

void BulkDataNTSender::stopSend(FlowNumberType flownumber)
{
	writeFrame(flownumber, ACSBulkData::BD_STOP);
}//stopSend


void BulkDataNTSender::createFlow(const unsigned short numberOfFlows)
{
	std::string topicName;
	// should we check if we already have flows ?
	if (senderFlows_m!=NULL)
	{
		printf("flows already created !!");
		return;
	}
	senderFlows_m = new SenderFlowData[numberOfFlows];
	numOfFlows_m = numberOfFlows;
	// + max number of flows ?
	char strFlowNumber[2];

	for(unsigned int i=0; i<numberOfFlows; i++)
	{
		sprintf(strFlowNumber,"%d",i);
		// error handling !!!
		DDS::Topic *topic = createDDSTopic(topicName.c_str());

		DDS::Publisher *pub = createDDSPublisher();
		// the anme of the flow is stream anme + flow number;
		std::string topicName = streamName_m + "#" + strFlowNumber;

		ACSBulkData::BulkDataNTFrameDataWriter *dw= createDDSWriter(pub, topic);

		senderFlows_m[i].topicName = topicName;
		senderFlows_m[i].topic = topic;
		senderFlows_m[i].publisher = pub;

		senderFlows_m[i].dataWriter = dw;
	}//for
}//createFlow

unsigned int BulkDataNTSender::destroyFlows()
{
	unsigned int i=0;
	if (senderFlows_m==NULL) return 0;

	for(i=0; i<numOfFlows_m; i++)
		{
		senderFlows_m[i].publisher->delete_datawriter(senderFlows_m[i].dataWriter);
			// do we have to destroy also topic and publisher ?
		}//for

	delete[] senderFlows_m;
	senderFlows_m = NULL;
	numOfFlows_m =0;
	return i;
}//destroyFlows




void BulkDataNTSender::writeFrame(FlowNumberType flownumber, ACSBulkData::DataType dataType,  const unsigned char *param, size_t len)
{

	DDS::ReturnCode_t ret;
	DDS::InstanceHandle_t handle = DDS::HANDLE_NIL;
	ACSBulkData::BulkDataNTFrame *frame;

	if (len>ACSBulkData::FRAME_MAX_LEN){
		printf("parameter too long !!\n");
		// delete
		//TBD:: error handling
	}

	// do we have to create the frame each time or ... ?
	frame = ACSBulkData::BulkDataNTFrameTypeSupport::create_data();
	if (frame == NULL) {
		printf("BD_DDS::BDdataMessageTypeSupport::create_data error - frame null\n");
		// delete
		//TBD:: error handling
	}

	//frame
	frame->dataType = dataType;
	frame->data.length(len);
	if (param!=0 && len!=0)
		frame->data.from_array(param, len); //1st parameter  is of type const unsigned char !!

	ret = senderFlows_m[flownumber].dataWriter->write(*frame, handle);
	if( ret != DDS::RETCODE_OK)
	{
		//TBD EH  special for timeout
		std::cerr << "Failed to send parameter. Return code: " << ret << std::endl;
	}//if

	// should we wait for all ACKs? timeout should be configurable
	DDS::Duration_t ack_timeout_delay = {0, 100};//100ms
	ret = senderFlows_m[flownumber].dataWriter->wait_for_acknowledgments(ack_timeout_delay);
	if( ret != DDS::RETCODE_OK) {
		std::cerr << " !Failed while waiting for acknowledgment of "
				<< "data being received by subscriptions, some data "
				<< "may not have been delivered." << std::endl;
		//TBD error handling
	}//if

}//writeFrame

/*___oOo___*/
