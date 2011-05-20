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
* "@(#) $Id: bulkDataNTDDSPublisher.cpp,v 1.1 2011/05/20 13:39:23 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/
#include "bulkDataNTDDSPublisher.h"
#include <iostream>

using namespace AcsBulkdata;
using namespace std;

BulkDataNTDDSPublisher::BulkDataNTDDSPublisher()
{

}



BulkDataNTDDSPublisher::~BulkDataNTDDSPublisher()
{

	//TBD: do we have to delete something here ?
}


DDS::Publisher* BulkDataNTDDSPublisher::createDDSPublisher()
{
	if (participant==NULL)
	{
		printf("participant NULL\n");
		return NULL;
	}
//PUBLISHER
	//Setup Publisher QoS, add the partition QoS policy
	DDS::PublisherQos pub_qos;
	participant->get_default_publisher_qos(pub_qos);
	//pub_qos.asynchronous_publisher.thread.priority =    RTI_OSAPI_THREAD_PRIORITY_HIGH;
	DDS::Publisher *pub = participant->create_publisher(pub_qos,
			NULL,
			DDS::STATUS_MASK_NONE);
	if(pub==NULL){
		std::cerr << "create publisher failed" << std::endl;
		//TBD: error handling
	}
//REGISTER TYPE
	/* Register the type before creating the topic */
	const char* type_name = ACSBulkData::BulkDataNTFrameTypeSupport::get_type_name();
	int retcode = ACSBulkData::BulkDataNTFrameTypeSupport::register_type(participant, type_name);

	if (retcode != DDS::RETCODE_OK) {
		printf("register_type error %d\n", retcode);

		if (participant != NULL) {
			retcode = participant->delete_contained_entities();
			if (retcode != DDS::RETCODE_OK) { printf("delete_contained_entities error %d\n", retcode);}

			retcode = TheParticipantFactory->delete_participant(participant);
			if (retcode != DDS::RETCODE_OK) { printf("delete_participant error %d\n", retcode);	}
		}

		//TBD:: error handling
	}//if

	return pub;
}//createDDSParticipant


ACSBulkData::BulkDataNTFrameDataWriter* BulkDataNTDDSPublisher::createDDSWriter(DDS::Publisher* pub, DDS::Topic *topic)
{
	DDS::DataWriterQos dw_qos;
	pub->get_default_datawriter_qos (dw_qos);

	// reliability bursty
	dw_qos.reliability.kind =  DDS::RELIABLE_RELIABILITY_QOS;//DDS::BEST_EFFORT_RELIABILITY_QOS;
	dw_qos.reliability.max_blocking_time.sec = 10;
	dw_qos.reliability.max_blocking_time.nanosec = 0;
	dw_qos.history.kind  = ::DDS::KEEP_ALL_HISTORY_QOS;
	dw_qos.history.depth  = 1; // maybe it is not good if it is too big -> it slows the writer and  receivers down if there is a rpoblem with one receiver
	dw_qos.resource_limits.max_samples = 200;//worst_burst_in_samples;
	dw_qos.resource_limits.initial_samples = 1;//worst_burst_in_samples;

	dw_qos.protocol.push_on_write = DDS_BOOLEAN_TRUE;


	// if worst burst == expected burst
	dw_qos.resource_limits.max_samples_per_instance = dw_qos.resource_limits.max_samples;

	dw_qos.protocol.rtps_reliable_writer.high_watermark = 1;
	dw_qos.protocol.rtps_reliable_writer.fast_heartbeat_period.sec = 0;
	dw_qos.protocol.rtps_reliable_writer.fast_heartbeat_period.nanosec =
			10 * 1000000;//NANOSEC_PER_MILLISEC;

	// NOTE: piggyback HB irrelevant when push_on_write is turned off
	dw_qos.protocol.rtps_reliable_writer.heartbeats_per_max_samples = dw_qos.resource_limits.max_samples;
	//dw_qos.protocol.rtps_reliable_writer.heartbeats_per_max_samples = 100 / 64;
	dw_qos.protocol.rtps_reliable_writer.heartbeat_period.sec = 3600 * 24 * 7; //turn off slow

	dw_qos.protocol.rtps_reliable_writer.max_heartbeat_retries = 100;

	dw_qos.protocol.rtps_reliable_writer.min_nack_response_delay.sec = 0;
	dw_qos.protocol.rtps_reliable_writer.min_nack_response_delay.nanosec = 0;
	dw_qos.protocol.rtps_reliable_writer.max_nack_response_delay.sec = 0;
	dw_qos.protocol.rtps_reliable_writer.max_nack_response_delay.nanosec = 0;


	/*
			dw_qos.liveliness.lease_duration.sec=5;
			dw_qos.liveliness.lease_duration.nanosec=0;//500000000;
	 */


	/*
	  		// multicast ???
	  		// commenting out this helps that pub does not blocks, but sample rejected on writer side and reported by reader
	  		// it seems it has to be 64k - max size of the message
			dw_qos.protocol.rtps_reliable_writer.max_bytes_per_nack_response = 32*1024;
	 */


	dw_qos.protocol.rtps_reliable_writer.max_bytes_per_nack_response = 2*32*1024; //works it does not block
	dw_qos.protocol.rtps_reliable_writer.min_nack_response_delay.sec = 0;
	dw_qos.protocol.rtps_reliable_writer.min_nack_response_delay.nanosec =
			0 * 1000000;//NANOSEC_PER_MILLISEC;
	dw_qos.protocol.rtps_reliable_writer.max_nack_response_delay.sec = 0;
	dw_qos.protocol.rtps_reliable_writer.max_nack_response_delay.nanosec =
			1 * 1000000;//NANOSEC_PER_MILLISEC;



	// deadline
	/*dw_qos.deadline.period.sec = 1;
			dw_qos.deadline.period.nanosec = 0;
	 */
	//Create the data writer listener

	//BDDDSWriterListenerImpl* writer_listener_servant =	new BDDDSWriterListenerImpl();

	DDS::DataWriterListener* writerListener =  NULL; //writer_listener_servant;
	//BDDDSWriterListenerImpl* writer_listener_servant =	dynamic_cast<BDDDSWriterListenerImpl*>(writerListener.in());

	if(/*CORBA::is_nil (writerListener.in())*/writerListener==NULL){
		std::cerr << "writer listener is nil" << std::endl;
//TBD error handling
	}


	DDS::DataWriter* temp_dw = pub->create_datawriter(topic,
														dw_qos,
														writerListener,
														DDS::STATUS_MASK_ALL
														);
	if(/*CORBA::is_nil(dw.in())*/temp_dw==NULL){
		std::cerr << "create datawriter failed" << std::endl;
	}

	//? is it ok to narrow a local temp_dw and return it
	return ACSBulkData::BulkDataNTFrameDataWriter::narrow(temp_dw);

}//createDDSWriter


/*___oOo___*/

