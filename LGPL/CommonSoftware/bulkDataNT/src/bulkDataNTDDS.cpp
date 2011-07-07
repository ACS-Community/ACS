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
* "@(#) $Id: bulkDataNTDDS.cpp,v 1.2 2011/07/07 15:05:39 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/
#include "bulkDataNTDDS.h"
#include <iostream>

using namespace AcsBulkdata;
using namespace std;

BulkDataNTDDS::BulkDataNTDDS()
	: factory(0), participant(0)
{

}



BulkDataNTDDS::~BulkDataNTDDS()
{

	//TBD: do we have to delete something here ?
}




void BulkDataNTDDS::createDDSFactory()
{
    //DDS::DomainParticipantFactoryQos factory_qos;
    factory = DDS::DomainParticipantFactory::get_instance();


/* needed by RTI only
   factory->get_qos(factory_qos);
    factory_qos.entity_factory.autoenable_created_entities = DDS_BOOLEAN_FALSE;
    factory->set_qos(factory_qos);
    */
}//createDDSFactory

void BulkDataNTDDS::createDDSParticipant()
{
	DDS::DomainParticipantQos participant_qos;

	if (participant!=NULL)
	{
		printf("participant already created\n");
		return;
	}
	if (factory==NULL)
	{
		std::cerr << "BulkDataNTDDS::createDDSParticipant factory NULL !!" << std::endl;
	}

	//PARTICIPANT
	factory->get_default_participant_qos(&participant_qos);
	// Configure built in IPv4 transport to handle large messages
	/*TBD::  we have to configure those things from XML
	DDS_DomainParticipantQos qos;
	  DDS_ReturnCode_t retcode = DDSTheParticipantFactory->
	    get_participant_qos_from_profile(qos,
					     args.qosLibrary.c_str(),
					     args.qosProfile.c_str());
	 */
	/* RTI specific
	participant_qos.transport_builtin.mask = 0; // clear all xport first
	participant_qos.transport_builtin.mask |= DDS_TRANSPORTBUILTIN_UDPv4;
	participant_qos.receiver_pool.buffer_size = 65536;
	participant_qos.event.max_count = 1024*16;
	*/
//TBD: where to get domain ID
	participant =factory->create_participant(0,
			participant_qos,
			NULL,
			0/*DDS::STATUS_MASK_NONE*/
	);
	if (participant!=NULL)
	{
		std::cout << "created participant with domain ID:" << participant->get_domain_id() << std::endl;
	}
	else
	{
		std::cerr << "Create Participant Failed." << std::endl;
		//TBD: error handling exception !!
	//	return 1;
	}

	//TBS should be completly replace by QoS configuration (?)
// TRANSPORT
/* RTI
	struct NDDS_Transport_UDPv4_Property_t udpv4TransportProperty = NDDS_TRANSPORT_UDPV4_PROPERTY_DEFAULT;
	retcode = NDDSTransportSupport::get_builtin_transport_property(participant, DDS_TRANSPORTBUILTIN_UDPv4,
			(struct NDDS_Transport_Property_t&)udpv4TransportProperty);

	udpv4TransportProperty.parent.message_size_max = 65536; //UDP_SIZE_MAX;
	udpv4TransportProperty.send_socket_buffer_size = 65536; //UDP_SOCKET_SEND_BUFFER_SIZE;
	udpv4TransportProperty.recv_socket_buffer_size = 65536*2; //UDP_SOCKET_RECV_BUFFER_SIZE;

	udpv4TransportProperty.multicast_ttl = 1;

	retcode = NDDSTransportSupport::set_builtin_transport_property( participant,DDS_TRANSPORTBUILTIN_UDPv4,
			(struct NDDS_Transport_Property_t&)udpv4TransportProperty);
	if (retcode != DDS_RETCODE_OK) {
		printf("Error in setting built-in transport UDPv4 "
				"property\n");
	}

	int max_gather_send_buffers = udpv4TransportProperty.parent.gather_send_buffer_count_max;

	retcode = participant->enable();
	*/
}//createDDSParticipant


DDS::Topic* BulkDataNTDDS::createDDSTopic(const char* topicName)
{
	if (participant==0)
	{
		std::cerr << "BulkDataNTDDS::createDDSTopic participant is 0" << std::cerr << endl;
		return 0;
	}
//TOPIC
	//TBD: check if topic already exists find_topic ??
		DDS::TopicQos topic_qos;
		participant->get_default_topic_qos(&topic_qos);
//		topic_qos.ownership.kind = DDS_EXCLUSIVE_OWNERSHIP_QOS;
		topic_qos.reliability.kind = ::DDS::RELIABLE_RELIABILITY_QOS; //::DDS::BEST_EFFORT_RELIABILITY_QOS;
		topic_qos.durability.kind = DDS::VOLATILE_DURABILITY_QOS;
//		topic_qos.resource_limits.max_samples_per_instance = 2;
		//TBD: type name could be a parameter of the method or class member

		//REGISTER TYPE
		/* Register the type before creating the topic */
//	ACSBulkData::BulkDataNTFrameTypeSupport* ts =new ACSBulkData::BulkDataNTFrameTypeSupport();
//		const char* type_name = ts->get_type_name();

		const char* type_name = ACSBulkData::BulkDataNTFrameTypeSupport::get_type_name();
		int retcode = ACSBulkData::BulkDataNTFrameTypeSupport::register_type(participant, type_name);

		if (retcode != DDS::RETCODE_OK)
				{
			printf("register_type error %d\n", retcode);
				}

		cout << "Going to create DDS topic: " << topicName << " " << type_name << endl;
		DDS::Topic *topic =  participant->create_topic(topicName,
				type_name,
				topic_qos,
				NULL,
				0/*DDS::STATUS_MASK_NONE*/
		);
		if (topic==NULL){
			std::cerr << "create_topic failed" << std::endl;
		//TBD: error handling
		}
		return topic;
}//createDDSTopic




/*___oOo___*/

