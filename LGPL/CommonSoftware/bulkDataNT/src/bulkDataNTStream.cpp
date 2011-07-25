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
* "@(#) $Id: bulkDataNTStream.cpp,v 1.1 2011/07/25 13:51:01 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/
#include "bulkDataNTStream.h"
#include <iostream>

using namespace AcsBulkdata;

BulkDataNTStream::BulkDataNTStream(const char* name) :
	streamName_m(name), factory_m(0), participant_m(0)
{

}


BulkDataNTStream::~BulkDataNTStream()
{

}

void BulkDataNTStream::createDDSFactory()
{
	printf("BulkDataNTStream::createDDSFactory\n");
    //DDS::DomainParticipantFactoryQos factory_qos;
    factory_m = DDS::DomainParticipantFactory::get_instance();


/* needed by RTI only
   factory->get_qos(factory_qos);
    factory_qos.entity_factory.autoenable_created_entities = DDS_BOOLEAN_FALSE;
    factory->set_qos(factory_qos);
    */
}

void BulkDataNTStream::createDDSParticipant()
{
	DDS::DomainParticipantQos participant_qos;

	if (participant_m!=NULL)
	{
		printf("participant already created\n");
		return;
	}
	if (factory_m==NULL)
	{
		std::cerr << "BulkDataNTDDS::createDDSParticipant factory NULL !!" << std::endl;
	}

	//PARTICIPANT
	factory_m->get_default_participant_qos(&participant_qos);
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
	participant_m =factory_m->create_participant(0,
			participant_qos,
			NULL,
			0/*DDS::STATUS_MASK_NONE*/
	);
	if (participant_m!=NULL)
	{
		std::cout << "created participant with domain ID:" << participant_m->get_domain_id() << std::endl;
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
}


