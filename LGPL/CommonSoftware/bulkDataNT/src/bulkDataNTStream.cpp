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
* "@(#) $Id: bulkDataNTStream.cpp,v 1.5 2011/07/28 15:11:54 bjeram Exp $"
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
	AUTO_TRACE(__PRETTY_FUNCTION__);

	createDDSFactory();
	createDDSParticipant(); //should be somewhere else in initialize or createStream
}


BulkDataNTStream::~BulkDataNTStream()
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	try
	{
	destroyDDSParticipant();
	DDS::DomainParticipantFactory::finalize_instance();
	}catch(ACSErr::ACSbaseExImpl &ex)
	{
//throw
	}
}


void BulkDataNTStream::createDDSFactory()
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	DDS::ReturnCode_t ret;
	DDS::DomainParticipantFactoryQos factory_qos;

	factory_m = DDS::DomainParticipantFactory::get_instance();

	// needed by RTI only
	factory_m->get_qos(factory_qos);
	factory_qos.entity_factory.autoenable_created_entities = DDS_BOOLEAN_FALSE;
	factory_m->set_qos(factory_qos);

	//RTI logging
    NDDSConfigLogger::get_instance()->set_verbosity_by_category(
                        NDDS_CONFIG_LOG_CATEGORY_API,
                        NDDS_CONFIG_LOG_VERBOSITY_WARNING);
}

void BulkDataNTStream::createDDSParticipant()
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	DDS::ReturnCode_t ret;
	DDS::DomainParticipantQos participant_qos;

	if (factory_m==NULL)
		{
			std::cerr << "BulkDataNTDDS::createDDSParticipant factory NULL !!" << std::endl;
		}

	if (participant_m!=NULL)
	{
		printf("participant already created\n");
		return;
	}


	//PARTICIPANT
	ret = factory_m->get_default_participant_qos(participant_qos);
	// Configure built in IPv4 transport to handle large messages
	/*TBD::  we have to configure those things from XML
	DDS_DomainParticipantQos qos;
	  DDS_ReturnCode_t retcode = DDSTheParticipantFactory->
	    get_participant_qos_from_profile(qos,
					     args.qosLibrary.c_str(),
					     args.qosProfile.c_str());
	 */
	// RTI specific
	participant_qos.transport_builtin.mask = 0; // clear all xport first
	participant_qos.transport_builtin.mask |= DDS_TRANSPORTBUILTIN_UDPv4;
	participant_qos.receiver_pool.buffer_size = 65536;
	participant_qos.event.max_count = 1024*16;

//TBD: where to get domain ID
	participant_m =factory_m->create_participant(0, participant_qos, NULL, DDS::STATUS_MASK_NONE );
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
// RTI
	struct NDDS_Transport_UDPv4_Property_t udpv4TransportProperty = NDDS_TRANSPORT_UDPV4_PROPERTY_DEFAULT;
	ret = NDDSTransportSupport::get_builtin_transport_property(participant_m, DDS_TRANSPORTBUILTIN_UDPv4,
			(struct NDDS_Transport_Property_t&)udpv4TransportProperty);

	udpv4TransportProperty.parent.message_size_max = 65536; //UDP_SIZE_MAX;
	udpv4TransportProperty.send_socket_buffer_size = 65536; //UDP_SOCKET_SEND_BUFFER_SIZE;
	udpv4TransportProperty.recv_socket_buffer_size = 65536*2; //UDP_SOCKET_RECV_BUFFER_SIZE;

	udpv4TransportProperty.multicast_ttl = 1;

	ret = NDDSTransportSupport::set_builtin_transport_property(participant_m, DDS_TRANSPORTBUILTIN_UDPv4,
			(struct NDDS_Transport_Property_t&)udpv4TransportProperty);
	if (ret != DDS_RETCODE_OK) {
		printf("Error in setting built-in transport UDPv4 "
				"property\n");
	}

	int max_gather_send_buffers = udpv4TransportProperty.parent.gather_send_buffer_count_max;

	ret = participant_m->enable();
}

void BulkDataNTStream::destroyDDSParticipant()
{
	DDS::ReturnCode_t ret;
	AUTO_TRACE(__PRETTY_FUNCTION__);
	ret = factory_m->delete_participant(participant_m);
}
