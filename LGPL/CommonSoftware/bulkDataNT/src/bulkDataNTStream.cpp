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
* "@(#) $Id: bulkDataNTStream.cpp,v 1.10 2011/08/03 15:06:32 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/
#include "bulkDataNTStream.h"
#include <iostream>
using namespace ACS_BD_Errors;
using namespace ACSErrTypeCommon;
using namespace ACS_DDS_Errors;

using namespace AcsBulkdata;

BulkDataNTStream::BulkDataNTStream(const char* name, const StreamConfiguration &cfg) :
	streamName_m(name), configuration_m(cfg), factory_m(0), participant_m(0)
{
	AUTO_TRACE(__PRETTY_FUNCTION__);

	try
	{
		createDDSFactory();
		createDDSParticipant(); //should be somewhere else in initialize or createStream
	}catch(const ACSErr::ACSbaseExImpl &e)
	{
		if (factory_m!=0)
			DDS::DomainParticipantFactory::finalize_instance();
		StreamCreateProblemExImpl ex (e, __FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setStreamName(name);
		throw ex;
	}//try-catch
}//BulkDataNTStream


BulkDataNTStream::~BulkDataNTStream()
{
	AUTO_TRACE(__PRETTY_FUNCTION__);

	destroyDDSParticipant();
	DDS::DomainParticipantFactory::finalize_instance();
}//~BulkDataNTStream


void BulkDataNTStream::createDDSFactory()
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	DDS::ReturnCode_t ret;
	DDS::DomainParticipantFactoryQos factory_qos;

	factory_m = DDS::DomainParticipantFactory::get_instance();

	factory_m->set_default_library(configuration_m.libraryQos.c_str());
	factory_m->set_default_profile(configuration_m.libraryQos.c_str(), configuration_m.profileQos.c_str());

	// needed by RTI only
	ret = factory_m->get_qos(factory_qos);
	if (ret!=DDS::RETCODE_OK)
	{
		DDSQoSSetProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setDDSTypeCode(ret);
		ex.setQoS("factory_m->get_qos");
		throw ex;
	}//if
	factory_qos.entity_factory.autoenable_created_entities = DDS_BOOLEAN_FALSE;
	ret = factory_m->set_qos(factory_qos);
	if (ret!=DDS::RETCODE_OK)
	{
		DDSQoSSetProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setDDSTypeCode(ret);
		ex.setQoS("factory_m->set_qos");
		throw ex;
	}//if

	//RTI logging
	NDDSConfigLogger::get_instance()->set_verbosity_by_category(
			NDDS_CONFIG_LOG_CATEGORY_API,
			(NDDS_Config_LogVerbosity)(configuration_m.DDSLogVerbosity));
}//createDDSFactory

void BulkDataNTStream::createDDSParticipant()
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	DDS::ReturnCode_t ret;
	DDS::DomainParticipantQos participant_qos;
	int domainID=0; //TBD: where to get domain ID

	if (factory_m==NULL)
	{
		NullPointerExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setVariable("factory_m");
		throw ex;
	}

	if (participant_m!=NULL)
	{
		printf("participant already created\n");
		return;
	}
/* is now read from XML
	ret = factory_m->get_default_participant_qos(participant_qos);
	if (ret!=DDS::RETCODE_OK)
	{
		DDSQoSSetProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setDDSTypeCode(ret);
		ex.setQoS("get_default_participant_qos");
		throw ex;
	}//if


	// Configure built in IPv4 transport to handle large messages
	// RTI specific
	participant_qos.transport_builtin.mask = 0; // clear all xport first
	participant_qos.transport_builtin.mask |= DDS_TRANSPORTBUILTIN_UDPv4;
	participant_qos.receiver_pool.buffer_size = 65536;
	participant_qos.event.max_count = 1024*16;

	//participant_m =factory_m->create_participant(domainID, participant_qos, NULL, DDS::STATUS_MASK_NONE );
*/

	participant_m =factory_m->create_participant_with_profile(domainID,
				configuration_m.libraryQos.c_str(), configuration_m.profileQos.c_str(),
				NULL, DDS::STATUS_MASK_NONE );

	if (participant_m==NULL)
	{
		DDSParticipantCreateProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setDomainID(domainID);
		throw ex;
	}

/*
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
*/
	ret = participant_m->enable();
}//createDDSParticipant

void BulkDataNTStream::destroyDDSParticipant()
{
	DDS::ReturnCode_t ret;
	AUTO_TRACE(__PRETTY_FUNCTION__);
	ret = factory_m->delete_participant(participant_m);
	if (ret != DDS_RETCODE_OK)
	{
		DDSParticipantDestroyProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.log();
	}//if
}//destroyDDSParticipant
