#include <iostream>
#include "DDSPublisher.h"

using namespace ddsnc;

int DDSPublisher::attachToTransport()
{
	OpenDDS::DCPS::AttachStatus status =
		pub_impl->attach_transport(transport_impl.in());
	if (status != OpenDDS::DCPS::ATTACH_OK) {
		std::string status_str;
		switch (status) {
			case OpenDDS::DCPS::ATTACH_BAD_TRANSPORT:
				status_str = "ATTACH_BAD_TRANSPORT";
				break;
			case OpenDDS::DCPS::ATTACH_ERROR:
				status_str = "ATTACH_ERROR";
				break;
			case OpenDDS::DCPS::ATTACH_INCOMPATIBLE_QOS:
				status_str = "ATTACH_INCOMPATIBLE_QOS";
				break;
			default:
				status_str = "Unknown Status";
				break;
		}
		::std::cerr << "Failed to attach to the transport. Status == "
			<< status_str.c_str() << ::std::endl;
		return 1;
	}
	return 0;
}

void DDSPublisher::initialize()
{
	std::cerr<< "DDSPublisher::initialize()"<<std::endl;
	createParticipant();
	if (CORBA::is_nil (participant.in()))
		std::cerr << "Participant is nil" << std::endl;
	if(partitionName!=NULL){
		participant->get_default_publisher_qos(pubQos);
		pubQos.partition.name.length(1);
		pubQos.partition.name[0]=CORBA::string_dup(partitionName);
	}
	initializeTransport();
	createPublisher();

	pub->get_default_datawriter_qos (dwQos);
	dwQos.reliability.kind = ::DDS::RELIABLE_RELIABILITY_QOS;
	dwQos.reliability.max_blocking_time.sec = 1;
	dwQos.history.kind = ::DDS::KEEP_LAST_HISTORY_QOS;
	dwQos.history.depth = 100;

	dwQos.durability.kind = ::DDS::TRANSIENT_LOCAL_DURABILITY_QOS;
	dwQos.durability.service_cleanup_delay.sec = 10;
}

void DDSPublisher::initializeDataWriter()
{
	dw = pub->create_datawriter(topic.in(),
			dwQos,  DDS::DataWriterListener::_nil());
	if(CORBA::is_nil(dw.in())){
		std::cerr << "create datawriter failed" << std::endl;
	}
}

int DDSPublisher::createPublisher()
{
	std::cerr << "DDSPublisher::createPublisher" << std::endl;

	if(partitionName==NULL){
		pub =  participant->create_publisher(PUBLISHER_QOS_DEFAULT,
				DDS::PublisherListener::_nil());
		std::cerr << "Creating Publisher with default Qos" << std::endl;
	}

	else{
		pub = participant->create_publisher(pubQos,
				DDS::PublisherListener::_nil());
		std::cerr << "Creating Publisher with partition " << partitionName 
			<< std::endl;
	}

	if(CORBA::is_nil(pub.in())){
		std::cerr << "create publisher failed" << std::endl;
		return 1;
	}

	pub_impl= dynamic_cast<OpenDDS::DCPS::PublisherImpl*>(pub.in());
	if(pub_impl == NULL){
		std::cerr << "Failed to obtain publisher servant" << std::endl;
		return 1;
	}
	return attachToTransport();
}
