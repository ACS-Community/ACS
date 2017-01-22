#include <iostream>
#include "DDSPublisher.h"

using namespace ddsnc;

void DDSPublisher::initialize()
{
        ACS_TRACE("DDSPublisher::initialize");
	createParticipant();
	if (CORBA::is_nil (participant.in()))
	        ACS_STATIC_LOG(LM_FULL_INFO, "DDSPublisher::initialize", (LM_ERROR,
                	                  "Participant is nil"));
	if(partitionName!=NULL){
		participant->get_default_publisher_qos(pubQos);
		pubQos.partition.name.length(1);
		pubQos.partition.name[0]=CORBA::string_dup(partitionName);
	}
	createPublisher();


	pub->get_default_datawriter_qos (dwQos);
	dwQos.reliability.kind = ::DDS::RELIABLE_RELIABILITY_QOS;
	dwQos.reliability.max_blocking_time.sec = 1;

	// CDB QoS
	DDS::QosPolicyCountSeq tmp = CDBProperties::getCDBQoSProps(DDSHelper::getChannelName());
        
        ACS_STATIC_LOG(LM_FULL_INFO, "DDSPublisher::initialize", (LM_INFO,
                                  "setting the QoS"));
	for(int i=0;i<(int)tmp.length();i++)
	{
		if( tmp[i].policy_id == 12 )
		{
        		//Destination_order
			if(tmp[i].count == 0)
				dwQos.destination_order.kind = ::DDS::BY_RECEPTION_TIMESTAMP_DESTINATIONORDER_QOS;
			else if(tmp[i].count == 1)
				dwQos.destination_order.kind = ::DDS::BY_SOURCE_TIMESTAMP_DESTINATIONORDER_QOS;
		}
		else if(tmp[i].policy_id == 13)
		{
			//History (MaxQueueLength)
			dwQos.history.kind = ::DDS::KEEP_LAST_HISTORY_QOS;
			dwQos.history.depth = tmp[i].count;
		}
		else if(tmp[i].policy_id == 20)
		{
			//Transport_Priority
			dwQos.transport_priority.value = (CORBA::Long)tmp[i].count;
		}
		else if(tmp[i].policy_id == 30)
		{
			//Lifespan (Timeout)
			// 0 is infinity....I think.
			if(tmp[i].count == 0)
			{
				DDS::Duration_t d = { DDS::DURATION_INFINITE_SEC , DDS::DURATION_INFINITE_NSEC };
				dwQos.lifespan.duration = d;
			}
			else
			{
				DDS::Duration_t d = { (CORBA::Long)tmp[i].count , (CORBA::ULong)tmp[i].count };
				dwQos.lifespan.duration = d;
			}

		}
		else
		{
			//This shouldn't happen
		}
	}


	dwQos.durability.kind = ::DDS::TRANSIENT_LOCAL_DURABILITY_QOS;
	dwQos.durability_service.service_cleanup_delay.sec = 10;
}

void DDSPublisher::initializeDataWriter()
{
	dw = pub->create_datawriter(topic.in(),
			dwQos,  DDS::DataWriterListener::_nil(),
			OpenDDS::DCPS::DEFAULT_STATUS_MASK);
	if(CORBA::is_nil(dw.in())){
	        ACS_STATIC_LOG(LM_FULL_INFO, "DDSPublisher::initializeDataWriter", (LM_ERROR,
                	                  "Create datawriter failed"));
	}
}

int DDSPublisher::createPublisher()
{
        ACS_TRACE("DDSPublisher::createPublisher");
	if(partitionName==NULL){
		pub =  participant->create_publisher(PUBLISHER_QOS_DEFAULT,
				DDS::PublisherListener::_nil(),
				OpenDDS::DCPS::DEFAULT_STATUS_MASK);
	        ACS_STATIC_LOG(LM_FULL_INFO, "DDSPublisher::createPublisher", (LM_INFO,
               	                  "Creating Publisher with default QoS"));
	}

	else{
		pub = participant->create_publisher(pubQos,
				DDS::PublisherListener::_nil(),
				OpenDDS::DCPS::DEFAULT_STATUS_MASK);
	        ACS_STATIC_LOG(LM_FULL_INFO, "DDSPublisher::createPublisher", (LM_INFO,
               	                  "Creating Publisher with partition %s ",partitionName));
	}

	if(CORBA::is_nil(pub.in())){
	        ACS_STATIC_LOG(LM_FULL_INFO, "DDSPublisher::createPublisher", (LM_ERROR,
	               	                  "Create Publisher failed"));
		return 1;
	}

	pub_impl= dynamic_cast<OpenDDS::DCPS::PublisherImpl*>(pub.in());
	if(pub_impl == NULL){
	        ACS_STATIC_LOG(LM_FULL_INFO, "DDSPublisher::createPublisher", (LM_ERROR,
	               	                  "Failed to obtain publisher servant"));
		return 1;
	}
	return 0;
}
