#include <DDSSubscriber.h>
#include <iostream>

using namespace ddsnc;

DDSSubscriber::DDSSubscriber(CORBA::String_var channel_name):
	DDSHelper(channel_name)
{
}

int DDSSubscriber::createSubscriber()
{
	ACS_STATIC_LOG(LM_FULL_INFO, "DDSSubscriber::createSubscriber", (LM_INFO,
			 ""));
	if(partitionName==NULL){
		sub=participant->create_subscriber(SUBSCRIBER_QOS_DEFAULT,
				DDS::SubscriberListener::_nil(),
				OpenDDS::DCPS::DEFAULT_STATUS_MASK);
		ACS_STATIC_LOG(LM_FULL_INFO, "DDSSubscriber::createSubscriber", (LM_INFO,
				 "Creating Subscriber with default Qos"));
	}

	else{
		 sub = participant->create_subscriber(subQos,
				 DDS::SubscriberListener::_nil(),
				 OpenDDS::DCPS::DEFAULT_STATUS_MASK);
		ACS_STATIC_LOG(LM_FULL_INFO, "DDSSubscriber::createSubscriber", (LM_INFO,
				 "Creating Subscriber with partition: '%s'",partitionName));
	}

	if ((CORBA::is_nil (sub.in ()))) {
		ACS_STATIC_LOG(LM_FULL_INFO, "DDSSubscriber::createSubscriber", (LM_ERROR,
				 "create_subscriber failed"));
		return 1;
	}
	
	sub_impl=dynamic_cast<OpenDDS::DCPS::SubscriberImpl*>(sub.in());
	if (0 == sub_impl) {
		ACS_STATIC_LOG(LM_FULL_INFO, "DDSSubscriber::createSubscriber", (LM_ERROR,
				 "Failed to obtain publisher servant"));
		return 1;
	}
	return attachToTransport();
}


int DDSSubscriber::attachToTransport()
{
	OpenDDS::DCPS::AttachStatus status = 
		sub_impl->attach_transport(transport_impl.in());
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
	ACS_STATIC_LOG(LM_FULL_INFO, "DDSSubscriber::attachToTransport", (LM_ERROR,
			 "Failed to attach to the transport. Status == '%s'",status_str.c_str()));
        return 1;
      }
	return 0;
}

void DDSSubscriber::consumerReady()
{
	ACS_STATIC_LOG(LM_FULL_INFO, "DDSSubscriber::consumerReady", (LM_INFO,
			 ""));

	DDS::DataReader_var dr = sub->create_datareader(topic.in(),
			drQos, listener->in(), OpenDDS::DCPS::DEFAULT_STATUS_MASK);
	if(CORBA::is_nil(dr.in())){
		ACS_STATIC_LOG(LM_FULL_INFO, "DDSSubscriber::consumerReady", (LM_ERROR,
				 "create_datareader failed"));
	}
	initialized=true;
}

