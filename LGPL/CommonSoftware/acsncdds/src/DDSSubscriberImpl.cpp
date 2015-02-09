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

