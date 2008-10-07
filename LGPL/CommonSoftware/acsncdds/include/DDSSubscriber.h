#ifndef _DDS_SUBSCRIBER_H
#define _DDS_SUBSCRIBER_H

#include "DDSHelper.h"
#include <dds/DCPS/SubscriberImpl.h>

namespace ddsnc{
	class DDSSubscriber: public ::ddsnc::DDSHelper{
		private:
		DDS::Subscriber_var sub;
		OpenDDS::DCPS::SubscriberImpl *sub_impl;
		DDS::SubscriberQos sub_qos;

		int attachToTransport();
		void initialize();

		public:
		DDSSubscriber(const char *channelName, int argc, char** argv);
		int createSubscriber();
	};
}

#endif
