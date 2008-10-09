#ifndef _DDS_HELPER_H_
#define _DDS_HELPER_H_

#include <iostream>
#include <string.h>
#include <dds/DCPS/Service_Participant.h>
#include <dds/DCPS/Marked_Default_Qos.h>
#include <dds/DCPS/transport/framework/TheTransportFactory.h>
#include <dds/DCPS/transport/simpleTCP/SimpleTcpConfiguration.h>

static unsigned int DOMAIN_ID=411;

namespace ddsnc{
	
	class DDSHelper{
		/*private:
		int argc;
		const ACE_TCHAR* argv[];*/

		protected:
		OpenDDS::DCPS::TransportIdType transport_impl_id;
		DDS::DomainParticipant_var participant;
		DDS::DomainParticipantFactory *dpf;
		OpenDDS::DCPS::TransportImpl_rch transport_impl;
		DDS::Topic_var topic;
		char* partitionName;
		char* topicName;
		bool initialized;

		DDSHelper(const char *channelName, int argv, char** argc);
		~DDSHelper();
		int createParticipant();
		void initializeTransport();
		void setTopicName(const char* topicName);
		void initializeTopic(const char* topicName, CORBA::String_var typeName);
		void setPartitionName(const char* partitionName);
		
		public:
		void disconnect();

	};
}

#endif
