#ifndef _DDS_HELPER_H_
#define _DDS_HELPER_H_

#include <iostream>
#include <string.h>
#include <loggingACEMACROS.h>
#include <dds/DCPS/Service_Participant.h>
#include <dds/DCPS/Marked_Default_Qos.h>
#include <dds/DCPS/transport/framework/TheTransportFactory.h>
#include <dds/DCPS/transport/framework/TransportImpl.h>
#include <dds/DCPS/transport/simpleTCP/SimpleTcpConfiguration.h>

const unsigned int DOMAIN_ID=411;

namespace ddsnc{

/**
 * Base class for ACSNCDDS, this class contains all the common functionality
 * offered by DDS publishers and DDS subscribers. If it wanted to use this 
 * class, it must be inherited.
 *
 * @author Jorge Avarias <javarias[at]inf.utfsm.cl>
 * @see DDSPublisher
 * @see DDSSubscriber
 */	
	
	class DDSHelper{
		private:
		void setTopicName(const char* topicName);
		void init(const char* channelName, const char* DCPSInfoRepoLoc);
		/*int argc;
		const ACE_TCHAR* argv[];*/

		protected:
		DDS::DomainParticipantFactory_ptr dpf;
		OpenDDS::DCPS::TransportIdType transport_impl_id;
		DDS::DomainParticipant_var participant;
		OpenDDS::DCPS::TransportImpl_rch transport_impl;
		DDS::Topic_var topic;
		char* partitionName;
		char* topicName;
		bool initialized; /**< a flag that shows the initialization status 
								  of the class*/

		/**
		 * Constructor for DDSHelper, this constructor will be set the
		 * location of DCPSInfoRepo to corbaloc:iiop:127.0.0.1:3999/DCPSInfoRepo
		 *
		 * @param channelName the name of the channel that will be mapped to a
		 * partition.
		 * 
		 */
		DDSHelper(CORBA::String_var channelName);

		/**
		 * Constructor for DDSHelper
		 *
		 * @param channelName the name of the channel that will be mapped to a
		 * partition.
		 *
		 * @param DCPSInfoRepoLoc address of the DCPSInfoRepo. The address
		 * must be a format supported by TAO
		 */
		DDSHelper(const char* channelName, const char* DCPSInfoRepoLoc);		
		
		/**
		 * Destructor for DDSHelper before free the varibales it call to
		 * disconnect method, if disconnect method was called, the
		 * destructor only free the variables.
		 *
		 * @see disconnect()
		 */
		virtual ~DDSHelper();
		int createParticipant();

		/**
		 *	Locates a Transport id not in use and assign it to transport_impl_id.
		 *	The transport configuration is automatically configured by TAO
		 *	pluggable framework.
		 *	
		 */
		//Maybe this method could generate a race condition. It's neccessary to
		//test more.
		void initializeTransport();

		/**
		 * Initialize the topic and register the type supported by the topic, 
		 * this method must be called after creation of the
		 * participant, initialization of the transport, creation of the 
		 * Publisher or Subscriber and initialization of the type support
		 *
		 * @param topicName the topicName
		 * @param typeName the type to be registered in the topic, the topic
		 * must be in CORBA type name format
		 */
		void initializeTopic(const char* topicName, CORBA::String_var typeName);
		
		void initializeTopic(CORBA::String_var typeName);
		
		void setPartitionName(const char* partitionName);
		
		public:
		/**
		 * Disconnect method will destroy all the DDS entities initilizated, 
		 * it will
		 * release the trasport factory, shutdown the DDS participant service
		 * and set the status of the class as not initilizated.
		 * This method should be used when you want to destroy a Publisher or 
		 * Subscriber object that inherit this class.
		 *
		 * @see ~DDSHelper()
		 */
		void disconnect();
		DDS::TopicQos topicQos;

		static void cleanUp();
		CORBA::String_var getChannelName();

	};
}

#endif
