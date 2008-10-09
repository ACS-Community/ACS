#ifndef _DDS_SUBSCRIBER_H
#define _DDS_SUBSCRIBER_H

#include <DDSHelper.h>
#include <acsddsncDataReaderListener.h>
#include <dds/DCPS/SubscriberImpl.h>

namespace ddsnc{
	class DDSSubscriber : public ddsnc::DDSHelper{
		private:
		DDS::Subscriber_var sub;
		OpenDDS::DCPS::SubscriberImpl *sub_impl;
		DDS::SubscriberQos sub_qos;

		int attachToTransport();
		int createSubscriber();

		public:
		DDSSubscriber(const char *channelName, int argc, char** argv) : 
			ddsnc::DDSHelper(channelName, argc, argv)
		{}
		~DDSSubscriber();

		template <class DRV, class DR, class D>
			void addSubscription(
					void (*templateFunction)(D, void *), void *handlerParam=0)
			{
				std::cerr << "DDSSubscriber::addSubscription" << std::endl;

				DDS::DataReaderListener_var listener 
					(new ddsnc::ACSDDSNCDataReaderListener
					 <DRV,DR,D>(templateFunction));
				
				ddsnc::ACSDDSNCDataReaderListener<DRV,DR,D>* listener_servant=
					dynamic_cast<ddsnc::ACSDDSNCDataReaderListener<DRV,DR,D>*>
					(listener.in());

				if(CORBA::is_nil (listener.in())){
					std::cerr << "listener is nil" << std::endl;
				}
				DDS::DataReaderQos dr_qos;
				sub->get_default_datareader_qos (dr_qos);
				DDS::DataReader_var dr = sub->create_datareader(topic.in(),
						dr_qos, listener.in());
				if(CORBA::is_nil(dr.in())){
					std::cerr << "create_datareader failed" << std::endl;
				}
			}

		template <class D, class TSV, class TSI>
		void initialize()
		{
			std::cerr<< "DDSSubscriber::initialize()" << std::endl;
			createParticipant();
			if (CORBA::is_nil (participant.in()))
				 std::cerr << "Participant is nil" << std::endl;
			if(partitionName!=NULL){
				participant->get_default_subscriber_qos(sub_qos);
				sub_qos.partition.name.length(1);
				sub_qos.partition.name[0]=CORBA::string_dup(partitionName);
			}
			initializeTransport();
			createSubscriber();
			/*Initialize Type Support*/
			TSV ts;
			ts = new TSI();
			if (DDS::RETCODE_OK != ts->register_type(participant.in(),"")){
				std::cerr << "register_type failed" << std::endl;
			}
			/*Initialize the Topic*/
			initializeTopic(topicName, ts->get_type_name());
			if(CORBA::is_nil(topic.in()))
				std::cerr<< "Topic is nil" << std::endl;

		}

	};
}

#endif
