#include <iostream>
#include "temperatureDataBlockEventTypeSupportImpl.h"
#include "DataReaderListener.h"
#include <dds/DCPS/Service_Participant.h>
#include <dds/DCPS/Marked_Default_Qos.h>
#include <dds/DCPS/SubscriberImpl.h>
#include <dds/DCPS/transport/framework/TheTransportFactory.h>
#include <dds/DCPS/transport/simpleTCP/SimpleTcpConfiguration.h>
#include <dds/DCPS/transport/simpleTCP/SimpleTcpTransport.h>

#include <ace/streams.h>
#include "ace/Get_Opt.h"

using namespace TESTFRIDGE;


OpenDDS::DCPS::TransportIdType transport_impl_id = 1;

int main (int argc, char *argv[])
{
	try{
		DDS::DomainParticipantFactory_var dpf =
        TheParticipantFactoryWithArgs(argc,argv);
		DDS::DomainParticipant_var participant =
        dpf->create_participant(411, //Arbitrary Domain ID
                        PARTICIPANT_QOS_DEFAULT,
                        DDS::DomainParticipantListener::_nil());
		if (CORBA::is_nil (participant.in())){
      	std::cerr << "Create Participant Failed." << std::endl;
        	return 1;
		}

		//Data type support initialization
		temperatureDataBlockEventTypeSupport_var ts;
		ts=new temperatureDataBlockEventTypeSupportImpl();
		if (DDS::RETCODE_OK != ts->register_type(participant.in(),"")){
			std::cerr << "register_type failed" << std::endl;
		}
		
		//Topic registration
		CORBA::String_var type_name = ts->get_type_name();
		DDS::TopicQos topic_qos;
		participant->get_default_topic_qos(topic_qos);
		::std::cout << "Type name: " << type_name << ::std::endl;
		DDS::Topic_var topic =
				  participant->create_topic("Default Topic",
										type_name.in(),
										topic_qos,
										DDS::TopicListener::_nil());
		if (CORBA::is_nil(topic.in())){
			std::cerr << "create_topic failed" << std::endl;
			return 1;
		}
		
		//Configuring network protocol
      OpenDDS::DCPS::TransportImpl_rch tcp_impl =
         TheTransportFactory->create_transport_impl (transport_impl_id,
                         ::OpenDDS::DCPS::AUTO_CONFIG);

      OpenDDS::DCPS::SimpleTcpTransport * tcp; 
      tcp =  (OpenDDS::DCPS::SimpleTcpTransport *) tcp_impl.in(); 
      std::cout << "Network Configuration => "; 
      std::cout << tcp->get_configuration()->local_address_str_ << endl; 

      //Setup Publisher QoS, add the partition QoS policy
      DDS::SubscriberQos sub_qos;
      participant->get_default_subscriber_qos(sub_qos);
      sub_qos.partition.name.length(1);
      sub_qos.partition.name[0]=CORBA::string_dup("partition");


		DDS::Subscriber_var sub = participant->create_subscriber(sub_qos,
			DDS::SubscriberListener::_nil());
		if(CORBA::is_nil(sub.in())){
			std::cerr << "create subscriber failed" << std::endl;
			return 1;
		}
		
		//Create the subscriber implementation and attach the network transport
		OpenDDS::DCPS::SubscriberImpl* sub_impl=
				  dynamic_cast<OpenDDS::DCPS::SubscriberImpl*>(sub.in());
		if(0 == sub_impl){
			std::cerr << "Failed to obtain publisher servant" << std::endl;
			return 1;
		}
		OpenDDS::DCPS::AttachStatus status =
				  sub_impl->attach_transport(tcp_impl.in());
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
        cerr << "Failed to attach to the transport. Status == "
          << status_str.c_str() << endl;
        exit(1);
      }

		//Create the data reader listener
		DDS::DataReaderListener_var listener (new DataReaderListenerImpl);
		DataReaderListenerImpl* listener_servant =
			dynamic_cast<DataReaderListenerImpl*>(listener.in());
		
		if(CORBA::is_nil (listener.in())){
			std::cerr << "listener is nil" << std::endl;
			return 1;
		}

		//Apply Qos Policies, in this case the partition 
		DDS::DataReaderQos dr_qos;
		sub->get_default_datareader_qos (dr_qos);
		DDS::DataReader_var dr = sub->create_datareader(topic.in(),
				dr_qos,
				listener.in());
		if(CORBA::is_nil(dr.in())){
			std::cerr << "create_datareader failed" << std::endl;
			return 1;
		}

		//Maybe I can use a mutex here
		while(listener_servant->num_reads() < 100){
			sleep(1);
		}

		//TODO: Clean
	}
  	catch( CORBA::Exception &e){
		cerr << "SUB: Exception caught in main ():" << endl << e << endl;
		return 1;
	}       
}
