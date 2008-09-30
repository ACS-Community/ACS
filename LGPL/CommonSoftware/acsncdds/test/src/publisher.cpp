#include <iostream>
#include <temperatureDataBlockEventTypeSupportImpl.h>
#include <dds/DCPS/Service_Participant.h>
#include <dds/DCPS/Marked_Default_Qos.h>
#include <dds/DCPS/PublisherImpl.h>
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


		temperatureDataBlockEventTypeSupport_var ts;
		ts=new temperatureDataBlockEventTypeSupportImpl();
		if (DDS::RETCODE_OK != ts->register_type(participant.in(),"")){
			std::cerr << "register_type failed" << std::endl;
		}

		CORBA::String_var type_name = ts->get_type_name();
		DDS::TopicQos topic_qos;
		participant->get_default_topic_qos(topic_qos);
		DDS::Topic_var topic =
				  participant->create_topic("Default Topic",
										type_name.in(),
										topic_qos,
										DDS::TopicListener::_nil());
		if (CORBA::is_nil(topic.in())){
			std::cerr << "create_topic failed" << std::endl;
			return 1;
		}
      OpenDDS::DCPS::TransportImpl_rch tcp_impl =
         TheTransportFactory->create_transport_impl (transport_impl_id,
                         ::OpenDDS::DCPS::AUTO_CONFIG);

		OpenDDS::DCPS::SimpleTcpTransport * tcp;
		tcp =  (OpenDDS::DCPS::SimpleTcpTransport *) tcp_impl.in();
		std::cout << "Network Configuration => ";
		std::cout << tcp->get_configuration()->local_address_str_ << endl;
		
		
		//Setup Publisher QoS, add the partition QoS policy
		DDS::PublisherQos pub_qos;
		participant->get_default_publisher_qos(pub_qos);
		pub_qos.partition.name.length(1);
		pub_qos.partition.name[0]=CORBA::string_dup("partition");

		DDS::Publisher_var pub = participant->create_publisher(pub_qos,
				DDS::PublisherListener::_nil());
		if(CORBA::is_nil(pub.in())){
			std::cerr << "create publisher failed" << std::endl;
			return 1;
		}

		OpenDDS::DCPS::PublisherImpl* sub_impl=
				  dynamic_cast<OpenDDS::DCPS::PublisherImpl*>(pub.in());
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
		DDS::DataWriterQos dw_qos;
		pub->get_default_datawriter_qos (dw_qos);

		DDS::DataWriter_var dw = pub->create_datawriter(topic.in(),
				dw_qos,
				DDS::DataWriterListener::_nil());
		if(CORBA::is_nil(dw.in())){
			std::cerr << "create datawriter failed" << std::endl;
		}

		//Write Messages to DCPS, Subscriber must recive the same
		::TESTFRIDGE::temperatureDataBlockEventDataWriter_var temp_dw=
			::TESTFRIDGE::temperatureDataBlockEventDataWriter::_narrow(dw.in());
		TESTFRIDGE::temperatureDataBlockEvent message;
		message.key = 1;
		std::cout<< "Trying to send a message" << std::endl;
		DDS::InstanceHandle_t handle = temp_dw->_cxx_register(message);
		for(int i=0;i<100;i++){
			message.status = ATREF;
			message.absolutDiff = 20.0+i*0.1;
			std::cout << "Send Message "<< i << std::endl; 
			DDS::ReturnCode_t ret = temp_dw->write(message, handle);
			sleep(1);
		}

		//TODO: Clean 
	}
  	catch( CORBA::Exception &e){
		cerr << "SUB: Exception caught in main ():" << endl << e << endl;
		return 1;
	}

}
