#include <DDSHelper.h>
#include <ace/Service_Config.h>
#include <string.h>
#include <stdlib.h>

using namespace ddsnc;

DDSHelper::DDSHelper(const char* channelName)
{
	int argc;
	char* argv[5];
	argc=5;

	argv[0] = strdup("Participant");
	argv[1] = strdup("-DCPSInforepo");
	argv[2] = strdup("corbaloc:iiop:127.0.0.1:3999/DCPSInfoRepo");
	argv[3] = strdup("-DCPSDebugLevel");
	argv[4] = strdup("0");

	transport_impl_id=1;

	std::cerr << "Registering TransportImpl"	<< std::endl;
	ACE_Service_Config::process_directive(
			"static DCPS_SimpleTcpLoader \"-type SimpleTcp\"");

	dpf=TheParticipantFactoryWithArgs(argc, (ACE_TCHAR**)argv);
	
	initialized=false;

	setPartitionName(channelName);
//	::std::string channelStr(channelName);
//	 int pos=channelStr.find_last_of("_");
//   if(pos<0){
//
//      setTopicName(channelName);
//      return;
//   }
//   setTopicName(channelStr.substr(++pos).c_str());
//	  std::cerr << "Trying to set topic name: " << topicName <<std::endl;
//   setPartitionName(channelStr.substr(0,--pos).c_str());

}

int DDSHelper::createParticipant(){
	std::cerr << "DDSHelper::createParticipant" << std::endl;
	participant = dpf->create_participant(DOMAIN_ID,
			PARTICIPANT_QOS_DEFAULT,
			DDS::DomainParticipantListener::_nil());
	
	std::cerr << "Created the participant" << std::endl;

	if (CORBA::is_nil(participant.in())){
		std::cerr << "Create Participant Failed." << std::endl;
		return 1;
	}
	return 0;
}


void DDSHelper::initializeTransport(){
	std::cerr << " DDSHelper::initializeTransport()" << std::endl;
	try{
	transport_impl=
		TheTransportFactory->create_transport_impl(transport_impl_id,
				"SimpleTcp", ::OpenDDS::DCPS::AUTO_CONFIG);
	}
	catch(OpenDDS::DCPS::Transport::Duplicate &ex){
		transport_impl=TheTransportFactory->obtain(transport_impl_id);
	}
//	std::cerr << "Configuring transport" << std::cerr;	
//	OpenDDS::DCPS::TransportConfiguration_rch config=
//		         TheTransportFactory->create_configuration(transport_impl_id,
//							               "SimpleTcp");
//
//	OpenDDS::DCPS::SimpleTcpConfiguration* tcp_config =
//	  	static_cast<OpenDDS::DCPS::SimpleTcpConfiguration*>(config.in());
//
//	ACE_TCHAR dir[1024];
//	ACE_INET_Addr local_address ;
//	local_address.addr_to_string(dir, 1024);
//	std::string dir_str(dir);
//	tcp_config->local_address_ = local_address;
//	tcp_config->local_address_str_ = dir_str;
//	std::cerr << dir_str << std::endl;
//
//	transport_impl->configure(tcp_config);
//
	std::cerr << "Finishing configuration" << std::endl;

}

void DDSHelper::setTopicName(const char* topicName)
{
	this->topicName=strdup(topicName);
}

void DDSHelper::initializeTopic(const char* topicName, CORBA::String_var typeName)
{
	std::cerr << "Initializing topic: " << this->topicName << std::endl;
	std::cerr << "with type: " << typeName << std::endl;
	participant->get_default_topic_qos(topicQos);
	topic=participant->create_topic(topicName, typeName.in(),
			topicQos, DDS::TopicListener::_nil());

	if (CORBA::is_nil(topic.in())){
         std::cerr << "create_topic failed" << std::endl;
	}
	
}

void DDSHelper::initializeTopic(CORBA::String_var typeName)
{
	std::string topicStr (typeName);
	int f = topicStr.find_first_of(":");
	int l = topicStr.find_last_of(":");
	topicName = strdup(topicStr.substr(f+1, (l-1)-f).c_str());
	initializeTopic(topicName, typeName);
}

void DDSHelper::setPartitionName(const char* partitionName){
	this->partitionName=strdup(partitionName);
}

void DDSHelper::disconnect()
{
	std::cerr<< "DDSHelper::disconnect()"<<std::endl;
	if(initialized==true){
		participant->delete_contained_entities();
		dpf->delete_participant(participant.in());
		//TheTransportFactory->release();
		//TheServiceParticipant->shutdown();
		initialized=false;
	}
}

DDSHelper::~DDSHelper()
{
	std::cerr << "DDSHelper::~DDSHelper()" << std::endl;
	disconnect();
	free(partitionName);
	free(topicName);
}
