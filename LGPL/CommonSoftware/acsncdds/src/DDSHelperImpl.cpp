#include <DDSHelper.h>
#include <stdlib.h>

using namespace ddsnc;

DDSHelper::DDSHelper(const char* channelName, int argc, char** argv)
{
	/*
	argc=5;
	argv[0]="Participant\0";
	argv[1]="-ORBSvcConf\0";
	argv[2]="tcp.conf\0";
	argv[3]="-DCPSConfigFile\0";
	argv[4]="sub.ini\0";
	*/
	transport_impl_id=1;
	dpf=TheParticipantFactoryWithArgs(argc, (ACE_TCHAR**)argv);
	initialized=false;

	::std::string channelStr(channelName);
   int pos=channelStr.find_last_of("_");
   if(pos<0){

      setTopicName(channelName);
      return;
   }
   setTopicName(channelStr.substr(++pos).c_str());
	std::cerr << "Trying to set topic name: " << topicName <<std::endl;
   setPartitionName(channelStr.substr(0,pos).c_str());

}

int DDSHelper::createParticipant(){
	std::cerr << "DDSHelper::createParticipant" << std::endl;
	participant = dpf->create_participant(DOMAIN_ID,
						  PARTICIPANT_QOS_DEFAULT,
						  DDS::DomainParticipantListener::_nil());
	if (CORBA::is_nil(participant.in())){
		std::cerr << "Create Participant Failed." << std::endl;
		return 1;
	}
	return 0;
}


void DDSHelper::initializeTransport(){
	transport_impl=
		TheTransportFactory->create_transport_impl(transport_impl_id,
											  ::OpenDDS::DCPS::AUTO_CONFIG);
}

void DDSHelper::setTopicName(const char* topicName)
{
	this->topicName=strdup(topicName);
}

void DDSHelper::initializeTopic(const char* topicName, CORBA::String_var typeName)
{
	std::cerr << "Initializing topic: " << this->topicName << std::endl;
	std::cerr << "with type: " << typeName << std::endl;
	DDS::TopicQos topicQos;
	participant->get_default_topic_qos(topicQos);
	topic=participant->create_topic(topicName, typeName.in(),
			topicQos, DDS::TopicListener::_nil());

	if (CORBA::is_nil(topic.in())){
         std::cerr << "create_topic failed" << std::endl;
	}
	
}

void DDSHelper::setPartitionName(const char* partitionName){
	this->partitionName=strdup(partitionName);
}

void DDSHelper::disconnect()
{
	if(initialized==true){
		participant->delete_contained_entities();
		dpf->delete_participant(participant.in());
		TheTransportFactory->release();
		TheServiceParticipant->shutdown();
		initialized=false;
	}
}

DDSHelper::~DDSHelper()
{
	disconnect();
	free(partitionName);
	free(topicName);
}
