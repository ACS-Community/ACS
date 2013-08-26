#include <maciSimpleClient.h>
#include <iostream>
#include <DDSSubscriber.h>
#include "simpleMessageTypeSupportImpl.h"


int testNum=0;

void handlerFunction(DDS_SIMPLE_EXAMPLE::simpleMessage m, void *other)
{
	testNum++;
	std::cout << "Arrived message!" << std::endl;
	std::cout << m.seqnum << std::endl;
}

int main(int argc, char**argv)
{
	maci::SimpleClient client;

	client.init(argc,argv);
	client.login();
	
	ddsnc::DDSSubscriber *sub_p=0;
	
	ACS_NEW_DDS_SUBSCRIBER(sub_p, DDS_SIMPLE_EXAMPLE::simpleMessage,
			DDS_SIMPLE_EXAMPLE::CHANNEL_NAME, &handlerFunction, (void *)0);

	sub_p->consumerReady();

	ACE_Time_Value tv(100);
	client.run(tv);

	client.logout();
	ACE_OS::sleep(5);

	sub_p->disconnect();
	std::cout<< "Number of message: ";
	std::cout<< testNum << std::endl;	

	delete sub_p;
	return 0;

}
