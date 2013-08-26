#include <maciSimpleClient.h>
#include <iostream>
#include <acsncSimpleConsumer.h>
#include "SimpleExampleS.h"

void handlerFunction(DDS_SIMPLE_EXAMPLE::simpleMessage m, void *other)
{
	std::cout << "Arrived message!!" << std::endl;

}

int main(int argc, char**argv)
{
	maci::SimpleClient client;

	client.init(argc,argv);
	client.login();

	nc::SimpleConsumer<DDS_SIMPLE_EXAMPLE::simpleMessage> *cons_p=0;
	
	ACS_NEW_SIMPLE_CONSUMER(cons_p, DDS_SIMPLE_EXAMPLE::simpleMessage,
			DDS_SIMPLE_EXAMPLE::CHANNEL_NAME, 
			handlerFunction, 
			(void *)0);

	cons_p->consumerReady();

	ACE_Time_Value tv(100);
	client.run(tv);

	client.logout();
	ACE_OS::sleep(5);

	cons_p->disconnect();
	cons_p=0;

	return 0;

}
