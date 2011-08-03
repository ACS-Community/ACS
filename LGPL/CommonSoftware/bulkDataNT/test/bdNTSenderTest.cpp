#include "bulkDataNTSenderFlow.h"
#include <iostream>

using namespace AcsBulkdata;

int main()
{

	LoggingProxy m_logger(0, 0, 31, 0);

	LoggingProxy::init (&m_logger);
    ACS_CHECK_LOGGER;


	BulkDataNTSenderStream senderStream1("DefaultStream");

	BulkDataNTSenderFlow* flow0 = senderStream1.createFlow("00");
	BulkDataNTSenderFlow* flow1 = senderStream1.createFlow("01");

	std::cout << "press a key to start.." << std::endl;
	getchar();

	unsigned char parm[]="123";

	flow0->startSend(parm, 3);

	//unsigned char data[]="Hello wrold !!!!";
	unsigned char *data= new unsigned char[65000];
	for (unsigned int i=0; i<65000; i++)
			data[i]=i;
	flow0->sendData(data, 65000);
	flow1->sendData(data, 65000);

	for (unsigned int i=0; i<65000; i++)
				data[i]=i%10;
	flow0->sendData(data, 65000);
	flow1->sendData(data, 65000);

	std::cout << "press a key to send stop.." << std::endl;
	getchar();
	flow0->stopSend();
	flow1->stopSend();
	std::cout << "press a key to exit.." << std::endl;
	getchar();

	delete flow0;
// flow1 will be deleted when senderStream1 is deleted

}
