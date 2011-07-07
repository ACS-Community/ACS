#include "bulkDataNTSender.h"
#include <iostream>



int main()
{
	AcsBulkdata::BulkDataNTSender sender;
	sender.initialize();
	sender.createFlow(2);
	std::cout << "press a key to start.." << std::endl;
	getchar();

	unsigned char parm[]="123";

	sender.startSend(0, parm, 3);

	//unsigned char data[]="Hello wrold !!!!";
	unsigned char *data= new unsigned char[65000];
	for (unsigned int i=0; i<65000; i++)
			data[i]=i;
	sender.sendData(0, data, 65000);
	sender.sendData(1, data, 65000);

	for (unsigned int i=0; i<65000; i++)
				data[i]=i%10;
	sender.sendData(0, data, 65000);
	sender.sendData(1, data, 65000);

	std::cout << "press a key to send stop.." << std::endl;
	getchar();
	sender.stopSend(0);
	sender.stopSend(1);
	std::cout << "press a key to exit.." << std::endl;
	getchar();
	sender.destroyFlows();
}
