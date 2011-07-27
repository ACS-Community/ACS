//#include "bulkDataNTReceiverFlow.h"
#include "bulkDataNTReceiverStream.h"
#include "bulkDataNTCallback.h"
#include <iostream>

class  TestCB:  public BulkDataCallback
{
public:
	int cbStart(ACE_Message_Block * userParam_p = 0)
	{
		std::cout << "cbStart: got " << userParam_p->length() << " :";
		for(unsigned int i=0; i<userParam_p->length(); i++)
		{
			std::cout <<  *(char*)(userParam_p->base()+i);
		}
		std::cout << std::endl;
		return 0;
	}

	int cbReceive(ACE_Message_Block * frame_p)
	{
		std::cout << "cbReceive: got " << frame_p->length() << " :";
/*		for(unsigned int i=0; i<frame_p->length(); i++)
		{
			std::cout <<  *(char*)(frame_p->base()+i);
		}
	*/	std::cout << std::endl;
		return 0;
	}

	int cbStop()
	{
		std::cout << "cbStop" << std::endl;
		return 0;
	}

};


int main()
{
	AcsBulkdata::BulkDataNTReceiverStream<TestCB> receiverStream("TestFlow");

	receiverStream.createFlow("00");

	TestCB *cb = new TestCB();
	receiverStream.createFlow("01", cb);

	BulkDataNTReceiverFlow *flow0 = receiverStream.getFlow("00");

	std::cout << "press a key to end.." << std::endl;
	getchar();

}
