#include "bulkDataNTReceiverImpl.h"
#include "bulkDataNTCallback.h"
#include <iostream>

class  TestCB:  public BulkDataCallback
{
public:
	int cbStart(unsigned char* userParam_p, unsigned  int size)
	{
		std::cout << "=>cbStart[" << recvName_m << "/" << flowName_m << "]: got " << size << " :";
		for(unsigned int i=0; i<size; i++)
		{
			std::cout <<  *(char*)(userParam_p+i);
		}
		std::cout << std::endl;
		return 0;
	}

	int cbReceive(unsigned char* userParam_p, unsigned  int size)
	{
		std::cout << "=>cbReceive[" << recvName_m << "/" << flowName_m << "]: got " << size << " :";
/*		for(unsigned int i=0; i<frame_p->length(); i++)
		{
			std::cout <<  *(char*)(frame_p->base()+i);
		}
	*/	std::cout << std::endl;
		return 0;
	}

	int cbStop()
	{
		std::cout << "=>cbStop[" << recvName_m << "/" << flowName_m << "]" << std::endl;
		return 0;
	}

};


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BulkDataNTReceiverImpl<TestCB>)
/* ----------------------------------------------------------------*/
