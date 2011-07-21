#include "bulkDataNTReceiverImpl.h"
#include "bulkDataNTCallback.h"
#include <iostream>

class  TestCB:  public BulkDataCallback
{
public:
	int cbStart(ACE_Message_Block * userParam_p = 0)
	{
		std::cout << "=>cbStart[" << recvName_m << "/" << flowname_m << "]: got " << userParam_p->length() << " :";
		for(unsigned int i=0; i<userParam_p->length(); i++)
		{
			std::cout <<  *(char*)(userParam_p->base()+i);
		}
		std::cout << std::endl;
		return 0;
	}

	int cbReceive(ACE_Message_Block * frame_p)
	{
		std::cout << "=>cbReceive[" << recvName_m << "/" << flowname_m << "]: got " << frame_p->length() << " :";
/*		for(unsigned int i=0; i<frame_p->length(); i++)
		{
			std::cout <<  *(char*)(frame_p->base()+i);
		}
	*/	std::cout << std::endl;
		return 0;
	}

	int cbStop()
	{
		std::cout << "=>cbStop[" << recvName_m << "/" << flowname_m << "]" << std::endl;
		return 0;
	}

};


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BulkDataNTReceiverImpl<TestCB>)
/* ----------------------------------------------------------------*/
