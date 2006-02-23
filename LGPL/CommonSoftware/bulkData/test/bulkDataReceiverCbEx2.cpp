#include "bulkDataReceiverCbEx2.h"

BulkDataReceiverCbEx2::BulkDataReceiverCbEx2() : count1_m(0)
{
    ACS_TRACE("BulkDataReceiverCbEx2::BulkDataReceiverCbEx2"); 

}


BulkDataReceiverCbEx2::~BulkDataReceiverCbEx2()
{
    ACS_TRACE("BulkDataReceiverCbEx2::~BulkDataReceiverCbEx2"); 
}


int
BulkDataReceiverCbEx2::cbStart(ACE_Message_Block * userParam_p)
{
    ACS_TRACE("BulkDataReceiverCbEx1::cbStart");

    count1_m = 0;

    return 0;
}

int
BulkDataReceiverCbEx2::cbReceive(ACE_Message_Block * frame_p)
{
    ACS_TRACE("BulkDataReceiverCbEx1::cbReceive");

    count1_m += frame_p->length();

    return 0;
}

int
BulkDataReceiverCbEx2::cbStop()
{
    ACS_TRACE("BulkDataReceiverCbEx1::cbStop");

    ACS_SHORT_LOG((LM_INFO, "flow 1 total length: %d", count1_m)); 

    return 0;
}

void
BulkDataReceiverCbEx2::setReceiverImpl(BulkDataReceiverEx2Impl<BulkDataReceiverCbEx2> *recv)
{
    ACE_TRACE("BulkDataReceiverCbEx2::setReceiverImpl");

    if (recv == NULL)
	{
	ACS_SHORT_LOG((LM_INFO," BulkDataReceiverCbEx2::setReceiverImpl recv = 0"));
	}
    else
	{
	recv->myMethod();
	}
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BulkDataReceiverEx2Impl<BulkDataReceiverCbEx2>)
/* ----------------------------------------------------------------*/

    
