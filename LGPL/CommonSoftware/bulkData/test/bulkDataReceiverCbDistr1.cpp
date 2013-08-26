#include "bulkDataReceiverCbDistr1.h"

BulkDataReceiverCbDistr1::BulkDataReceiverCbDistr1() : count1_m(0)
{
    ACS_TRACE("BulkDataReceiverCbDistr1::BulkDataReceiverCbDistr1"); 
}


BulkDataReceiverCbDistr1::~BulkDataReceiverCbDistr1()
{
    ACS_TRACE("BulkDataReceiverCbDistr1::~BulkDataReceiverCbDistr1"); 
}


int
BulkDataReceiverCbDistr1::cbStart(ACE_Message_Block * userParam_p)
{
    ACS_TRACE("BulkDataReceiverCbDistr1::cbStart"); 

    if(flowNumber_m == 1)
	{
	ACS_SHORT_LOG((LM_INFO, "RECEIVER 1 flowname 1: %s", flowname_m.c_str()));
	ACS_SHORT_LOG((LM_INFO, "RECEIVER 1 length param flowname 1: %d", userParam_p->length()));

	count1_m = 0;
	}

    return 0;
}


int
BulkDataReceiverCbDistr1::cbReceive(ACE_Message_Block * frame_p)
{
    ACS_TRACE("BulkDataReceiverCbDistr1::cbReceive"); 

    if(flowNumber_m == 1)
	{
        ACS_SHORT_LOG((LM_DEBUG, "RECEIVER 1 flowname 1: %s", flowname_m.c_str()));
        ACS_SHORT_LOG((LM_DEBUG, "RECEIVER 1 length data flowname 1: %d", frame_p->length()));
	count1_m += frame_p->length();
	}

    return 0;
}


int
BulkDataReceiverCbDistr1::cbStop()
{
    ACS_TRACE("BulkDataReceiverCbDistr1::cbStop"); 

    if(flowNumber_m == 1)
	ACS_SHORT_LOG((LM_INFO, "RECEIVER 1 flow 1 total length: %d", count1_m)); 

    return 0;
}


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BulkDataReceiverDistr1Impl<BulkDataReceiverCbDistr1>)
/* ----------------------------------------------------------------*/

    
    
    
