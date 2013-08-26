#include "bulkDataReceiverCbThread.h"

BulkDataReceiverCbThread::BulkDataReceiverCbThread() : count1_m(0),count2_m(0),count3_m(0),count4_m(0)
{
    ACS_TRACE("BulkDataReceiverCbThread::BulkDataReceiverCbThread"); 
}


BulkDataReceiverCbThread::~BulkDataReceiverCbThread()
{
    ACS_TRACE("BulkDataReceiverCbThread::~BulkDataReceiverCbThread"); 
}


int
BulkDataReceiverCbThread::cbStart(ACE_Message_Block * userParam_p)
{
    ACS_TRACE("BulkDataReceiverCbThread::cbStart"); 

    if(flowNumber_m == 1)
	{
	ACS_SHORT_LOG((LM_DEBUG, "flowname 1: %s", flowname_m.c_str()));
	ACS_SHORT_LOG((LM_DEBUG, "length param flowname 1: %d", userParam_p->length()));

	count1_m = 0;
	}
    else if(flowNumber_m == 2)
	{
	ACS_SHORT_LOG((LM_DEBUG, "flowname 2: %s", flowname_m.c_str()));
	ACS_SHORT_LOG((LM_DEBUG, "length param flowname 2: %d", userParam_p->length()));

	count2_m = 0;
	}
    else if(flowNumber_m == 3)
	{
	ACS_SHORT_LOG((LM_DEBUG, "flowname 3: %s", flowname_m.c_str()));
	ACS_SHORT_LOG((LM_DEBUG, "length param flowname 3: %d", userParam_p->length()));

	count3_m = 0;
	}
    else if(flowNumber_m == 4)
	{
	ACS_SHORT_LOG((LM_DEBUG, "flowname 4: %s", flowname_m.c_str()));
	ACS_SHORT_LOG((LM_DEBUG, "length param flowname 4: %d", userParam_p->length()));

	count4_m = 0;
	}

    return 0;
}


int
BulkDataReceiverCbThread::cbReceive(ACE_Message_Block * frame_p)
{
    ACS_TRACE("BulkDataReceiverCbThread::cbReceive"); 

    if(flowNumber_m == 1)
	{
        ACS_SHORT_LOG((LM_DEBUG, "flowname 1: %s", flowname_m.c_str()));
        ACS_SHORT_LOG((LM_DEBUG, "length data flowname 1: %d", frame_p->length()));
	count1_m += frame_p->length();
	}
    else if(flowNumber_m == 2)
	{
        ACS_SHORT_LOG((LM_DEBUG, "flowname 2: %s", flowname_m.c_str()));
        ACS_SHORT_LOG((LM_DEBUG, "length data flowname 2: %d", frame_p->length()));
	count2_m += frame_p->length();
	}
    else if(flowNumber_m == 3)
	{
        ACS_SHORT_LOG((LM_DEBUG, "flowname 3: %s", flowname_m.c_str()));
        ACS_SHORT_LOG((LM_DEBUG, "length data flowname 3: %d", frame_p->length()));
	count3_m += frame_p->length();
	}
    else if(flowNumber_m == 4)
	{
        ACS_SHORT_LOG((LM_DEBUG, "flowname 4: %s", flowname_m.c_str()));
        ACS_SHORT_LOG((LM_DEBUG, "length data flowname 4: %d", frame_p->length()));
	count4_m += frame_p->length();
	}

    return 0;
}


int
BulkDataReceiverCbThread::cbStop()
{
    ACS_TRACE("BulkDataReceiverCbThread::cbStop"); 

    if(flowNumber_m == 1)
	ACS_SHORT_LOG((LM_INFO, "flow 1 total length: %d", count1_m)); 
    if(flowNumber_m == 2)
	ACS_SHORT_LOG((LM_INFO, "flow 2 total length: %d", count2_m)); 
    if(flowNumber_m == 3)
	ACS_SHORT_LOG((LM_INFO, "flow 3 total length: %d", count3_m)); 
    if(flowNumber_m == 4)
	ACS_SHORT_LOG((LM_INFO, "flow 4 total length: %d", count4_m)); 

    return 0;
}


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BulkDataReceiverImpl<BulkDataReceiverCbThread>)
/* ----------------------------------------------------------------*/
