#include "bulkDataSendRecvCb.h"

BulkDataSendRecvCb::BulkDataSendRecvCb() : count1_m(0), count2_m(0)
{
    ACS_TRACE("BulkDataSendRecvCb::BulkDataSendRecvCb"); 
}


BulkDataSendRecvCb::~BulkDataSendRecvCb()
{
    ACS_TRACE("BulkDataSendRecvCb::~BulkDataSendRecvCb"); 
}


int
BulkDataSendRecvCb::cbStart(ACE_Message_Block * userParam_p)
{
    if(flowNumber_m == 1)
      {
      ACS_SHORT_LOG((LM_INFO, "flowname 1: %s", flowname_m.c_str()));
      ACS_SHORT_LOG((LM_INFO, "length param flowname 1: %d", userParam_p->length()));

      count1_m = 0;
      }
    else if(flowNumber_m == 2)
      {
      ACS_SHORT_LOG((LM_INFO, "flowname 2: %s", flowname_m.c_str()));
      ACS_SHORT_LOG((LM_INFO, "length param flowname 2: %d", userParam_p->length()));

      count2_m = 0;
      }

    // flows 3 and 4 not handled (see bulkDataReceiverCbEx1.cpp)

    return 0;
}

int
BulkDataSendRecvCb::cbReceive(ACE_Message_Block * frame_p)
{
    if(flowNumber_m == 1)
      {
      //ACS_SHORT_LOG((LM_INFO, "flowname 1: %s", flowname_m.c_str()));
      //ACS_SHORT_LOG((LM_INFO, "length param flowname 1: %d", frame_p->length()));
	count1_m += frame_p->length();
      }
    else if(flowNumber_m == 2)
      {
      //ACS_SHORT_LOG((LM_INFO, "flowname 2: %s", flowname_m.c_str()));
      //ACS_SHORT_LOG((LM_INFO, "length param flowname 2: %d", frame_p->length()));
	count2_m += frame_p->length();
      }

    // flows 3 and 4 not handled (see bulkDataReceiverCbEx1.cpp)

    return 0;
}

int
BulkDataSendRecvCb::cbStop()
{
    if(flowNumber_m == 1)
	ACS_SHORT_LOG((LM_INFO, "flow 1 total length: %d", count1_m)); 
    if(flowNumber_m == 2)
	ACS_SHORT_LOG((LM_INFO, "flow 2 total length: %d", count2_m)); 

    // flows 3 and 4 not handled (see bulkDataReceiverCbEx1.cpp)

    return 0;
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BulkDataSendRecvImpl<BulkDataSendRecvCb>)
/* ----------------------------------------------------------------*/

    
