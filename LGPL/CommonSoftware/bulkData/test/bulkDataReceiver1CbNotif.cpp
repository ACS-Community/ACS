#include "bulkDataReceiver1CbNotif.h"

using namespace ACSBulkDataError;
BulkDataReceiver1CbNotif::BulkDataReceiver1CbNotif() : count1_m(0),count2_m(0)
{
    ACS_TRACE("BulkDataReceiver1CbNotif::BulkDataReceiver1CbNotif");
}


BulkDataReceiver1CbNotif::~BulkDataReceiver1CbNotif()
{
    ACS_TRACE("BulkDataReceiver1CbNotif::~BulkDataReceiver1CbNotif"); 
}


int
BulkDataReceiver1CbNotif::cbStart(ACE_Message_Block *userParam_p)
{
    if(flowNumber_m == 1)
	{
	ACS_SHORT_LOG((LM_INFO, "BulkDataReceiver1CbNotif - received parameter on flow 1: %s", userParam_p->rd_ptr()));

	count1_m = 0;
	}
    else if(flowNumber_m == 2)
	{
	ACS_SHORT_LOG((LM_INFO, "BulkDataReceiver1CbNotif - received parameter on flow 2: %s", userParam_p->rd_ptr()));

	count2_m = 0;
	}

    return 0;
}

int
BulkDataReceiver1CbNotif::cbReceive(ACE_Message_Block *frame_p)
{
    if(flowNumber_m == 1)
	{
	count1_m += frame_p->length();
	}
    else if(flowNumber_m == 2)
	{
	count2_m += frame_p->length();
	}

    return 0;
}

int
BulkDataReceiver1CbNotif::cbStop()
{
    try
	{
	if(flowNumber_m == 1)
	    {
	    ACS_SHORT_LOG((LM_INFO, "BulkDataReceiver1CbNotif - data on flow 1 total length: %d", count1_m));
	    }
	else if(flowNumber_m == 2)
	    {
	    ACS_SHORT_LOG((LM_INFO, "BulkDataReceiver1CbNotif - data on flow 2 total length: %d", count2_m));
	    // An error is generated to test the notification mechanism
	    AVInvalidFlowNumberExImpl err = AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataReceiver1CbNotif::cbStop");
	    throw err;
	    }
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVInvalidFlowNumberCompletion comp(ex,__FILE__, __LINE__, "BulkDataReceiver1CbNotif::cbStop");
	comp.log();
	if(recv_p)
	    {
	    // The notifySender method of the receiver is called in order to notify
	    // the sender (distributor) of the error. The completion is passed back as parameter.
	    recv_p->notifySender(comp);
	    }
	else
	    {
	    ACS_SHORT_LOG((LM_ERROR, "BulkDataReceiver1CbNotif - Receiver reference NULL"));
	    }
	}
    
    return 0;
}


void
BulkDataReceiver1CbNotif::setReceiver(AcsBulkdata::BulkDataReceiver<BulkDataReceiver1CbNotif> *recv)
{
    ACE_TRACE("BulkDataReceiver1CbNotif::setReceiver");

    if (recv == NULL)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver1CbNotif::setReceiver recv = 0"));
	}
    else
	{
	recv_p = recv;
	}
}


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BulkDataReceiverImpl<BulkDataReceiver1CbNotif>)
/* ----------------------------------------------------------------*/
