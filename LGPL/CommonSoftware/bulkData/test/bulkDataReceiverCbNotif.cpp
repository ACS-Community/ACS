#include "bulkDataReceiverCbNotif.h"

using namespace ACSBulkDataError;

BulkDataReceiverCbNotif::BulkDataReceiverCbNotif() : count1_m(0),count2_m(0)
{
    ACS_TRACE("BulkDataReceiverCbNotif::BulkDataReceiverCbNotif");
}


BulkDataReceiverCbNotif::~BulkDataReceiverCbNotif()
{
    ACS_TRACE("BulkDataReceiverCbNotif::~BulkDataReceiverCbNotif"); 
}


int
BulkDataReceiverCbNotif::cbStart(ACE_Message_Block *userParam_p)
{
    if(flowNumber_m == 1)
	{
	ACS_SHORT_LOG((LM_INFO, "BulkDataReceiverCbNotif - received parameter on flow 1: %s", userParam_p->rd_ptr()));

	count1_m = 0;
	}
    else if(flowNumber_m == 2)
	{
	ACS_SHORT_LOG((LM_INFO, "BulkDataReceiverCbNotif - received parameter on flow 2: %s", userParam_p->rd_ptr()));

	count2_m = 0;
	}

    return 0;
}

int
BulkDataReceiverCbNotif::cbReceive(ACE_Message_Block *frame_p)
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
BulkDataReceiverCbNotif::cbStop()
{
    try
	{
	if(flowNumber_m == 1)
	    {
	    ACS_SHORT_LOG((LM_INFO, "BulkDataReceiverCbNotif - data on flow 1 total length: %d", count1_m));
	    // An error is generated to test the notification mechanism
	    AVFlowEndpointErrorExImpl err = AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiverCbNotif::cbStop");
	    throw err;
	    }
	else if(flowNumber_m == 2)
	    {
	    ACS_SHORT_LOG((LM_INFO, "BulkDataReceiverCbNotif - data on flow 2 total length: %d", count2_m));
	    }
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVFlowEndpointErrorCompletion comp(ex,__FILE__, __LINE__, "BulkDataReceiverCbNotif::cbStop");
	comp.log();
	if(recv_p)
	    {
	    // The notifySender method of the receiver is called in order to notify
	    // the sender (distributor) of the error. The completion is passed back as parameter.
	    recv_p->notifySender(comp);
	    }
	else
	    {
	    ACS_SHORT_LOG((LM_ERROR, "BulkDataReceiverCbNotif - Receiver reference NULL"));
	    }
	}
    
    return 0;
}


void
BulkDataReceiverCbNotif::setReceiver(AcsBulkdata::BulkDataReceiver<BulkDataReceiverCbNotif> *recv)
{
    ACE_TRACE("BulkDataReceiverCbNotif::setReceiver");

    if (recv == NULL)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiverCbNotif::setReceiver recv = 0"));
	}
    else
	{
	recv_p = recv;
	}
}


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BulkDataReceiverImpl<BulkDataReceiverCbNotif>)
/* ----------------------------------------------------------------*/
