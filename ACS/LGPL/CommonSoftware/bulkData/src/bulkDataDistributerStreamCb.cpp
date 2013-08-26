#include "bulkDataDistributerStreamCb.h"

BulkDataDistributerStreamCb::BulkDataDistributerStreamCb()
{
    ACS_TRACE("BulkDataDistributerStreamCb::BulkDataDistributerStreamCb");

}


BulkDataDistributerStreamCb::BulkDataDistributerStreamCb(TAO_StreamCtrl * stream_p)
{
    ACS_TRACE("BulkDataDistributerStreamCb::BulkDataDistributerStreamCb"); 

    isFepAlive_m = true;
}


BulkDataDistributerStreamCb::~BulkDataDistributerStreamCb()
{
    ACS_TRACE("BulkDataDistributerStreamCb::~BulkDataDistributerStreamCb"); 
}


int BulkDataDistributerStreamCb::handle_start(void)
{
    ACS_TRACE("BulkDataDistributerStreamCb::handle_start");
    
    if(distr_m == 0)
	{
	ACS_SHORT_LOG((LM_INFO, "BulkDataDistributerStreamCb::handle_start - distr_m == 0"));
	return -1;
	}

    distr_m->getDistributer()->distSendStart(flowname_m);

    return 0;
}


int BulkDataDistributerStreamCb::handle_stop (void)
{
    ACS_TRACE("BulkDataDistributerStreamCb::handle_stop");

    if(distr_m == 0)
	{
	ACS_SHORT_LOG((LM_INFO, "BulkDataDistributerStreamCb::handle_stop - distr_m == 0"));
	return -1;
	}

    distr_m->getDistributer()->distSendStop(flowname_m);

    return 0;
}


int BulkDataDistributerStreamCb::handle_destroy (void)
{
    ACS_TRACE("BulkDataDistributerStreamCb::handle_destroy");
      
    isFepAlive_m = false;

    return 0;
}



int BulkDataDistributerStreamCb::receive_frame (ACE_Message_Block *frame_p, TAO_AV_frame_info *frame_info, const ACE_Addr &)
{

    ACS_TRACE("BulkDataDistributerStreamCb::receive_frame");
    //ACS_SHORT_LOG((LM_INFO,"BulkDataDistributerStreamCb::receive_frame for flow %s", flowname_m.c_str()));
    
    if(distr_m == 0)
	{
	ACS_SHORT_LOG((LM_INFO, "BulkDataDistributerStreamCb::receive_frame - distr_m == 0"));
	return -1;
	}

    distr_m->getDistributer()->distSendData(flowname_m, frame_p);

    return 0;
}


void BulkDataDistributerStreamCb::setFlowname (const char * flowname_p)
{
    ACS_TRACE("BulkDataDistributerStreamCb::setFlowname");
    //ACS_SHORT_LOG((LM_INFO,"RRRRRRRRRRRRRRRRR BulkDataDistributerStreamCb::flowname for flow %s", flow_name));
    flowname_m = flowname_p;

    string flwName(flowname_p);
    string flwNumber(flwName, 4, 1);

    flowNumber_m = atol(flwNumber.c_str());
}


void BulkDataDistributerStreamCb::setDistributerImpl(BulkDataDistributerImpl<BulkDataDistributerStreamCb> *distr_p)
{
    ACS_TRACE("BulkDataDistributerStreamCb::setDistributerImpl");

    distr_m = distr_p;
}


ACE_HANDLE BulkDataDistributerStreamCb::getHandle()
{
    ACS_TRACE("BulkDataDistributerStreamCb::getHandle");

    ACE_Event_Handler *event_handler = handler_->event_handler();
	
    ACE_HANDLE handle = event_handler->get_handle();

    return handle;
}


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BulkDataDistributerImpl<BulkDataDistributerStreamCb>)
/* ----------------------------------------------------------------*/

