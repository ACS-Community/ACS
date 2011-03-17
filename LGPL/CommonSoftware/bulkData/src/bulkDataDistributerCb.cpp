#include "bulkDataDistributerCb.h"

BulkDataDistributerCb::BulkDataDistributerCb()
{
    ACS_TRACE("BulkDataDistributerCb::BulkDataDistributerCb");

    state_m = CB_UNS; 
    substate_m = CB_SUB_UNS;

    dim_m = 0;
    count_m = 0;

    //loop_m = 5;
    loop_m = 1000;
    //waitPeriod_m.set(0L, 400000L);
    waitPeriod_m.set(0L, 10L); // set to 0.01 ms to improve Distributor performance

    frameCount_m = 0;

    timeout_m = false;

    working_m = false;

    isFepAlive_m = true;
}


BulkDataDistributerCb::BulkDataDistributerCb(TAO_StreamCtrl * stream_p)
{
    ACS_TRACE("BulkDataDistributerCb::BulkDataDistributerCb"); 
}


BulkDataDistributerCb::~BulkDataDistributerCb()
{
    ACS_TRACE("BulkDataDistributerCb::~BulkDataDistributerCb"); 

    timeout_m = false;
}


int BulkDataDistributerCb::handle_start(void)
{
    //ACS_TRACE("BulkDataDistributerCb::handle_start");
    
    //cout << "BulkDataDistributerCb::handle_start - state_m: " << state_m << endl;
    
    timeout_m = false;
    
    state_m = CB_UNS;
    substate_m = CB_SUB_UNS;
    
    frameCount_m = 1; // we need to wait for one frame before doing anything else  
    
    return 0;
}


int BulkDataDistributerCb::handle_stop (void)
{
    //ACS_TRACE("BulkDataDistributerCb::handle_stop");

    //cout << flowname_m.c_str() << " - wait_period  sec: " << waitPeriod_m.sec() << endl;
    //cout << flowname_m.c_str() << " - wait_period usec: " << waitPeriod_m.usec() << endl;

    int locLoop;

    locLoop = loop_m;
    while ( (frameCount_m != 0) && locLoop > 0)  
	{
	ACE_OS::sleep(waitPeriod_m);
	locLoop--; 
	} // we didn't receive the first frame yet
    if ( locLoop == 0 )
	{
	ACS_SHORT_LOG((LM_WARNING,"BulkDataDistributerCb::handle_stop timeout %f sec expired for flow: %s", loop_m * (waitPeriod_m.sec()+waitPeriod_m.usec()/1000000.0), flowname_m.c_str()));
	throw CORBA::TIMEOUT();
	}

    //cout << "CCCCCCCCCCCCCCCCC enter stop state " << state_m << " " << substate_m << endl;

    if(state_m == CB_UNS)
	{ 
	cbFwdUserStop();
	return 0;
	}

    if((state_m == CB_SEND_PARAM) || (state_m == CB_SEND_DATA))
	{ 
	if  (substate_m == CB_SUB_INIT)
	    {
	    substate_m = CB_SUB_UNS;
	    return 0;
	    }

	//TO BE REMOVED (?)
	/*locLoop = loop_m;
	while((count_m != dim_m) && locLoop > 0)
	    {
	    ACE_OS::sleep(waitPeriod_m);
	    locLoop--;
	    }

	
	if ( locLoop == 0 )
	    {
	    ACS_SHORT_LOG((LM_INFO,"BulkDataDistributerCb::handle_stop timeout expired, not all data received"));

	    timeout_m = true;

	    //return -1;
	    }*/

	int res = cbFwdStop();

	state_m = CB_UNS;
	substate_m = CB_SUB_UNS;

	return res;
	}

    return 0;
}


int BulkDataDistributerCb::handle_destroy (void)
{
    //ACS_TRACE("BulkDataDistributerCb::handle_destroy");

    //cout << "BulkDataDistributerCb::handle_destroy" << endl;

    isFepAlive_m = false;

    return 0;
}



int BulkDataDistributerCb::receive_frame (ACE_Message_Block *frame, TAO_AV_frame_info *frame_info, const ACE_Addr &)
{
    //ACS_TRACE("BulkDataDistributerCb::receive_frame");
    //cout << "BulkDataDistributerCb::receive_frame - state_m: " << state_m << endl;

    working_m = true;

    if(timeout_m == true)
	{
	state_m = CB_SEND_DATA;
	substate_m = CB_SUB_UNS;
	}
	
    //ACS_SHORT_LOG((LM_INFO,"BulkDataDistributerCb::receive_frame for flow %s", flowname_m.c_str()));
    
    //cout << "BulkDataDistributerCb::receive_frame - state_m: " << state_m << endl;

    CORBA::ULong val1, val2;
    int res = 0;

    if(state_m == CB_UNS)
	{ 
	char tmp[255];
	ACE_OS::strcpy(tmp, frame->rd_ptr());

	std::string strDataInfo(tmp);
	
	std::istringstream iss(strDataInfo);

	iss >> val1 >> val2;

	//cout << "CCCCCCCCCCCCCCC " << val1 << " " << val2 << endl;

	if(val1 == 1)
	    {
	    state_m = CB_SEND_PARAM;
	    substate_m = CB_SUB_INIT;
	    }
	else if(val1 == 2)
	    {
	    state_m = CB_SEND_DATA;
	    substate_m = CB_SUB_INIT;
	    }
	else
	    {
	    state_m = CB_UNS;
	    substate_m = CB_SUB_UNS;
	    }

	dim_m = val2;
	count_m = 0;
	
// we received the first frame and we pass it to the receiver
// the variable frameCount_m is equal 1 and this help us
// to distinguish between first frame and the data

	res = cbHandshake(frame);

	frameCount_m = 0;

	working_m = true;

	return res;
	}


    if(state_m == CB_SEND_PARAM)
	{

	if ( dim_m == 0 )
	    {
	    res = cbFwdStart();
	    working_m = false;
	    return res;
	    }

	res = cbFwdStart(frame);
	count_m += frame->length();
	working_m = false;
	return res;
	}

    if (state_m == CB_SEND_DATA)
	{
	res = cbFwdReceive(frame);
	count_m += frame->length();
	working_m = false;
	return res;
	}

    working_m = false;

    return 0;
}


void BulkDataDistributerCb::setFlowname (const char * flowname_p)
{
    ACS_TRACE("BulkDataDistributerCb::setFlowname");

    flowname_m = flowname_p;

    std::string flwName(flowname_p);
    std::string flwNumber(flwName, 4, 1);

    flowNumber_m = atol(flwNumber.c_str());
}


void BulkDataDistributerCb::setSleepTime(ACE_Time_Value locWaitPeriod)
{
    ACS_TRACE("BulkDataDistributerCb::setSleepTime");

    waitPeriod_m = locWaitPeriod;
}


void BulkDataDistributerCb::setSafeTimeout(CORBA::ULong locLoop)
{
    ACS_TRACE("BulkDataDistributerCb::setSafeTimeout");

    loop_m = locLoop;
}


int
BulkDataDistributerCb::cbHandshake(ACE_Message_Block * info_p)
{
    //ACS_TRACE("BulkDataDistributerCb::cbHandshake"); 

    if(distr_m == 0)
	{
	ACS_SHORT_LOG((LM_INFO, "BulkDataDistributerCb::cbHandshake - distr_m == 0"));
	return -1;
	}

    try
	{
	distr_m->getDistributer()->distSendStart(flowname_m, flowNumber_m);
	
	distr_m->getDistributer()->distSendDataHsk(flowname_m, info_p, flowNumber_m);
	
	distr_m->getDistributer()->distSendStop(flowname_m, flowNumber_m);
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributerCb::cbHandshake exception"));
	}


// we replicate the mechanism of the sender
/*
	TAO_AV_Protocol_Object *dp_p = 0;
	ACE_CString flwn = "Flow1";
	distr_m->distributer.getFlowProtocol(flwn, dp_p);

	AVStreams::flowSpec locSpec(1);
	locSpec.length(1);
	locSpec[0] = distr_m->distributer.flowSpec_m[0];
	
// we replicate the mechanism of the sender for the start

	distr_m->distributer.streamctrl_p->start(locSpec);

	int res = dp_p->send_frame(info_p);
	if(res < 0) { return -1; }
	
	distr_m->distributer.streamctrl_p->stop(locSpec);

*/

	return 0;

}


int
BulkDataDistributerCb::cbFwdStart(ACE_Message_Block * userParam_p)
{
    //ACS_TRACE("BulkDataDistributerCb::cbFwdStart"); 
 
/*
    TAO_AV_Protocol_Object *dp_p = 0;
    ACE_CString flwn = "Flow1";
    distr_m->distributer.getFlowProtocol(flwn, dp_p);

    int res = dp_p->send_frame(userParam_p);
    if(res < 0) { return -1; }
*/

    try
	{
    	if (distr_m!=NULL)
    	{
    		distr_m->getDistributer()->distSendData(flowname_m,userParam_p, flowNumber_m);
    	}
    	else
    	{
    		ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributerCb::cbFwdStart distr_m==NULL !!!"));
    	}
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributerCb::cbFwdStart exception"));
	}
    return 0;

}

int
BulkDataDistributerCb::cbFwdReceive(ACE_Message_Block * frame_p)
{
    //ACS_TRACE("BulkDataDistributerCb::cbReceive"); 

/*
    TAO_AV_Protocol_Object *dp_p = 0;
    ACE_CString flwn = "Flow1";
    distr_m->distributer.getFlowProtocol(flwn, dp_p);

    int res = dp_p->send_frame(frame_p);
    if(res < 0) { return -1; }
*/

    try
	{
    	if (distr_m!=NULL)
    		distr_m->getDistributer()->distSendData(flowname_m, frame_p, flowNumber_m);
    	else
    		ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributerCb::cbFwdReceive distr_m==NULL !!!"));
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributerCb::cbFwdReceive exception"));
	}

    return 0;
}

int
BulkDataDistributerCb::cbFwdStop()
{
    //ACS_TRACE("BulkDataDistributerCb::cbFwdStop"); 

/*
    TAO_AV_Protocol_Object *dp_p = 0;
    ACE_CString flwn = "Flow1";
    distr_m->distributer.getFlowProtocol(flwn, dp_p);
  
    AVStreams::flowSpec locSpec(1);
    locSpec.length(1);
    locSpec[0] = distr_m->distributer.flowSpec_m[0];

    distr_m->distributer.streamctrl_p->stop(locSpec);
*/

    try
	{
    	if (distr_m!=NULL)
    		timeout_m = distr_m->getDistributer()->distSendStopTimeout(flowname_m, flowNumber_m);
    	else
    		ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributerCb::cbFwdStop distr_m==NULL !!!"));
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributerCb::cbFwdStop exception"));
	}

    return 0;
}



int
BulkDataDistributerCb::cbFwdUserStop()
{
    //ACS_TRACE("BulkDataDistributerCb::cbFwdUserStop"); 

    try
	{
    	if (distr_m!=NULL)
    		distr_m->getDistributer()->distSendStop(flowname_m, flowNumber_m);
    	else
    		ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributerCb::cbFwdUserStop distr_m==NULL !!!"));
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributerCb::cbFwdUserStop exception"));
	}

    return 0;
}




void BulkDataDistributerCb::setDistributerImpl(BulkDataDistributerImpl<BulkDataDistributerCb> *distr_p)
{
    ACS_TRACE("BulkDataDistributerCb::setDistributerImpl");

    distr_m = distr_p;
}


CORBA::Boolean BulkDataDistributerCb::isTimeout()
{
    ACS_TRACE("BulkDataDistributerCb::isTimeout");

    return timeout_m;
}


CORBA::Boolean BulkDataDistributerCb::isWorking()
{
    ACS_TRACE("BulkDataDistributerCb::isWorking");

    return working_m;
}


ACE_HANDLE BulkDataDistributerCb::getHandle()
{
    ACS_TRACE("BulkDataDistributerCb::getHandle");

    ACE_Event_Handler *event_handler = handler_->event_handler();
	
    ACE_HANDLE handle = event_handler->get_handle();
	
    return handle;
}


void
BulkDataDistributerCb::setCbTimeout(ACE_Time_Value cbTimeout)
{
    waitPeriod_m = cbTimeout;
    ACS_SHORT_LOG((LM_INFO,"BulkDataDistributerCb::setCbTimeout - distributor timeout set to ( (%ld sec - %ld usec) * %ld )",waitPeriod_m.sec(),waitPeriod_m.usec(),loop_m));
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BulkDataDistributerImpl<BulkDataDistributerCb>)
/* ----------------------------------------------------------------*/

