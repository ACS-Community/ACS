#include "bulkDataDistributerCb.h"

BulkDataDistributerCb::BulkDataDistributerCb()
{
    ACS_TRACE("BulkDataCallback::BulkDataCallback");

    state_m = CB_UNS; 
    substate_m = CB_SUB_UNS;

    dim_m = 0;
    count_m = 0;

    loop_m = 5;
    waitPeriod_m.set(0L, 400000L);

    frameCount_m = 0;

    timeout_m = false;

    working_m = false;
}


BulkDataDistributerCb::BulkDataDistributerCb(TAO_StreamCtrl * stream_p)
{
    ACS_TRACE("BulkDataDistributerCb::BulkDataDistributerCb"); 
}


BulkDataDistributerCb::~BulkDataDistributerCb()
{
    ACS_TRACE("BulkDataCallback::~BulkDataCallback"); 
}


int BulkDataDistributerCb::handle_start(void)
{
    ACS_TRACE("BulkDataDistributerCb::handle_start");
    
    //cout << "BulkDataDistributerCb::handle_start - state_m: " << state_m << endl;
    
    timeout_m = false;
    
    state_m = CB_UNS;
    substate_m = CB_SUB_UNS;
    
    frameCount_m = 1; // we need to wait for one frame before doing anything else  
    
    return 0;
}


int BulkDataDistributerCb::handle_stop (void)
{
    ACS_TRACE("BulkDataDistributerCb::handle_stop");

    int locLoop;

    locLoop = loop_m;
    while ( (frameCount_m != 0) && locLoop > 0)  
	{
	  ACE_OS::sleep(waitPeriod_m);
	  locLoop--; 
	} // we didn't receive the first frame yet

    //cout << "CCCCCCCCCCCCCCCCC enter stop state " << state_m << " " << substate_m << endl;

    if(state_m == CB_UNS)
	{ 
	int res = cbFwdStop();
	return res;
	}

    if((state_m == CB_SEND_PARAM) || (state_m == CB_SEND_DATA))
	{ 

	if  (substate_m == CB_SUB_INIT)
	    {
	    substate_m = CB_SUB_UNS;
	    return 0;
	    }

	locLoop = loop_m;
	while((count_m != dim_m) && locLoop > 0)
	    {
	    ACE_OS::sleep(waitPeriod_m);
	    locLoop--;
	    }


	if ( locLoop == 0 )
	    {
	    ACS_SHORT_LOG((LM_INFO,"BulkDataCallback::handle_stop timeout expired, not all data received"));

	    timeout_m = true;

	    //return -1;
	    }

	int res = cbFwdStop();

	state_m = CB_UNS;
	substate_m = CB_SUB_UNS;

	return res;
	}

    return 0;
}


int BulkDataDistributerCb::handle_destroy (void)
{
    ACS_TRACE("BulkDataDistributerCb::handle_destroy");

    //cout << "BulkDataDistributerCb::handle_destroy" << endl;

    delete this;
      
    return 0;
}



int BulkDataDistributerCb::receive_frame (ACE_Message_Block *frame, TAO_AV_frame_info *frame_info, const ACE_Addr &)
{
    working_m = true;

    //ACS_TRACE("BulkDataDistributerCb::receive_frame");
    //ACS_SHORT_LOG((LM_INFO,"BulkDataDistributerCb::receive_frame for flow %s", flowname_m.c_str()));
    
    //cout << "BulkDataDistributerCb::receive_frame - state_m: " << state_m << endl;

    CORBA::ULong val1, val2;
    int res = 0;

    if(state_m == CB_UNS)
	{ 
	char tmp[255];
	ACE_OS::strcpy(tmp, frame->rd_ptr());

	string strDataInfo(tmp);
	
	istringstream iss(strDataInfo);

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
    //ACS_SHORT_LOG((LM_INFO,"RRRRRRRRRRRRRRRRR BulkDataDistributerCb::flowname for flow %s", flow_name));
    flowname_m = flowname_p;

    string flwName(flowname_p);
    string flwNumber(flwName, 4, 1);

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
    ACS_TRACE("BulkDataDistributerCb::cbHandshake"); 


    if(distr_m == 0)
	{
	ACS_SHORT_LOG((LM_INFO, "BulkDataDistributerCb::cbReceive - distr_m == 0"));
	return -1;
	}

    distr_m->getDistributer()->distSendStart(flowname_m);

    distr_m->getDistributer()->distSendData(flowname_m,info_p);

    distr_m->getDistributer()->distSendStop(flowname_m);


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
    ACS_TRACE("BulkDataDistributerCb::cbFwdStart"); 
 
/*
    TAO_AV_Protocol_Object *dp_p = 0;
    ACE_CString flwn = "Flow1";
    distr_m->distributer.getFlowProtocol(flwn, dp_p);

    int res = dp_p->send_frame(userParam_p);
    if(res < 0) { return -1; }
*/

    distr_m->getDistributer()->distSendData(flowname_m,userParam_p);

    return 0;

}

int
BulkDataDistributerCb::cbFwdReceive(ACE_Message_Block * frame_p)
{
    ACS_TRACE("BulkDataDistributerCb::cbReceive"); 

/*
    TAO_AV_Protocol_Object *dp_p = 0;
    ACE_CString flwn = "Flow1";
    distr_m->distributer.getFlowProtocol(flwn, dp_p);

    int res = dp_p->send_frame(frame_p);
    if(res < 0) { return -1; }
*/

    distr_m->getDistributer()->distSendData(flowname_m, frame_p);

    return 0;
}

int
BulkDataDistributerCb::cbFwdStop()
{
    ACS_TRACE("BulkDataDistributerCb::cbFwdStop"); 

/*
    TAO_AV_Protocol_Object *dp_p = 0;
    ACE_CString flwn = "Flow1";
    distr_m->distributer.getFlowProtocol(flwn, dp_p);
  
    AVStreams::flowSpec locSpec(1);
    locSpec.length(1);
    locSpec[0] = distr_m->distributer.flowSpec_m[0];

    distr_m->distributer.streamctrl_p->stop(locSpec);
*/

    distr_m->getDistributer()->distSendStop(flowname_m);

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


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BulkDataDistributerImpl<BulkDataDistributerCb>)
/* ----------------------------------------------------------------*/

