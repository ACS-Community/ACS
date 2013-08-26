#include "bulkDataCallback.h"

using namespace ACSBulkDataError;
using namespace ACSBulkDataStatus;

BulkDataCallback::BulkDataCallback()
{
    ACS_TRACE("BulkDataCallback::BulkDataCallback");

    state_m = CB_UNS; 
    substate_m = CB_SUB_UNS;

    dim_m = 0;
    count_m = 0;

    //loop_m = 5;
    loop_m = 1000;
    //waitPeriod_m.set(0L, 400000L);
    waitPeriod_m.set(0L, 10L); // set to 0.01 ms to improve Receiver performance
                               // (similar to Distributor callback)

    frameCount_m = 0;

    bufParam_p = 0;

    timeout_m = false;
    working_m = false;
    error_m = false;

    errComp_p = 0;

    flowTimeout_m = 0;

    isFepAlive_m = true;

    fwdData2UserCB_m = true;
}


BulkDataCallback::~BulkDataCallback()
{
    ACS_TRACE("BulkDataCallback::~BulkDataCallback"); 

    if (error_m == true)
	{
	if (errComp_p != 0)
	    {
	    delete errComp_p;
	    errComp_p = 0;
	    }
	}
    error_m = false;
    
    if(bufParam_p)
	{
	bufParam_p->release();
	bufParam_p = 0;
	}
}


int BulkDataCallback::handle_start(void)
{
    
    //cout << "BulkDataCallback::handle_start " << flowname_m << "- state_m: " << state_m << endl;
    //if(timeout_m == true)
    //ACS_SHORT_LOG((LM_INFO,"BulkDataCallback::handle_start - timeout_m == true !!!"));

    timeout_m = false;

    if (error_m == true)
	{
	if (errComp_p != 0)
	    {
	    delete errComp_p;
	    errComp_p = 0;
	    }
	}
    // error is cleared
    error_m = false;    

    // parameter buffer is cleared (just in case) 
    if(bufParam_p)
	{
	bufParam_p->release();
	bufParam_p = 0;
	}

    state_m = CB_UNS;
    substate_m = CB_SUB_UNS;
    
    frameCount_m = 1; // we need to wait for 1 following frame before doing anything else  

    if (flowTimeout_m != 0)
	startTime_m = ACE_OS::gettimeofday();

    return 0;
}


int BulkDataCallback::handle_stop (void)
{
    //ACS_TRACE("BulkDataCallback::handle_stop");
    // cout << "CCCCCCCCCCCCCCCCC " << flowname_m << "enter stop state " << state_m << " " << substate_m << endl;

    try
	{
	int locLoop;
	int res;

	locLoop = loop_m;
	while ( (frameCount_m != 0) && locLoop > 0)  
	    {
	    ACE_OS::sleep(waitPeriod_m);
	    locLoop--; 
	    } // we didn't receive the first frame yet
	if ( locLoop == 0 )
			{
			ACS_SHORT_LOG((LM_ERROR,"BulkDataCallback::handle_stop timeout ( (%ld sec + %ld usec) * %ld ) expired for flow: %s of receiver: %s (state: %d) - not received first frame",
					waitPeriod_m.sec(), waitPeriod_m.usec(), loop_m, flowname_m.c_str(), recvName_m.c_str(), state_m));
			}


	if(state_m == CB_UNS)
	    {
		if (fwdData2UserCB_m)
			res = cbStop();
	    return 0;
	    }

	if((state_m == CB_SEND_PARAM) || (state_m == CB_SEND_DATA))
	    { 

	    if  (substate_m == CB_SUB_INIT)
		{
		substate_m = CB_SUB_UNS;
		return 0;
		}


	    if (error_m == true)
		{
		timeout_m = true;
		throw CORBA::BAD_OPERATION();
		}

	    locLoop = loop_m;
	    while((count_m != dim_m) && locLoop > 0)
		{
		ACE_OS::sleep(waitPeriod_m);
		locLoop--;
		checkFlowTimeout();

		// re-check error_m in case that is not set the first time
		if (error_m == true)
		    {
		    timeout_m = true;
		    throw CORBA::BAD_OPERATION();
		    }
		}

	    if ( locLoop == 0 )
		{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataCallback::handle_stop timeout ( (%ld sec + %ld usec) * %ld ) expired for flow: %s of receiver: %s (state: %d) - not all data received",
				waitPeriod_m.sec(), waitPeriod_m.usec(), loop_m, flowname_m.c_str(), recvName_m.c_str(), state_m));

		timeout_m = true;
		//cleaning the recv buffer
		///cleanRecvBuffer();
		//cout << "BulkDataCallback::handle_stop - handle removed: " << getHandle() << endl;
		TAO_AV_CORE::instance()->reactor()->remove_handler(getHandle(),ACE_Event_Handler::READ_MASK);
		ACS_SHORT_LOG((LM_ERROR,"BulkDataCallback::handle_stop - due to TIMEOUT receiver handler removed from the ACE reactor"));
		//ACE_OS::sleep(1);  // seems necessary to give time to remove
		                   // the handler from the reactor
		throw CORBA::TIMEOUT(9, CORBA::COMPLETED_MAYBE);
		}

	    if(state_m == CB_SEND_PARAM)
		{		
		//res ignored by reactor
		if (dim_m != 0)
			if (fwdData2UserCB_m)
				res = cbStart(bufParam_p);

		if(bufParam_p)
		    {
		    bufParam_p->release();
		    bufParam_p = 0;
		    }
		checkFlowTimeout();
		}

	    state_m = CB_UNS;
	    substate_m = CB_SUB_UNS;

	    return 0;
	    }

	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVCallbackErrorExImpl err = AVCallbackErrorExImpl(ex,__FILE__,__LINE__,"BulkDataCallback::handle_stop");
	err.log();
	// add to the completion
	errComp_p = new AVCbErrorCompletion(err, __FILE__, __LINE__, "BulkDataCallback::handle_stop"); 
	error_m = true;

	if(bufParam_p)
	    {
	    bufParam_p->release();
	    bufParam_p = 0;
	    }

	state_m = CB_UNS;
	substate_m = CB_SUB_UNS;

	throw CORBA::BAD_OPERATION();
	//throw CORBA::TIMEOUT();
	}
    catch(CORBA::Exception &ex)
	{
	AVCallbackErrorExImpl err = AVCallbackErrorExImpl(__FILE__,__LINE__,"BulkDataCallback::handle_stop");
	err.addData("CORBA::Exception", ex._info());
	err.log();

	if (errComp_p == 0)
	    {
	    errComp_p = new AVCbErrorCompletion(err, __FILE__, __LINE__, "BulkDataCallback::handle_stop"); 
	    error_m = true;
	    }
	

	if(bufParam_p)
	    {
	    bufParam_p->release();
	    bufParam_p = 0;
	    }

	state_m = CB_UNS;
	substate_m = CB_SUB_UNS;

	throw;
	//throw CORBA::TIMEOUT();
	}
    catch(std::exception &stdex)
    {
    	AVCallbackErrorExImpl err = AVCallbackErrorExImpl(__FILE__,__LINE__,"BulkDataCallback::handle_stop");
    	err.addData("std::exception", stdex.what());
    	err.log();

    	if (errComp_p == 0)
    	{
    		errComp_p = new AVCbErrorCompletion(err, __FILE__, __LINE__, "BulkDataCallback::handle_stop");
    		error_m = true;
    	}

    	if(bufParam_p)
    	{
    		bufParam_p->release();
    		bufParam_p = 0;
    	}

    	state_m = CB_UNS;
    	substate_m = CB_SUB_UNS;
    	throw;
    }
    catch(...)
    {
    	AVCallbackErrorExImpl err = AVCallbackErrorExImpl(__FILE__,__LINE__,"BulkDataCallback::handle_stop");
    	err.addData("UNKNOWN Exception", "Unknown ex in BulkDataCallback::handle_stop");
    	err.log();

    	if (errComp_p == 0)
    	{
    		errComp_p = new AVCbErrorCompletion(err, __FILE__, __LINE__, "BulkDataCallback::handle_stop");
    		error_m = true;
    	}

    	if(bufParam_p)
    	{
    		bufParam_p->release();
    		bufParam_p = 0;
    	}

	state_m = CB_UNS;
	substate_m = CB_SUB_UNS;

	throw;
	//throw CORBA::TIMEOUT();
	}

    state_m = CB_UNS;
    substate_m = CB_SUB_UNS;

    return 0;
}


int BulkDataCallback::handle_destroy (void)
{
    //ACS_TRACE("BulkDataCallback::handle_destroy");

    //cout << "BulkDataCallback::handle_destroy" << endl;

    isFepAlive_m = false;

    return 0;
}



int BulkDataCallback::receive_frame (ACE_Message_Block *frame, TAO_AV_frame_info *frame_info, const ACE_Addr &)
{
    //cout << "BulkDataCallback::receive_frame - state_m: " << state_m << endl;
    working_m = true;

    if(error_m == true)
	{
	///cleanRecvBuffer();
	working_m = false;
	return 0;
	}

    //ACS_SHORT_LOG((LM_INFO,"BulkDataCallback::receive_frame for flow %s", flowname_m.c_str()));  
    // cout << "BulkDataCallback::receive_frame - state_m: " << state_m << endl;

    try
	{
	CORBA::ULong val1, val2;
	int res;

	if(state_m == CB_UNS)
	    { 
	    char tmp[255];
	    ACE_OS::strcpy(tmp, frame->rd_ptr());

	    std::string strDataInfo(tmp);
	
	    std::istringstream iss(strDataInfo);

	    iss >> val1 >> val2;

	    // cout << "CCCCCCCCCCCCCCC " << val1 << " " << val2 << endl;

	    if(val1 == 1)
		{
		state_m = CB_SEND_PARAM;
		substate_m = CB_SUB_INIT;

		if(val2 != 0)
		    bufParam_p = new ACE_Message_Block(val2);
		else
		    bufParam_p = 0;
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

	    frameCount_m = 0;
	    working_m = false;

	    return 0;
	    }

	if(state_m == CB_SEND_PARAM)
	    {

	    if ( dim_m == 0 )
		{
	    	if (fwdData2UserCB_m)
	    		res = cbStart();

		checkFlowTimeout();
		working_m = false;
		return 0;
		}

	    bufParam_p->copy(frame->rd_ptr(),frame->length());
	    count_m += frame->length();

	    working_m = false;
	    return 0;
	    }
	
	if (state_m == CB_SEND_DATA)
	    {
		if (fwdData2UserCB_m)
			res = cbReceive(frame);
	    count_m += frame->length();

	    checkFlowTimeout();
	    working_m = false;
	    return 0;
	    }

	}
    catch(ACSErr::ACSbaseExImpl &ex)
    {
    	AVCallbackErrorExImpl err = AVCallbackErrorExImpl(ex,__FILE__,__LINE__,"BulkDataCallback::receive_frame");
    	err.log();
    	// add to the completion
    	errComp_p = new AVCbErrorCompletion(err, __FILE__, __LINE__, "BulkDataCallback::receive_frame");
    	error_m = true;
    }
    catch(CORBA::Exception &ex)
	{
	AVCallbackErrorExImpl err = AVCallbackErrorExImpl(__FILE__,__LINE__,"BulkDataCallback::receive_frame");
	err.addData("CORBA::Exception", ex._info());
	err.log();
	error_m = true;
	}
    catch(std::exception &stdex)
        {
    	AVCallbackErrorExImpl err = AVCallbackErrorExImpl(__FILE__,__LINE__,"BulkDataCallback::receive_frame");
    	err.addData("std::exception", stdex.what());
    	err.log();
    	error_m = true;
        }
    catch(...)
	{
	AVCallbackErrorExImpl err = AVCallbackErrorExImpl(__FILE__,__LINE__,"BulkDataCallback::receive_frame");
	err.addData("UNKNOWN Exception", "Unknown ex in BulkDataCallback::receive_frame");
	err.log();
	error_m = true;
	}

    working_m = false;
    return 0;
}


void BulkDataCallback::setFlowname (const char * flowname_p)
{
    ACS_TRACE("BulkDataCallback::setFlowname");
    //ACS_SHORT_LOG((LM_INFO,"RRRRRRRRRRRRRRRRR BulkDataCallback::flowname for flow %s", flow_name));
    flowname_m = flowname_p;

    std::string flwName(flowname_p);
    std::string flwNumber(flwName, 4, 1);

    flowNumber_m = atol(flwNumber.c_str());
}


void BulkDataCallback::setReceiverName(ACE_CString recvName)
{
    recvName_m = recvName;
}


void BulkDataCallback::setSleepTime(ACE_Time_Value locWaitPeriod)
{
    ACS_TRACE("BulkDataCallback::setSleepTime");

    waitPeriod_m = locWaitPeriod;
}


void BulkDataCallback::setSafeTimeout(CORBA::ULong locLoop)
{
    ACS_TRACE("BulkDataCallback::setSafeTimeout");

    loop_m = locLoop;
}


CORBA::Boolean BulkDataCallback::isTimeout()
{
    ACS_TRACE("BulkDataCallback::isTimeout");

    return timeout_m;
}


CORBA::Boolean BulkDataCallback::isWorking()
{
    ACS_TRACE("BulkDataCallback::isWorking");

    return working_m;
}


CORBA::Boolean BulkDataCallback::isError()
{
//    ACS_TRACE("BulkDataCallback::isError");

    return error_m;
}


CompletionImpl *BulkDataCallback::getErrorCompletion()
{
    ACS_TRACE("BulkDataCallback::getErrorCompletion");

    // error is cleared; completion cannot be retrieved two times
    error_m = false;

    return errComp_p;
}


void BulkDataCallback::cleanRecvBuffer()
{
    ACE_Svc_Handler<ACE_SOCK_STREAM,ACE_NULL_SYNCH> *svch = dynamic_cast<ACE_Svc_Handler<ACE_SOCK_STREAM,ACE_NULL_SYNCH> *>(handler_);   
	
    char buf[BUFSIZ];
    int bufSize = sizeof(buf);
    int nn = 1;
    while(nn > 0)
	{
	nn = svch->peer().recv(buf,bufSize);
	}

//    svch->peer().close();    
}


void BulkDataCallback::setFlowTimeout(CORBA::ULong timeout)
{
    flowTimeout_m = timeout;
}


void BulkDataCallback::fwdData2UserCB(CORBA::Boolean enable)
{
	fwdData2UserCB_m = enable;
	if (fwdData2UserCB_m)
	{
	    ACS_SHORT_LOG((LM_WARNING,"BulkDataCallback::fwdData2UserCB enabled data forwarding to the user's CB for flow %s", flowname_m.c_str()));
	}
	else
	{
		ACS_SHORT_LOG((LM_WARNING,"BulkDataCallback::fwdData2UserCB disabled data forwarding to the user's CB for flow %s", flowname_m.c_str()));
	}
}//fwdData2UserCB

void BulkDataCallback::checkFlowTimeout()
{
    if(flowTimeout_m == 0)
	return;

    ACE_Time_Value elapsedTime = ACE_OS::gettimeofday() - startTime_m;
    double dtime = (elapsedTime.sec() * 1000.) + ( elapsedTime.usec() / 1000. );
    if(dtime > flowTimeout_m)
	{
	///cleanRecvBuffer();
	timeout_m = true;
	AVCbFlowTimeoutExImpl err = AVCbFlowTimeoutExImpl(__FILE__,__LINE__,"BulkDataCallback::checkFlowTimeout");
	throw err;
	}
}


void BulkDataCallback::closePeer()
{
    ACE_Svc_Handler<ACE_SOCK_STREAM,ACE_NULL_SYNCH> *svch = 
	dynamic_cast<ACE_Svc_Handler<ACE_SOCK_STREAM,ACE_NULL_SYNCH> *>(handler_);   

    if (svch != 0)
	{
	svch->peer().close();
	}
}


ACE_HANDLE BulkDataCallback::getHandle()
{
    ACS_TRACE("BulkDataCallback::getHandle");

    ACE_Event_Handler *event_handler = handler_->event_handler();

    ACE_HANDLE handle = event_handler->get_handle();

    return handle;
}
