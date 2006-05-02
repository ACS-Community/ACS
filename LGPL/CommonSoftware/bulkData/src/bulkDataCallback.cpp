#include "bulkDataCallback.h"

BulkDataCallback::BulkDataCallback()
{
    ACS_TRACE("BulkDataCallback::BulkDataCallback");

    state_m = CB_UNS; 
    substate_m = CB_SUB_UNS;

    dim_m = 0;
    count_m = 0;

    loop_m = 5;
    waitPeriod_m.set(0L, 400000L);

    frameCount_m = 0;

    bufParam_p = 0;

    timeout_m = false;
    working_m = false;
    error_m = false;

    errorCounter_m = 0;
    errComp_p = 0;
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

}


int BulkDataCallback::handle_start(void)
{
    //ACS_TRACE("BulkDataCallback::handle_start");
    
    // cout << "BulkDataCallback::handle_start - state_m: " << state_m << endl;

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

    state_m = CB_UNS;
    substate_m = CB_SUB_UNS;
    
    frameCount_m = 1; // we need to wait for 1 following frame before doing anything else  
 
    return 0;
}


int BulkDataCallback::handle_stop (void)
{
    //ACS_TRACE("BulkDataCallback::handle_stop");

    // cout << "CCCCCCCCCCCCCCCCC enter stop state " << state_m << " " << substate_m << endl;

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


	if(state_m == CB_UNS)
	    { 

	    int res = cbStop();
	    errorCounter_m = 0;
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

		//cleaning the recv buffer

		ACE_Svc_Handler<ACE_SOCK_STREAM,ACE_NULL_SYNCH> *svch = dynamic_cast<ACE_Svc_Handler<ACE_SOCK_STREAM,ACE_NULL_SYNCH> *>(handler_);   

		char buf[BUFSIZ];
		int bufSize = sizeof(buf);
		//cout << "sssssss: " << bufSize << endl;
		int nn = 1;
		while(nn > 0)
		    {
		    nn = svch->peer().recv(buf,bufSize);
		    //cout << "nnnnnnn: " << nn << endl;
		    }
		//return -1;

		if (error_m == true)
		    throw CORBA::TIMEOUT();

		}

	    if(state_m == CB_SEND_PARAM)
		{		
		//res ignored by reactor
		if (dim_m != 0)
		    res = cbStart(bufParam_p);

		if(bufParam_p)
		    bufParam_p->release();
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

	/* TBD what do to after several attempts ? */
	errorCounter_m++;
	if(errorCounter_m == maxErrorRepetition)
	    {
	    errorCounter_m = 0;
	    //return 0;
	    }

	// add to the completion
	errComp_p = new AVCbErrorCompletion(err, __FILE__, __LINE__, "BulkDataCallback::handle_stop"); 

	error_m = true;

	throw CORBA::TIMEOUT();
	}
    catch(CORBA::Exception &ex)
	{
	AVCallbackErrorExImpl err = AVCallbackErrorExImpl(__FILE__,__LINE__,"BulkDataCallback::handle_stop");
	err.addData("CORBA::Exception", ex._info());
	err.log();

	errorCounter_m++;
	if(errorCounter_m == maxErrorRepetition)
	    {
	    errorCounter_m = 0;
	    //return 0;
	    }
	error_m = true;
	throw CORBA::TIMEOUT();
	}
    catch(...)
	{
	AVCallbackErrorExImpl err = AVCallbackErrorExImpl(__FILE__,__LINE__,"BulkDataCallback::handle_stop");
	err.addData("UNKNOWN Exception", "Unknown ex in BulkDataCallback::handle_stop");
	err.log();

	errorCounter_m++;
	if(errorCounter_m == maxErrorRepetition)
	    {
	    errorCounter_m = 0;
	    //return 0;
	    }
	error_m = true;
	throw CORBA::TIMEOUT();
	}

    return 0;
}


int BulkDataCallback::handle_destroy (void)
{
    //ACS_TRACE("BulkDataCallback::handle_destroy");

    //cout << "BulkDataCallback::handle_destroy" << endl;

    if (error_m == true)
	{
	if (errComp_p != 0)
	    {
	    delete errComp_p;
	    errComp_p = 0;
	    }
	}
    error_m = false;

    delete this;
      
    return 0;
}



int BulkDataCallback::receive_frame (ACE_Message_Block *frame, TAO_AV_frame_info *frame_info, const ACE_Addr &)
{
    working_m = true;

    if(error_m == true)
	{
	cleanRecvBuffer();
	working_m = false;
	return 0;
	}


    //ACS_TRACE("BulkDataCallback::receive_frame");
    //ACS_SHORT_LOG((LM_INFO,"BulkDataCallback::receive_frame for flow %s", flowname_m.c_str()));
    
    // cout << "BulkDataCallback::receive_frame - state_m: " << state_m << endl;

    if(timeout_m == true)
	ACS_SHORT_LOG((LM_INFO, "BulkDataCallback::receive_frame - timeout_m true !!!"));

    try
	{
	CORBA::ULong val1, val2;
	int res;

	if(state_m == CB_UNS)
	    { 
	    char tmp[255];
	    ACE_OS::strcpy(tmp, frame->rd_ptr());

	    string strDataInfo(tmp);
	
	    istringstream iss(strDataInfo);

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
		res = cbStart();
		errorCounter_m = 0;
		working_m = false;
		return 0;
		}

	    bufParam_p->copy(frame->rd_ptr(),frame->length());

	    count_m += frame->length();
	    errorCounter_m = 0;
	    working_m = false;
	    return 0;
	    }
	
	if (state_m == CB_SEND_DATA)
	    {
	    res = cbReceive(frame);
//	    if(timeout_m == true)
//		{
//		//cout << "!!!!!!! in receive_frame timeout_m == true after timeout - set it to false" << endl;
//		timeout_m = false;
//		}
	    count_m += frame->length();
	    errorCounter_m = 0;
	    working_m = false;
	    return 0;
	    }

	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVCallbackErrorExImpl err = AVCallbackErrorExImpl(ex,__FILE__,__LINE__,"BulkDataCallback::receive_frame");
	err.log();

	/* TBD what do to after several attempts ? */
	errorCounter_m++;
	if(errorCounter_m == maxErrorRepetition)
	    {
	    errorCounter_m = 0;
	    //return 0;
	    }

	// add to the completion
	errComp_p = new AVCbErrorCompletion(err, __FILE__, __LINE__, "BulkDataCallback::handle_stop"); 

	error_m = true;
	}
    catch(CORBA::Exception &ex)
	{
	AVCallbackErrorExImpl err = AVCallbackErrorExImpl(__FILE__,__LINE__,"BulkDataCallback::receive_frame");
	err.addData("CORBA::Exception", ex._info());
	err.log();

	errorCounter_m++;
	if(errorCounter_m == maxErrorRepetition)
	    {
	    errorCounter_m = 0;
	    //return 0;
	    }

	error_m = true;
	}
    catch(...)
	{
	AVCallbackErrorExImpl err = AVCallbackErrorExImpl(__FILE__,__LINE__,"BulkDataCallback::receive_frame");
	err.addData("UNKNOWN Exception", "Unknown ex in BulkDataCallback::receive_frame");
	err.log();

	errorCounter_m++;
	if(errorCounter_m == maxErrorRepetition)
	    {
	    errorCounter_m = 0;
	    //return 0;
	    }
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

    string flwName(flowname_p);
    string flwNumber(flwName, 4, 1);

    flowNumber_m = atol(flwNumber.c_str());
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
