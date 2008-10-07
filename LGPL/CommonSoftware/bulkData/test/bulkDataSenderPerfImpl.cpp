#include "bulkDataSenderPerfImpl.h"

using namespace ACSBulkDataError;

BulkDataSenderPerfImpl::BulkDataSenderPerfImpl(const ACE_CString& name,maci::ContainerServices* containerServices) :
    BulkDataSenderDefaultImpl(name,containerServices)
{
//    ACS_TRACE("BulkDataSenderPerfImpl::BulkDataSenderPerfImpl");
}


BulkDataSenderPerfImpl::~BulkDataSenderPerfImpl()
{
//    ACS_TRACE("BulkDataSenderPerfImpl::~BulkDataSenderPerfImpl");
}


void BulkDataSenderPerfImpl::startSend()
{
    //   ACS_TRACE("BulkDataSenderPerfImpl::startSend");

    int size;
    size = 10;

    try
	{
	
	ACE_Message_Block *mb;
	mb = new ACE_Message_Block(size);
	
	for (CORBA::Long j = 0; j < (size-1); j++)
	    {
	    *mb->wr_ptr()='p';
	    mb->wr_ptr(sizeof(char));
	    }
	*mb->wr_ptr()='\0';
	mb->wr_ptr(sizeof(char));
    
	CORBA::ULong flowNumber = 1;
	getSender()->startSend(flowNumber, mb);
    
	//ACS_SHORT_LOG ((LM_INFO,"flow 1 length start parameter sent = %d", mb->length()));
    
	mb->release();

	}

    catch (AVInvalidFlowNumberExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderPerfImpl::startSend AVInvalidFlowNumberExImpl exception catched !"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderPerfImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (AVSendFrameErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderPerfImpl::startSend AVSendFrameErrorExImpl exception catched !"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderPerfImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (AVFlowEndpointErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderPerfImpl::startSend  AVFlowEndpointErrorExImplexception catched !"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderPerfImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (AVProtocolErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderPerfImpl::startSend  AVProtocolErrorExImplexception catched !"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderPerfImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderPerfImpl::startSend UNKNOWN exception"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(__FILE__,__LINE__,"BulkDataSenderPerfImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
}


void BulkDataSenderPerfImpl::paceData()
{
    //ACS_TRACE("BulkDataSenderPerfImpl::paceData");

    int size;
    CORBA::ULong flowNumber;

    size = 300000000;

    try
	{

	ACE_Message_Block *mb;
	mb = new ACE_Message_Block(size);

	for (CORBA::Long j = 0; j < (size-1); j++)
	    {
	    *mb->wr_ptr()='d';
	    mb->wr_ptr(sizeof(char));
	    }
	*mb->wr_ptr()='\0';
	mb->wr_ptr(sizeof(char));

	flowNumber = 1;
	getSender()->sendData(flowNumber, mb);

	//ACS_SHORT_LOG ((LM_INFO,"flow 1 length sent data = %d", mb->length()));

	mb->release();
	}

    catch (AVInvalidFlowNumberExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderPerfImpl::paceData AVInvalidFlowNumberExImpl exception catched !"));
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderPerfImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
    catch (AVSendFrameErrorExImpl & ex)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderPerfImpl::paceData AVSendFrameErrorExImpl exception catched !"));
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderPerfImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderPerfImpl::paceData UNKNOWN exception"));
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(__FILE__,__LINE__,"BulkDataSenderPerfImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
}


void BulkDataSenderPerfImpl::stopSend()
{
    //ACS_TRACE("BulkDataSenderPerfImpl::stopSend");

    CORBA::ULong flowNumber;

    try
	{

	flowNumber = 1;
	getSender()->stopSend(flowNumber);

	}

    catch (AVInvalidFlowNumberExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderPerfImpl::stopSend AVInvalidFlowNumberExImpl exception catched !"));
	AVStopSendErrorExImpl err = AVStopSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderPerfImpl::stopSend");
	throw err.getAVStopSendErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderPerfImpl::stopSend UNKNOWN exception"));
	AVStopSendErrorExImpl err = AVStopSendErrorExImpl(__FILE__,__LINE__,"BulkDataSenderPerfImpl::stopSend");
	throw err.getAVStopSendErrorEx();
	}
}


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BulkDataSenderPerfImpl)
/* ----------------------------------------------------------------*/
