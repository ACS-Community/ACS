#include "bulkDataSenderEx3Impl.h"

BulkDataSenderEx3Impl::BulkDataSenderEx3Impl(const ACE_CString& name,ContainerServices* containerServices) :
    BulkDataSenderDefaultImpl(name,containerServices)
{
    ACS_TRACE("BulkDataSenderEx3Impl::BulkDataSenderEx3Impl");
}


BulkDataSenderEx3Impl::~BulkDataSenderEx3Impl()
{
    ACS_TRACE("BulkDataSenderEx3Impl::~BulkDataSenderEx3Impl");
}


void BulkDataSenderEx3Impl::startSend()
    throw (CORBA::SystemException, AVStartSendErrorEx)
{
    
    ACS_TRACE("BulkDataSenderEx3Impl::startSend");
    

    char message[] = "OK";

    try
	{
	ACE_Message_Block mb1(ACE_OS::strlen(message) + 1);
	mb1.copy(message);

	CORBA::ULong flowNumber = 1;
	getSender()->startSend(flowNumber, &mb1);

	ACS_SHORT_LOG ((LM_INFO,"flow 1 length start parameter sent = %d", mb1.length()));
	}

    catch (AVInvalidFlowNumberExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::startSend AVInvalidFlowNumberExImpl exception catched !"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (AVSendFrameErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::startSend AVSendFrameErrorExImpl exception catched !"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (AVFlowEndpointErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::startSend  AVFlowEndpointErrorExImplexception catched !"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (AVProtocolErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::startSend  AVProtocolErrorExImplexception catched !"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::startSend UNKNOWN exception"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(__FILE__,__LINE__,"BulkDataSenderImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
}


void BulkDataSenderEx3Impl::startSendErr()
    throw (CORBA::SystemException, AVStartSendErrorEx)
{
    
    ACS_TRACE("BulkDataSenderEx3Impl::startSendErr");
    

    char message[] = "ERROR";

    try
	{
	ACE_Message_Block mb1(ACE_OS::strlen(message) + 1);
	mb1.copy(message);

	CORBA::ULong flowNumber = 1;
	getSender()->startSend(flowNumber, &mb1);

	ACS_SHORT_LOG ((LM_INFO,"flow 1 length start parameter sent = %d", mb1.length()));
	}

    catch (AVInvalidFlowNumberExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::startSend AVInvalidFlowNumberExImpl exception catched !"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (AVSendFrameErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::startSend AVSendFrameErrorExImpl exception catched !"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (AVFlowEndpointErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::startSend  AVFlowEndpointErrorExImplexception catched !"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (AVProtocolErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::startSend  AVProtocolErrorExImplexception catched !"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::startSend UNKNOWN exception"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(__FILE__,__LINE__,"BulkDataSenderImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
}


void BulkDataSenderEx3Impl::startSendTimeout()
    throw (CORBA::SystemException, AVStartSendErrorEx)
{
    
    ACS_TRACE("BulkDataSenderEx3Impl::startSendTimeout");
    

    char message[] = "TIMEOUT";

    try
	{
	ACE_Message_Block mb1(ACE_OS::strlen(message) + 1);
	mb1.copy(message);

	CORBA::ULong flowNumber = 1;
	getSender()->startSend(flowNumber, &mb1);

	ACS_SHORT_LOG ((LM_INFO,"flow 1 length start parameter sent = %d", mb1.length()));
	}

    catch (AVInvalidFlowNumberExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::startSend AVInvalidFlowNumberExImpl exception catched !"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (AVSendFrameErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::startSend AVSendFrameErrorExImpl exception catched !"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (AVFlowEndpointErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::startSend  AVFlowEndpointErrorExImplexception catched !"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (AVProtocolErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::startSend  AVProtocolErrorExImplexception catched !"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::startSend UNKNOWN exception"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(__FILE__,__LINE__,"BulkDataSenderImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
}


void BulkDataSenderEx3Impl::paceDataErr()
    throw (CORBA::SystemException, AVPaceDataErrorEx)
{
    ACS_TRACE("BulkDataSenderEx3Impl::paceDataErr");

    try
	{
	int size;
	CORBA::ULong flowNumber;

	/******************************** flow 1 *********************************/

	size = 100000;

	ACE_Message_Block *mb1;
	mb1 = new ACE_Message_Block(size);

	for (CORBA::Long j = 0; j < (size-1); j++)
	    {
	    *mb1->wr_ptr()='d';
	    mb1->wr_ptr(sizeof(char));
	    }
	*mb1->wr_ptr()='\0';
	mb1->wr_ptr(sizeof(char));

	flowNumber = 1;
	getSender()->sendData(flowNumber, mb1);

	ACS_SHORT_LOG ((LM_DEBUG,"flow 1 length sent data = %d", mb1->length()));

	mb1->release();
    	}

    catch (AVInvalidFlowNumberExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::paceData AVInvalidFlowNumberExImpl exception catched !"));
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
    catch (AVSendFrameErrorExImpl & ex)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::paceData AVSendFrameErrorExImpl exception catched !"));
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
    catch (AVCouldNotOpenFileExImpl & ex)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::paceData AVCouldNotOpenFileExImpl exception catched !"));
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::paceData UNKNOWN exception"));
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(__FILE__,__LINE__,"BulkDataSenderImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
}


void BulkDataSenderEx3Impl::paceDataTimeout(CORBA::ULong timeout)
    throw (CORBA::SystemException, AVPaceDataErrorEx)
{
    ACS_TRACE("BulkDataSenderEx3Impl::paceDataTimeout");

    try
	{
	int size;
	CORBA::ULong flowNumber;

	/******************************** flow 1 *********************************/

	size = 100000;

	ACE_Message_Block *mb1;
	mb1 = new ACE_Message_Block(size);

	for (CORBA::Long j = 0; j < (size-1); j++)
	    {
	    *mb1->wr_ptr()='d';
	    mb1->wr_ptr(sizeof(char));
	    }
	*mb1->wr_ptr()='\0';
	mb1->wr_ptr(sizeof(char));

	flowNumber = 1;
	getSender()->sendData(flowNumber, mb1);

	ACS_SHORT_LOG ((LM_DEBUG,"flow 1 length sent data = %d", mb1->length()));

	mb1->release();
    	}

    catch (AVInvalidFlowNumberExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::paceData AVInvalidFlowNumberExImpl exception catched !"));
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
    catch (AVSendFrameErrorExImpl & ex)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::paceData AVSendFrameErrorExImpl exception catched !"));
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
    catch (AVCouldNotOpenFileExImpl & ex)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::paceData AVCouldNotOpenFileExImpl exception catched !"));
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::paceData UNKNOWN exception"));
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(__FILE__,__LINE__,"BulkDataSenderImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
}


void BulkDataSenderEx3Impl::paceData()
    throw (CORBA::SystemException, AVPaceDataErrorEx)
{
    ACS_TRACE("BulkDataSenderEx3Impl::paceData");

    try
	{
	int size;
	CORBA::ULong flowNumber;

	/******************************** flow 1 *********************************/

	size = 14000000;

	ACE_Message_Block *mb1;
	mb1 = new ACE_Message_Block(size);

	for (CORBA::Long j = 0; j < (size-1); j++)
	    {
	    *mb1->wr_ptr()='d';
	    mb1->wr_ptr(sizeof(char));
	    }
	*mb1->wr_ptr()='\0';
	mb1->wr_ptr(sizeof(char));

	flowNumber = 1;
	getSender()->sendData(flowNumber, mb1);

	ACS_SHORT_LOG ((LM_DEBUG,"flow 1 length sent data = %d", mb1->length()));

	mb1->release();
    	}

    catch (AVInvalidFlowNumberExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::paceData AVInvalidFlowNumberExImpl exception catched !"));
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
    catch (AVSendFrameErrorExImpl & ex)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::paceData AVSendFrameErrorExImpl exception catched !"));
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
    catch (AVCouldNotOpenFileExImpl & ex)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::paceData AVCouldNotOpenFileExImpl exception catched !"));
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::paceData UNKNOWN exception"));
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(__FILE__,__LINE__,"BulkDataSenderImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
}



void BulkDataSenderEx3Impl::stopSend()
    throw (CORBA::SystemException, AVStopSendErrorEx)
{
    ACS_TRACE("BulkDataSenderEx3Impl::stopSend");

    CORBA::ULong flowNumber = 1;

    try
	{
	getSender()->stopSend(flowNumber);
	}

    catch (AVInvalidFlowNumberExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::stopSend AVInvalidFlowNumberExImpl exception catched !"));
	AVStopSendErrorExImpl err = AVStopSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::stopSend");
	throw err.getAVStopSendErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::stopSend UNKNOWN exception"));
	AVStopSendErrorExImpl err = AVStopSendErrorExImpl(__FILE__,__LINE__,"BulkDataSenderImpl::stopSend");
	throw err.getAVStopSendErrorEx();
	}
}


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BulkDataSenderEx3Impl)
/* ----------------------------------------------------------------*/

