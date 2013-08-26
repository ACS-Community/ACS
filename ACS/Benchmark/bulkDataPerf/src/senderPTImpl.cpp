#include "senderPTImpl.h"

SenderPTImpl::SenderPTImpl(const ACE_CString& name,maci::ContainerServices* containerServices) :
    BulkDataSenderDefaultImpl(name,containerServices)
{
    ACS_TRACE("SenderPTImpl::SenderPTImpl");
}


SenderPTImpl::~SenderPTImpl()
{
    ACS_TRACE("SenderPTImpl::~SenderPTImpl");
}


void SenderPTImpl::startSend()
{    
    ACS_TRACE("SenderPTImpl::startSend");
}


void SenderPTImpl::paceData()
{
    ACS_TRACE("SenderPTImpl::paceData");
}


void SenderPTImpl::stopSend()
{
    ACS_TRACE("SenderPTImpl::stopSend");
}

/***************************************************************************************************************/

void SenderPTImpl::startSendNew(CORBA::Long flwNmbr, CORBA::Long sz)
{
    int size = sz;
    CORBA::ULong flowNumber = flwNmbr;
    
    ACE_Message_Block *mb1;
    try
	{
	mb1 = new ACE_Message_Block(size);

	for (CORBA::Long j = 0; j < (size-1); j++)
	    {
	    *mb1->wr_ptr()='p';
	    mb1->wr_ptr(sizeof(char));
	    }
	*mb1->wr_ptr()='\0';
	mb1->wr_ptr(sizeof(char));

	//flowNumber = 1;
	getSender()->startSend(flowNumber, mb1);

	//ACS_SHORT_LOG ((LM_DEBUG,"flow 1 length start parameter sent = %d", mb1->length()));
	//ACS_SHORT_LOG ((LM_INFO,"flow 1 length start parameter sent = %d", mb1->length()));

	mb1->release();
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	ACSBulkDataError::AVStartSendErrorExImpl err = ACSBulkDataError::AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"senderPTImpl::startSendNew");
	throw err.getAVStartSendErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_ERROR,"senderPTImpl::startSendNew UNKNOWN exception"));
	ACSBulkDataError::AVStartSendErrorExImpl err = ACSBulkDataError::AVStartSendErrorExImpl(__FILE__,__LINE__,"senderPTImpl::startSendNew");
	throw err.getAVStartSendErrorEx();
	}
}


void SenderPTImpl::paceDataNew(CORBA::Long flwNmbr, CORBA::Long sz)
{
    CORBA::ULong flowNumber = flwNmbr;
    int size = sz;

    try
	{
	ACE_Message_Block *mb1;
	mb1 = new ACE_Message_Block(size);

	for (CORBA::Long j = 0; j < (size-1); j++)
	    {
	    *mb1->wr_ptr()='d';
	    mb1->wr_ptr(sizeof(char));
	    }
	*mb1->wr_ptr()='\0';
	mb1->wr_ptr(sizeof(char));

	//flowNumber = 1;
	getSender()->sendData(flowNumber, mb1);

	//ACS_SHORT_LOG ((LM_DEBUG,"flow 1 length sent data = %d", mb1->length()));
	//ACS_SHORT_LOG ((LM_INFO,"flow 1 length sent data = %d", mb1->length()));

	mb1->release();
     	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	ACSBulkDataError::AVPaceDataErrorExImpl err = ACSBulkDataError::AVPaceDataErrorExImpl(ex,__FILE__,__LINE__,"senderPTImpl::paceDataNew");
	throw err.getAVPaceDataErrorEx();
	}
   catch (...)
	{
	ACS_SHORT_LOG((LM_ERROR,"senderPTImpl::paceDataNew UNKNOWN exception"));
	ACSBulkDataError::AVPaceDataErrorExImpl err = ACSBulkDataError::AVPaceDataErrorExImpl(__FILE__,__LINE__,"senderPTImpl::paceDataNew");
	throw err.getAVPaceDataErrorEx();
	}
}


void SenderPTImpl::stopSendNew(CORBA::Long flwNmbr)
{
    CORBA::ULong flowNumber = flwNmbr;

    try
	{
	getSender()->stopSend(flowNumber);
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	ACS_SHORT_LOG((LM_WARNING,"senderPTImpl::stopSendNew base exception catched !"));	
	ACSBulkDataError::AVStopSendErrorExImpl err = ACSBulkDataError::AVStopSendErrorExImpl(ex,__FILE__,__LINE__,"senderPTImpl::stopSendNew");
	throw err.getAVStopSendErrorEx();
	}
    /*catch (AVInvalidFlowNumberExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"SenderPTImpl::stopSendNew AVInvalidFlowNumberExImpl exception catched !"));
	AVStopSendErrorExImpl err = AVStopSendErrorExImpl(ex,__FILE__,__LINE__,"SenderPTImpl::stopSendNew");
	throw err.getAVStopSendErrorEx();
	}*/
    catch (...)
	{
	ACS_SHORT_LOG((LM_ERROR,"senderPTImpl::stopSendNew UNKNOWN exception"));
	ACSBulkDataError::AVStopSendErrorExImpl err = ACSBulkDataError::AVStopSendErrorExImpl(__FILE__,__LINE__,"senderPTImpl::stopSendNew");
	throw err.getAVStopSendErrorEx();
	}
}


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(SenderPTImpl)
/* ----------------------------------------------------------------*/

    
    
