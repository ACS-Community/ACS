#include "senderPTImpl.h"

SenderPTImpl::SenderPTImpl(const ACE_CString& name,ContainerServices* containerServices) :
    BulkDataSenderDefaultImpl(name,containerServices)
{
    ACS_TRACE("SenderPTImpl::SenderPTImpl");
}


SenderPTImpl::~SenderPTImpl()
{
    ACS_TRACE("SenderPTImpl::~SenderPTImpl");
}


void SenderPTImpl::startSend()
    throw (CORBA::SystemException, AVStartSendErrorEx)
{    
    ACS_TRACE("SenderPTImpl::startSend");
}


void SenderPTImpl::paceData()
    throw (CORBA::SystemException, AVPaceDataErrorEx)
{
    ACS_TRACE("SenderPTImpl::paceData");
}


void SenderPTImpl::stopSend()
    throw (CORBA::SystemException, AVStopSendErrorEx)
{
    ACS_TRACE("SenderPTImpl::stopSend");
}

/***************************************************************************************************************/

void SenderPTImpl::startSendNew(CORBA::Long flwNmbr, CORBA::Long sz)
    throw (CORBA::SystemException, AVStartSendErrorEx)
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
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"senderPTImpl::startSendNew");
	throw err.getAVStartSendErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_ERROR,"senderPTImpl::startSendNew UNKNOWN exception"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(__FILE__,__LINE__,"senderPTImpl::startSendNew");
	throw err.getAVStartSendErrorEx();
	}
}


void SenderPTImpl::paceDataNew(CORBA::Long flwNmbr, CORBA::Long sz)
    throw (CORBA::SystemException, AVPaceDataErrorEx)
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
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(ex,__FILE__,__LINE__,"senderPTImpl::paceDataNew");
	throw err.getAVPaceDataErrorEx();
	}
   catch (...)
	{
	ACS_SHORT_LOG((LM_ERROR,"senderPTImpl::paceDataNew UNKNOWN exception"));
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(__FILE__,__LINE__,"senderPTImpl::paceDataNew");
	throw err.getAVPaceDataErrorEx();
	}
}


void SenderPTImpl::stopSendNew(CORBA::Long flwNmbr)
    throw (CORBA::SystemException, AVStopSendErrorEx)
{
    CORBA::ULong flowNumber = flwNmbr;

    try
	{
	getSender()->stopSend(flowNumber);
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	ACS_SHORT_LOG((LM_WARNING,"senderPTImpl::stopSendNew base exception catched !"));	
	AVStopSendErrorExImpl err = AVStopSendErrorExImpl(ex,__FILE__,__LINE__,"senderPTImpl::stopSendNew");
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
	AVStopSendErrorExImpl err = AVStopSendErrorExImpl(__FILE__,__LINE__,"senderPTImpl::stopSendNew");
	throw err.getAVStopSendErrorEx();
	}
}


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(SenderPTImpl)
/* ----------------------------------------------------------------*/

    
    
