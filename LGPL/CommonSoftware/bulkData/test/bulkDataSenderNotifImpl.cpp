#include "bulkDataSenderNotifImpl.h"

using namespace ACSBulkDataError;

BulkDataSenderNotifImpl::BulkDataSenderNotifImpl(const ACE_CString& name,maci::ContainerServices* containerServices) :
    BulkDataSenderDefaultImpl(name,containerServices)
{
    ACS_TRACE("BulkDataSenderNotifImpl::BulkDataSenderNotifImpl");
}


BulkDataSenderNotifImpl::~BulkDataSenderNotifImpl()
{
    ACS_TRACE("BulkDataSenderNotifImpl::~BulkDataSenderNotifImpl");
}


void BulkDataSenderNotifImpl::startSend()
{
    
    ACS_TRACE("BulkDataSenderNotifImpl::startSend");

    int size;

    CORBA::ULong flowNumber;

/****************************** flow 1 **********************************/
    
    ACE_Message_Block *mb1;
    try
	{
	size = 1000;
	
	mb1 = new ACE_Message_Block(size);

	char par1[20];
	strcpy(par1,"PARAMETER ON FLOW 1");
	strcpy(mb1->wr_ptr(),par1);
	mb1->wr_ptr(sizeof(par1));
	*mb1->wr_ptr()='\0';
	mb1->wr_ptr(sizeof(char));

	flowNumber = 1;
	getSender()->startSend(flowNumber, mb1);

	mb1->release();

/****************************** flow 2 **********************************/
    
	ACE_Message_Block *mb2;

	size = 2000;
	
	mb2 = new ACE_Message_Block(size);

	char par2[20];
	strcpy(par2,"PARAMETER ON FLOW 2");
	strcpy(mb2->wr_ptr(),par2);
	mb2->wr_ptr(sizeof(par2));
	*mb2->wr_ptr()='\0';
	mb2->wr_ptr(sizeof(char));

	flowNumber = 2;
	getSender()->startSend(flowNumber, mb2);

	mb2->release();
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderNotifImpl::startSend");
	err.log(LM_DEBUG);
	throw err.getAVStartSendErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSenderNotifImpl::startSend UNKNOWN exception"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(__FILE__,__LINE__,"BulkDataSenderNotifImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
}


void BulkDataSenderNotifImpl::paceData()
{
    ACS_TRACE("BulkDataSenderNotifImpl::paceData");

    try
	{
	int size;
	CORBA::ULong flowNumber;

	/******************************** flow 1 *********************************/

	size = 100;

	ACE_Message_Block *mb1;
	mb1 = new ACE_Message_Block(size);

	for(CORBA::Long j = 0; j < (size-1); j++)
	    {
	    *mb1->wr_ptr()='D';
	    mb1->wr_ptr(sizeof(char));
	    }
	*mb1->wr_ptr()='\0';
	mb1->wr_ptr(sizeof(char));

	flowNumber = 1;
	getSender()->sendData(flowNumber, mb1);

	mb1->release();

	/******************************** flow 2 *********************************/

	size = 1000;

	ACE_Message_Block *mb2;
	mb2 = new ACE_Message_Block(size);

	for(CORBA::Long j = 0; j < (size-1); j++)
	    {
	    *mb2->wr_ptr()='D';
	    mb2->wr_ptr(sizeof(char));
	    }
	*mb2->wr_ptr()='\0';
	mb2->wr_ptr(sizeof(char));

	flowNumber = 2;
	getSender()->sendData(flowNumber, mb2);

	mb2->release();
     	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderNotifImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSenderNotifImpl::paceData UNKNOWN exception"));
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(__FILE__,__LINE__,"BulkDataSenderNotifImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
}


void BulkDataSenderNotifImpl::stopSend()
{
    ACS_TRACE("BulkDataSenderNotifImpl::stopSend");

    CORBA::ULong flowNumber;

    try
	{
	flowNumber = 1;
	getSender()->stopSend(flowNumber);

	flowNumber = 2;
	getSender()->stopSend(flowNumber);
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVStopSendErrorExImpl err = AVStopSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderNotifImpl::stopSend");
	err.log(LM_DEBUG);
	throw err.getAVStopSendErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSenderNotifImpl::stopSend UNKNOWN exception"));
	AVStopSendErrorExImpl err = AVStopSendErrorExImpl(__FILE__,__LINE__,"BulkDataSenderNotifImpl::stopSend");
	throw err.getAVStopSendErrorEx();
	}
}


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BulkDataSenderNotifImpl)
/* ----------------------------------------------------------------*/
