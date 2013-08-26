#include "bulkDataSenderEx3Impl.h"

using namespace ACSBulkDataError;

BulkDataSenderEx3Impl::BulkDataSenderEx3Impl(const ACE_CString& name,maci::ContainerServices* containerServices) :
    BulkDataSenderDefaultImpl(name,containerServices)
{
    ACS_TRACE("BulkDataSenderEx3Impl::BulkDataSenderEx3Impl");
}


BulkDataSenderEx3Impl::~BulkDataSenderEx3Impl()
{
    ACS_TRACE("BulkDataSenderEx3Impl::~BulkDataSenderEx3Impl");
}


void BulkDataSenderEx3Impl::startSend()
{
  
    char message[] = "OK";

    try
	{
	ACE_Message_Block mb1(ACE_OS::strlen(message) + 1);
	mb1.copy(message);

	CORBA::ULong flowNumber = 1;
	getSender()->startSend(flowNumber, &mb1);

	ACS_SHORT_LOG ((LM_INFO,"flow 1 length start parameter sent = %d", mb1.length()));
	}

    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderEx3Impl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (...)
	{
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(__FILE__,__LINE__,"BulkDataSenderEx3Impl::startSend UNKNOWN exception");
	throw err.getAVStartSendErrorEx();
	}

}


void BulkDataSenderEx3Impl::startSendErr()
{  

    char message[] = "ERROR";

    try
	{
	ACE_Message_Block mb1(ACE_OS::strlen(message) + 1);
	mb1.copy(message);

	CORBA::ULong flowNumber = 1;
	getSender()->startSend(flowNumber, &mb1);

	ACS_SHORT_LOG ((LM_INFO,"flow 1 length start parameter sent = %d", mb1.length()));
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderEx3Impl::startSendErr");
	throw err.getAVStartSendErrorEx();
	}
    catch (...)
	{
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(__FILE__,__LINE__,"BulkDataSenderEx3Impl::startSendErr UNKNOWN exception");
	throw err.getAVStartSendErrorEx();
	}
}


void BulkDataSenderEx3Impl::startSendTimeout()
{
    
    char message[] = "TIMEOUT";

    try
	{
	ACE_Message_Block mb1(ACE_OS::strlen(message) + 1);
	mb1.copy(message);

	CORBA::ULong flowNumber = 1;
	getSender()->startSend(flowNumber, &mb1);

	ACS_SHORT_LOG ((LM_INFO,"flow 1 length start parameter sent = %d", mb1.length()));
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderEx3Impl::startSendTimeout");
	throw err.getAVStartSendErrorEx();
	}
    catch (...)
	{
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(__FILE__,__LINE__,"BulkDataSenderEx3Impl::startSendTimeout UNKNOWN exception");
	throw err.getAVStartSendErrorEx();
	}
}


void BulkDataSenderEx3Impl::paceDataErr()
{

    try
	{
	int size;
	CORBA::ULong flowNumber;

	/******************************** flow 1 *********************************/

	size = 1000000;

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
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderEx3Impl::paceDataErr");
	throw err.getAVPaceDataErrorEx();
	}
    catch (...)
	{
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(__FILE__,__LINE__,"BulkDataSenderEx3Impl::paceDataErr UNKNOWN exception");
	throw err.getAVPaceDataErrorEx();
	}

}


void BulkDataSenderEx3Impl::paceData()
{

    try
	{
	int size;
	CORBA::ULong flowNumber;

	/******************************** flow 1 *********************************/

	size = 1000000;

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

    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderEx3Impl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
    catch (...)
	{
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(__FILE__,__LINE__,"BulkDataSenderEx3Impl::paceData UNKNOWN exception");
	throw err.getAVPaceDataErrorEx();
	}

}



void BulkDataSenderEx3Impl::stopSend()
{

    CORBA::ULong flowNumber = 1;

    try
	{
	getSender()->stopSend(flowNumber);
	}

    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVStopSendErrorExImpl err = AVStopSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderEx3Impl::stopSend");
	throw err.getAVStopSendErrorEx();
	}
    catch (...)
	{
	AVStopSendErrorExImpl err = AVStopSendErrorExImpl(__FILE__,__LINE__,"BulkDataSenderEx3Impl::stopSend UNKNOWN exception");
	throw err.getAVStopSendErrorEx();
	}
}


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BulkDataSenderEx3Impl)
/* ----------------------------------------------------------------*/

