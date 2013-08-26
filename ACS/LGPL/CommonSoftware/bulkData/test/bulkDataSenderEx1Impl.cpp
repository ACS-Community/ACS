#include "bulkDataSenderEx1Impl.h"

using namespace ACSBulkDataError;

BulkDataSenderEx1Impl::BulkDataSenderEx1Impl(const ACE_CString& name,maci::ContainerServices* containerServices) :
    BulkDataSenderDefaultImpl(name,containerServices)
{
    ACS_TRACE("BulkDataSenderEx1Impl::BulkDataSenderEx1Impl");
}


BulkDataSenderEx1Impl::~BulkDataSenderEx1Impl()
{
    ACS_TRACE("BulkDataSenderEx1Impl::~BulkDataSenderEx1Impl");
}



void BulkDataSenderEx1Impl::startSend()
{
    ACS_TRACE("BulkDataSenderEx1Impl::startSend");

    try
	{
	int size;

	/******************************** flow 1 *********************************/

	size = 10000;

	ACE_Message_Block *mb1;
	mb1 = new ACE_Message_Block(size);

	for (CORBA::Long j = 0; j < (size-1); j++)
	    {
	    *mb1->wr_ptr()='p';
	    mb1->wr_ptr(sizeof(char));
	    }
	*mb1->wr_ptr()='\0';
	mb1->wr_ptr(sizeof(char));

	CORBA::ULong flowNumber = 1;
	getSender()->startSend(flowNumber, mb1);

	ACS_SHORT_LOG ((LM_DEBUG,"flow 1 length start parameter sent = %d", mb1->length()));

	mb1->release();

	/******************************** flow 2 *********************************/

	size = 12000;

	char *buf = 0; 
	buf = new char[size];
	for(CORBA::Long j = 0; j < (size-1); j++)
	    {
	    buf[j]='1';
	    }
	buf[size-1] = '\0';

    
	flowNumber = 2;
	getSender()->startSend(flowNumber, buf, size);

	ACS_SHORT_LOG ((LM_DEBUG,"flow 2 length start parameter sent = %d", size));

	delete [] buf;

	/******************************** flow 3 *********************************/


	size = 256;
    
	char fileName[size];

	ACE_OS::strcpy(fileName, "bulkDataOutput.txt");

	const char * ptr = fileName;

	flowNumber = 3;
	getSender()->startSend(flowNumber,ptr,size);
    
	ACS_SHORT_LOG ((LM_DEBUG,"flow 3 start parameter sent = %s", fileName));

	/******************************** flow 4 *********************************/


	flowNumber = 4;
	getSender()->startSend(flowNumber);
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


void BulkDataSenderEx1Impl::paceData()
{
    ACS_TRACE("BulkDataSenderImpl::paceData");

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
    
	/******************************** flow 2 *********************************/

	size = 12000000;

	char *buf = 0; 
	buf = new char[size];
	for(CORBA::Long j = 0; j < (size-1); j++)
	    {
	    buf[j]='2';
	    }
	buf[size-1] = '\0';
    
	flowNumber = 2;
	getSender()->sendData(flowNumber, buf, size);

	ACS_SHORT_LOG ((LM_DEBUG,"flow 2 length sent data = %d", size));

	delete [] buf;

	/******************************** flow 3 *********************************/


	ACE_Message_Block mb(BUFSIZ);

	FILE * fp = ACE_OS::fopen("bulkDataInput.txt","r");
	if (fp == 0)
	    {
	    ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl<>::paceData file not open successfully"));
	    AVCouldNotOpenFileExImpl err = AVCouldNotOpenFileExImpl(__FILE__,__LINE__,"BulkDataSenderImpl::paceData");
	    throw err;
	    }
	
	// Continue to send data till the file is read to the end.
	while (1)
	    {
	    // Read from the file into a message block.
	    int n = ACE_OS::fread(mb.rd_ptr (),
				  1,
				  mb.size (),
				  fp);
	
	    if (n < 0)
		{
		ACS_SHORT_LOG((LM_DEBUG," BulkDataSenderImpl<>::paceData sending file"));
		break;
		}	

	    if (n == 0)
		{
		if (feof (fp))
		    {
		    // At end of file break the loop and end the client.
		    ACS_SHORT_LOG((LM_DEBUG,"BulkDataSenderImpl<>::paceData end of file"));
		    break;
		    }
		}
	
	    mb.wr_ptr (n);
	

	    flowNumber = 3;
	    getSender()->sendData(flowNumber,&mb);
    
	    ACS_SHORT_LOG ((LM_DEBUG,"flow 3 file bulkDatainput.txt sent"));

	    // Reset the mb.
	    mb.reset ();	
	    } // end while
	
	// Close the input file
	ACE_OS::fclose(fp);	    

	/******************************** flow 4 *********************************/

	size = 37000000;

	ACE_Message_Block *mb2;
	mb2 = new ACE_Message_Block(size);
    
	for (CORBA::Long j = 0; j < (size-1); j++)
	    {
	    *mb2->wr_ptr()='d';
	    mb2->wr_ptr(sizeof(char));
	    }
	*mb2->wr_ptr()='\0';
	mb2->wr_ptr(sizeof(char));
    
	flowNumber = 4;
	getSender()->sendData(flowNumber, mb2);
    
	ACS_SHORT_LOG ((LM_DEBUG,"flow 4 length sent data = %d", mb2->length()));
    
	mb2->release();
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


void BulkDataSenderEx1Impl::stopSend()
{
    ACS_TRACE("BulkDataSenderImpl::stopSend");

    try
	{
	CORBA::ULong flowNumber = 1;
	getSender()->stopSend(flowNumber);

	flowNumber = 2;
	getSender()->stopSend(flowNumber);

	flowNumber = 3;
	getSender()->stopSend(flowNumber);

	flowNumber = 4;
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
MACI_DLL_SUPPORT_FUNCTIONS(BulkDataSenderEx1Impl)
/* ----------------------------------------------------------------*/


