template<class TSenderCallback>
BulkDataSenderDistrImpl<TSenderCallback>::BulkDataSenderDistrImpl(const ACE_CString& name,ContainerServices* containerServices) :
    BulkDataSenderImpl<TSenderCallback>(name,containerServices)
{
    ACS_TRACE("BulkDataSenderDistrImpl::BulkDataSenderDistrImpl");
}


template<class TSenderCallback>
BulkDataSenderDistrImpl<TSenderCallback>::~BulkDataSenderDistrImpl()
{
    ACS_TRACE("BulkDataSenderDistrImpl::~BulkDataSenderDistrImpl");
}


template<class TSenderCallback>
void BulkDataSenderDistrImpl<TSenderCallback>::cleanUp()
{
    ACS_TRACE("BulkDataSenderDistrImpl::cleanUp");
}



template<class TSenderCallback>
void BulkDataSenderDistrImpl<TSenderCallback>::startSend()
    throw (CORBA::SystemException, AVStartSendErrorEx)
{
    ACS_TRACE("BulkDataSenderDistrImpl::startSend");

    int size;

    size = 10000;

    try
	{


        /************************* FIRST FLOW ************************/

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
	this->getSender()->startSend(flowNumber, mb);
    
	ACS_SHORT_LOG ((LM_INFO,"flow 1 length start parameter sent = %d", mb->length()));
    
	mb->release();

        /************************* SECOND FLOW ************************/

	size = 256;

	char fileName[size];

	ACE_OS::strcpy(fileName, "bulkDataDistrOutput.txt");

	const char * ptr = fileName;

	flowNumber = 2;
	this->getSender()->startSend(flowNumber,ptr,size);
    
	ACS_SHORT_LOG ((LM_DEBUG,"flow 2 start parameter sent = %s", fileName));

	}

    catch (AVInvalidFlowNumberExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderDistrImpl::startSend AVInvalidFlowNumberExImpl exception catched !"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderDistrImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (AVSendFrameErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderDistrImpl::startSend AVSendFrameErrorExImpl exception catched !"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderDistrImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (AVFlowEndpointErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderDistrImpl::startSend  AVFlowEndpointErrorExImplexception catched !"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderDistrImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (AVProtocolErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderDistrImpl::startSend  AVProtocolErrorExImplexception catched !"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderDistrImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderDistrImpl::startSend UNKNOWN exception"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(__FILE__,__LINE__,"BulkDataSenderDistrImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
}

template<class TSenderCallback>
void BulkDataSenderDistrImpl<TSenderCallback>::paceData()
    throw (CORBA::SystemException, AVPaceDataErrorEx)
{
    ACS_TRACE("BulkDataSenderDistrImpl::paceData");

    int size;
    CORBA::ULong flowNumber;

    size = 140000;

    try
	{

        /************************* FIRST FLOW ************************/

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
	this->getSender()->sendData(flowNumber, mb);

	ACS_SHORT_LOG ((LM_INFO,"flow 1 length sent data = %d", mb->length()));

	mb->release();


        /************************* SECOND FLOW ************************/

	ACS_SHORT_LOG ((LM_INFO,"flow 2 sending file: bulkDataInput.txt ..."));

	ACE_Message_Block mb1(BUFSIZ);

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
	    int n = ACE_OS::fread(mb1.rd_ptr (),
				  1,
				  mb1.size (),
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
	
	    mb1.wr_ptr (n);
	

	    flowNumber = 2;
	    this->getSender()->sendData(flowNumber,&mb1);


	    // Reset the mb.
	    mb1.reset ();	
	    } // end while
	
	// Close the input file
	ACE_OS::fclose(fp);
	ACS_SHORT_LOG ((LM_INFO,"flow 2 file bulkDataInput.txt sent"));


	}

    catch (AVInvalidFlowNumberExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderDistrImpl::paceData AVInvalidFlowNumberExImpl exception catched !"));
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderDistrImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
    catch (AVSendFrameErrorExImpl & ex)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderDistrImpl::paceData AVSendFrameErrorExImpl exception catched !"));
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderDistrImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderDistrImpl::paceData UNKNOWN exception"));
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(__FILE__,__LINE__,"BulkDataSenderDistrImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
}


template<class TSenderCallback>
void BulkDataSenderDistrImpl<TSenderCallback>::stopSend()
    throw (CORBA::SystemException, AVStopSendErrorEx)
{
    ACS_TRACE("BulkDataSenderDistrImpl::stopSend");

    CORBA::ULong flowNumber;

    try
	{

	flowNumber = 1;
	this->getSender()->stopSend(flowNumber);

	flowNumber = 2;
	this->getSender()->stopSend(flowNumber);

	}

    catch (AVInvalidFlowNumberExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderDistrImpl::stopSend AVInvalidFlowNumberExImpl exception catched !"));
	AVStopSendErrorExImpl err = AVStopSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderDistrImpl::stopSend");
	throw err.getAVStopSendErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderDistrImpl::stopSend UNKNOWN exception"));
	AVStopSendErrorExImpl err = AVStopSendErrorExImpl(__FILE__,__LINE__,"BulkDataSenderDistrImpl::stopSend");
	throw err.getAVStopSendErrorEx();
	}
}
