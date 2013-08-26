template<class TSenderCallback>
BulkDataSenderDistrImpl<TSenderCallback>::BulkDataSenderDistrImpl(const ACE_CString& name,maci::ContainerServices* containerServices) :
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

    catch (ACSBulkDataError::AVInvalidFlowNumberExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderDistrImpl::startSend ACSBulkDataError::AVInvalidFlowNumberExImpl exception catched !"));
	ACSBulkDataError::AVStartSendErrorExImpl err = ACSBulkDataError::AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderDistrImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (ACSBulkDataError::AVSendFrameErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderDistrImpl::startSend ACSBulkDataError::AVSendFrameErrorExImpl exception catched !"));
	ACSBulkDataError::AVStartSendErrorExImpl err = ACSBulkDataError::AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderDistrImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (ACSBulkDataError::AVFlowEndpointErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderDistrImpl::startSend  ACSBulkDataError::AVFlowEndpointErrorExImplexception catched !"));
	ACSBulkDataError::AVStartSendErrorExImpl err = ACSBulkDataError::AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderDistrImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (ACSBulkDataError::AVProtocolErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderDistrImpl::startSend  ACSBulkDataError::AVProtocolErrorExImplexception catched !"));
	ACSBulkDataError::AVStartSendErrorExImpl err = ACSBulkDataError::AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderDistrImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderDistrImpl::startSend UNKNOWN exception"));
	ACSBulkDataError::AVStartSendErrorExImpl err = ACSBulkDataError::AVStartSendErrorExImpl(__FILE__,__LINE__,"BulkDataSenderDistrImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
}

template<class TSenderCallback>
void BulkDataSenderDistrImpl<TSenderCallback>::paceData()
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
	    ACSBulkDataError::AVCouldNotOpenFileExImpl err = ACSBulkDataError::AVCouldNotOpenFileExImpl(__FILE__,__LINE__,"BulkDataSenderImpl::paceData");
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

    catch (ACSBulkDataError::AVInvalidFlowNumberExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderDistrImpl::paceData ACSBulkDataError::AVInvalidFlowNumberExImpl exception catched !"));
	ACSBulkDataError::AVPaceDataErrorExImpl err = ACSBulkDataError::AVPaceDataErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderDistrImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
    catch (ACSBulkDataError::AVSendFrameErrorExImpl & ex)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderDistrImpl::paceData ACSBulkDataError::AVSendFrameErrorExImpl exception catched !"));
	ACSBulkDataError::AVPaceDataErrorExImpl err = ACSBulkDataError::AVPaceDataErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderDistrImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderDistrImpl::paceData UNKNOWN exception"));
	ACSBulkDataError::AVPaceDataErrorExImpl err = ACSBulkDataError::AVPaceDataErrorExImpl(__FILE__,__LINE__,"BulkDataSenderDistrImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
}


template<class TSenderCallback>
void BulkDataSenderDistrImpl<TSenderCallback>::stopSend()
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

    catch (ACSBulkDataError::AVInvalidFlowNumberExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderDistrImpl::stopSend ACSBulkDataError::AVInvalidFlowNumberExImpl exception catched !"));
	ACSBulkDataError::AVStopSendErrorExImpl err = ACSBulkDataError::AVStopSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderDistrImpl::stopSend");
	throw err.getAVStopSendErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderDistrImpl::stopSend UNKNOWN exception"));
	ACSBulkDataError::AVStopSendErrorExImpl err = ACSBulkDataError::AVStopSendErrorExImpl(__FILE__,__LINE__,"BulkDataSenderDistrImpl::stopSend");
	throw err.getAVStopSendErrorEx();
	}
}
