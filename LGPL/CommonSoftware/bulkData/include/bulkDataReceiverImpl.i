template<class TCallback>
BulkDataReceiverImpl<TCallback>::BulkDataReceiverImpl(const ACE_CString& name,ContainerServices* containerServices) : 
    CharacteristicComponentImpl(name,containerServices)
{
    ACS_TRACE("BulkDataReceiverImpl<>::BulkDataReceiverImpl");

    containerServices_p=containerServices;

}


template<class TCallback>
BulkDataReceiverImpl<TCallback>::~BulkDataReceiverImpl()
{
    ACS_TRACE("BulkDataReceiverImpl<>::~BulkDataReceiverImpl");
}


template<class TCallback>
void BulkDataReceiverImpl<TCallback>::cleanUp()
{
    ACS_TRACE("BulkDataReceiverImpl<>::cleanUp");
}


template<class TCallback>
void BulkDataReceiverImpl<TCallback>::openReceiver() 
    throw (CORBA::SystemException, AVOpenReceiverErrorEx)
{
    ACS_TRACE("BulkDataReceiverImpl<>::openReceiver");   
 
    char buf[BUFSIZ];

    try
	{

	CDB::DAL_ptr dal_p = containerServices_p->getCDB();
	if (CORBA::is_nil (dal_p))
	    {
	    ACS_SHORT_LOG((LM_INFO,"BulkDataReceiverImpl<>::openReceiver dal_p nil"));
	    AVStreamEndpointErrorExImpl err = AVStreamEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiverImpl::openReceiver");
	    throw err.getAVStreamEndpointErrorEx();
	    }

	ACE_CString CDBpath="alma/";
	CDBpath += name();

	CDB::DAO_ptr dao_p = dal_p->get_DAO_Servant(CDBpath.c_str());
	if (CORBA::is_nil (dao_p))
	    {
	    ACS_SHORT_LOG((LM_INFO,"BulkDataReceiverImpl<>::openReceiver dao_p nil"));
	    AVStreamEndpointErrorExImpl err = AVStreamEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiverImpl::openReceiver");
	    throw err.getAVStreamEndpointErrorEx();
	    }

	ACE_OS::strcpy(buf,dao_p->get_string("recv_protocols"));
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_WARNING,"BulkDataReceiverImpl<>::openReceiver CDB failure."));
	AVOpenReceiverErrorExImpl err = AVOpenReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataReceiverImpl::openReceiver");
	throw err.getAVOpenReceiverErrorEx();	
	}

    try
	{
	receiver.initialize();

	receiver.createMultipleFlows(buf);
	}

    catch (AVInitErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataReceiverImpl<>::openReceiver AVInitErrorEx exception catched !"));
	AVOpenReceiverErrorExImpl err = AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiverImpl::openReceiver");
	throw err.getAVOpenReceiverErrorEx();
	}
    catch (AVStreamEndpointErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataReceiverImpl<>::openReceiver AVStreamEndpointErrorEx exception catched !"));
	AVOpenReceiverErrorExImpl err = AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiverImpl::openReceiver");
	throw err.getAVOpenReceiverErrorEx();
	}
    catch (AVFlowEndpointErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataReceiverImpl<>::openReceiver AVFlowEndpointErrorEx exception catched !"));
	AVOpenReceiverErrorExImpl err = AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiverImpl::openReceiver");
	throw err.getAVOpenReceiverErrorEx();
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataReceiverImpl<>::openReceiver UNKNOWN exception"));
	AVOpenReceiverErrorExImpl err = AVOpenReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataReceiverImpl::openReceiver");
	throw err.getAVOpenReceiverErrorEx();
	}
}



template<class TCallback>
bulkdata::BulkDataReceiverConfig * BulkDataReceiverImpl<TCallback>::getReceiverConfig()
    throw (CORBA::SystemException, AVReceiverConfigErrorEx)
{
    ACS_TRACE("BulkDataReceiverImpl::getReceiverConfig");

    bulkdata::BulkDataReceiverConfig *receiverConfig = 0;
    try
	{
	receiverConfig = receiver.getReceiverConfig();
	}
    catch(AVReceiverConfigErrorExImpl & ex)
	{
	throw ex.getAVReceiverConfigErrorEx();
	}

    return receiverConfig;
}


template<class TCallback>
void BulkDataReceiverImpl<TCallback>::closeReceiver() 
    throw (CORBA::SystemException, AVCloseReceiverErrorEx)
{
    ACS_TRACE("BulkDataReceiverImpl::close");


    try
	{
	receiver.closeReceiver();
	}
    catch(...)
	{
	AVCloseReceiverErrorExImpl err = AVCloseReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataReceiverImpl::closeReceiver");
	throw err.getAVCloseReceiverErrorEx();
	}
}


template<class TCallback>
ACSErr::Completion *BulkDataReceiverImpl<TCallback>::getCbStatus(CORBA::ULong flowNumber) 
    throw (CORBA::SystemException, AVInvalidFlowNumberEx)
{
    ACS_TRACE("BulkDataReceiverImpl::getCbStatus");
	
    TCallback *cb = 0;

    try
	{
	getReceiver()->getFlowCallback(flowNumber,cb);
	}
    catch(AVInvalidFlowNumberExImpl & ex)
	{
	throw ex.getAVInvalidFlowNumberEx();
	}

    if(cb->isError())
	{
	AVCbErrorCompletion *comp = cb->getErrorCompletion();
	//comp->log();
	return comp->returnCompletion();
	}	
    if(cb->isTimeout() && cb->isWorking())
	{
	AVCbWorkingTimeoutCompletion *comp = new AVCbWorkingTimeoutCompletion();
	//comp->log();
	return comp->returnCompletion();
	}
    if(cb->isTimeout())
	{
	AVCbTimeoutCompletion *comp = new AVCbTimeoutCompletion();
	//comp->log();
	return comp->returnCompletion();
	}
    if(cb->isWorking())
	{
	AVCbWorkingCompletion *comp = new AVCbWorkingCompletion();
	//comp->log();
	return comp->returnCompletion();
	}	
    
    AVCbReadyCompletion *comp = new AVCbReadyCompletion();
    //comp->log();

    return comp->returnCompletion();
}
