template<class TCallback>
BulkDataReceiverImpl<TCallback>::BulkDataReceiverImpl(const ACE_CString& name,ContainerServices* containerServices) : 
    CharacteristicComponentImpl(name,containerServices)
{
    ACS_TRACE("BulkDataReceiverImpl<>::BulkDataReceiverImpl");

    containerServices_p = containerServices;
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

    CDB::DAL_ptr dal_p = containerServices_p->getCDB();
    if(CORBA::is_nil(dal_p))
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiverImpl<>::openReceiver error getting DAL reference"));
	AVOpenReceiverErrorExImpl err = AVOpenReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataReceiverImpl::openReceiver");
	throw err.getAVOpenReceiverErrorEx();
	}

    ACE_CString CDBpath="alma/";
    CDBpath += name();

    CDB::DAO_ptr dao_p = dal_p->get_DAO_Servant(CDBpath.c_str());
    if(CORBA::is_nil(dao_p))
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiverImpl<>::openReceiver error getting DAO reference"));
	AVOpenReceiverErrorExImpl err = AVOpenReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataReceiverImpl::openReceiver");
	throw err.getAVOpenReceiverErrorEx();
	}
    
    ACE_OS::strcpy(buf,dao_p->get_string("recv_protocols"));

    try
	{
	receiver.initialize();

	receiver.createMultipleFlows(buf);
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVOpenReceiverErrorExImpl err = AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiverImpl::openReceiver");
	err.log(LM_DEBUG);
	throw err.getAVOpenReceiverErrorEx();
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiverImpl::openReceiver UNKNOWN exception"));
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
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiverImpl<>::getReceiverConfig UNKNOWN exception"));
	AVReceiverConfigErrorExImpl err = AVReceiverConfigErrorExImpl(__FILE__,__LINE__,"BulkDataReceiverImpl::getReceiverConfig");
	throw err.getAVReceiverConfigErrorEx();
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
    throw (CORBA::SystemException, AVInvalidFlowNumberEx, AVFlowEndpointErrorEx)
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
    catch(AVFlowEndpointErrorExImpl &ex)
	{
	throw ex.getAVFlowEndpointErrorEx();
	}

    if(cb->isError())
	{
	CompletionImpl *comp = cb->getErrorCompletion();
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


template<class TCallback>
void BulkDataReceiverImpl<TCallback>::setTimeout(CORBA::ULong flowNumber, CORBA::ULong timeout) 
    throw (CORBA::SystemException, AVInvalidFlowNumberEx, AVFlowEndpointErrorEx)
{
    TCallback *cb = 0;

    try
	{
	getReceiver()->getFlowCallback(flowNumber,cb);
	}
    catch(AVInvalidFlowNumberExImpl & ex)
	{
	throw ex.getAVInvalidFlowNumberEx();
	}
    catch(AVFlowEndpointErrorExImpl &ex)
	{
	throw ex.getAVFlowEndpointErrorEx();
	}

    cb->setFlowTimeout(timeout);
}


template<class TCallback>
void BulkDataReceiverImpl<TCallback>::setRecvName(const char *recvName) 
    throw (CORBA::SystemException, AVSetReceiverNameErrorEx)
{
    try
	{
	getReceiver()->setReceiverName(recvName);
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVSetReceiverNameErrorExImpl err = AVSetReceiverNameErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::setRecvName");
	throw err.getAVSetReceiverNameErrorEx();
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiverImpl<>::setRecvName UNKNOWN exception"));
	AVSetReceiverNameErrorExImpl err = AVSetReceiverNameErrorExImpl(__FILE__,__LINE__,"BulkDataSenderImpl::setRecvName");
	throw err.getAVSetReceiverNameErrorEx();
	}    
}
