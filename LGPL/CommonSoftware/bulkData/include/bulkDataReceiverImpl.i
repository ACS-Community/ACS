template<class TCallback>
BulkDataReceiverImpl<TCallback>::BulkDataReceiverImpl(const ACE_CString& name,maci::ContainerServices* containerServices) : 
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
{
    ACS_TRACE("BulkDataReceiverImpl<>::openReceiver");   
 
    char buf[BUFSIZ];

    CDB::DAL_ptr dal_p = containerServices_p->getCDB();
    if(CORBA::is_nil(dal_p))
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiverImpl<>::openReceiver error getting DAL reference"));
	ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataReceiverImpl::openReceiver");
	throw err.getAVOpenReceiverErrorEx();
	}

    ACE_CString CDBpath="alma/";
    CDBpath += name();

    CDB::DAO_ptr dao_p = dal_p->get_DAO_Servant(CDBpath.c_str());
    if(CORBA::is_nil(dao_p))
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiverImpl<>::openReceiver error getting DAO reference"));
	ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataReceiverImpl::openReceiver");
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
	ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiverImpl::openReceiver");
	err.log(LM_DEBUG);
	throw err.getAVOpenReceiverErrorEx();
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataReceiverImpl::openReceiver");
	ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiverImpl::openReceiver");
	throw err.getAVOpenReceiverErrorEx();
	}
}

template<class TCallback>
bulkdata::BulkDataReceiverConfig * BulkDataReceiverImpl<TCallback>::getReceiverConfig()
{
    ACS_TRACE("BulkDataReceiverImpl::getReceiverConfig");

    bulkdata::BulkDataReceiverConfig *receiverConfig = 0;
    try
	{
	receiverConfig = getReceiver()->getReceiverConfig();
	}
    catch(ACSBulkDataError::AVReceiverConfigErrorExImpl &ex)
	{
	ACSBulkDataError::AVReceiverConfigErrorExImpl err = ACSBulkDataError::AVReceiverConfigErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiverImpl::getReceiverConfig");
	err.log(LM_DEBUG);
	throw err.getAVReceiverConfigErrorEx();
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataReceiverImpl::getReceiverConfig");
	ACSBulkDataError::AVReceiverConfigErrorExImpl err = ACSBulkDataError::AVReceiverConfigErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiverImpl::getReceiverConfig");
	throw err.getAVReceiverConfigErrorEx();
	}

    return receiverConfig;
}


template<class TCallback>
void BulkDataReceiverImpl<TCallback>::closeReceiver() 
{
    ACS_TRACE("BulkDataReceiverImpl::close");

    try
	{
	getReceiver()->closeReceiver();
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	ACSBulkDataError::AVCloseReceiverErrorExImpl err = ACSBulkDataError::AVCloseReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiverImpl::closeReceiver");
	err.log(LM_DEBUG);
	throw err.getAVCloseReceiverErrorEx();
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataReceiverImpl::closeReceiver");
	ACSBulkDataError::AVCloseReceiverErrorExImpl err = ACSBulkDataError::AVCloseReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiverImpl::closeReceiver");
	throw err.getAVCloseReceiverErrorEx();
	}
}

template<class TCallback>
void BulkDataReceiverImpl<TCallback>::closeReceiverStream(const char * stream_name)
{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiverImpl<>::closeReceiverStream NOT implemented"));
}

template<class TCallback>
ACSErr::Completion *BulkDataReceiverImpl<TCallback>::getCbStatus(CORBA::ULong flowNumber) 
{
    ACS_TRACE("BulkDataReceiverImpl::getCbStatus");
	
    TCallback *cb = 0;

    try
	{
	getReceiver()->getFlowCallback(flowNumber,cb);
	}
    catch(ACSBulkDataError::AVInvalidFlowNumberExImpl &ex)
	{
	ACSBulkDataError::AVInvalidFlowNumberExImpl err = ACSBulkDataError::AVInvalidFlowNumberExImpl(ex,__FILE__,__LINE__,"BulkDataReceiverImpl::getCbStatus");
	err.log(LM_DEBUG);
	throw err.getAVInvalidFlowNumberEx();
	}
    catch(ACSBulkDataError::AVFlowEndpointErrorExImpl &ex)
	{
	ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiverImpl::getCbStatus");
	err.log(LM_DEBUG);
	throw err.getAVFlowEndpointErrorEx();
	}

    if(cb->isError())
	{
	CompletionImpl *comp = cb->getErrorCompletion();
	//comp->log();
	return comp->returnCompletion();
	}	
    if(cb->isTimeout() && cb->isWorking())
	{
	ACSBulkDataStatus::AVCbWorkingTimeoutCompletion *comp = new ACSBulkDataStatus::AVCbWorkingTimeoutCompletion();
	//comp->log();
	return comp->returnCompletion();
	}
    if(cb->isTimeout())
	{
	ACSBulkDataStatus::AVCbTimeoutCompletion *comp = new ACSBulkDataStatus::AVCbTimeoutCompletion();
	//comp->log();
	return comp->returnCompletion();
	}
    if(cb->isWorking())
	{
	ACSBulkDataStatus::AVCbWorkingCompletion *comp = new ACSBulkDataStatus::AVCbWorkingCompletion();
	//comp->log();
	return comp->returnCompletion();
	}	
    
    ACSBulkDataStatus::AVCbReadyCompletion *comp = new ACSBulkDataStatus::AVCbReadyCompletion();
    //comp->log();

    return comp->returnCompletion();
}


template<class TCallback>
void BulkDataReceiverImpl<TCallback>::setTimeout(CORBA::ULong flowNumber, CORBA::ULong timeout) 
{
    TCallback *cb = 0;

    try
	{
	getReceiver()->getFlowCallback(flowNumber,cb);
	}
    catch(ACSBulkDataError::AVInvalidFlowNumberExImpl &ex)
	{
	ACSBulkDataError::AVInvalidFlowNumberExImpl err = ACSBulkDataError::AVInvalidFlowNumberExImpl(ex,__FILE__,__LINE__,"BulkDataReceiverImpl::setTimeout");
	err.log(LM_DEBUG);
	throw err.getAVInvalidFlowNumberEx();
	}
    catch(ACSBulkDataError::AVFlowEndpointErrorExImpl &ex)
	{
	ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiverImpl::setTimeout");
	err.log(LM_DEBUG);
	throw err.getAVFlowEndpointErrorEx();
	}

    if(cb != 0)
	{
	cb->setFlowTimeout(timeout);
	}
    else
	{
	ACS_SHORT_LOG((LM_WARNING,"BulkDataReceiverImpl<>::setTimeout - callback = 0 - no timeout set"));
	}
}


template<class TCallback>
void BulkDataReceiverImpl<TCallback>::setRecvName(const char *recvName) 
{
    try
	{
	getReceiver()->setReceiverName(recvName);
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	ACSBulkDataError::AVSetReceiverNameErrorExImpl err = ACSBulkDataError::AVSetReceiverNameErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::setRecvName");
	err.log(LM_DEBUG);
	throw err.getAVSetReceiverNameErrorEx();
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataReceiverImpl::setRecvName");
	ACSBulkDataError::AVSetReceiverNameErrorExImpl err = ACSBulkDataError::AVSetReceiverNameErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::setRecvName");
	throw err.getAVSetReceiverNameErrorEx();
	}    
}


template<class TCallback>
void BulkDataReceiverImpl<TCallback>::subscribeNotification(ACS::CBvoid_ptr notifCb)
{
    try
	{
	getReceiver()->subscribeNotification(notifCb);
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	ACSBulkDataError::AVNotificationMechanismErrorExImpl err = ACSBulkDataError::AVNotificationMechanismErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiverImpl::subscribeNotification");
	err.log(LM_DEBUG);
	throw err.getAVNotificationMechanismErrorEx();
	}
   catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataReceiverImpl::subscribeNotification");
	ACSBulkDataError::AVNotificationMechanismErrorExImpl err = ACSBulkDataError::AVNotificationMechanismErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiverImpl::subscribeNotification");
	err.log(LM_DEBUG);
	throw err.getAVNotificationMechanismErrorEx();
	}
}

template<class TCallback>
void BulkDataReceiverImpl<TCallback>::fwdData2UserCB(CORBA::Boolean enable)
{
	try
	{
		getReceiver()->fwdData2UserCB(enable);
	}
	catch(...)
	{
		ACS_SHORT_LOG((LM_WARNING,"BulkDataReceiverImpl<>::fwdData2UserCB - problem to set fwdData2UserCB"));
	}
}//fwdData2UserCB



