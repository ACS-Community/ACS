template<class TReceiverCallback, class TSenderCallback>
BulkDataDistributerImpl<TReceiverCallback, TSenderCallback>::BulkDataDistributerImpl(const ACE_CString& name,ContainerServices* containerServices) : 
    CharacteristicComponentImpl(name,containerServices)
{
    ACS_TRACE("BulkDataDistributerImpl<>::BulkDataDistributerImpl");

    containerServices_p = containerServices;
}


template<class TReceiverCallback, class TSenderCallback>
BulkDataDistributerImpl<TReceiverCallback, TSenderCallback>::~BulkDataDistributerImpl()
{
    ACS_TRACE("BulkDataDistributerImpl<>::~BulkDataDistributerImpl");
}

template<class TReceiverCallback, class TSenderCallback>
void BulkDataDistributerImpl<TReceiverCallback, TSenderCallback>::initialize()
    throw (ACSErr::ACSbaseExImpl)
{
    ACS_TRACE("BulkDataDistributerImpl<>::initialize");

    dal_p = containerServices_p->getCDB();
    if (CORBA::is_nil(dal_p))
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributerImpl<>::initialize error getting CDB reference"));
	AVObjectNotFoundExImpl err = AVObjectNotFoundExImpl(__FILE__,__LINE__,"BulkDataDistributerImpl<>::initialize");
	err.log();
	throw err;
	}

    distributer.setContSvc(containerServices_p);
}


template<class TReceiverCallback, class TSenderCallback>
void BulkDataDistributerImpl<TReceiverCallback, TSenderCallback>::cleanUp()
{
    ACS_TRACE("BulkDataDistributerImpl<>::cleanUp");
}


/**************************** Sender part *****************************************/

template<class TReceiverCallback, class TSenderCallback>
void BulkDataDistributerImpl<TReceiverCallback, TSenderCallback>::connect(bulkdata::BulkDataReceiver_ptr receiverObj_p)
    throw (CORBA::SystemException, AVConnectErrorEx)
{
    ACS_TRACE("BulkDataDistributerImpl<>::connect");

    // single connect not allowed
    ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributerImpl::connect AVFlowEndpointErrorExImpl single connection not allowed"));
    AVConnectErrorExImpl err = AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataDistributerImpl::connect");
    err.log(LM_DEBUG);
    throw err.getAVConnectErrorEx();
}




template<class TReceiverCallback, class TSenderCallback>
void BulkDataDistributerImpl<TReceiverCallback, TSenderCallback>::multiConnect(bulkdata::BulkDataReceiver_ptr receiverObj_p)
    throw (CORBA::SystemException, AVConnectErrorEx)
{
    ACS_TRACE("BulkDataDistributerImpl<>::multiConnect");

    if (CORBA::is_nil(receiverObj_p))
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributerImpl::multiConnect receiver reference null"));
	AVConnectErrorExImpl err = AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataDistributerImpl::multiConnect");
	throw err.getAVConnectErrorEx();
	}

    char buf[BUFSIZ];

    CORBA::ULong timeout = 0; 

    ACE_CString CDBpath="alma/";
    CDBpath += name();

    CDB::DAO_ptr dao_p = dal_p->get_DAO_Servant(CDBpath.c_str());
    if (CORBA::is_nil(dao_p))
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributerImpl::multiConnect error getting DAO reference"));
	AVConnectErrorExImpl err = AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataDistributerImpl::multiConnect");
	throw err.getAVConnectErrorEx();
	}

    ACE_OS::strcpy(buf,dao_p->get_string("sender_protocols"));

    // TBD; not used for now; waiting for requirements 
    try
	{
	timeout = (CORBA::ULong) dao_p->get_long("distr_timeout");
	}
    catch(...)
	{
	timeout = 0;
	}
    
    try
	{
	receiverObj_p->openReceiver();

	bulkdata::BulkDataReceiverConfig *receiverConfig = receiverObj_p->getReceiverConfig();

	ACE_CString recvName = receiverObj_p->name();

	distributer.multiConnect(receiverConfig,buf,recvName);

	distributer.setTimeout(timeout);
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVConnectErrorExImpl err = AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributerImpl::multiConnect");
	err.log(LM_DEBUG);
	throw err.getAVConnectErrorEx();
	}
    catch(AVOpenReceiverErrorEx &ex)
	{
	AVConnectErrorExImpl err = AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributerImpl::multiConnect");
	err.log(LM_DEBUG);
	throw err.getAVConnectErrorEx();
	}
    catch(AVReceiverConfigErrorEx &ex)
	{
	AVConnectErrorExImpl err = AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributerImpl::multiConnect");
	err.log(LM_DEBUG);
	throw err.getAVConnectErrorEx();
	}
   catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributerImpl::multiConnect UNKNOWN exception"));
	AVConnectErrorExImpl err = AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataDistributerImpl::multiConnect");
	throw err.getAVConnectErrorEx();
	}
}


template<class TReceiverCallback, class TSenderCallback>
void BulkDataDistributerImpl<TReceiverCallback, TSenderCallback>::connectByName(const char *receiverName_p)
    throw (CORBA::SystemException, AVConnectErrorEx)
{
    ACS_TRACE("BulkDataDistributerImpl<>::connectByName");

    bulkdata::BulkDataReceiver_var receiver = containerServices_p->maci::ContainerServices::getComponent<bulkdata::BulkDataReceiver>(receiverName_p);
    if(!CORBA::is_nil(receiver.in()))
	{
	try
	    {
	    multiConnect(receiver.in());
	    }
	catch(AVConnectErrorExImpl &ex)
	    {
	    ACS_SHORT_LOG((LM_INFO,"BulkDataDistributerImpl::connectByName AVConnectErrorExImpl exception catched !"));
	    AVConnectErrorExImpl err = AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributerImpl::connectByName");
	    throw err.getAVConnectErrorEx();
	    }
	}
    else
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataDistributerImpl::connectByName AVConnectErrorExImpl exception catched !"));
	AVConnectErrorExImpl err = AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataDistributerImpl::connectByName");
	throw err.getAVConnectErrorEx();
	}
}


template<class TReceiverCallback, class TSenderCallback>
void BulkDataDistributerImpl<TReceiverCallback, TSenderCallback>::disconnect()
    throw (CORBA::SystemException, AVDisconnectErrorEx)
{
    ACS_TRACE("BulkDataDistributerImpl<>::disconnect");
}



template<class TReceiverCallback, class TSenderCallback>
void BulkDataDistributerImpl<TReceiverCallback, TSenderCallback>::multiDisconnect(bulkdata::BulkDataReceiver_ptr receiverObj_p)
    throw (CORBA::SystemException, AVDisconnectErrorEx)
{
    ACE_CString recvName = receiverObj_p->name();

    try
	{
	distributer.multiDisconnect(recvName);
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVDisconnectErrorExImpl err = AVDisconnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributerImpl::multiDisconnect");
	err.log(LM_DEBUG);
	throw err.getAVDisconnectErrorEx();
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributerImpl::multiDisconnect UNKNOWN exception"));
	AVDisconnectErrorExImpl err = AVDisconnectErrorExImpl(__FILE__,__LINE__,"BulkDataDistributerImpl::disconnect");
	throw err.getAVDisconnectErrorEx();
	}
}


template<class TReceiverCallback, class TSenderCallback>
void BulkDataDistributerImpl<TReceiverCallback, TSenderCallback>::disconnectByName(const char *receiverName_p)
    throw (CORBA::SystemException, AVDisconnectErrorEx)
{
    ACS_TRACE("BulkDataDistributerImpl<>::disconnectByName");

    if(!distributer.isRecvConnected(receiverName_p))
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataDistributerImpl::disconnectByName AVDisconnectErrorExImpl - receiver %s not connected",receiverName_p));
	AVDisconnectErrorExImpl err = AVDisconnectErrorExImpl(__FILE__,__LINE__,"BulkDataDistributerImpl::disconnectByName");
	throw err.getAVDisconnectErrorEx();
	}

    bulkdata::BulkDataReceiver_var receiver = containerServices_p->maci::ContainerServices::getComponent<bulkdata::BulkDataReceiver>(receiverName_p);
    if(!CORBA::is_nil(receiver.in()))
	{
	try
	    {
	    multiDisconnect(receiver.in());
	    containerServices_p->releaseComponent(receiverName_p);	
	    }
	catch(...)
	    {
	    ACS_SHORT_LOG((LM_INFO,"BulkDataDistributerImpl::disconnectByName AVDisconnectErrorExImpl exception catched !"));
	    AVDisconnectErrorExImpl err = AVDisconnectErrorExImpl(__FILE__,__LINE__,"BulkDataDistributerImpl::disconnectByName");
	    throw err.getAVDisconnectErrorEx();
	    }
	}
    else
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataDistributerImpl::disconnectByName AVDisconnectErrorExImpl exception catched !"));
	AVDisconnectErrorExImpl err = AVDisconnectErrorExImpl(__FILE__,__LINE__,"BulkDataDistributerImpl::disconnectByName");
	throw err.getAVDisconnectErrorEx();
	}


}


template<class TReceiverCallback, class TSenderCallback>
void BulkDataDistributerImpl<TReceiverCallback, TSenderCallback>::startSend()
    throw (CORBA::SystemException, AVStartSendErrorEx)
{
    ACS_TRACE("BulkDataDistributerImpl<>::startSend");
}


template<class TReceiverCallback, class TSenderCallback>
void BulkDataDistributerImpl<TReceiverCallback, TSenderCallback>::paceData()
    throw (CORBA::SystemException, AVPaceDataErrorEx)
{
    ACS_TRACE("BulkDataDistributerImpl<>::paceData");
}


template<class TReceiverCallback, class TSenderCallback>
void BulkDataDistributerImpl<TReceiverCallback, TSenderCallback>::stopSend()
    throw (CORBA::SystemException, AVStopSendErrorEx)
{
    ACS_TRACE("BulkDataDistributerImpl<>::stopSend");
}


template<class TReceiverCallback, class TSenderCallback>
void BulkDataDistributerImpl<TReceiverCallback, TSenderCallback>::openReceiver()
    throw (CORBA::SystemException, AVOpenReceiverErrorEx)
{
    ACS_TRACE("BulkDataDistributerImpl<>::openReceiver");
  
    char buf[BUFSIZ];

    ACE_CString CDBpath="alma/";
    CDBpath += name();

    CDB::DAO_ptr dao_p = dal_p->get_DAO_Servant(CDBpath.c_str());
    if (CORBA::is_nil(dao_p))
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributerImpl<>::openReceiver error getting DAO reference"));
	AVOpenReceiverErrorExImpl err = AVOpenReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataDistributerImpl::openReceiver");
	throw err.getAVOpenReceiverErrorEx();	
	}
    
    ACE_OS::strcpy(buf,dao_p->get_string("recv_protocols"));

    AcsBulkdata::BulkDataReceiver<TReceiverCallback> *recv = distributer.getReceiver();
    if(recv == 0)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributerImpl<>::openReceiver error getting receiver reference"));
	AVOpenReceiverErrorExImpl err = AVOpenReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataDistributerImpl::openReceiver");
	throw err.getAVOpenReceiverErrorEx();	
	}

    try
	{
	recv->initialize();

	recv->createMultipleFlows(buf);
	}

    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVOpenReceiverErrorExImpl err = AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributerImpl::openReceiver");
	throw err.getAVOpenReceiverErrorEx();
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataDistributerImpl<>::openReceiver UNKNOWN exception"));
	AVOpenReceiverErrorExImpl err = AVOpenReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataDistributerImpl::openReceiver");
	throw err.getAVOpenReceiverErrorEx();
	}
}


template<class TReceiverCallback, class TSenderCallback>
bulkdata::BulkDataReceiverConfig * BulkDataDistributerImpl<TReceiverCallback, TSenderCallback>::getReceiverConfig()
    throw (CORBA::SystemException, AVReceiverConfigErrorEx)
{
    ACS_TRACE("BulkDataDistributerImpl<>::getReceiverConfig");

    bulkdata::BulkDataReceiverConfig *receiverConfig = 0;
    try
	{
	receiverConfig = distributer.getReceiver()->getReceiverConfig();
	}
    catch(AVReceiverConfigErrorExImpl & ex)
	{
	throw ex.getAVReceiverConfigErrorEx();
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributerImpl<>::getReceiverConfig UNKNOWN exception"));
	AVReceiverConfigErrorExImpl err = AVReceiverConfigErrorExImpl(__FILE__,__LINE__,"BulkDataDistributerImpl::getReceiverConfig");
	throw err.getAVReceiverConfigErrorEx();
	}

    return receiverConfig;
}


template<class TReceiverCallback, class TSenderCallback>
void BulkDataDistributerImpl<TReceiverCallback, TSenderCallback>::closeReceiver()
    throw (CORBA::SystemException, AVCloseReceiverErrorEx)
{
    ACS_TRACE("BulkDataDistributerImpl<>::closeReceiver");

    try
	{
	distributer.getReceiver()->closeReceiver();
	}
    catch(...)
	{
	AVCloseReceiverErrorExImpl err = AVCloseReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataDistributerImpl::closeReceiver");
	throw err.getAVCloseReceiverErrorEx();
	}
}


template<class TReceiverCallback, class TSenderCallback>
void BulkDataDistributerImpl<TReceiverCallback, TSenderCallback>::setReceiver(const bulkdata::BulkDataReceiverConfig &receiverConfig)
    throw (CORBA::SystemException, AVFlowEndpointErrorEx)
{
    ACS_TRACE("BulkDataDistributerImpl<>::setReceiver");

    CORBA::ULong dim = receiverConfig.fepsInfo.length();
    
// loop on all flows
    for(CORBA::ULong i = 0; i < dim; i++)
	{ 
	ACE_CString flw = TAO_AV_Core::get_flowname(receiverConfig.fepsInfo[i]);

	TReceiverCallback *cb = 0;

	AcsBulkdata::BulkDataReceiver<TReceiverCallback> *recv = distributer.getReceiver();
	if(recv == 0)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributerImpl<>::setReceiver error getting receiver reference"));
	    AVFlowEndpointErrorExImpl err = AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataBulkDataDistributerImpl::setReceiver");
	    throw err.getAVFlowEndpointErrorEx();
	    }
	try
	    {
	    recv->getFlowCallback(flw, cb);
	    }
	catch(ACSErr::ACSbaseExImpl &ex)
	    {
	    AVFlowEndpointErrorExImpl err = AVFlowEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataBulkDataDistributerImpl::setReceiver");
	    throw err.getAVFlowEndpointErrorEx();
	    }
	catch(...)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributerImpl::setReceiver UNKNOWN exception"));
	    AVFlowEndpointErrorExImpl err = AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataBulkDataDistributerImpl::setReceiver");
	    throw err.getAVFlowEndpointErrorEx();
	    }

	if(cb == 0)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributerImpl<>::setReceiver distributor callback null"));
	    AVFlowEndpointErrorExImpl err = AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataBulkDataDistributerImpl::setReceiver");
	    throw err.getAVFlowEndpointErrorEx();
	    } 
	else
	    {
	    cb->setDistributerImpl(this);
	    }	    
	}
}


template<class TReceiverCallback, class TSenderCallback>
ACSErr::Completion * BulkDataDistributerImpl<TReceiverCallback, TSenderCallback>::getCbStatus(CORBA::ULong flowNumber) 
    throw (CORBA::SystemException, AVInvalidFlowNumberEx, AVFlowEndpointErrorEx)
{
    ACS_TRACE("BulkDataDistributerImpl<>::getCbStatus");
    

    TReceiverCallback *cb = 0;

    try
	{
	getDistributer()->getReceiver()->getFlowCallback(flowNumber,cb);
	}
    catch(AVInvalidFlowNumberExImpl & ex)
	{
	throw ex.getAVInvalidFlowNumberEx();
	}
    catch(AVFlowEndpointErrorExImpl &ex)
	{
	throw ex.getAVFlowEndpointErrorEx();
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


template<class TReceiverCallback, class TSenderCallback>
ACSErr::Completion * BulkDataDistributerImpl<TReceiverCallback, TSenderCallback>::getReceiverCbStatus(const char *recvName, CORBA::ULong flowNumber) 
    throw (CORBA::SystemException)
{
    ACS_TRACE("BulkDataDistributerImpl<>::getReceiverCbStatus");

    bulkdata::BulkDataReceiver_var receiver = containerServices_p->maci::ContainerServices::getComponent<bulkdata::BulkDataReceiver>(recvName);
    if(!CORBA::is_nil(receiver.in()))
	{
	return receiver->getCbStatus(flowNumber);
	}
    
    AVCbNotAvailableCompletion *comp = new AVCbNotAvailableCompletion();
    return comp->returnCompletion();
}
