/**********************Sender part *******************************/

template<class TReceiverCallback, class TSenderCallback>
BulkDataSendRecvImpl<TReceiverCallback, TSenderCallback>::BulkDataSendRecvImpl(const ACE_CString& name, maci::ContainerServices* containerServices) :
    CharacteristicComponentImpl(name,containerServices)
{
    ACS_TRACE("BulkDataSendRecvImpl::BulkDataSendRecvImpl");

    containerServices_p=containerServices;

    dal_p = containerServices_p->getCDB();
    if (CORBA::is_nil (dal_p))
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::connect dal_p nil"));
	ACSBulkDataError::AVStreamEndpointErrorExImpl err = ACSBulkDataError::AVStreamEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSendRecvImpl::connect");
	throw err.getAVStreamEndpointErrorEx();
	}
  
}


template<class TReceiverCallback, class TSenderCallback>
BulkDataSendRecvImpl<TReceiverCallback, TSenderCallback>::~BulkDataSendRecvImpl()
{
    ACS_TRACE("BulkDataSendRecvImpl::~BulkDataSendRecvImpl");
}


template<class TReceiverCallback, class TSenderCallback>
void BulkDataSendRecvImpl<TReceiverCallback, TSenderCallback>::cleanUp()
{
    ACS_TRACE("BulkDataSendRecvImpl::cleanUp");
}



template<class TReceiverCallback, class TSenderCallback>
void BulkDataSendRecvImpl<TReceiverCallback, TSenderCallback>::connect(bulkdata::BulkDataReceiver_ptr receiverObj_p)
{
    ACS_TRACE("BulkDataSendRecvImpl::connect");

    char buf[BUFSIZ];

    try
	{
 
	ACE_CString CDBpath="alma/";
	CDBpath += name();
    
	dao_p = dal_p->get_DAO_Servant(CDBpath.c_str());
	if (CORBA::is_nil (dao_p))
	    {
	    ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::connect dao_p nil"));
	    ACSBulkDataError::AVStreamEndpointErrorExImpl err = ACSBulkDataError::AVStreamEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSendRecvImpl::connect");
	    throw err.getAVStreamEndpointErrorEx();
	    }
		
	ACE_OS::strcpy(buf,dao_p->get_string("send_protocols"));
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_WARNING,"BulkDataSendRecvImpl::connect CDB failure."));
	ACSBulkDataError::AVConnectErrorExImpl err = ACSBulkDataError::AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataSendRecvImpl::connect");
	throw err.getAVConnectErrorEx();
	}

    try
	{
	sender.initialize();

	sender.createMultipleFlows(buf);

	receiverObj_p->openReceiver();

	bulkdata::BulkDataReceiverConfig *receiverConfig = receiverObj_p->getReceiverConfig();
	if(receiverConfig == 0)
	    {
	    ACS_SHORT_LOG((LM_INFO, "BulkDataSendRecvImpl::connect ACSBulkDataError::AVReceiverConfigErrorExImpl - receiverConfig NULL"));
	    throw ACSBulkDataError::AVReceiverConfigErrorExImpl(__FILE__,__LINE__,"BulkDataSendRecvImpl::connect");
	    }
	
	sender.connectToPeer(receiverConfig);
	}

    catch (ACSBulkDataError::AVInitErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO, "BulkDataSendRecvImpl::connect ACSBulkDataError::AVInitErrorExImpl exception catched !"));
	ACSBulkDataError::AVConnectErrorExImpl err = ACSBulkDataError::AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::connect");
	throw err.getAVConnectErrorEx();
	}
    catch (ACSBulkDataError::AVStreamEndpointErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO, "BulkDataSendRecvImpl::connect ACSBulkDataError::AVStreamEndpointErrorExImpl exception catched !"));
	ACSBulkDataError::AVConnectErrorExImpl err = ACSBulkDataError::AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::connect");
	throw err.getAVConnectErrorEx();
	}
    catch (ACSBulkDataError::AVFlowEndpointErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::connect ACSBulkDataError::AVFlowEndpointErrorExImpl exception catched !"));
	ACSBulkDataError::AVConnectErrorExImpl err = ACSBulkDataError::AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::connect");
	throw err.getAVConnectErrorEx();
	}
    catch (ACSBulkDataError::AVOpenReceiverErrorEx & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::connect ACSBulkDataError::AVOpenReceiverErrorEx exception catched !"));
	ACSBulkDataError::AVConnectErrorExImpl err = ACSBulkDataError::AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::connect");
	throw err.getAVConnectErrorEx();
	}
    catch (ACSBulkDataError::AVReceiverConfigErrorEx & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::init ACSBulkDataError::AVReceiverConfigErrorEx exception catched !"));
	ACSBulkDataError::AVConnectErrorExImpl err = ACSBulkDataError::AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::connect");
	throw err.getAVConnectErrorEx();
	}
    catch (ACSBulkDataError::AVReceiverConfigErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::init ACSBulkDataError::AVReceiverConfigErrorExImpl exception catched !"));
	ACSBulkDataError::AVConnectErrorExImpl err = ACSBulkDataError::AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::connect");
	throw err.getAVConnectErrorEx();
	}
    catch (ACSBulkDataError::AVStreamBindErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::init ACSBulkDataError::AVStreamBindErrorExImpl exception catched !"));
	ACSBulkDataError::AVConnectErrorExImpl err = ACSBulkDataError::AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::connect");
	throw err.getAVConnectErrorEx();
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::connect UNKNOWN exception"));
	ACSBulkDataError::AVConnectErrorExImpl err = ACSBulkDataError::AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataSendRecvImpl::connect");
	throw err.getAVConnectErrorEx();
	}
}


template<class TReceiverCallback, class TSenderCallback>
void BulkDataSendRecvImpl<TReceiverCallback, TSenderCallback>::disconnect()
{
    ACS_TRACE("BulkDataSendRecvImpl::disconnect");

    try
	{
	sender.disconnectPeer();
	}
    catch(...)
	{
	ACSBulkDataError::AVDisconnectErrorExImpl err = ACSBulkDataError::AVDisconnectErrorExImpl(__FILE__,__LINE__,"BulkDataSendRecvImpl::disconnect");
	throw err.getAVDisconnectErrorEx();
	}
}


template<class TReceiverCallback, class TSenderCallback>
void BulkDataSendRecvImpl<TReceiverCallback, TSenderCallback>::startSend()
{
    ACS_TRACE("BulkDataSendRecvImpl::startSend");

    CORBA::Long size = 14000;

    try
	{
	ACE_Message_Block *mb1;
	mb1 = new ACE_Message_Block(size);

	for (CORBA::Long j = 0; j < (size-1); j++)
	    {
	    *mb1->wr_ptr() = 'p';
	    mb1->wr_ptr(sizeof(char));
	    }
	*mb1->wr_ptr() = '\0';
	mb1->wr_ptr(sizeof(char));
  

	CORBA::ULong flowNumber = 1;
	sender.startSend(flowNumber, mb1);

	ACS_SHORT_LOG ((LM_INFO,"ACE Message block 1 length = %d", mb1->length()));

	mb1->release();
	}

    catch (ACSBulkDataError::AVInvalidFlowNumberExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::startSend ACSBulkDataError::AVInvalidFlowNumberExImpl exception catched !"));
	ACSBulkDataError::AVStartSendErrorExImpl err = ACSBulkDataError::AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (ACSBulkDataError::AVSendFrameErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::startSend ACSBulkDataError::AVSendFrameErrorExImpl exception catched !"));
	ACSBulkDataError::AVStartSendErrorExImpl err = ACSBulkDataError::AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (ACSBulkDataError::AVFlowEndpointErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::startSend  ACSBulkDataError::AVFlowEndpointErrorExImplexception catched !"));
	ACSBulkDataError::AVStartSendErrorExImpl err = ACSBulkDataError::AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (ACSBulkDataError::AVProtocolErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::startSend  AVProtocolErrorExImplexception catched !"));
	ACSBulkDataError::AVStartSendErrorExImpl err = ACSBulkDataError::AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::startSend UNKNOWN exception"));
	ACSBulkDataError::AVStartSendErrorExImpl err = ACSBulkDataError::AVStartSendErrorExImpl(__FILE__,__LINE__,"BulkDataSendRecvImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}

}


template<class TReceiverCallback, class TSenderCallback>
void BulkDataSendRecvImpl<TReceiverCallback, TSenderCallback>::paceData()
{
    ACS_TRACE("BulkDataSendRecvImpl::paceData");

    CORBA::Long size = 1000000;

    try
	{
	ACE_Message_Block *mb;
	mb = new ACE_Message_Block(size);

	for (CORBA::Long j = 0; j < (size-1); j++)
	    {
	    *mb->wr_ptr() = 'd';
	    mb->wr_ptr(sizeof(char));
	    }
	*mb->wr_ptr() = '\0';
	mb->wr_ptr(sizeof(char));

	CORBA::ULong flowNumber = 1;
	sender.sendData(flowNumber,mb);

	ACS_SHORT_LOG ((LM_INFO,"ACE Message block length = %d", mb->length()));

	mb->release(); 
	}

    catch (ACSBulkDataError::AVInvalidFlowNumberExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::paceData ACSBulkDataError::AVInvalidFlowNumberExImpl exception catched !"));
	ACSBulkDataError::AVPaceDataErrorExImpl err = ACSBulkDataError::AVPaceDataErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
    catch (ACSBulkDataError::AVSendFrameErrorExImpl & ex)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::paceData ACSBulkDataError::AVSendFrameErrorExImpl exception catched !"));
	ACSBulkDataError::AVPaceDataErrorExImpl err = ACSBulkDataError::AVPaceDataErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::paceData UNKNOWN exception"));
	ACSBulkDataError::AVPaceDataErrorExImpl err = ACSBulkDataError::AVPaceDataErrorExImpl(__FILE__,__LINE__,"BulkDataSendRecvImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
}


template<class TReceiverCallback, class TSenderCallback>
void BulkDataSendRecvImpl<TReceiverCallback, TSenderCallback>::stopSend()
{
    ACS_TRACE("BulkDataSendRecvImpl::stopSend");

    CORBA::ULong flowNumber = 1;

    try
	{
	sender.stopSend(flowNumber);
	}
 
    catch (ACSBulkDataError::AVInvalidFlowNumberExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::stopSend ACSBulkDataError::AVInvalidFlowNumberExImpl exception catched !"));
	ACSBulkDataError::AVStopSendErrorExImpl err = ACSBulkDataError::AVStopSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::stopSend");
	throw err.getAVStopSendErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::stopSend UNKNOWN exception"));
	ACSBulkDataError::AVStopSendErrorExImpl err = ACSBulkDataError::AVStopSendErrorExImpl(__FILE__,__LINE__,"BulkDataSendRecvImpl::stopSend");
	throw err.getAVStopSendErrorEx();
	}
}


/********************** Receiver part *******************************/

template<class TReceiverCallback, class TSenderCallback>
void BulkDataSendRecvImpl<TReceiverCallback, TSenderCallback>::openReceiver() 
{
    ACS_TRACE("BulkDataSendRecvImpl::openReceiver");   
 
    char buf[BUFSIZ];

    try
	{

	ACE_CString CDBpath="alma/";
	CDBpath += name();

	dao_p = dal_p->get_DAO_Servant(CDBpath.c_str());
	if (CORBA::is_nil (dao_p))
	    {
	    ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::connect dao_p nil"));
	    ACSBulkDataError::AVStreamEndpointErrorExImpl err = ACSBulkDataError::AVStreamEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSendRecvImpl::connect");
	    throw err.getAVStreamEndpointErrorEx();
	    }
	
	ACE_OS::strcpy(buf,dao_p->get_string("recv_protocols"));
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_WARNING,"BulkDataSendRecvImpl::connect CDB failure."));
	ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataSendRecvImpl::openReceiver");
	throw err.getAVOpenReceiverErrorEx();	
	}
    
    try
	{
	receiver.initialize();

	receiver.createMultipleFlows(buf);
	}

    catch (ACSBulkDataError::AVInitErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl<>::openReceiver ACSBulkDataError::AVInitErrorEx exception catched !"));
	ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::openReceiver");
	throw err.getAVOpenReceiverErrorEx();
	}
    catch (ACSBulkDataError::AVStreamEndpointErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl<>::openReceiver ACSBulkDataError::AVStreamEndpointErrorEx exception catched !"));
	ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::openReceiver");
	throw err.getAVOpenReceiverErrorEx();
	}
    catch (ACSBulkDataError::AVFlowEndpointErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl<>::openReceiver ACSBulkDataError::AVFlowEndpointErrorEx exception catched !"));
	ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::openReceiver");
	throw err.getAVOpenReceiverErrorEx();
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl<>::openReceiver UNKNOWN exception"));
	ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataSendRecvImpl::openReceiver");
	throw err.getAVOpenReceiverErrorEx();
	}
}



template<class TReceiverCallback, class TSenderCallback>
bulkdata::BulkDataReceiverConfig * BulkDataSendRecvImpl<TReceiverCallback, TSenderCallback>::getReceiverConfig()
{
    ACS_TRACE("BulkDataSendRecvImpl::getReceiverConfig");

    bulkdata::BulkDataReceiverConfig *receiverConfig = 0;
    try
	{
	receiverConfig = receiver.getReceiverConfig();
	}
    catch(ACSBulkDataError::AVReceiverConfigErrorExImpl & ex)
	{
	throw ex.getAVReceiverConfigErrorEx();
	}

    return receiverConfig;
}




template<class TReceiverCallback, class TSenderCallback>
void BulkDataSendRecvImpl<TReceiverCallback, TSenderCallback>::closeReceiver() 
{
    ACS_TRACE("BulkDataSendRecvImpl::close");
    /*
      TReceiverCallback *cb = 0;
      ACE_CString flw = "Flow1";
      receiver.get_flow_callback(flw, cb);
      if(cb == 0)
      {
      cout << "JJJJJJJJJJJJJJJJJJJJJJ cb == 0" << endl;
      return;
      }
      cb->Holger();
    */

    try
	{
	receiver.closeReceiver();
	}
    catch(...)
	{
	ACSBulkDataError::AVCloseReceiverErrorExImpl err = ACSBulkDataError::AVCloseReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataSendRecvImpl::closeReceiver");
	throw err.getAVCloseReceiverErrorEx();
	}
}

