/**********************Sender part *******************************/

template<class TReceiverCallback, class TSenderCallback>
BulkDataSendRecvImpl<TReceiverCallback, TSenderCallback>::BulkDataSendRecvImpl(const ACE_CString& name, ContainerServices* containerServices) :
    CharacteristicComponentImpl(name,containerServices)
{
    ACS_TRACE("BulkDataSendRecvImpl::BulkDataSendRecvImpl");

    containerServices_p=containerServices;

    dal_p = containerServices_p->getCDB();
    if (CORBA::is_nil (dal_p))
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::connect dal_p nil"));
	AVStreamEndpointErrorExImpl err = AVStreamEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSendRecvImpl::connect");
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
    throw (CORBA::SystemException, AVConnectErrorEx)
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
	    AVStreamEndpointErrorExImpl err = AVStreamEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSendRecvImpl::connect");
	    throw err.getAVStreamEndpointErrorEx();
	    }
		
	ACE_OS::strcpy(buf,dao_p->get_string("send_protocols"));
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_WARNING,"BulkDataSendRecvImpl::connect CDB failure."));
	AVConnectErrorExImpl err = AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataSendRecvImpl::connect");
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
	    ACS_SHORT_LOG((LM_INFO, "BulkDataSendRecvImpl::connect AVReceiverConfigErrorExImpl - receiverConfig NULL"));
	    throw AVReceiverConfigErrorExImpl(__FILE__,__LINE__,"BulkDataSendRecvImpl::connect");
	    }
	
	sender.connectToPeer(receiverConfig);
	}

    catch (AVInitErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO, "BulkDataSendRecvImpl::connect AVInitErrorExImpl exception catched !"));
	AVConnectErrorExImpl err = AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::connect");
	throw err.getAVConnectErrorEx();
	}
    catch (AVStreamEndpointErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO, "BulkDataSendRecvImpl::connect AVStreamEndpointErrorExImpl exception catched !"));
	AVConnectErrorExImpl err = AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::connect");
	throw err.getAVConnectErrorEx();
	}
    catch (AVFlowEndpointErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::connect AVFlowEndpointErrorExImpl exception catched !"));
	AVConnectErrorExImpl err = AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::connect");
	throw err.getAVConnectErrorEx();
	}
    catch (AVOpenReceiverErrorEx & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::connect AVOpenReceiverErrorEx exception catched !"));
	AVConnectErrorExImpl err = AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::connect");
	throw err.getAVConnectErrorEx();
	}
    catch (AVReceiverConfigErrorEx & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::init AVReceiverConfigErrorEx exception catched !"));
	AVConnectErrorExImpl err = AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::connect");
	throw err.getAVConnectErrorEx();
	}
    catch (AVReceiverConfigErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::init AVReceiverConfigErrorExImpl exception catched !"));
	AVConnectErrorExImpl err = AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::connect");
	throw err.getAVConnectErrorEx();
	}
    catch (AVStreamBindErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::init AVStreamBindErrorExImpl exception catched !"));
	AVConnectErrorExImpl err = AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::connect");
	throw err.getAVConnectErrorEx();
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::connect UNKNOWN exception"));
	AVConnectErrorExImpl err = AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataSendRecvImpl::connect");
	throw err.getAVConnectErrorEx();
	}
}


template<class TReceiverCallback, class TSenderCallback>
void BulkDataSendRecvImpl<TReceiverCallback, TSenderCallback>::disconnect()
    throw (CORBA::SystemException, AVDisconnectErrorEx)
{
    ACS_TRACE("BulkDataSendRecvImpl::disconnect");

    try
	{
	sender.disconnectPeer();
	}
    catch(...)
	{
	AVDisconnectErrorExImpl err = AVDisconnectErrorExImpl(__FILE__,__LINE__,"BulkDataSendRecvImpl::disconnect");
	throw err.getAVDisconnectErrorEx();
	}
}


template<class TReceiverCallback, class TSenderCallback>
void BulkDataSendRecvImpl<TReceiverCallback, TSenderCallback>::startSend()
    throw (CORBA::SystemException, AVStartSendErrorEx)
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

    catch (AVInvalidFlowNumberExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::startSend AVInvalidFlowNumberExImpl exception catched !"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (AVSendFrameErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::startSend AVSendFrameErrorExImpl exception catched !"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (AVFlowEndpointErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::startSend  AVFlowEndpointErrorExImplexception catched !"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (AVProtocolErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::startSend  AVProtocolErrorExImplexception catched !"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::startSend UNKNOWN exception"));
	AVStartSendErrorExImpl err = AVStartSendErrorExImpl(__FILE__,__LINE__,"BulkDataSendRecvImpl::startSend");
	throw err.getAVStartSendErrorEx();
	}

}


template<class TReceiverCallback, class TSenderCallback>
void BulkDataSendRecvImpl<TReceiverCallback, TSenderCallback>::paceData()
    throw (CORBA::SystemException, AVPaceDataErrorEx)
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

    catch (AVInvalidFlowNumberExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::paceData AVInvalidFlowNumberExImpl exception catched !"));
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
    catch (AVSendFrameErrorExImpl & ex)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::paceData AVSendFrameErrorExImpl exception catched !"));
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::paceData UNKNOWN exception"));
	AVPaceDataErrorExImpl err = AVPaceDataErrorExImpl(__FILE__,__LINE__,"BulkDataSendRecvImpl::paceData");
	throw err.getAVPaceDataErrorEx();
	}
}


template<class TReceiverCallback, class TSenderCallback>
void BulkDataSendRecvImpl<TReceiverCallback, TSenderCallback>::stopSend()
    throw (CORBA::SystemException, AVStopSendErrorEx)
{
    ACS_TRACE("BulkDataSendRecvImpl::stopSend");

    CORBA::ULong flowNumber = 1;

    try
	{
	sender.stopSend(flowNumber);
	}
 
    catch (AVInvalidFlowNumberExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::stopSend AVInvalidFlowNumberExImpl exception catched !"));
	AVStopSendErrorExImpl err = AVStopSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::stopSend");
	throw err.getAVStopSendErrorEx();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl::stopSend UNKNOWN exception"));
	AVStopSendErrorExImpl err = AVStopSendErrorExImpl(__FILE__,__LINE__,"BulkDataSendRecvImpl::stopSend");
	throw err.getAVStopSendErrorEx();
	}
}


/********************** Receiver part *******************************/

template<class TReceiverCallback, class TSenderCallback>
void BulkDataSendRecvImpl<TReceiverCallback, TSenderCallback>::openReceiver() 
    throw (CORBA::SystemException, AVOpenReceiverErrorEx)
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
	    AVStreamEndpointErrorExImpl err = AVStreamEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSendRecvImpl::connect");
	    throw err.getAVStreamEndpointErrorEx();
	    }
	
	ACE_OS::strcpy(buf,dao_p->get_string("recv_protocols"));
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_WARNING,"BulkDataSendRecvImpl::connect CDB failure."));
	AVOpenReceiverErrorExImpl err = AVOpenReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataSendRecvImpl::openReceiver");
	throw err.getAVOpenReceiverErrorEx();	
	}
    
    try
	{
	receiver.initialize();

	receiver.createMultipleFlows(buf);
	}

    catch (AVInitErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl<>::openReceiver AVInitErrorEx exception catched !"));
	AVOpenReceiverErrorExImpl err = AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::openReceiver");
	throw err.getAVOpenReceiverErrorEx();
	}
    catch (AVStreamEndpointErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl<>::openReceiver AVStreamEndpointErrorEx exception catched !"));
	AVOpenReceiverErrorExImpl err = AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::openReceiver");
	throw err.getAVOpenReceiverErrorEx();
	}
    catch (AVFlowEndpointErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl<>::openReceiver AVFlowEndpointErrorEx exception catched !"));
	AVOpenReceiverErrorExImpl err = AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSendRecvImpl::openReceiver");
	throw err.getAVOpenReceiverErrorEx();
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSendRecvImpl<>::openReceiver UNKNOWN exception"));
	AVOpenReceiverErrorExImpl err = AVOpenReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataSendRecvImpl::openReceiver");
	throw err.getAVOpenReceiverErrorEx();
	}
}



template<class TReceiverCallback, class TSenderCallback>
bulkdata::BulkDataReceiverConfig * BulkDataSendRecvImpl<TReceiverCallback, TSenderCallback>::getReceiverConfig()
    throw (CORBA::SystemException, AVReceiverConfigErrorEx)
{
    ACS_TRACE("BulkDataSendRecvImpl::getReceiverConfig");

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




template<class TReceiverCallback, class TSenderCallback>
void BulkDataSendRecvImpl<TReceiverCallback, TSenderCallback>::closeReceiver() 
    throw (CORBA::SystemException, AVCloseReceiverErrorEx)
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
	AVCloseReceiverErrorExImpl err = AVCloseReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataSendRecvImpl::closeReceiver");
	throw err.getAVCloseReceiverErrorEx();
	}
}

