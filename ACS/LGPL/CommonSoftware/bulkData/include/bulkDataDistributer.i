template<class TReceiverCallback, class TSenderCallback>
AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::BulkDataDistributer() : sender_p(0),timeout_m(0),numberOfFlows(0),offset(100),contSvc_p(0)
{
    ACE_TRACE("BulkDataDistributer<>::BulkDataDistributer");

    distributerNotifCb_p = 0;

    locNotifCb_p = 0;
}


template<class TReceiverCallback, class TSenderCallback>
AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::~BulkDataDistributer()
{
    ACE_TRACE("BulkDataDistributer<>::~BulkDataDistributer");

    Sender_Map_Iterator iterator (senderMap_m);
    Sender_Map_Entry *entry = 0;
    for (;iterator.next (entry) !=  0;iterator.advance ())
	{	
	(entry->int_id_).second()->disconnectPeer();
	BulkDataSender<TSenderCallback> *locSender_p = (entry->int_id_).second();
	if (locSender_p != 0)
	    delete locSender_p;

	CORBA::release((entry->int_id_).first().receiver);
	}

    if(distributerNotifCb_p)
	distributerNotifCb_p->_remove_ref();

    CORBA::release(locNotifCb_p);
}


template<class TReceiverCallback, class TSenderCallback>
void AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::multiConnect(bulkdata::BulkDataReceiverConfig *recvConfig_p, const char *fepsConfig, const ACE_CString& receiverName)
{
    ACS_TRACE("BulkDataDistributer<>::multiConnect");

    try
	{
	if(isRecvConnected(receiverName))
	    {
	    if(getSenderConnectionState() == bulkdata::CONNECTED) // the sender is already connected, simply return
		{
		return;
		}
	    else
		{
		ACS_SHORT_LOG((LM_WARNING,"BulkDataDistributer<>::multiConnect receiver %s already connected",receiverName.c_str()));
		ACSBulkDataError::AVConnectErrorExImpl err = ACSBulkDataError::AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataDistributer<>::multiConnect");
		throw err;
		}
	    }

	sender_p = new BulkDataSender<TSenderCallback>;
	if(sender_p == 0)
	    {
	    ACS_SHORT_LOG((LM_ERROR, "BulkDataDistributer<>::multiConnect error creating sender"));
	    ACSBulkDataError::AVConnectErrorExImpl err = ACSBulkDataError::AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataDistributer<>::multiConnect");
	    throw err;
	    }

	sender_p->initialize();

	sender_p->createMultipleFlows(fepsConfig);
  
	sender_p->connectToPeer(recvConfig_p);

	bulkdata::BulkDataReceiver_var receiver_p = contSvc_p->maci::ContainerServices::getComponentNonSticky<bulkdata::BulkDataReceiver>(receiverName.c_str());
	if(CORBA::is_nil(receiver_p.in()))
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributer<>::multiConnect could not get receiver reference"));	
	    ACSBulkDataError::AVConnectErrorExImpl err = ACSBulkDataError::AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataDistributer<>::multiConnect");
	    throw err;
	    }

	Sender_Map_Pair pair;
	RecvData rd;
	rd.receiver = bulkdata::BulkDataReceiver::_duplicate(receiver_p);
	rd.mutex = new ACE_RW_Thread_Mutex();

	pair.first(rd);
	pair.second(sender_p);

	senderMap_m.bind(receiverName,pair);	

	recvStatusMap_m.bind(receiverName,offset);

	std::vector<std::string> flowNames = sender_p->getFlowNames();
	
	numberOfFlows = flowNames.size();

	std::vector<std::string>::size_type flwIdx;
	for (flwIdx = 0; flwIdx < numberOfFlows; ++flwIdx)
	    {
	    CORBA::ULong currVal = (CORBA::ULong)flwIdx + offset + 1;	    
	    flowsStatusMap_m.bind(currVal,FLOW_AVAILABLE);
	    }
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	ACSBulkDataError::AVConnectErrorExImpl err = ACSBulkDataError::AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributer<>::multiConnect");
	throw err;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataDistributer::multiConnect");
	ACSBulkDataError::AVConnectErrorExImpl err = ACSBulkDataError::AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributer<>::multiConnect");
	throw err;
	}

    offset += 100;
}


template<class TReceiverCallback, class TSenderCallback>
void AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::multiDisconnect(const ACE_CString& receiverName)
{
    ACS_TRACE("BulkDataDistributer<>::multiDisconnect");

    try
	{
	BulkDataSender<TSenderCallback> *locSender_p;

	Sender_Map_Pair pair;

	//if (senderMap_m.find(receiverName,locSender_p) != 0)
	if (senderMap_m.find(receiverName,pair) != 0)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributer<>::multiDisconnect connected sender not found"));	
	    ACSBulkDataError::AVDisconnectErrorExImpl err = ACSBulkDataError::AVDisconnectErrorExImpl(__FILE__,__LINE__,"BulkDataDistributer<>::multiDisconnect");
	    throw err;
	    }
	else
	    {
	    int r;
	    ACS_Write_Guard<ACE_RW_Thread_Mutex> g(*(pair.first().mutex), r); // the mutex should be requred with timeout, but it is not supported for RW mutexes
	    recvStatusMap_m.unbind(receiverName);
	    senderMap_m.unbind(receiverName);
	    locSender_p = pair.second();
	    g.release();
	    g.take_ownership(); //we give mutex dor the guard - not the nicest way, but in this way we delete the mutex
	    // when the guard is deleted

	    bulkdata::BulkDataReceiver_var receiver = contSvc_p->maci::ContainerServices::getComponentNonSticky<bulkdata::BulkDataReceiver>(receiverName.c_str());
	    if(CORBA::is_nil(receiver.in()))
		{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributer<>::multiDisconnect could not get receiver reference"));	
		ACSBulkDataError::AVDisconnectErrorExImpl err = ACSBulkDataError::AVDisconnectErrorExImpl(__FILE__,__LINE__,"BulkDataDistributer<>::multiDisconnect");
		throw err;
		}
	    else
		{
		std::vector<std::string> vec = locSender_p->getFlowNames();
		for(CORBA::ULong i = 0; i < vec.size(); i++)
		    {
		    CORBA::Boolean loop = true;

		    try
			{
			while(loop)
			    {
			    CompletionImpl comp = receiver->getCbStatus(i+1);
			
			    /*
			      if(comp.getCode() == ACSBulkDataStatus::AVCbReady)
			      cout << "ACSBulkDataStatus::AVCbReady" << endl;
			      if(comp.getCode() == ACSBulkDataStatus::AVCbTimeout)
			      cout << "ACSBulkDataStatus::AVCbTimeout" << endl;
			      if(comp.getCode() == ACSBulkDataStatus::AVCbWorking)
			      cout << "ACSBulkDataStatus::AVCbWorking" << endl;
			      if(comp.getCode() == ACSBulkDataStatus::AVCbError)
			      cout << "ACSBulkDataStatus::AVCbError" << endl;
			      if(comp.getCode() == ACSBulkDataStatus::AVCbWorkingTimeout)
			      cout << "ACSBulkDataStatus::AVCbWorkingTimeout" << endl;
			      if(comp.getCode() == ACSBulkDataStatus::AVCbNotAvailable)
			      cout << "ACSBulkDataStatus::AVCbNotAvailable" << endl;
			    */		   

			    if ((comp.getCode() == ACSBulkDataStatus::AVCbReady) || 
				(comp.getCode() == ACSBulkDataStatus::AVCbTimeout))
				{
				loop = false;
				}
			    ACE_OS::sleep(1);
			    }
			}
		    catch(ACSBulkDataError::AVInvalidFlowNumberEx &ex)
			{
			ACSBulkDataError::AVDisconnectErrorExImpl err = ACSBulkDataError::AVDisconnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributer::multiDisconnect");
			throw err;
			}
		    catch(ACSBulkDataError::AVFlowEndpointErrorEx &ex)
			{
			ACSBulkDataError::AVDisconnectErrorExImpl err = ACSBulkDataError::AVDisconnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributer::multiDisconnect");
			throw err;
			}
		    catch(...)
			{
			ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataDistributer::multiDisconnect");
			ACSBulkDataError::AVDisconnectErrorExImpl err = ACSBulkDataError::AVDisconnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributer::multiDisconnect");
			throw err;
			}
		    }
		}
	
	    try
		{
		locSender_p->disconnectPeer();
		receiver->closeReceiver();
		delete locSender_p;

		CORBA::release(pair.first().receiver);

		}
	    catch(ACSErr::ACSbaseExImpl &ex)
		{
		ACSBulkDataError::AVDisconnectErrorExImpl err = ACSBulkDataError::AVDisconnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributer::multiDisconnect");
		throw err;
		}
	    catch(ACSBulkDataError::AVCloseReceiverErrorEx &ex)
		{
		ACSBulkDataError::AVDisconnectErrorExImpl err = ACSBulkDataError::AVDisconnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributer::multiDisconnect");
		throw err;
		}
	    catch(...)
		{
		ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataDistributer::multiDisconnect");
		ACSBulkDataError::AVDisconnectErrorExImpl err = ACSBulkDataError::AVDisconnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributer::multiDisconnect");
		throw err;
		}

	    }
	}
    catch(ACSBulkDataError::AVDisconnectErrorExImpl &ex)
	{
	ACSBulkDataError::AVDisconnectErrorExImpl err = ACSBulkDataError::AVDisconnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributer::multiDisconnect");
	throw err;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataDistributer::multiDisconnect");
	ACSBulkDataError::AVDisconnectErrorExImpl err = ACSBulkDataError::AVDisconnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributer::multiDisconnect");
	throw err;
	}

}


template<class TReceiverCallback, class TSenderCallback>
bool AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::isRecvConnected(const ACE_CString& receiverName)
{
    ACS_TRACE("BulkDataDistributer<>::isRecvConnected");

    if (senderMap_m.find(receiverName) == 0)
	return true;
    
    return false;
}


template<class TReceiverCallback, class TSenderCallback>
bool AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::isSenderConnected(const ACE_CString& receiverName)
{
    ACS_TRACE("BulkDataDistributer<>::isSenderConnected");

    if (senderMap_m.find(receiverName) == 0)
	return true;
    
    return false;
}

template<class TReceiverCallback, class TSenderCallback>
bool AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::isReceiverConnected(const ACE_CString& receiverName)
{
    ACS_TRACE("BulkDataDistributer<>::isReceiverConnected");

    Sender_Map_Pair pair;

    if (senderMap_m.find(receiverName,pair) == 0)
	{
	CORBA::Boolean avail = false;
	std::vector<std::string> flowNamesVec = pair.second()->getFlowNames();
	for(CORBA::ULong i = 0; i < flowNamesVec.size(); i++)
	    {
	    avail = getFlowReceiverStatus(receiverName, i+1);
	    if(avail)
		return true;
		
	    }

	}
	
    return false;
}



template<class TReceiverCallback, class TSenderCallback>
void AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::distSendStart(ACE_CString& flowName, CORBA::ULong flowNumber)
{
    ACS_TRACE("BulkDataDistributer<>::distSendStart");
    // call start on all the receivers.

    Sender_Map_Iterator iterator (senderMap_m);
    Sender_Map_Entry *entry = 0;
    ACE_CString recvName = "";

    try
	{
	for (;iterator.next (entry) !=  0;iterator.advance ())
	    {
	    int r;// = (entry->int_id_).first().mutex->tryacquire();
	    AcsBulkdata::ACS_Read_Guard<ACE_RW_Thread_Mutex> g(*((entry->int_id_).first().mutex), r);
	    if (r==-1)
		{
		continue;
		}     

            // checks if the handler referenced by handle is registered inside the ACE Reactor
	    ACE_HANDLE handle = (entry->int_id_).second()->findHandle(flowName);
	    if(handle == -1)
		{
		continue;
		}

	    AVStreams::flowSpec locSpec(1);
	    locSpec.length(1);
	    locSpec[0] = CORBA::string_dup( (entry->int_id_).second()->getFlowSpec(flowName));
	
	    recvName = entry->ext_id_;

	    CORBA::Boolean avail = isFlowReceiverAvailable(recvName, flowNumber);
	    if (!avail)
		avail = getFlowReceiverStatus(recvName, flowNumber);

	    if(avail)
		(entry->int_id_).second()->getStreamCtrl()->start(locSpec);
	    }
	}
    catch(CORBA::Exception &ex)
	{
	ACE_PRINT_EXCEPTION(ex,"BulkDataDistributer::distSendStart");
	(entry->int_id_).second()->disconnectPeer();
	BulkDataSender<TSenderCallback> *locSender_p = (entry->int_id_).second();
	if (locSender_p != 0)
	    delete locSender_p;

	CORBA::release((entry->int_id_).first().receiver);
	senderMap_m.unbind(recvName);
	ACS_SHORT_LOG((LM_WARNING,"BulkDataDistributer<>::distSendStart - Receiver %s removed from Distributor",recvName.c_str()));
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributer<>::distSendStart Unknown exception"));
	(entry->int_id_).second()->disconnectPeer();
	BulkDataSender<TSenderCallback> *locSender_p = (entry->int_id_).second();
	if (locSender_p != 0)
	    delete locSender_p;

	CORBA::release((entry->int_id_).first().receiver);

	senderMap_m.unbind(recvName);
	ACS_SHORT_LOG((LM_WARNING,"BulkDataDistributer<>::distSendStart - Receiver %s removed from Distributor",recvName.c_str()));
	}
}


template<class TReceiverCallback, class TSenderCallback>
int AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::distSendDataHsk(ACE_CString& flowName,ACE_Message_Block * frame_p,CORBA::ULong flowNumber)
{
    ACS_TRACE("BulkDataDistributer<>::distSendDataHsk");

    int res = -1;

    // call send_frame on all the receivers.
    Sender_Map_Iterator iterator (senderMap_m);
    Sender_Map_Entry *entry = 0;
    ACE_CString recvName = "";

    try
	{
	for (;iterator.next (entry) !=  0;iterator.advance ())
	    {
	    //printf("-> distSendDataHsk w m %s\n", entry->ext_id_.c_str());
	    int r;
	    AcsBulkdata::ACS_Read_Guard<ACE_RW_Thread_Mutex> g(*((entry->int_id_).first().mutex), r);
	    //printf("<- distSendDataHsk w m %s %d\n", entry->ext_id_.c_str(), r);
	    if (r==-1)
		{
		continue;
		}

            // checks if the handler referenced by handle is registered inside the ACE Reactor
	    ACE_HANDLE handle = (entry->int_id_).second()->findHandle(flowName);
	    if(handle == -1)
		{
		continue;
		}

	    TAO_AV_Protocol_Object *dp_p = 0;
	    (entry->int_id_).second()->getFlowProtocol(flowName, dp_p);

	    recvName = entry->ext_id_;
	    CORBA::Boolean avail = isFlowReceiverAvailable(recvName, flowNumber);
	    if(avail)
		{
		res = dp_p->send_frame(frame_p);
		if(res < 0)
		    {
		    ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributer<>::distSendDataHsk send frame error"));
		    }
		}	
	    }
    
	}
    catch(CORBA::Exception &ex)
	{
	ACE_PRINT_EXCEPTION(ex,"BulkDataDistributer::distSendDataHsk");
	(entry->int_id_).second()->disconnectPeer();
	BulkDataSender<TSenderCallback> *locSender_p = (entry->int_id_).second();
	if (locSender_p != 0)
	    delete locSender_p;

	CORBA::release((entry->int_id_).first().receiver);
	senderMap_m.unbind(recvName);
	ACS_SHORT_LOG((LM_WARNING,"BulkDataDistributer<>::distSendDataHsk - Receiver %s removed from Distributor",recvName.c_str()));
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributer<>::distSendDataHsk Unknown exception"));
	(entry->int_id_).second()->disconnectPeer();
	BulkDataSender<TSenderCallback> *locSender_p = (entry->int_id_).second();
	if (locSender_p != 0)
	    delete locSender_p;

	CORBA::release((entry->int_id_).first().receiver);
	senderMap_m.unbind(recvName);
	ACS_SHORT_LOG((LM_WARNING,"BulkDataDistributer<>::distSendDataHsk - Receiver %s removed from Distributor",recvName.c_str()));
	}	
    return res;
}


template<class TReceiverCallback, class TSenderCallback>
int AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::distSendData(ACE_CString& flowName,ACE_Message_Block * frame_p, CORBA::ULong flowNumber)
{
    ACS_TRACE("BulkDataDistributer<>::distSendData");

    int res = -1;

    // call send_frame on all the receivers.
    Sender_Map_Iterator iterator (senderMap_m);
    Sender_Map_Entry *entry = 0;
    ACE_CString recvName = "";
    CORBA::ULong currRecvNo, currFlowPos;

    TAO_AV_frame_info frame_info;
    ACE_Time_Value timeout;
    timeout.set(30L, 0L); // sender timeout set to 30s

    try
	{
	for (;iterator.next (entry) !=  0;iterator.advance ())
	    {
	    //printf("-> distSendData w m %s\n", entry->ext_id_.c_str());
	    int r;
	    AcsBulkdata::ACS_Read_Guard<ACE_RW_Thread_Mutex> g(*((entry->int_id_).first().mutex), r);
	    //printf("<- distSendData w m %s %d\n", entry->ext_id_.c_str(), r);
	    if (r==-1)
		{
		//printf("next item please \n");
		ACE_Thread::yield();
		continue;
		}
	    //ACE_Guard<ACE_Recursive_Thread_Mutex> guard(*(entry->int_id_).first().mutex);
	    //printf("<- w m %s\n", entry->ext_id_.c_str());

            // checks if the handler referenced by handle is registered inside the ACE Reactor
	    ACE_HANDLE handle = (entry->int_id_).second()->findHandle(flowName);
	    if(handle == -1)
		{
		continue;
		}

	    TAO_AV_Protocol_Object *dp_p = 0;
	    (entry->int_id_).second()->getFlowProtocol(flowName, dp_p);
	
	    recvName = entry->ext_id_;
	    CORBA::Boolean avail = isFlowReceiverAvailable(recvName, flowNumber);
	    if(avail)
		{		
		res = dp_p->send_frame(frame_p, &frame_info, &timeout);
		if(res < 0)
		    {
		    ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributer<>::distSendData send frame error"));

		    if(errno == ETIME)
			{
			ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributer<>::distSendData send frame timeout expired"));
			}
		    
		    if ( recvStatusMap_m.find(recvName,currRecvNo) != 0 )
			{
			ACS_SHORT_LOG((LM_INFO,"BulkDataDistributer<>::distSendData - error in getting currRecvNo"));
			ACSBulkDataError::AVPaceDataErrorExImpl err = ACSBulkDataError::AVPaceDataErrorExImpl(__FILE__,__LINE__,"BulkDataDistributer<>::distSendData");
			throw err;
			}
		    
		    currFlowPos = currRecvNo + flowNumber;
		    flowsStatusMap_m.rebind(currFlowPos,FLOW_NOT_AVAILABLE);

		    /*(entry->int_id_).second()->disconnectPeer();
		      BulkDataSender<TSenderCallback> *locSender_p = (entry->int_id_).second();
		      if (locSender_p != 0)
		      delete locSender_p;

		      CORBA::release((entry->int_id_).first().receiver);

		      senderMap_m.unbind(recvName);
		      iterator--;
		      recvStatusMap_m.unbind(recvName);
		      getFlowReceiverStatus(recvName,flowNumber);*/
		    }
		}	
	    }
	}
    catch(CORBA::Exception &ex)
	{
	ACE_PRINT_EXCEPTION(ex,"BulkDataDistributer::distSendData");

	if ( recvStatusMap_m.find(recvName,currRecvNo) != 0 )
	    {
	    ACS_SHORT_LOG((LM_INFO,"BulkDataDistributer<>::distSendData - error in getting currRecvNo"));
	    ACSBulkDataError::AVPaceDataErrorExImpl err = ACSBulkDataError::AVPaceDataErrorExImpl(__FILE__,__LINE__,"BulkDataDistributer<>::distSendData");
	    throw err;
	    }

	currFlowPos = currRecvNo + flowNumber;
	flowsStatusMap_m.rebind(currFlowPos,FLOW_NOT_AVAILABLE);

	/*(entry->int_id_).second()->disconnectPeer();
	  BulkDataSender<TSenderCallback> *locSender_p = (entry->int_id_).second();
	  if (locSender_p != 0)
	  delete locSender_p;

	  CORBA::release((entry->int_id_).first().receiver);
	  senderMap_m.unbind(recvName);
	  ACS_SHORT_LOG((LM_WARNING,"BulkDataDistributer<>::distSendData - Receiver %s removed from Distributor",recvName.c_str()));*/
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributer<>::distSendData Unknown exception"));

	if ( recvStatusMap_m.find(recvName,currRecvNo) != 0 )
	    {
	    ACS_SHORT_LOG((LM_INFO,"BulkDataDistributer<>::distSendData - error in getting currRecvNo"));
	    ACSBulkDataError::AVPaceDataErrorExImpl err = ACSBulkDataError::AVPaceDataErrorExImpl(__FILE__,__LINE__,"BulkDataDistributer<>::distSendData");
	    throw err;
	    }

	currFlowPos = currRecvNo + flowNumber;
	flowsStatusMap_m.rebind(currFlowPos,FLOW_NOT_AVAILABLE);

	/*(entry->int_id_).second()->disconnectPeer();
	  BulkDataSender<TSenderCallback> *locSender_p = (entry->int_id_).second();
	  if (locSender_p != 0)
	  delete locSender_p;

	  CORBA::release((entry->int_id_).first().receiver);
	  senderMap_m.unbind(recvName);
	  ACS_SHORT_LOG((LM_WARNING,"BulkDataDistributer<>::distSendData - Receiver %s removed from Distributor",recvName.c_str()));*/
	}	
    
    return res;
}



template<class TReceiverCallback, class TSenderCallback>
CORBA::Boolean AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::distSendStopTimeout(ACE_CString& flowName, CORBA::ULong flowNumber)
{
    ACS_TRACE("BulkDataDistributer<>::distSendStopTimeout");

// call stop on all the receivers.

    CORBA::Boolean timeout = true; 

    Sender_Map_Iterator iterator (senderMap_m);
    Sender_Map_Entry *entry = 0;
    CORBA::ULong currRecvNo, currFlowPos;

    for (;iterator.next (entry) !=  0;iterator.advance ())
	{    
	int r;// = (entry->int_id_).first().mutex->tryacquire_read();
	AcsBulkdata::ACS_Read_Guard<ACE_RW_Thread_Mutex> g(*((entry->int_id_).first().mutex), r);
	//printf("<- distSendStopTimeout w m %s %d\n", entry->ext_id_.c_str(), r);
	if (r==-1)
	    continue;

        // checks if the handler referenced by handle is registered inside the ACE Reactor
	ACE_HANDLE handle = (entry->int_id_).second()->findHandle(flowName);
	if(handle == -1)
	    {
	    continue;
	    }

	AVStreams::flowSpec locSpec(1);
	locSpec.length(1);
	locSpec[0] = CORBA::string_dup( (entry->int_id_).second()->getFlowSpec(flowName));
    
	ACE_CString recvName = entry->ext_id_;
	CORBA::Boolean avail = isFlowReceiverAvailable(recvName, flowNumber);
	if(avail)
	    {	
	    try 
		{
		(entry->int_id_).second()->getStreamCtrl()->stop(locSpec);
		}
	    catch(CORBA::TIMEOUT & ex)
		{
		//ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributer::distSendStopTimeout CORBA::TIMEOUT exception"));
		//ACE_PRINT_EXCEPTION(ex,"BulkDataDistributer::distSendStopTimeout");
		//senderMap_m.unbind(recvName);
		//ACS_SHORT_LOG((LM_WARNING,"BulkDataDistributer<>::distSendStopTimeout - Receiver %s removed from Distributor",recvName.c_str()));
		getFlowReceiverStatus(recvName, flowNumber);
		timeout = true;
		}
	    catch(CORBA::SystemException &ex)
		{
		if ( recvStatusMap_m.find(recvName,currRecvNo) != 0 )
		    {
		    ACS_SHORT_LOG((LM_INFO,"BulkDataDistributer<>::distSendStopTimeout - error in getting currRecvNo"));
		    ACSBulkDataError::AVStopSendErrorExImpl err = ACSBulkDataError::AVStopSendErrorExImpl(__FILE__,__LINE__,"BulkDataDistributer<>::distSendStopTimeout");
		    throw err;
		    }

		currFlowPos = currRecvNo + flowNumber;
		flowsStatusMap_m.rebind(currFlowPos,FLOW_NOT_AVAILABLE);

		/*(entry->int_id_).second()->disconnectPeer();
		  BulkDataSender<TSenderCallback> *locSender_p = (entry->int_id_).second();
		  if (locSender_p != 0)
		  delete locSender_p;

		  CORBA::release((entry->int_id_).first().receiver);
		  senderMap_m.unbind(recvName);
		  iterator--;
		  recvStatusMap_m.unbind(recvName);*/

		ACSErrTypeCommon::CORBAProblemExImpl err = ACSErrTypeCommon::CORBAProblemExImpl(__FILE__,__LINE__,"BulkDataDistributer::distSendStopTimeout");
		err.setMinor(ex.minor());
		err.setCompletionStatus(ex.completed());
		err.setInfo(ex._info().c_str());
		err.log();
		}
	    catch(CORBA::Exception &ex)
		{
		ACE_PRINT_EXCEPTION(ex,"BulkDataDistributer::distSendStopTimeout");

		if ( recvStatusMap_m.find(recvName,currRecvNo) != 0 )
		    {
		    ACS_SHORT_LOG((LM_INFO,"BulkDataDistributer<>::distSendStopTimeout - error in getting currRecvNo"));
		    ACSBulkDataError::AVStopSendErrorExImpl err = ACSBulkDataError::AVStopSendErrorExImpl(__FILE__,__LINE__,"BulkDataDistributer<>::distSendStopTimeout");
		    throw err;
		    }

		currFlowPos = currRecvNo + flowNumber;
		flowsStatusMap_m.rebind(currFlowPos,FLOW_NOT_AVAILABLE);

		/*(entry->int_id_).second()->disconnectPeer();
		  BulkDataSender<TSenderCallback> *locSender_p = (entry->int_id_).second();
		  if (locSender_p != 0)
		  delete locSender_p;

		  CORBA::release((entry->int_id_).first().receiver);
		  senderMap_m.unbind(recvName);
		  ACS_SHORT_LOG((LM_WARNING,"BulkDataDistributer<>::distSendStopTimeout - Receiver %s removed from Distributor",recvName.c_str()));*/
		}
	    catch(...)
		{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributer::distSendStopTimeout Unknown exception"));

		if ( recvStatusMap_m.find(recvName,currRecvNo) != 0 )
		    ACS_SHORT_LOG((LM_INFO,"BulkDataDistributer<>::distSendStopTimeout - ERROR in getting currRecvNo"));

		currFlowPos = currRecvNo + flowNumber;
		flowsStatusMap_m.rebind(currFlowPos,FLOW_NOT_AVAILABLE);

		/*(entry->int_id_).second()->disconnectPeer();
		  BulkDataSender<TSenderCallback> *locSender_p = (entry->int_id_).second();
		  if (locSender_p != 0)
		  delete locSender_p;

		  CORBA::release((entry->int_id_).first().receiver);
		  senderMap_m.unbind(recvName);
		  ACS_SHORT_LOG((LM_WARNING,"BulkDataDistributer<>::distSendStopTimeout - Receiver %s removed from Distributor",recvName.c_str()));*/
		}
	    }   
	}

    return timeout;
}


template<class TReceiverCallback, class TSenderCallback>
void AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::distSendStop(ACE_CString& flowName, CORBA::ULong flowNumber)
{
    ACS_TRACE("BulkDataDistributer<>::distSendStop");
    // call stop on all the receivers.

    Sender_Map_Iterator iterator (senderMap_m);
    Sender_Map_Entry *entry = 0;
    ACE_CString recvName = "";

    try
	{
	for (;iterator.next (entry) !=  0;iterator.advance ())
	    {
	    //printf("-> distSendStop w m %s\n", entry->ext_id_.c_str());
//		ACE_Guard<ACE_Recursive_Thread_Mutex> guard(*(entry->int_id_).first().mutex);
	    int r;// = (entry->int_id_).first().mutex->tryacquire_read();
	    AcsBulkdata::ACS_Read_Guard<ACE_RW_Thread_Mutex> g(*((entry->int_id_).first().mutex), r);
	    //printf("<- distSendStop w m %s %d\n", entry->ext_id_.c_str(), r);
	    if (r==-1)
		{
		continue;
		}

            // checks if the handler referenced by handle is registered inside the ACE Reactor
	    ACE_HANDLE handle = (entry->int_id_).second()->findHandle(flowName);
	    if(handle == -1)
		{
		continue;
		}

	    recvName = entry->ext_id_;
	    CORBA::Boolean avail = isFlowReceiverAvailable(recvName, flowNumber);
	    if(avail)
		{
		AVStreams::flowSpec locSpec(1);
		locSpec.length(1);
		locSpec[0] = CORBA::string_dup( (entry->int_id_).second()->getFlowSpec(flowName));
		(entry->int_id_).second()->getStreamCtrl()->stop(locSpec);
		}
	    }
	}
    catch(CORBA::Exception &ex)
	{
	ACE_PRINT_EXCEPTION(ex,"BulkDataDistributer::distSendStop");
	(entry->int_id_).second()->disconnectPeer();
	BulkDataSender<TSenderCallback> *locSender_p = (entry->int_id_).second();
	if (locSender_p != 0)
	    delete locSender_p;

	CORBA::release((entry->int_id_).first().receiver);
	senderMap_m.unbind(recvName);
	ACS_SHORT_LOG((LM_WARNING,"BulkDataDistributer<>::distSendStop - Receiver %s removed from Distributor",recvName.c_str()));
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributer<>::distSenStop Unknown exception"));
	(entry->int_id_).second()->disconnectPeer();
	BulkDataSender<TSenderCallback> *locSender_p = (entry->int_id_).second();
	if (locSender_p != 0)
	    delete locSender_p;

	CORBA::release((entry->int_id_).first().receiver);
	senderMap_m.unbind(recvName);
	ACS_SHORT_LOG((LM_WARNING,"BulkDataDistributer<>::distSendStop - Receiver %s removed from Distributor",recvName.c_str()));
	}	
}


template<class TReceiverCallback, class TSenderCallback>
CORBA::Boolean AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::getFlowReceiverStatus(const ACE_CString& receiverName, CORBA::ULong flowNumber)
{
    ACS_TRACE("BulkDataDistributer<>::getFlowReceiverStatus");

    CORBA::ULong currRecvNo, currFlowPos;

    if ( recvStatusMap_m.find(receiverName,currRecvNo) != 0 )
	return true;

    Flow_Status currStatus;

    currFlowPos = currRecvNo + flowNumber;

    if ( flowsStatusMap_m.find(currFlowPos,currStatus) != 0 )
	return true;
	
    bulkdata::BulkDataReceiver_var receiver = contSvc_p->maci::ContainerServices::getComponentNonSticky<bulkdata::BulkDataReceiver>(receiverName.c_str());
    if(!CORBA::is_nil(receiver.in()))
	{
	CompletionImpl comp = receiver->getCbStatus(flowNumber);

	/*
	  if(comp.getCode() == ACSBulkDataStatus::AVCbReady)
	  cout << "ACSBulkDataStatus::AVCbReady" << endl;
	  if(comp.getCode() == ACSBulkDataStatus::AVCbTimeout)
	  cout << "ACSBulkDataStatus::AVCbTimeout" << endl;
	  if(comp.getCode() == ACSBulkDataStatus::AVCbWorking)
	  cout << "ACSBulkDataStatus::AVCbWorking" << endl;
	  if(comp.getCode() == ACSBulkDataStatus::AVCbError)
	  cout << "ACSBulkDataStatus::AVCbError" << endl;
	  if(comp.getCode() == ACSBulkDataStatus::AVCbWorkingTimeout)
	  cout << "ACSBulkDataStatus::AVCbWorkingTimeout" << endl;
	  if(comp.getCode() == ACSBulkDataStatus::AVCbNotAvailable)
	  cout << "ACSBulkDataStatus::AVCbNotAvailable" << endl;
	*/


	if ((comp.getCode() == ACSBulkDataStatus::AVCbError) || 
	    (comp.getCode() == ACSBulkDataStatus::AVCbWorkingTimeout) ||
	    (comp.getCode() == ACSBulkDataStatus::AVCbWorking) )
	    {
	    flowsStatusMap_m.rebind(currFlowPos,FLOW_NOT_AVAILABLE);
	    return false;
	    }
	else
	    {
	    flowsStatusMap_m.rebind(currFlowPos,FLOW_AVAILABLE);
	    return true;
	    }
	}

    return true;    

}


template<class TReceiverCallback, class TSenderCallback>
CORBA::Boolean AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::isFlowReceiverAvailable(const ACE_CString& receiverName, CORBA::ULong flowNumber)
{
    ACS_TRACE("BulkDataDistributer<>::isFlowReceiverAvailable");

    CORBA::ULong currRecvNo, currFlowPos;

    if ( recvStatusMap_m.find(receiverName,currRecvNo) != 0 )
	return true;

    Flow_Status currStatus;

    currFlowPos = currRecvNo + flowNumber;

    if ( flowsStatusMap_m.find(currFlowPos,currStatus) != 0 )
	return true;

    if (currStatus == FLOW_AVAILABLE)
	{
	return true;
	}
    else
	{
	return false;
	}
}


template<class TReceiverCallback, class TSenderCallback>
void AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::subscribeNotification(ACS::CBvoid_ptr notifCb)
{
    distributerNotifCb_p = new BulkDataDistributerNotifCb<TReceiverCallback, TSenderCallback>(this);
    if(distributerNotifCb_p == 0)
	{
	ACSErrTypeCommon::CouldntCreateObjectExImpl ex = ACSErrTypeCommon::CouldntCreateObjectExImpl(__FILE__,__LINE__,"BulkDataDistributer::multiConnect");
	ACSBulkDataError::AVNotificationMechanismErrorExImpl err = ACSBulkDataError::AVNotificationMechanismErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributer<>::subscribeNotification");
	throw err;
	}

    try
	{
	locNotifCb_p = ACS::CBvoid::_duplicate(notifCb);
	
	ACS::CBvoid_var cb = distributerNotifCb_p->_this();

	Sender_Map *map = getSenderMap();

	Sender_Map_Iterator iterator(*map);
	Sender_Map_Entry *entry = 0;
	for (;iterator.next (entry) !=  0;iterator.advance ())
	    {
	    ((entry->int_id_).first().receiver)->subscribeNotification(cb.in());
	    }
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	ACSBulkDataError::AVNotificationMechanismErrorExImpl err = ACSBulkDataError::AVNotificationMechanismErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributer<>::subscribeNotification");
	throw err;
	}
    catch(ACSBulkDataError::AVNotificationMechanismErrorEx &ex)
	{
	ACSBulkDataError::AVNotificationMechanismErrorExImpl err = ACSBulkDataError::AVNotificationMechanismErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributer<>::subscribeNotification");
	throw err;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataDistributer::subscribeNotification");
	ACSBulkDataError::AVNotificationMechanismErrorExImpl err = ACSBulkDataError::AVNotificationMechanismErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributer::subscribeNotification");
	throw err;	
	}
}


template<class TReceiverCallback, class TSenderCallback>
void AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::notifySender(const ACSErr::Completion& comp)
{
    CompletionImpl complImp = comp;
    complImp.log(LM_DEBUG);

    ACS::CBDescOut desc;
    if(locNotifCb_p)
	{
	locNotifCb_p->done(comp,desc);
	}
    else
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributer<>::notifySender callback reference null"));
	ACSBulkDataError::AVCallbackErrorExImpl ex = ACSBulkDataError::AVCallbackErrorExImpl(__FILE__,__LINE__,"BulkDataDistributer::notifySender");
	ACSBulkDataError::AVNotificationMechanismErrorExImpl err = ACSBulkDataError::AVNotificationMechanismErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributer::notifySender");
	throw err;
	}
}

