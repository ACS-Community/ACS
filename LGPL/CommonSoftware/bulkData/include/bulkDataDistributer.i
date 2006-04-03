template<class TReceiverCallback, class TSenderCallback>
AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::BulkDataDistributer() : timeout_m(0),numberOfFlows(0),offset(100),contSvc_p(0)
{
    ACE_TRACE("BulkDataDistributer<>::BulkDataDistributer");

}


template<class TReceiverCallback, class TSenderCallback>
AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::~BulkDataDistributer()
{
    ACE_TRACE("BulkDataDistributer<>::~BulkDataDistributer");

    Sender_Map_Iterator iterator (senderMap_m);
    Sender_Map_Entry *entry = 0;
    for (;iterator.next (entry) !=  0;iterator.advance ())
	{	
	entry->int_id_->disconnectPeer();
	BulkDataSender<TSenderCallback> * locSender_p = entry->int_id_;
	if ( locSender_p != 0 )
	    delete locSender_p;
	}
}


template<class TReceiverCallback, class TSenderCallback>
void AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::multiConnect(bulkdata::BulkDataReceiverConfig *recvConfig_p, const char *fepsConfig, const ACE_CString& receiverName)
{
    ACS_TRACE("BulkDataDistributer<>::multiConnect");


    if ( isRecvConnected(receiverName) )
	{
	ACS_SHORT_LOG((LM_INFO, "BulkDataDistributerImpl::connect AVReceiverConfigErrorExImpl - ALREADY CONNECTED"));
	throw AVReceiverConfigErrorExImpl(__FILE__,__LINE__,"BulkDataDistributerImpl::connect");
	}


    sender_p = new BulkDataSender<TSenderCallback>;
    if(sender_p == 0)
	{
	ACS_SHORT_LOG((LM_INFO, "BulkDataDistributer::multiconnect sender NULL"));
	}

    sender_p->initialize();

    sender_p->createMultipleFlows(fepsConfig);
  
    sender_p->connectToPeer(recvConfig_p);

    senderMap_m.bind(receiverName,sender_p);


    recvStatusMap_m.bind(receiverName,offset);

    vector<string> flowNames = sender_p->getFlowNames();

    numberOfFlows = flowNames.size();

    vector<string>::size_type flwIdx;
    for (flwIdx = 0; flwIdx < flowNames.size(); ++flwIdx)
	{
	CORBA::ULong currVal = (CORBA::ULong)flwIdx + offset + 1;

	flowsStatusMap_m.bind(currVal,FLOW_AVAILABLE);
	}

    offset += 100;
}


template<class TReceiverCallback, class TSenderCallback>
void AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::multiDisconnect(const ACE_CString& receiverName)
{
    ACS_TRACE("BulkDataDistributer<>::multiDisconnect");

    BulkDataSender<TSenderCallback> * locSender_p;

    if ( senderMap_m.find(receiverName,locSender_p) == 0 )
	{
	locSender_p->disconnectPeer();
	delete locSender_p;

	senderMap_m.unbind(receiverName);

	}

}


template<class TReceiverCallback, class TSenderCallback>
bool AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::isRecvConnected(const ACE_CString& receiverName)
{
    ACS_TRACE("BulkDataDistributer<>::isRecvConnected");

    if ( senderMap_m.find(receiverName) == 0 )
	return true;

    return false;
 
}



template<class TReceiverCallback, class TSenderCallback>
void AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::distSendStart(ACE_CString& flowName, CORBA::ULong flowNumber)
{
    // ACS_TRACE("BulkDataDistributer<>::distSendStart");

    // call start on all the receivers.
    Sender_Map_Iterator iterator (senderMap_m);
    Sender_Map_Entry *entry = 0;


    for (;iterator.next (entry) !=  0;iterator.advance ())
	{

	AVStreams::flowSpec locSpec(1);
	locSpec.length(1);
	locSpec[0] = CORBA::string_dup(entry->int_id_->getFlowSpec(flowName));
	
	ACE_CString recvName = entry->ext_id_;
	CORBA::Boolean avail = isFlowReceiverAvailable(recvName, flowNumber);
	if (!avail)
	    avail = getFlowReceiverStatus(recvName, flowNumber);

	if(avail)
	    entry->int_id_->getStreamCtrl()->start(locSpec);

	}
}


template<class TReceiverCallback, class TSenderCallback>
int AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::distSendDataHsk(ACE_CString& flowName,ACE_Message_Block * frame_p,CORBA::ULong flowNumber)
{
//    ACS_TRACE("BulkDataDistributer<>::distSendDataHsk");

    int res = -1;

    // call send_frame on all the receivers.
    Sender_Map_Iterator iterator (senderMap_m);
    Sender_Map_Entry *entry = 0;

    for (;iterator.next (entry) !=  0;iterator.advance ())
	{

	TAO_AV_Protocol_Object *dp_p = 0;
	entry->int_id_->getFlowProtocol(flowName, dp_p);

	ACE_CString recvName = entry->ext_id_;
	CORBA::Boolean avail = isFlowReceiverAvailable(recvName, flowNumber);
	if(avail)
	    {
	    res = dp_p->send_frame(frame_p);
	    if(res < 0)
		{
		ACS_SHORT_LOG((LM_INFO,"BulkDataDistributer<>::distSendDataHsk send frame error"));
		}
	    }
	
	}
    
    return res;
}


template<class TReceiverCallback, class TSenderCallback>
int AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::distSendData(ACE_CString& flowName,ACE_Message_Block * frame_p, CORBA::ULong flowNumber)
{
//    ACS_TRACE("BulkDataDistributer<>::distSendData");

    int res = -1;

    // call send_frame on all the receivers.
    Sender_Map_Iterator iterator (senderMap_m);
    Sender_Map_Entry *entry = 0;

    
    for (;iterator.next (entry) !=  0;iterator.advance ())
	{
	
	TAO_AV_Protocol_Object *dp_p = 0;
	entry->int_id_->getFlowProtocol(flowName, dp_p);
	
	ACE_CString recvName = entry->ext_id_;
	CORBA::Boolean avail = isFlowReceiverAvailable(recvName, flowNumber);
	if(avail)
	    {
	    res = dp_p->send_frame(frame_p);
	    if(res < 0)
		{
		ACS_SHORT_LOG((LM_INFO,"BulkDataDistributer<>::distSendData send frame error"));
		getFlowReceiverStatus(recvName,flowNumber);
		}
	    }
	
	}
    
    return res;
}



template<class TReceiverCallback, class TSenderCallback>
CORBA::Boolean AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::distSendStopTimeout(ACE_CString& flowName, CORBA::ULong flowNumber)
{
// ACS_TRACE("BulkDataDistributer<>::distSendStopTimeout");

// call stop on all the receivers.

    CORBA::Boolean timeout = true; 

    Sender_Map_Iterator iterator (senderMap_m);
    Sender_Map_Entry *entry = 0;
    for (;iterator.next (entry) !=  0;iterator.advance ())
	{
    
	AVStreams::flowSpec locSpec(1);
	locSpec.length(1);
	locSpec[0] = CORBA::string_dup(entry->int_id_->getFlowSpec(flowName));
    
	ACE_CString recvName = entry->ext_id_;
	CORBA::Boolean avail = isFlowReceiverAvailable(recvName, flowNumber);
	if(avail)
	    {	
	    try 
		{
		if (timeout_m == 0)  timeout_m = 10000000; // very long timeout to avoid timeout
		acsQoS::Timeout tim(timeout_m);
		entry->int_id_->getStreamCtrl()->stop(locSpec);
		}
	    catch(CORBA::TIMEOUT & ex)
		{
		ACS_SHORT_LOG((LM_INFO,"BulkDataDistributer::distSendStopTimeout CORBA::TIMEOUT exception catched!"));
		getFlowReceiverStatus(recvName, flowNumber);
		timeout = false;
		}
	    }
   
	}

    return timeout;
}


template<class TReceiverCallback, class TSenderCallback>
void AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::distSendStop(ACE_CString& flowName, CORBA::ULong flowNumber)
{
    // ACS_TRACE("BulkDataDistributer<>::distSendStop");

    // call stop on all the receivers.

    Sender_Map_Iterator iterator (senderMap_m);
    Sender_Map_Entry *entry = 0;

    for (;iterator.next (entry) !=  0;iterator.advance ())
	{


	ACE_CString recvName = entry->ext_id_;
	CORBA::Boolean avail = isFlowReceiverAvailable(recvName, flowNumber);
	if(avail)
	    {
	    AVStreams::flowSpec locSpec(1);
	    locSpec.length(1);
	    locSpec[0] = CORBA::string_dup(entry->int_id_->getFlowSpec(flowName));
	    entry->int_id_->getStreamCtrl()->stop(locSpec);
	    }

	}

}


template<class TReceiverCallback, class TSenderCallback>
CORBA::Boolean AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::getFlowReceiverStatus(const ACE_CString& receiverName, CORBA::ULong flowNumber)
{
    // ACS_TRACE("BulkDataDistributer<>::getFlowReceiverStatus");

    CORBA::ULong currRecvNo, currFlowPos;

    if ( recvStatusMap_m.find(receiverName,currRecvNo) != 0 )
	return true;

    Flow_Status currStatus;

    currFlowPos = currRecvNo + flowNumber;

    if ( flowsStatusMap_m.find(currFlowPos,currStatus) != 0 )
	return true;
	
    bulkdata::BulkDataReceiver_var receiver = contSvc_p->maci::ContainerServices::getComponent<bulkdata::BulkDataReceiver>(receiverName.c_str());
    if(!CORBA::is_nil(receiver.in()))
	{
	CompletionImpl comp = receiver->getCbStatus(flowNumber);
	    
	if ( (comp.getCode() == ACSBulkDataStatus::AVCbWorkingTimeout) ||
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
    // ACS_TRACE("BulkDataDistributer<>::isFlowReceiverAvailable");

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
