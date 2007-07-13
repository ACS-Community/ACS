template<class TReceiverCallback, class TSenderCallback>
AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::BulkDataDistributer() : sender_p(0),timeout_m(0),numberOfFlows(0),offset(100),contSvc_p(0)
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
	(entry->int_id_).second()->disconnectPeer();
	BulkDataSender<TSenderCallback> *locSender_p = (entry->int_id_).second();
	if (locSender_p != 0)
	    delete locSender_p;

	CORBA::release((entry->int_id_).first()); 
	}
}


template<class TReceiverCallback, class TSenderCallback>
void AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::multiConnect(bulkdata::BulkDataReceiverConfig *recvConfig_p, const char *fepsConfig, const ACE_CString& receiverName)
    throw (AVConnectErrorExImpl)
{
    ACS_TRACE("BulkDataDistributer<>::multiConnect");

    try
	{
	if(isRecvConnected(receiverName))
	    {
	    ACS_SHORT_LOG((LM_WARNING,"BulkDataDistributer<>::multiConnect receiver %s already connected",receiverName.c_str()));
	    AVConnectErrorExImpl err = AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataDistributer<>::multiConnect");
	    throw err;
	    }

	sender_p = new BulkDataSender<TSenderCallback>;
	if(sender_p == 0)
	    {
	    ACS_SHORT_LOG((LM_ERROR, "BulkDataDistributer<>::multiConnect error creating sender"));
	    AVConnectErrorExImpl err = AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataDistributer<>::multiConnect");
	    throw err;
	    }

	sender_p->initialize();

	sender_p->createMultipleFlows(fepsConfig);
  
	sender_p->connectToPeer(recvConfig_p);

	bulkdata::BulkDataReceiver_var receiver_p = contSvc_p->maci::ContainerServices::getComponentNonSticky<bulkdata::BulkDataReceiver>(receiverName.c_str());
	if(CORBA::is_nil(receiver_p.in()))
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributer<>::multiConnect could not get receiver reference"));	
	    AVConnectErrorExImpl err = AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataDistributer<>::multiConnect");
	    throw err;
	    }

	Sender_Map_Pair pair;
	//pair.first(receiver_p.in());
	pair.first(bulkdata::BulkDataReceiver::_duplicate(receiver_p));
	pair.second(sender_p);

	//senderMap_m.bind(receiverName,sender_p);
	senderMap_m.bind(receiverName,pair);	

	recvStatusMap_m.bind(receiverName,offset);

	vector<string> flowNames = sender_p->getFlowNames();
	
	numberOfFlows = flowNames.size();

	vector<string>::size_type flwIdx;
	for (flwIdx = 0; flwIdx < numberOfFlows; ++flwIdx)
	    {
	    CORBA::ULong currVal = (CORBA::ULong)flwIdx + offset + 1;	    
	    flowsStatusMap_m.bind(currVal,FLOW_AVAILABLE);
	    }
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVConnectErrorExImpl err = AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributer<>::multiConnect");
	throw err;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataDistributer::multiConnect");
	AVConnectErrorExImpl err = AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributer<>::multiConnect");
	throw err;
	}

   offset += 100;
}


template<class TReceiverCallback, class TSenderCallback>
void AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::multiDisconnect(const ACE_CString& receiverName)
    throw (AVDisconnectErrorExImpl)
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
	    AVDisconnectErrorExImpl err = AVDisconnectErrorExImpl(__FILE__,__LINE__,"BulkDataDistributer<>::multiDisconnect");
	    throw err;
	    }
	else
	    {
	    locSender_p = pair.second();
	    bulkdata::BulkDataReceiver_var receiver = contSvc_p->maci::ContainerServices::getComponent<bulkdata::BulkDataReceiver>(receiverName.c_str());
	    if(CORBA::is_nil(receiver.in()))
		{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributer<>::multiDisconnect could not get receiver reference"));	
		AVDisconnectErrorExImpl err = AVDisconnectErrorExImpl(__FILE__,__LINE__,"BulkDataDistributer<>::multiDisconnect");
		throw err;
		}
	    else
		{
		vector<string> vec = locSender_p->getFlowNames();
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
		    catch(AVInvalidFlowNumberEx &ex)
			{
			AVDisconnectErrorExImpl err = AVDisconnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributer::multiDisconnect");
			throw err;
			}
		    catch(AVFlowEndpointErrorEx &ex)
			{
			AVDisconnectErrorExImpl err = AVDisconnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributer::multiDisconnect");
			throw err;
			}
		    catch(...)
			{
			ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataDistributer::multiDisconnect");
			AVDisconnectErrorExImpl err = AVDisconnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributer::multiDisconnect");
			throw err;
			}
		    }
		}
	
	    try
		{
		locSender_p->disconnectPeer();
		receiver->closeReceiver();
		delete locSender_p;

		Sender_Map_Pair pair;
		senderMap_m.find(receiverName,pair);
		CORBA::release(pair.first());

		senderMap_m.unbind(receiverName);
		}
	    catch(ACSErr::ACSbaseExImpl &ex)
		{
		AVDisconnectErrorExImpl err = AVDisconnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributer::multiDisconnect");
		throw err;
		}
	    catch(AVCloseReceiverErrorEx &ex)
		{
		AVDisconnectErrorExImpl err = AVDisconnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributer::multiDisconnect");
		throw err;
		}
	    catch(...)
		{
		ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataDistributer::multiDisconnect");
		AVDisconnectErrorExImpl err = AVDisconnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributer::multiDisconnect");
		throw err;
		}

	    }
	}
    catch(AVDisconnectErrorExImpl &ex)
	{
	AVDisconnectErrorExImpl err = AVDisconnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributer::multiDisconnect");
	throw err;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataDistributer::multiDisconnect");
	AVDisconnectErrorExImpl err = AVDisconnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataDistributer::multiDisconnect");
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
	locSpec[0] = CORBA::string_dup( (entry->int_id_).second()->getFlowSpec(flowName));
	
	ACE_CString recvName = entry->ext_id_;
	CORBA::Boolean avail = isFlowReceiverAvailable(recvName, flowNumber);
	if (!avail)
	    avail = getFlowReceiverStatus(recvName, flowNumber);

	if(avail)
	    (entry->int_id_).second()->getStreamCtrl()->start(locSpec);
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
	(entry->int_id_).second()->getFlowProtocol(flowName, dp_p);

	ACE_CString recvName = entry->ext_id_;
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
	(entry->int_id_).second()->getFlowProtocol(flowName, dp_p);
	
	ACE_CString recvName = entry->ext_id_;
	CORBA::Boolean avail = isFlowReceiverAvailable(recvName, flowNumber);
	if(avail)
	    {
	    res = dp_p->send_frame(frame_p);
	    if(res < 0)
		{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributer<>::distSendData send frame error"));
		CORBA::release((entry->int_id_).first()); 
		senderMap_m.unbind(recvName);
		iterator--;
		recvStatusMap_m.unbind(recvName);
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
		ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributer::distSendStopTimeout CORBA::TIMEOUT exception"));
		getFlowReceiverStatus(recvName, flowNumber);
		timeout = false;
		}
	    catch(CORBA::SystemException &ex)
		{
		CORBA::release((entry->int_id_).first()); 
		senderMap_m.unbind(recvName);
		iterator--;
		recvStatusMap_m.unbind(recvName);
		ACSErrTypeCommon::CORBAProblemExImpl err = ACSErrTypeCommon::CORBAProblemExImpl(__FILE__,__LINE__,"BulkDataDistributer::distSendStopTimeout");
		err.setMinor(ex.minor());
		err.setCompletionStatus(ex.completed());
		err.setInfo(ex._info().c_str());
		err.log();
		}
	    catch(...)
		{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributer::distSendStopTimeout UNKNOWN exception"));
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
	    locSpec[0] = CORBA::string_dup( (entry->int_id_).second()->getFlowSpec(flowName));
	    (entry->int_id_).second()->getStreamCtrl()->stop(locSpec);
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
