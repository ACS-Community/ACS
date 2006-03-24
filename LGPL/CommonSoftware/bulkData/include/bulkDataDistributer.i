template<class TReceiverCallback, class TSenderCallback>
AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::BulkDataDistributer()
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
void AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::distSendStart(ACE_CString& flowName)
{
    ACS_TRACE("BulkDataDistributer<>::distSendStart");

    // call start on all the receivers.
    Sender_Map_Iterator iterator (senderMap_m);
    Sender_Map_Entry *entry = 0;
    for (;iterator.next (entry) !=  0;iterator.advance ())
	{

	AVStreams::flowSpec locSpec(1);
	locSpec.length(1);
	locSpec[0] = CORBA::string_dup(entry->int_id_->getFlowSpec(flowName));
	
	entry->int_id_->getStreamCtrl()->start(locSpec);
	}
}


template<class TReceiverCallback, class TSenderCallback>
int AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::distSendData(ACE_CString& flowName,ACE_Message_Block * frame_p)
{
    ACS_TRACE("BulkDataDistributer<>::distSendData");

    int res = -1;

    // call send_frame on all the receivers.
    Sender_Map_Iterator iterator (senderMap_m);
    Sender_Map_Entry *entry = 0;
    for (;iterator.next (entry) !=  0;iterator.advance ())
	{

	TAO_AV_Protocol_Object *dp_p = 0;
	entry->int_id_->getFlowProtocol(flowName, dp_p);

	res = dp_p->send_frame(frame_p);
	if(res < 0)
	    {
	    ACS_SHORT_LOG((LM_INFO,"BulkDataDistributer<>::distSendData send frame error"));
	    }
	}

    return res;
}



template<class TReceiverCallback, class TSenderCallback>
void AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback>::distSendStop(ACE_CString& flowName)
{
    ACS_TRACE("BulkDataDistributer<>::distSendStop");

    // call stop on all the receivers.
    Sender_Map_Iterator iterator (senderMap_m);
    Sender_Map_Entry *entry = 0;
    for (;iterator.next (entry) !=  0;iterator.advance ())
	{

	AVStreams::flowSpec locSpec(1);
	locSpec.length(1);
	locSpec[0] = CORBA::string_dup(entry->int_id_->getFlowSpec(flowName));
	entry->int_id_->getStreamCtrl()->stop(locSpec);

	//ACE_CString prova = entry->ext_id_;
	//cout << "IIIIIIIIIIIIIIIIIIIIIIIIIIIIII: " << prova.c_str() << endl; 
	}
}

