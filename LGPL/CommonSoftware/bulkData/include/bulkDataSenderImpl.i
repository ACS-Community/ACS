template<class TSenderCallback>
BulkDataSenderImpl<TSenderCallback>::BulkDataSenderImpl(const ACE_CString& name,ContainerServices* containerServices) :
    CharacteristicComponentImpl(name,containerServices)
{
    ACS_TRACE("BulkDataSenderImpl::BulkDataSenderImpl");

    containerServices_p = containerServices;

    receiverObj_m = 0;
}


template<class TSenderCallback>
BulkDataSenderImpl<TSenderCallback>::~BulkDataSenderImpl()
{
    ACS_TRACE("BulkDataSenderImpl::~BulkDataSenderImpl");

    CORBA::release(receiverObj_m);
}


template<class TSenderCallback>
void BulkDataSenderImpl<TSenderCallback>::cleanUp()
{
    ACS_TRACE("BulkDataSenderImpl::cleanUp");
}



template<class TSenderCallback>
void BulkDataSenderImpl<TSenderCallback>::connect(bulkdata::BulkDataReceiver_ptr receiverObj_p)
    throw (CORBA::SystemException, AVConnectErrorEx)
{
    ACS_TRACE("BulkDataSenderImpl::connect");

    receiverObj_m = bulkdata::BulkDataReceiver::_duplicate(receiverObj_p);

    char buf[BUFSIZ];

    try
	{

	CDB::DAL_ptr dal_p = containerServices_p->getCDB();
	if (CORBA::is_nil (dal_p))
	    {
	    ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::connect dal_p nil"));
	    AVStreamEndpointErrorExImpl err = AVStreamEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSenderImpl::connect");
	    throw err.getAVStreamEndpointErrorEx();
	    }

	ACE_CString CDBpath="alma/";
	CDBpath += name();

	CDB::DAO_ptr dao_p = dal_p->get_DAO_Servant(CDBpath.c_str());
	if (CORBA::is_nil (dao_p))
	    {
	    ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::connect dao_p nil"));
	    AVStreamEndpointErrorExImpl err = AVStreamEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSenderImpl::connect");
	    throw err.getAVStreamEndpointErrorEx();
	    }

	ACE_OS::strcpy(buf,dao_p->get_string("sender_protocols"));
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_WARNING,"BulkDataSenderImpl::connect CDB failure."));
	AVConnectErrorExImpl err = AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataSenderImpl::connect");
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
	    ACS_SHORT_LOG((LM_INFO, "BulkDataSenderImpl::connect AVReceiverConfigErrorExImpl - receiverConfig NULL"));
	    throw AVReceiverConfigErrorExImpl(__FILE__,__LINE__,"BulkDataSenderImpl::connect");
	    }

	sender.connectToPeer(receiverConfig);

	bulkdata::BulkDataReceiverDistr_var distrObj_p = bulkdata::BulkDataReceiverDistr::_narrow(receiverObj_p);
	if(!CORBA::is_nil(distrObj_p.in()))
	    {
	    const bulkdata::BulkDataReceiverConfig &rcvConf = *receiverConfig;
	    distrObj_p->setReceiver(rcvConf);
	    }

	}

    catch (AVInitErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO, "BulkDataSenderImpl::connect AVInitErrorExImpl exception catched !"));
	AVConnectErrorExImpl err = AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::connect");
	throw err.getAVConnectErrorEx();
	}
    catch (AVStreamEndpointErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO, "BulkDataSenderImpl::connect AVStreamEndpointErrorExImpl exception catched !"));
	AVConnectErrorExImpl err = AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::connect");
	throw err.getAVConnectErrorEx();
	}
    catch (AVFlowEndpointErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::connect AVFlowEndpointErrorExImpl exception catched !"));
	AVConnectErrorExImpl err = AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::connect");
	throw err.getAVConnectErrorEx();
	}
    catch (AVOpenReceiverErrorEx & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::connect AVOpenReceiverErrorEx exception catched !"));
	AVConnectErrorExImpl err = AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::connect");
	throw err.getAVConnectErrorEx();
	}
    catch (AVReceiverConfigErrorEx & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::init AVReceiverConfigErrorEx exception catched !"));
	AVConnectErrorExImpl err = AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::connect");
	throw err.getAVConnectErrorEx();
	}
    catch (AVReceiverConfigErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::init AVReceiverConfigErrorExImpl exception catched !"));
	AVConnectErrorExImpl err = AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::connect");
	throw err.getAVConnectErrorEx();
	}
    catch (AVStreamBindErrorExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::init AVStreamBindErrorExImpl exception catched !"));
	AVConnectErrorExImpl err = AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::connect");
	throw err.getAVConnectErrorEx();
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::connect UNKNOWN exception"));
	AVConnectErrorExImpl err = AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataSenderImpl::connect");
	throw err.getAVConnectErrorEx();
	}
}


template<class TSenderCallback>
void BulkDataSenderImpl<TSenderCallback>::disconnect()
    throw (CORBA::SystemException, AVDisconnectErrorEx)
{
    ACS_TRACE("BulkDataSenderImpl::disconnect");

    CORBA::Boolean loop = true;

    try
	{
	if(receiverObj_m != 0)
	    {
	    vector<string> vec = getSender()->getFlowNames();
	    for(CORBA::ULong i = 0; i < vec.size(); i++)
		{
		loop = true;
		while(loop)
		    {
		    CompletionImpl comp = receiverObj_m->getCbStatus(i+1);
		    if ((comp.getCode() == ACSBulkDataStatus::AVCbReady) || 
			(comp.getCode() == ACSBulkDataStatus::AVCbTimeout))
			{
			loop = false;
			}
		    ACE_OS::sleep(1);
		    }
		}
	    }

	    sender.disconnectPeer();
	}
    catch (AVInvalidFlowNumberExImpl & ex)
	{   
	ACS_SHORT_LOG((LM_INFO,"BulkDataSenderImpl::disconnect AVInvalidFlowNumberExImpl exception catched !"));

	sender.disconnectPeer();

	AVDisconnectErrorExImpl err = AVDisconnectErrorExImpl(__FILE__,__LINE__,"BulkDataSenderImpl::disconnect");
	throw err.getAVDisconnectErrorEx();
	}
    catch(...)
	{
	AVDisconnectErrorExImpl err = AVDisconnectErrorExImpl(__FILE__,__LINE__,"BulkDataSenderImpl::disconnect");
	throw err.getAVDisconnectErrorEx();
	}
}
