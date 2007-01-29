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

    if (CORBA::is_nil(receiverObj_p))
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSenderImpl::connect receiver reference null"));
	AVConnectErrorExImpl err = AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataSenderImpl::connect");
	throw err.getAVConnectErrorEx();
	}
    receiverObj_m = bulkdata::BulkDataReceiver::_duplicate(receiverObj_p);

    char buf[BUFSIZ];

    CDB::DAL_ptr dal_p = containerServices_p->getCDB();
    if (CORBA::is_nil(dal_p))
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSenderImpl::connect error getting CDB reference"));
	AVConnectErrorExImpl err = AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataSenderImpl::connect");
	throw err.getAVConnectErrorEx();
	}

    ACE_CString CDBpath="alma/";
    CDBpath += name();

    CDB::DAO_ptr dao_p = dal_p->get_DAO_Servant(CDBpath.c_str());
    if (CORBA::is_nil(dao_p))
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSenderImpl::connect error getting DAO reference"));
	AVConnectErrorExImpl err = AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataSenderImpl::connect");
	throw err.getAVConnectErrorEx();
	}

    ACE_OS::strcpy(buf,dao_p->get_string("sender_protocols"));

    try
	{
	sender.initialize();

	sender.createMultipleFlows(buf);

	receiverObj_p->openReceiver();

	bulkdata::BulkDataReceiverConfig *receiverConfig = receiverObj_p->getReceiverConfig();

	sender.connectToPeer(receiverConfig);

	receiverObj_p->setRecvName(receiverObj_p->name());

	bulkdata::BulkDataReceiverDistr_var distrObj_p = bulkdata::BulkDataReceiverDistr::_narrow(receiverObj_p);
	if(!CORBA::is_nil(distrObj_p.in()))
	    {
	    const bulkdata::BulkDataReceiverConfig &rcvConf = *receiverConfig;
	    distrObj_p->setReceiver(rcvConf);
	    }
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVConnectErrorExImpl err = AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::connect");
	err.log(LM_DEBUG);
	throw err.getAVConnectErrorEx();
	}
    /*catch(CORBA::UserException &ex) //is the base of the remote exceptions
	{
	AVConnectErrorExImpl err = AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataSenderImpl::connect");
	throw err.getAVConnectErrorEx();
	}*/
    catch(AVOpenReceiverErrorEx &ex)
	{
	AVConnectErrorExImpl err = AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::connect");
	err.log(LM_DEBUG);
	throw err.getAVConnectErrorEx();
	}
    catch(AVReceiverConfigErrorEx &ex)
	{
	AVConnectErrorExImpl err = AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::connect");
	err.log(LM_DEBUG);
	throw err.getAVConnectErrorEx();
	}
    catch(AVFlowEndpointErrorEx &ex)
	{
	AVConnectErrorExImpl err = AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::connect");
	err.log(LM_DEBUG);
	throw err.getAVConnectErrorEx();
	}
    catch(AVSetReceiverNameErrorEx &ex)
	{
	AVConnectErrorExImpl err = AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::connect");
	err.log(LM_DEBUG);
	throw err.getAVConnectErrorEx();
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSenderImpl::connect UNKNOWN exception"));
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
	    }

	getSender()->disconnectPeer();
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVDisconnectErrorExImpl err = AVDisconnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::disconnectPeer");
	err.log(LM_DEBUG);
	throw err.getAVDisconnectErrorEx();
	}
    catch(AVInvalidFlowNumberEx &ex)
	{   
	getSender()->disconnectPeer();

	AVDisconnectErrorExImpl err = AVDisconnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSenderImpl::disconnect");
	err.log(LM_DEBUG);
	throw err.getAVDisconnectErrorEx();
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSenderImpl::disconnect UNKNOWN exception"));
	AVDisconnectErrorExImpl err = AVDisconnectErrorExImpl(__FILE__,__LINE__,"BulkDataSenderImpl::disconnect");
	throw err.getAVDisconnectErrorEx();
	}
}
