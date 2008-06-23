template<class TReceiverCallback>
AcsBulkdata::BulkDataReceiver<TReceiverCallback>::BulkDataReceiver() : closeReceiverFlag(false)
{
    ACE_TRACE("BulkDataReceiver<>::BulkDataReceiver");
  
    sepB_p = AVStreams::StreamEndPoint_B::_nil();

    sepRefCount_p = 0;

    locNotifCb_p = 0;
//    recvConfig_p->streamendpoint_B = 0;
//    recvConfig_p->fepsInfo = 0;

    recvConfig_p = new bulkdata::BulkDataReceiverConfig();
    if(recvConfig_p == 0)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::BulkDataReceiver error creating BulkDataReceiverConfig"));
	AVReceiverConfigErrorExImpl err = AVReceiverConfigErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::BulkDataReceiver");
	throw err;
	}
}


template<class TReceiverCallback>
AcsBulkdata::BulkDataReceiver<TReceiverCallback>::~BulkDataReceiver()
{
    ACE_TRACE("BulkDataReceiver<>::~BulkDataReceiver");

    try
	{
	if(closeReceiverFlag == false)
	    {
	    closeReceiver();
	    }

	CORBA::release(locNotifCb_p);
	}
    catch(AVCloseReceiverErrorExImpl &ex)
	{
	AVCloseReceiverErrorExImpl err = AVCloseReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::~BulkDataReceiver");
	err.log(LM_ERROR);
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataReceiver::~BulkDataReceiver");
	ex.log(LM_ERROR);
	}
}


template<class TReceiverCallback>
void AcsBulkdata::BulkDataReceiver<TReceiverCallback>::initialize()
    throw (AVInitErrorExImpl)
{
    ACE_TRACE("BulkDataReceiver<>::initialize");

    try
	{
	initPartB();
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVInitErrorExImpl err = AVInitErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::initialize");
	throw err;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataReceiver::initialize");
	AVInitErrorExImpl err = AVInitErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::initialize");
	throw err;
	}
}


template<class TReceiverCallback>
void AcsBulkdata::BulkDataReceiver<TReceiverCallback>::createSingleFlow()
    throw (AVStreamEndpointErrorExImpl, AVFlowEndpointErrorExImpl)
{
    ACE_TRACE("BulkDataReceiver<>::createSingleFlow");

    try
	{
	sepB_p = createSepB();

	ACE_CString flowName = "Flow1";

	AVStreams::protocolSpec defProt(1);
	defProt.length(1);
	defProt[0] = CORBA::string_dup("TCP");
  
	ACE_CString format = "UNS1:ftp";

	AVStreams::FlowConsumer_var fepObj_p = createFepConsumerB(flowName, defProt, format); 

	addFepToSep(sepB_p.in(), fepObj_p.in());

	fepsData.length(1);

	ACE_CString address = CORBA::string_dup("TCP");
	const char  * locEntry = createFlowSpec(flowName,address);
	fepsData[0] = CORBA::string_dup(locEntry);
	}
    catch(AVStreamEndpointErrorExImpl &ex)
	{
	AVStreamEndpointErrorExImpl err = AVStreamEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::createSingleFlow");
	throw err;
	}
    catch(AVFlowEndpointErrorExImpl &ex)
	{
	AVFlowEndpointErrorExImpl err = AVFlowEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::createSingleFlow");
	throw err;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataReceiver::createSingleFlow");
	AVStreamEndpointErrorExImpl err = AVStreamEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::createSingleFlow");
	throw err;
	}
}


template<class TReceiverCallback>
void AcsBulkdata::BulkDataReceiver<TReceiverCallback>::createMultipleFlows(const char *fepsConfig)
    throw (AVStreamEndpointErrorExImpl, AVInvalidFlowNumberExImpl, AVFlowEndpointErrorExImpl)
{
    ACE_TRACE("BulkDataReceiver<>::createMultipleFlows");

    bulkdata::Connection conn = checkFlowCallbacks();
    recvConfig_p->connectionState = conn;
    if(conn == bulkdata::CONNECTED) // state CONNECTED, nothing is created on the receiver side
	{
	return;
	}

    try
	{
	if(ACE_OS::strcmp(fepsConfig, "") == 0)
	    {
	    createSingleFlow();
	    return;
	    }

	sepB_p = createSepB();

	FepsCfgB localStruct;

	AVStreams::FlowConsumer_var fepObj_p;

	AVStreams::protocolSpec defProt(1);
	defProt.length(1);

	TAO_Tokenizer addressToken(fepsConfig, '/');
	int numOtherFeps = addressToken.num_tokens();
	if(numOtherFeps > 19)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::createMultipleFlows too many flows specified - maximum 19"));
	    AVInvalidFlowNumberExImpl err = AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataReceiver::createMultipleFlows");
	    throw err;	
	    }

	fepsData.length(numOtherFeps);

	char hostName[255];
	string strAddressToken;
	for(int j = 0; j < numOtherFeps; j++)
	    {
	    strAddressToken = addressToken[j];
	    if(strAddressToken.find("${HOST}",0) != string::npos)
		{
		if(strAddressToken.find(":",0) != string::npos)		  
		    {
		    ACE_OS::hostname(hostName, sizeof(hostName));
		    strAddressToken.replace(4,7,hostName);
		    }
		else
		    {
		    strAddressToken.replace(3,8,"");
		    }
		}

	    char tmp[255];
	    ACE_OS::sprintf(tmp, "Flow%d", j+1);
	    localStruct.fepsFlowname = tmp;
	    ACE_OS::sprintf(tmp, "UNS%d:ftp", j+1);
	    localStruct.fepsFormat = tmp;
	    defProt[0] = CORBA::string_dup(strAddressToken.c_str());
	    localStruct.fepsProtocol = CORBA::string_dup(strAddressToken.c_str()); 

	    fepObj_p = createFepConsumerB(localStruct.fepsFlowname, defProt, localStruct.fepsFormat); 
	    addFepToSep(sepB_p.in(), fepObj_p.in());

	    const char *locEntry = createFlowSpec(localStruct.fepsFlowname, localStruct.fepsProtocol);
	    fepsData[j] = CORBA::string_dup(locEntry);
	    }
	}
    catch(AVStreamEndpointErrorExImpl &ex)
	{
	AVStreamEndpointErrorExImpl err = AVStreamEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::createMultipleFlows");
	throw err;
	}
    catch(AVInvalidFlowNumberExImpl &ex)
	{
	AVInvalidFlowNumberExImpl err = AVInvalidFlowNumberExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::createMultipleFlows");
	throw err;
	}
    catch(AVFlowEndpointErrorExImpl &ex)
	{
	AVFlowEndpointErrorExImpl err = AVFlowEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::createMultipleFlows");
	throw err;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataReceiver::createMultipleFlows");
	AVStreamEndpointErrorExImpl err = AVStreamEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::createMultipleFlows");
	throw err;
	}
}


template<class TReceiverCallback>
bulkdata::BulkDataReceiverConfig * AcsBulkdata::BulkDataReceiver<TReceiverCallback>::getReceiverConfig()
    throw (AVReceiverConfigErrorExImpl)
{
    ACS_TRACE("BulkDataReceiver<>::getReceiverConfig");

    bulkdata::BulkDataReceiverConfig *recvConfigLoc_p = 0;
    try
	{
	recvConfigLoc_p = new bulkdata::BulkDataReceiverConfig;
	if(recvConfigLoc_p == 0)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::getReceiverConfig error creating BulkDataReceiverConfig"));
	    AVReceiverConfigErrorExImpl err = AVReceiverConfigErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::getReceiverConfig");
	    throw err;
	    }
      
	recvConfig_p->streamendpoint_B = getStreamEndPointB();
	if (CORBA::is_nil((recvConfig_p->streamendpoint_B).in()))
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::getReceiverConfig Stream Endpoint not initilaized"));
	    AVReceiverConfigErrorExImpl err = AVReceiverConfigErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::getReceiverConfig");
	    throw err;
	    }
	else
	    {
	    recvConfigLoc_p->streamendpoint_B = recvConfig_p->streamendpoint_B;
	    }

	if((getFepsConfig())->length() > 0)
	    {
	    recvConfig_p->fepsInfo = *(getFepsConfig());
	    recvConfigLoc_p->fepsInfo = recvConfig_p->fepsInfo;
	    }
	else
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::getReceiverConfig Flow Specifications empty"));
	    AVReceiverConfigErrorExImpl err = AVReceiverConfigErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::getReceiverConfig");
	    throw err;
	    }

	recvConfigLoc_p->connectionState = recvConfig_p->connectionState;
	}
    catch(AVReceiverConfigErrorExImpl &ex)
	{
	AVReceiverConfigErrorExImpl err = AVReceiverConfigErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::getReceiverConfig");
	throw err;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataReceiver::getReceiverConfig");
	AVReceiverConfigErrorExImpl err = AVReceiverConfigErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::getReceiverConfig");
	throw err;
	}
    
    return recvConfigLoc_p;
}


template<class TReceiverCallback>
void AcsBulkdata::BulkDataReceiver<TReceiverCallback>::getFlowCallback(ACE_CString &flowName, TReceiverCallback *&cb_p)
    throw (AVFlowEndpointErrorExImpl)
{
    ACS_TRACE("BulkDataReceiver<>::getFlowCallback");   

    try
	{
	BulkDataFlowConsumer<TReceiverCallback> *fep = 0;
	fepMap_m.find(flowName, fep);
	if(fep == 0)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::getFlowCallback Flow End Point null"));
	    AVFlowEndpointErrorExImpl err = AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
	    throw err;
	    }
	else
	    {
	    AVStreams::FlowEndPoint_ptr connFep = 0; 
	    connFep = fep->get_connected_fep();
	    if(connFep == 0)
		{
		//cb_p = 0;
		// To be verified what do to here
		ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::getFlowCallback Flow End Point null"));
		AVFlowEndpointErrorExImpl err = AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
		throw err;
		} 
	    else
		cb_p = fep->getBulkDataCallback();
	    /*if(cb_p == 0)
	      {
	      AVCallbackErrorExImpl err = AVCallbackErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
	      throw err;
	      }*/
	    }
	}
    catch(AVFlowEndpointErrorExImpl &ex)
	{
	AVFlowEndpointErrorExImpl err = AVFlowEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
	throw err;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
	AVFlowEndpointErrorExImpl err = AVFlowEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
	throw err;
	}
}


template<class TReceiverCallback>
void AcsBulkdata::BulkDataReceiver<TReceiverCallback>::getFlowCallback(CORBA::ULong flowNumber, TReceiverCallback *&cb_p)
    throw (AVInvalidFlowNumberExImpl, AVFlowEndpointErrorExImpl)
{
    ACS_TRACE("BulkDataReceiver<>::getFlowCallback");   

    try
	{
	BulkDataFlowConsumer<TReceiverCallback> *fep = 0;
	ACE_CString flowName;

	CORBA::ULong dim = fepsData.length();
	if(flowNumber < 1 || flowNumber > dim)
	    {
	    AVInvalidFlowNumberExImpl err = AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
	    throw err;
	    }
	flowNumber--;

	vector<string> vec = getFlowNames();
	flowName = vec[flowNumber].c_str();

	fepMap_m.find(flowName, fep);
	if(fep == 0)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::getFlowCallback Flow End Point null"));
	    AVFlowEndpointErrorExImpl err = AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
	    throw err;
	    }
	else
	    {
	    AVStreams::FlowEndPoint_ptr connFep = 0; 
	    connFep = fep->get_connected_fep();
	    if(connFep == 0)
		{
		//cb_p = 0;
		// To be verified what do to here
		ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::getFlowCallback Flow End Point null"));
		AVFlowEndpointErrorExImpl err = AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
		throw err;
		} 
	    else
		cb_p = fep->getBulkDataCallback();
	    /*if(cb_p == 0)
	      {
	      AVCallbackErrorExImpl err = AVCallbackErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
	      throw err;
	      }*/
	    }
	}	
    catch(AVInvalidFlowNumberExImpl &ex)
	{
	AVInvalidFlowNumberExImpl err = AVInvalidFlowNumberExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
	throw err;
	}
    catch(AVFlowEndpointErrorExImpl &ex)
	{
	AVFlowEndpointErrorExImpl err = AVFlowEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
	throw err;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
	AVFlowEndpointErrorExImpl err = AVFlowEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
	throw err;
	}
}


template<class TReceiverCallback>
void AcsBulkdata::BulkDataReceiver<TReceiverCallback>::closeReceiver()
    throw (AVCloseReceiverErrorExImpl)
{
    ACE_TRACE("BulkDataReceiver<>::closeReceiver");

    try
	{
	deleteAcceptor();
	deleteFepsB();
	deleteSepB();
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVCloseReceiverErrorExImpl err = AVCloseReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::closeReceiver");
	throw err;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataReceiver::closeReceiver");
	AVCloseReceiverErrorExImpl err = AVCloseReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::closeReceiver");
	throw err;
	}

    closeReceiverFlag = true;
}


template<class TReceiverCallback>
vector<string> AcsBulkdata::BulkDataReceiver<TReceiverCallback>::getFlowNames()
{
    ACE_TRACE("BulkDataReceiver<>::getFlowNames");

    vector<string> flwNames;
    ACE_CString flowname;

    for(CORBA::ULong i = 0; i < fepsData.length(); i++)
	{
	flowname = TAO_AV_Core::get_flowname(fepsData[i]);
	flwNames.push_back(flowname.c_str());
	}

    return flwNames;
}

/*********************************** private methods *************************************/

template<class TReceiverCallback>
void AcsBulkdata::BulkDataReceiver<TReceiverCallback>::initPartB()
{
    ACE_TRACE("BulkDataReceiver<>::initPartB");

    CORBA::ORB_var testOrb = TAO_AV_CORE::instance()->orb ();
    if(CORBA::is_nil(testOrb.in()))
	{
	TAO_AV_CORE::instance()->init(BACI_CORBA::getORB(), BACI_CORBA::getPOARoot());
	}

    int result = reactiveStrategy_m.init (TAO_AV_CORE::instance()->orb(),
					  TAO_AV_CORE::instance()->poa());
    if (result != 0)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::initPartB reactiveStrategy failed"));
	AVInitErrorExImpl err = AVInitErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::initPartB");
	throw err;
	}
}


template<class TReceiverCallback>
AVStreams::StreamEndPoint_B_ptr AcsBulkdata::BulkDataReceiver<TReceiverCallback>::createSepB()
{
    ACE_TRACE("BulkDataReceiver<>::createSepB");

    TAO_StreamEndPoint_B *localSepB_p = new TAO_StreamEndPoint_B();
    if(localSepB_p == 0)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::createSepB Stream Endpoint null"));
	AVStreamEndpointErrorExImpl err = AVStreamEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::createSepB");
	throw err;
	}

    sepRefCount_p = localSepB_p;

    AVStreams::StreamEndPoint_B_var localSepObj_p = localSepB_p->_this();
    if (CORBA::is_nil(localSepObj_p.in()))
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::createSepB unable to activate Stream Endpoint"));
	AVStreamEndpointErrorExImpl err = AVStreamEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::createSepB");
	throw err;
	}

    return localSepObj_p._retn();
}


template<class TReceiverCallback>
AVStreams::FlowConsumer_ptr AcsBulkdata::BulkDataReceiver<TReceiverCallback>::createFepConsumerB(ACE_CString &flowName, AVStreams::protocolSpec protocols, ACE_CString &format)
{
    ACE_TRACE("BulkDataReceiver<>::createFepConsumerB");

    BulkDataFlowConsumer<TReceiverCallback> *localFepB_p = new BulkDataFlowConsumer<TReceiverCallback>(flowName.c_str(), protocols, format.c_str());
    if(localFepB_p == 0)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::createFepConsumerB Flow Consumer null"));
	AVFlowEndpointErrorExImpl err = AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::createFepConsumerB");
	throw err;
	}

    AVStreams::FlowConsumer_var localFepObj_p = localFepB_p->_this(); 
    if (CORBA::is_nil(localFepObj_p.in()))
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::createFepConsumerB unable to activate Flow Consumer"));
	AVFlowEndpointErrorExImpl err = AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::createFepConsumerB");
	throw err;
	}

    fepMap_m.bind(flowName, localFepB_p);
    
    return localFepObj_p._retn();
}


template<class TReceiverCallback>
void AcsBulkdata::BulkDataReceiver<TReceiverCallback>::addFepToSep(AVStreams::StreamEndPoint_B_ptr locSepB_p, AVStreams::FlowConsumer_ptr locFepB_p)
{
    ACE_TRACE("BulkDataReceiver<>::addFepToSep");

    CORBA::String_var s1 = locSepB_p->add_fep(locFepB_p);
    if(s1 == 0)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::addFepToSep Flow Endpoint cannot be created"));
	AVFlowEndpointErrorExImpl err = AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::addFepToSep");
	throw err;
	}

    ACS_SHORT_LOG((LM_INFO,"BulkDataReceiver<>::addFepToSep Added flowendpoint named: %s", s1.in()));
}


template<class TReceiverCallback>
AVStreams::StreamEndPoint_B_ptr AcsBulkdata::BulkDataReceiver<TReceiverCallback>::getStreamEndPointB()
{
    ACS_TRACE("BulkDataReceiver<>::getStreamEndPointB");   

    if(sepB_p.in() == 0)
	{
	return AVStreams::StreamEndPoint_B::_nil();
	}

    AVStreams::StreamEndPoint_B_var locSepB = sepB_p;

    return locSepB._retn();
    //return sepB_p._retn();
}


template<class TReceiverCallback>
AVStreams::flowSpec * AcsBulkdata::BulkDataReceiver<TReceiverCallback>::getFepsConfig()
{
    ACS_TRACE("BulkDataReceiver<>::getFepsConfig");
  
    return &fepsData;
}


template<class TReceiverCallback>
void AcsBulkdata::BulkDataReceiver<TReceiverCallback>::deleteFepsB()
{
    ACS_TRACE("BulkDataReceiver<>::deleteFepsB");   

    for(CORBA::ULong i = 0; i < fepsData.length(); i++)
	{
	ACE_CString flowname = TAO_AV_Core::get_flowname(fepsData[i]);
	
	if (sepRefCount_p != 0)
	    {
	    sepRefCount_p->remove_fep(flowname.c_str());
	    }
	
	BulkDataFlowConsumer<TReceiverCallback> *fep = 0;
	fepMap_m.find(flowname, fep);
	if(fep == 0)
	    {
	    closeReceiverFlag = true; //necessary to avoid container crash
	    AVFlowEndpointErrorExImpl err = AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver<>::deleteFepsB");
	    throw err;
	    }
	else
	    {
	    CORBA::Long dim = fep->_refcount_value /*_ref_count*/();
	    for(CORBA::Long n = 1; n < dim; n++)
		{
		TAO_AV_Core::deactivate_servant(fep);
		}
	    
	    if (fep != 0) fep->_remove_ref();
	    fepMap_m.unbind(flowname, fep);
	    }
	
	//TAO_AV_CORE::instance ()->remove_acceptor(flowname.c_str());
	}
}


template<class TReceiverCallback>
void AcsBulkdata::BulkDataReceiver<TReceiverCallback>::deleteSepB()
{
    ACS_TRACE("BulkDataReceiver<>::deleteSepB");   

    if (sepRefCount_p != 0)
	{
	CORBA::Long dim = sepRefCount_p->_refcount_value/*_ref_count*/();
	for(CORBA::Long n = 1; n < dim; n++)
	    {
	    TAO_AV_Core::deactivate_servant(sepRefCount_p);
	    }
	
	if (sepRefCount_p != 0) sepRefCount_p->_remove_ref();
	sepRefCount_p=0;
	}
}


template<class TReceiverCallback>
void AcsBulkdata::BulkDataReceiver<TReceiverCallback>::deleteAcceptor()
{
    ACS_TRACE("BulkDataReceiver<>::deleteAcceptor");   

    for(CORBA::ULong i = 0; i < fepsData.length(); i++)
	{
	ACE_CString flowname = TAO_AV_Core::get_flowname(fepsData[i]);
	TAO_AV_CORE::instance()->remove_acceptor(flowname.c_str());
	}
}


template<class TReceiverCallback>
void AcsBulkdata::BulkDataReceiver<TReceiverCallback>::closeSocket()
{
    ACS_TRACE("BulkDataReceiver<>::closeSocket");

    for(CORBA::ULong i = 0; i < fepsData.length(); i++)
	{
	ACE_CString flowName = TAO_AV_Core::get_flowname(fepsData[i]);
	
	BulkDataFlowConsumer<TReceiverCallback> *fep = 0;
	
	fepMap_m.find(flowName, fep);
	if(fep == 0)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver::closeSocket Flow End Point null"));
	    AVFlowEndpointErrorExImpl err = AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::closeSocket");
	    throw err;
	    } 
	else
	    {
	    TReceiverCallback *cb_p = fep->getBulkDataCallback();
	    if(cb_p == 0)
		{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver::closeSocket callback null"));
		AVCallbackErrorExImpl err = AVCallbackErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::closeSocket");
		throw err;
		}
	    else
		{
		cb_p->closePeer();
		}
	    }
	}
}   


template<class TReceiverCallback>
const char * AcsBulkdata::BulkDataReceiver<TReceiverCallback>::createFlowSpec(ACE_CString &flowName,
									      ACE_CString &fepProtocol)
{
    ACS_TRACE("BulkDataReceiver<>::createFlowSpec");

    ACE_CString direction = "";
    ACE_CString formatName = "";
    ACE_CString flowProtocol = "";

    TAO_Tokenizer address(fepProtocol.c_str(), '=');
    ACE_CString remoteAddress = CORBA::string_dup(address[1]);
    ACE_CString localAddress = "";
    ACE_CString carrierProtocol = CORBA::string_dup(address[0]);


    ACE_INET_Addr locAddr(localAddress.c_str ());
    ACE_INET_Addr remAddr(remoteAddress.c_str ());


    TAO_Forward_FlowSpec_Entry entry(flowName.c_str(),
				     direction.c_str(),
				     formatName.c_str(),
				     flowProtocol.c_str(),
				     carrierProtocol.c_str(),
				     &locAddr);
  
    entry.set_peer_addr(&remAddr);

    return CORBA::string_dup(entry.entry_to_string());
} 


template<class TReceiverCallback>
void AcsBulkdata::BulkDataReceiver<TReceiverCallback>::setReceiverName(ACE_CString recvName)
{
    for(CORBA::ULong i = 0; i < fepsData.length(); i++)
	{
	ACE_CString flowName = TAO_AV_Core::get_flowname(fepsData[i]);
	
	BulkDataFlowConsumer<TReceiverCallback> *fep = 0;
	
	fepMap_m.find(flowName, fep);
	if(fep == 0)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver::setReceiverName Flow Endpoint %s not found",flowName.c_str()));
	    AVFlowEndpointErrorExImpl err = AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::setReceiverName");
	    throw err;
	    } 
	else
	    {
	    TReceiverCallback *cb_p = fep->getBulkDataCallback();
	    if(cb_p == 0)
		{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver::setReceiverName callback null"));
		AVCallbackErrorExImpl err = AVCallbackErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::setReceiverName");
		throw err;
		}
	    else
		cb_p->setReceiverName(recvName);
	    }	
	}
}


template<class TReceiverCallback>
void AcsBulkdata::BulkDataReceiver<TReceiverCallback>::subscribeNotification(ACS::CBvoid_ptr notifCb)
    throw (AVNotificationMechanismErrorExImpl)
{
    try
	{
	locNotifCb_p = ACS::CBvoid::_duplicate(notifCb);
	
	TReceiverCallback *cb = 0;
	
	ACE_CString flowName = "";
	
	vector<string> vecNames = getFlowNames();
	for(CORBA::ULong i = 0; i < vecNames.size(); i++)
	    {
	    flowName = vecNames[i].c_str();
	    
	    getFlowCallback(flowName, cb);
	    if(cb == 0)
		{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::subscribeNotification callback null"));
		AVCallbackErrorExImpl err = AVCallbackErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::subscribeNotification");
		throw err;
		} 
	    else
		{
		cb->setReceiver(this);
		}
	    }
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVNotificationMechanismErrorExImpl err = AVNotificationMechanismErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::subscribeNotification");
	throw err;	
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataReceiver::subscribeNotification");
	AVNotificationMechanismErrorExImpl err = AVNotificationMechanismErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::subscribeNotification");
	throw err;	
	}
}


template<class TReceiverCallback>
void AcsBulkdata::BulkDataReceiver<TReceiverCallback>::notifySender(const ACSErr::Completion& comp)
    throw (AVNotificationMechanismErrorExImpl)
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
	ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::notifySender callback reference null"));
	AVCallbackErrorExImpl ex = AVCallbackErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::notifySender");
	AVNotificationMechanismErrorExImpl err = AVNotificationMechanismErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::notifySender");
	throw err;
	}
}


template<class TReceiverCallback>
bulkdata::Connection AcsBulkdata::BulkDataReceiver<TReceiverCallback>::checkFlowCallbacks()
    throw (AVFlowEndpointErrorExImpl)
{
    bulkdata::Connection connState = bulkdata::READY;

    if(fepMap_m.current_size() == 0) // no connection yet, state READY
	{
	connState = bulkdata::READY;
	}
    else
	{
	for(CORBA::ULong i = 0; i < fepsData.length(); i++)
	    {
	    ACE_CString flowName = TAO_AV_Core::get_flowname(fepsData[i]);
	    
	    BulkDataFlowConsumer<TReceiverCallback> *fep = 0;
	    
	    fepMap_m.find(flowName, fep);
	    if(fep == 0)
		{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver::checkFlowCallbacks Flow End Point null"));
		AVFlowEndpointErrorExImpl err = AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::checkFlowCallbacks");
		throw err;
		} 
	    else
		{
		TReceiverCallback *cb_p = fep->getBulkDataCallback();
		if(cb_p == 0)
		    {
		    connState = bulkdata::OPENED; // to be implemented
		    }
		else
		    {
		    connState = bulkdata::CONNECTED;
		    break;
		    }
		}
	    }
	}

    return connState;    
}
