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
	ACSBulkDataError::AVReceiverConfigErrorExImpl err = ACSBulkDataError::AVReceiverConfigErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::BulkDataReceiver");
	throw err;
	}

    cbTimeout_m.set(0L,10L); // timeout default value, 0 sec, 10 usec
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
    catch(ACSBulkDataError::AVCloseReceiverErrorExImpl &ex)
	{
	ACSBulkDataError::AVCloseReceiverErrorExImpl err = ACSBulkDataError::AVCloseReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::~BulkDataReceiver");
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
{
    ACE_TRACE("BulkDataReceiver<>::initialize");

    try
	{
	initPartB();
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	ACSBulkDataError::AVInitErrorExImpl err = ACSBulkDataError::AVInitErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::initialize");
	throw err;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataReceiver::initialize");
	ACSBulkDataError::AVInitErrorExImpl err = ACSBulkDataError::AVInitErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::initialize");
	throw err;
	}
}


template<class TReceiverCallback>
void AcsBulkdata::BulkDataReceiver<TReceiverCallback>::createSingleFlow()
{
    ACE_TRACE("BulkDataReceiver<>::createSingleFlow");

    try
	{
	if (!CORBA::is_nil(sepB_p.in()))
	    {
	    // delete old stuff
	    deleteHandler();
	    ACE_OS::sleep(1);  // seems necessary to give time to remove
	                       // the handler from the reactor
	    deleteFepsB();
	    deleteSepB();
	    }
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
    catch(ACSBulkDataError::AVStreamEndpointErrorExImpl &ex)
	{
	ACSBulkDataError::AVStreamEndpointErrorExImpl err = ACSBulkDataError::AVStreamEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::createSingleFlow");
	throw err;
	}
    catch(ACSBulkDataError::AVFlowEndpointErrorExImpl &ex)
	{
	ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::createSingleFlow");
	throw err;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataReceiver::createSingleFlow");
	ACSBulkDataError::AVStreamEndpointErrorExImpl err = ACSBulkDataError::AVStreamEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::createSingleFlow");
	throw err;
	}
}


template<class TReceiverCallback>
void AcsBulkdata::BulkDataReceiver<TReceiverCallback>::createMultipleFlows(const char *fepsConfig)
{
    ACE_TRACE("BulkDataReceiver<>::createMultipleFlows");

    bulkdata::Connection conn = checkFlowCallbacks();
    recvConfig_p->connectionState = conn;
    if(conn == bulkdata::CONNECTED) // state CONNECTED, nothing is created on the receiver side
	{							       
	//return; // in case of CONNECTED state new sep and fep(s) are created on receiver side
	          // uncomment the return to use the old ones
	}

    try
	{
	if(ACE_OS::strcmp(fepsConfig, "") == 0)
	    {
	    createSingleFlow();
	    return;
	    }

	if (!CORBA::is_nil(sepB_p.in()))
	    {
	    // delete old stuff
	    deleteHandler();
	    ACE_OS::sleep(1);  // seems necessary to give time to remove
	                       // the handler from the reactor
	    deleteFepsB();
	    deleteSepB();
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
	    ACSBulkDataError::AVInvalidFlowNumberExImpl err = ACSBulkDataError::AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataReceiver::createMultipleFlows");
	    throw err;	
	    }

	fepsData.length(numOtherFeps);

	char hostName[255];
	std::string strAddressToken;
	for(int j = 0; j < numOtherFeps; j++)
	    {
	    strAddressToken = addressToken[j];
	    if(strAddressToken.find("${HOST}",0) != std::string::npos)
		{
		if(strAddressToken.find(":",0) != std::string::npos)		  
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
    catch(ACSBulkDataError::AVStreamEndpointErrorExImpl &ex)
	{
	ACSBulkDataError::AVStreamEndpointErrorExImpl err = ACSBulkDataError::AVStreamEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::createMultipleFlows");
	throw err;
	}
    catch(ACSBulkDataError::AVInvalidFlowNumberExImpl &ex)
	{
	ACSBulkDataError::AVInvalidFlowNumberExImpl err = ACSBulkDataError::AVInvalidFlowNumberExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::createMultipleFlows");
	throw err;
	}
    catch(ACSBulkDataError::AVFlowEndpointErrorExImpl &ex)
	{
	ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::createMultipleFlows");
	throw err;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataReceiver::createMultipleFlows");
	ACSBulkDataError::AVStreamEndpointErrorExImpl err = ACSBulkDataError::AVStreamEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::createMultipleFlows");
	throw err;
	}
}


template<class TReceiverCallback>
bulkdata::BulkDataReceiverConfig * AcsBulkdata::BulkDataReceiver<TReceiverCallback>::getReceiverConfig()
{
    ACS_TRACE("BulkDataReceiver<>::getReceiverConfig");

    bulkdata::BulkDataReceiverConfig *recvConfigLoc_p = 0;
    try
	{
	recvConfigLoc_p = new bulkdata::BulkDataReceiverConfig;
	if(recvConfigLoc_p == 0)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::getReceiverConfig error creating BulkDataReceiverConfig"));
	    ACSBulkDataError::AVReceiverConfigErrorExImpl err = ACSBulkDataError::AVReceiverConfigErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::getReceiverConfig");
	    throw err;
	    }
      
	recvConfig_p->streamendpoint_B = getStreamEndPointB();
	if (CORBA::is_nil((recvConfig_p->streamendpoint_B).in()))
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::getReceiverConfig Stream Endpoint not initilaized"));
	    ACSBulkDataError::AVReceiverConfigErrorExImpl err = ACSBulkDataError::AVReceiverConfigErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::getReceiverConfig");
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
	    ACSBulkDataError::AVReceiverConfigErrorExImpl err = ACSBulkDataError::AVReceiverConfigErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::getReceiverConfig");
	    throw err;
	    }

	recvConfigLoc_p->connectionState = recvConfig_p->connectionState;
	}
    catch(ACSBulkDataError::AVReceiverConfigErrorExImpl &ex)
	{
	ACSBulkDataError::AVReceiverConfigErrorExImpl err = ACSBulkDataError::AVReceiverConfigErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::getReceiverConfig");
	throw err;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataReceiver::getReceiverConfig");
	ACSBulkDataError::AVReceiverConfigErrorExImpl err = ACSBulkDataError::AVReceiverConfigErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::getReceiverConfig");
	throw err;
	}
    
    return recvConfigLoc_p;
}


template<class TReceiverCallback>
void AcsBulkdata::BulkDataReceiver<TReceiverCallback>::getFlowCallback(ACE_CString &flowName, TReceiverCallback *&cb_p)
{
    ACS_TRACE("BulkDataReceiver<>::getFlowCallback");   

    try
	{
	BulkDataFlowConsumer<TReceiverCallback> *fep = 0;
	fepMap_m.find(flowName, fep);
	if(fep == 0)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::getFlowCallback Flow End Point null"));
	    ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
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
		ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
		throw err;
		} 
	    else
		cb_p = fep->getBulkDataCallback();
	    /*if(cb_p == 0)
	      {
	      ACSBulkDataError::AVCallbackErrorExImpl err = ACSBulkDataError::AVCallbackErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
	      throw err;
	      }*/
	    }
	}
    catch(ACSBulkDataError::AVFlowEndpointErrorExImpl &ex)
	{
	ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
	throw err;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
	ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
	throw err;
	}
}


template<class TReceiverCallback>
void AcsBulkdata::BulkDataReceiver<TReceiverCallback>::getFlowCallback(CORBA::ULong flowNumber, TReceiverCallback *&cb_p)
{
    ACS_TRACE("BulkDataReceiver<>::getFlowCallback");   

    try
	{
	BulkDataFlowConsumer<TReceiverCallback> *fep = 0;
	ACE_CString flowName;

	CORBA::ULong dim = fepsData.length();
	if(flowNumber < 1 || flowNumber > dim)
	    {
	    ACSBulkDataError::AVInvalidFlowNumberExImpl err = ACSBulkDataError::AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
	    throw err;
	    }
	flowNumber--;

	std::vector<std::string> vec = getFlowNames();
	flowName = vec[flowNumber].c_str();

	fepMap_m.find(flowName, fep);
	if(fep == 0)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::getFlowCallback Flow End Point null"));
	    ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
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
		ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
		throw err;
		} 
	    else
		cb_p = fep->getBulkDataCallback();
	    /*if(cb_p == 0)
	      {
	      ACSBulkDataError::AVCallbackErrorExImpl err = ACSBulkDataError::AVCallbackErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
	      throw err;
	      }*/
	    }
	}	
    catch(ACSBulkDataError::AVInvalidFlowNumberExImpl &ex)
	{
	ACSBulkDataError::AVInvalidFlowNumberExImpl err = ACSBulkDataError::AVInvalidFlowNumberExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
	throw err;
	}
    catch(ACSBulkDataError::AVFlowEndpointErrorExImpl &ex)
	{
	ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
	throw err;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
	ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::getFlowCallback");
	throw err;
	}
}


template<class TReceiverCallback>
void AcsBulkdata::BulkDataReceiver<TReceiverCallback>::closeReceiver()
{
    ACE_TRACE("BulkDataReceiver<>::closeReceiver");

    try
	{
	deleteAcceptor();
	deleteHandler();
	ACE_OS::sleep(1);  // seems necessary to give time to remove
	                   // the handler from the reactor
	deleteFepsB();
	deleteSepB();
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	ACSBulkDataError::AVCloseReceiverErrorExImpl err = ACSBulkDataError::AVCloseReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::closeReceiver");
	throw err;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataReceiver::closeReceiver");
	ACSBulkDataError::AVCloseReceiverErrorExImpl err = ACSBulkDataError::AVCloseReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::closeReceiver");
	throw err;
	}

    closeReceiverFlag = true;
}


template<class TReceiverCallback>
std::vector<std::string> AcsBulkdata::BulkDataReceiver<TReceiverCallback>::getFlowNames()
{
    ACE_TRACE("BulkDataReceiver<>::getFlowNames");

    std::vector<std::string> flwNames;
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
	ACSBulkDataError::AVInitErrorExImpl err = ACSBulkDataError::AVInitErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::initPartB");
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
	ACSBulkDataError::AVStreamEndpointErrorExImpl err = ACSBulkDataError::AVStreamEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::createSepB");
	throw err;
	}

    sepRefCount_p = localSepB_p;

    AVStreams::StreamEndPoint_B_var localSepObj_p = localSepB_p->_this();
    if (CORBA::is_nil(localSepObj_p.in()))
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::createSepB unable to activate Stream Endpoint"));
	ACSBulkDataError::AVStreamEndpointErrorExImpl err = ACSBulkDataError::AVStreamEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::createSepB");
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
	ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::createFepConsumerB");
	throw err;
	}

    try
	{
	localFepB_p->setCbTimeout(cbTimeout_m);
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::createFepConsumerB error setting callback timeout in Flow Consumer"));
	ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::createFepConsumerB");
	throw err;
	}

    AVStreams::FlowConsumer_var localFepObj_p = localFepB_p->_this(); 
    if (CORBA::is_nil(localFepObj_p.in()))
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::createFepConsumerB unable to activate Flow Consumer"));
	ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::createFepConsumerB");
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
	ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::addFepToSep");
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
	    ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver<>::deleteFepsB");
	    throw err;
	    }
	else
	    {
	    TReceiverCallback *cb_p = fep->getBulkDataCallback();
	    if(cb_p == 0)
		{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::deleteFepsB callback null"));
		ACSBulkDataError::AVCallbackErrorExImpl err = ACSBulkDataError::AVCallbackErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver<>::deleteFepsB");
		throw err;
		}
	    else
		{
		if(cb_p->isFepAlive())
		    fep->destroy();
		}

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

    sepB_p = AVStreams::StreamEndPoint_B::_nil();
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
void AcsBulkdata::BulkDataReceiver<TReceiverCallback>::deleteHandler()
{
    ACS_TRACE("BulkDataReceiver<>::deleteHandler");   

    for(CORBA::ULong i = 0; i < fepsData.length(); i++)
	{
	ACE_CString flowname = TAO_AV_Core::get_flowname(fepsData[i]);
        BulkDataFlowConsumer<TReceiverCallback> *fep = 0;
    
        fepMap_m.find(flowname, fep);

	if(fep == 0)
	    {
	    ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::deleteHandler");
	    throw err;
	    }
        else
            {
	    ACE_HANDLE handle;
	    if((handleMap_m.find(flowname,handle)) == -1)
		{
		//ACS_SHORT_LOG((LM_INFO,"BulkDataReceiver<>::deleteHandler - handle not present in HandleMap"));
		return; // to be better defined what to do here
		}
	    else
		{
		if(TAO_AV_CORE::instance()->reactor()->find_handler(handle))
		    {
		    TAO_AV_Flow_Handler *locHandler_p = fep->getFlowHandler();
		    ACE_Event_Handler *event_handler = locHandler_p ->event_handler();
		    TAO_AV_CORE::instance()->reactor()->remove_handler(event_handler,ACE_Event_Handler::READ_MASK);
		    handleMap_m.unbind(flowname,handle);
		    }
		else
		    {
		    //ACS_SHORT_LOG((LM_INFO,"BulkDataReceiver<>::deleteHandler - handle not present in the ACE_Reactor"));
		    handleMap_m.unbind(flowname,handle);
		    continue;
		    }
		}
	    }
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
	    ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::closeSocket");
	    throw err;
	    } 
	else
	    {
	    TReceiverCallback *cb_p = fep->getBulkDataCallback();
	    if(cb_p == 0)
		{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver::closeSocket callback null"));
		ACSBulkDataError::AVCallbackErrorExImpl err = ACSBulkDataError::AVCallbackErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::closeSocket");
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
	    ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::setReceiverName");
	    throw err;
	    } 
	else
	    {
	    TReceiverCallback *cb_p = fep->getBulkDataCallback();
	    if(cb_p == 0)
		{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver::setReceiverName callback null"));
		ACSBulkDataError::AVCallbackErrorExImpl err = ACSBulkDataError::AVCallbackErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::setReceiverName");
		throw err;
		}
	    else
		cb_p->setReceiverName(recvName);
		ACE_HANDLE handle = cb_p->getHandle();
		handleMap_m.rebind(flowName,handle);
	    }	
	}
}


template<class TReceiverCallback>
void AcsBulkdata::BulkDataReceiver<TReceiverCallback>::subscribeNotification(ACS::CBvoid_ptr notifCb)
{
    try
	{
	locNotifCb_p = ACS::CBvoid::_duplicate(notifCb);
	
	TReceiverCallback *cb = 0;
	
	ACE_CString flowName = "";
	
	std::vector<std::string> vecNames = getFlowNames();
	for(CORBA::ULong i = 0; i < vecNames.size(); i++)
	    {
	    flowName = vecNames[i].c_str();
	    
	    getFlowCallback(flowName, cb);
	    if(cb == 0)
		{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::subscribeNotification callback null"));
		ACSBulkDataError::AVCallbackErrorExImpl err = ACSBulkDataError::AVCallbackErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::subscribeNotification");
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
	ACSBulkDataError::AVNotificationMechanismErrorExImpl err = ACSBulkDataError::AVNotificationMechanismErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::subscribeNotification");
	throw err;	
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataReceiver::subscribeNotification");
	ACSBulkDataError::AVNotificationMechanismErrorExImpl err = ACSBulkDataError::AVNotificationMechanismErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::subscribeNotification");
	throw err;	
	}
}


template<class TReceiverCallback>
void AcsBulkdata::BulkDataReceiver<TReceiverCallback>::fwdData2UserCB(CORBA::Boolean enable)
{
	for(CORBA::ULong i = 0; i < fepsData.length(); i++)
	{
		ACE_CString flowName = TAO_AV_Core::get_flowname(fepsData[i]);

		BulkDataFlowConsumer<TReceiverCallback> *fep = 0;

		fepMap_m.find(flowName, fep);
		if(fep != 0)
		{
			TReceiverCallback *cb_p = fep->getBulkDataCallback();
			if(cb_p != 0)
			{
			cb_p->fwdData2UserCB(enable);
			}
		}
		else
		{
			ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver::fwdData2UserCB Flow End Point null"));
		}//if-else
	}//for
}//useUserCB

template<class TReceiverCallback>
void AcsBulkdata::BulkDataReceiver<TReceiverCallback>::notifySender(const ACSErr::Completion& comp)
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
	ACSBulkDataError::AVCallbackErrorExImpl ex = ACSBulkDataError::AVCallbackErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::notifySender");
	ACSBulkDataError::AVNotificationMechanismErrorExImpl err = ACSBulkDataError::AVNotificationMechanismErrorExImpl(ex,__FILE__,__LINE__,"BulkDataReceiver::notifySender");
	throw err;
	}
}


template<class TReceiverCallback>
bulkdata::Connection AcsBulkdata::BulkDataReceiver<TReceiverCallback>::checkFlowCallbacks()
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
		ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::checkFlowCallbacks");
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


template<class TReceiverCallback>
void AcsBulkdata::BulkDataReceiver<TReceiverCallback>::setCbTimeout(const char * cbTimeout)
{
    TAO_Tokenizer timeoutToken(cbTimeout,':');
    int numTokens = timeoutToken.num_tokens();
    if(numTokens != 2)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver::setCbTimeout wrong number of timeout entries in CDB"));
	ACSBulkDataError::AVCDBTimeoutErrorExImpl err = ACSBulkDataError::AVCDBTimeoutErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::setCbTimeout");
	throw err;
	}

    std::string timeoutSec(timeoutToken[0]); 
    std::string timeoutUsec(timeoutToken[1]);

    for(unsigned int i = 0; i < timeoutSec.length(); i++)
	{
	if(!std::isdigit(timeoutSec[i]))
	    { 
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver::setCbTimeout wrong timeout entry format in CDB"));
	    ACSBulkDataError::AVCDBTimeoutErrorExImpl err = ACSBulkDataError::AVCDBTimeoutErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::setCbTimeout");
	    throw err;
	    }
	}

    for(unsigned int i = 0; i < timeoutUsec.length(); i++)
	{
	if(!std::isdigit(timeoutUsec[i]))
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver::setCbTimeout wrong timeout entry format in CDB"));
	    ACSBulkDataError::AVCDBTimeoutErrorExImpl err = ACSBulkDataError::AVCDBTimeoutErrorExImpl(__FILE__,__LINE__,"BulkDataReceiver::setCbTimeout");
	    throw err;
	    }
	}

    std::istringstream iss0(timeoutToken[0]);
    std::istringstream iss1(timeoutToken[1]);

    CORBA::ULong timeout_sec;
    CORBA::ULong timeout_usec;

    iss0 >> timeout_sec;
    iss1 >> timeout_usec;

    cbTimeout_m.set(timeout_sec,timeout_usec);
}
