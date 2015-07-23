template<class TSenderCallback>
AcsBulkdata::BulkDataSender<TSenderCallback>::BulkDataSender() : disconnectPeerFlag(false)
{
    ACE_TRACE("BulkDataSender<>::BulkDataSender");
  
    sepA_p = AVStreams::StreamEndPoint_A::_nil();
    sepB_p = AVStreams::StreamEndPoint_B::_nil();

    sepRefCount_p = 0;
    streamctrl_p = 0;
}


template<class TSenderCallback>
AcsBulkdata::BulkDataSender<TSenderCallback>::~BulkDataSender()
{
    ACE_TRACE("BulkDataSender<>::~BulkDataSender");

    try
	{
	if(disconnectPeerFlag == false)
	    {
	    disconnectPeer();
	    }
	}
    catch(ACSBulkDataError::AVDisconnectErrorExImpl &ex)
	{
	ACSBulkDataError::AVDisconnectErrorExImpl err = ACSBulkDataError::AVDisconnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::~BulkDataSender");
	err.log(LM_ERROR);
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataSender::~BulkDataSender");
	ex.log(LM_ERROR);
	}
}


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::initialize()
{
    ACE_TRACE("BulkDataSender<>::initialize");

    try
	{
	initPartA();
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	ACSBulkDataError::AVInitErrorExImpl err = ACSBulkDataError::AVInitErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::initialize");
	throw err;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataSender::initialize");
	ACSBulkDataError::AVInitErrorExImpl err = ACSBulkDataError::AVInitErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::initialize");
	throw err;
	}
}


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::createSingleFlow()
{
    ACE_TRACE("BulkDataSender<>::createSingleFlow");

    try
	{
	if(streamctrl_p != 0)
	    {
	    // delete old stuff
	    deleteHandler();
	    ACE_OS::sleep(1);  // seems necessary to give time to remove
	                       // the handler from the reactor
	    deleteStreamCtrl();
	    }
	streamctrl_p = createStreamCtrl();

	if (!CORBA::is_nil(sepA_p.in()))
	    {
	    // delete old stuff
	    deleteFepsA();
	    deleteSepA();
	    }
	sepA_p = createSepA();

	ACE_CString flowname = "Flow1";

	AVStreams::protocolSpec defProt(1);
	defProt.length(1);
	defProt[0] = CORBA::string_dup("TCP");

	ACE_CString format = "UNS1:ftp";

	AVStreams::FlowProducer_var fepObj_p = createFepProducerA(flowname, defProt, format, streamctrl_p); 

	addFepToSep(sepA_p.in(), fepObj_p.in());

	senderFeps_m.length(1);

	ACE_CString address = CORBA::string_dup("TCP");
	const char *locEntry = createFlowSpec(flowname, address);
	senderFeps_m[0] = CORBA::string_dup(locEntry);
	}
    catch(ACSBulkDataError::AVStreamEndpointErrorExImpl &ex)
	{
	ACSBulkDataError::AVStreamEndpointErrorExImpl err = ACSBulkDataError::AVStreamEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::createSingleFlow");
	throw err;
	}
    catch(ACSBulkDataError::AVFlowEndpointErrorExImpl &ex)
	{
	ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::createSingleFlow");
	throw err;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataSender::createSingleFlow");
	ACSBulkDataError::AVStreamEndpointErrorExImpl err = ACSBulkDataError::AVStreamEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::createSingleFlow");
	throw err;
	}
}


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::createMultipleFlows(const char *fepsConfig)
{
    ACE_TRACE("BulkDataSender<>::createMultipleFlows");

    try
	{
	if(ACE_OS::strcmp(fepsConfig, "") == 0)
	    {
	    ACS_SHORT_LOG((LM_WARNING,"BulkDataSender<>::createMultipleFlows single flow created"));
	    createSingleFlow();
	    return;
	    }

	if(streamctrl_p != 0)
	    {
	    // delete old stuff
	    deleteHandler();
	    ACE_OS::sleep(1);  // seems necessary to give time to remove
	                       // the handler from the reactor
	    deleteStreamCtrl();
	    }
	streamctrl_p = createStreamCtrl();

	if (!CORBA::is_nil(sepA_p.in()))
	    {
	    // delete old stuff
	    deleteFepsA();
	    deleteSepA();
	    }
	sepA_p = createSepA();

	FepsCfgA localStruct;

	AVStreams::FlowProducer_var fepObj_p;

	AVStreams::protocolSpec defProt(1);
	defProt.length(1);

	TAO_Tokenizer addressToken(fepsConfig, '/');
	int numOtherFeps = addressToken.num_tokens();
	if(numOtherFeps > 19)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::createMultipleFlows too many flows specified - maximum 19"));
	    ACSBulkDataError::AVInvalidFlowNumberExImpl err = ACSBulkDataError::AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataSender::createMultipleFlows");
	    throw err;	
	    }

	senderFeps_m.length(numOtherFeps);

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

	    fepObj_p = createFepProducerA(localStruct.fepsFlowname, defProt, localStruct.fepsFormat, streamctrl_p); 

	    addFepToSep(sepA_p.in(), fepObj_p.in());

	    const char *locEntry = createFlowSpec(localStruct.fepsFlowname, localStruct.fepsProtocol);

	    senderFeps_m[j] = CORBA::string_dup(locEntry);
	    }
	}
    catch(ACSBulkDataError::AVStreamEndpointErrorExImpl &ex)
	{
	ACSBulkDataError::AVStreamEndpointErrorExImpl err = ACSBulkDataError::AVStreamEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::createMultipleFlows");
	throw err;
	}
    catch(ACSBulkDataError::AVInvalidFlowNumberExImpl &ex)
	{
	ACSBulkDataError::AVInvalidFlowNumberExImpl err = ACSBulkDataError::AVInvalidFlowNumberExImpl(ex,__FILE__,__LINE__,"BulkDataSender::createMultipleFlows");
	throw err;
	}
    catch(ACSBulkDataError::AVFlowEndpointErrorExImpl &ex)
	{
	ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::createMultipleFlows");
	throw err;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataSender::createMultipleFlows");
	ACSBulkDataError::AVStreamEndpointErrorExImpl err = ACSBulkDataError::AVStreamEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::createMultipleFlows");
	throw err;
	}
}


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::connectToPeer(bulkdata::BulkDataReceiverConfig *recvConfig_p)
{
    ACS_TRACE("BulkDataSender<>::connectToPeer");   

    try
	{
	setReceiverConfig(recvConfig_p);
	//the_qos = create_QoS();
	
	mergeFlowSpecs();
	AVStreams::streamQoS_var theQos(new AVStreams::streamQoS);

	CORBA::Boolean res = streamctrl_p->bind(sepA_p.in(),
						sepB_p.in(), 
						theQos.inout(), 
						flowSpec_m);
	if (res == 0)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::connectToPeer streams bind failed"));
	    ACSBulkDataError::AVStreamBindErrorExImpl err = ACSBulkDataError::AVStreamBindErrorExImpl(__FILE__,__LINE__,"BulkDataSender::connectToPeer");
	    throw err;
	    }

	for(CORBA::ULong i = 0; i < senderFeps_m.length(); i++)
	    {
	    ACE_CString flowName = TAO_AV_Core::get_flowname(senderFeps_m[i]);
	
	    BulkDataFlowProducer<TSenderCallback> *fep = 0;
	
	    fepMap_m.find(flowName, fep);
	    if(fep == 0)
		{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataSender::connectToPeer Flow End Point null"));
		ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSender::connectToPeer");
		throw err;
		} 
	    else
		{
		TSenderCallback *cb_p = fep->getBulkDataCallback();
		if(cb_p == 0)
		    {
		    ACS_SHORT_LOG((LM_ERROR,"BulkDataSender::connectToPeer callback null"));
		    ACSBulkDataError::AVCallbackErrorExImpl err = ACSBulkDataError::AVCallbackErrorExImpl(__FILE__,__LINE__,"Sender::connectToPeer");
		    throw err;
		    }
		else
		    {
		    ACE_HANDLE handle = cb_p->getHandle();
		    handleMap_m.rebind(flowName,handle);
		    }
		}
	    }

	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	ACSBulkDataError::AVConnectErrorExImpl err = ACSBulkDataError::AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::connectToPeer");
	throw err;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataSender::connectToPeer");
	ACSBulkDataError::AVConnectErrorExImpl err = ACSBulkDataError::AVConnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::connectToPeer");
	throw err;
	}

    // it seems necessary beacuse the bind method is not compeltely synchronous, i.e. it exits even if the connection is not fully established. TBA
    ACE_OS::sleep(1);
}


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::getFlowProtocol(ACE_CString &flowname, TAO_AV_Protocol_Object *&currentProtocol_p)
{
//    ACS_TRACE("BulkDataSender<>::getFlowProtocol");   

    try
	{
	BulkDataFlowProducer<TSenderCallback> *fep_p = 0;
	fepMap_m.find(flowname, fep_p);

	if(fep_p == 0)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::getFlowProtocol Flow protocol null"));
	    ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSender::getFlowProtocol");
	    throw err;
	    } 
	else
	    currentProtocol_p = fep_p->getProtocolObject();

	if(currentProtocol_p == 0)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::getFlowProtocol Protocol object null"));
	    ACSBulkDataError::AVProtocolErrorExImpl err = ACSBulkDataError::AVProtocolErrorExImpl(__FILE__,__LINE__,"BulkDataSender::getFlowProtocol");
	    throw err;
	    }
	}
    catch(ACSBulkDataError::AVFlowEndpointErrorExImpl &ex)
	{
	ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::getFlowProtocol");
	throw err;
	}
    catch(ACSBulkDataError::AVProtocolErrorExImpl &ex)
	{
	ACSBulkDataError::AVProtocolErrorExImpl err = ACSBulkDataError::AVProtocolErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::getFlowProtocol");
	throw err;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataSender::getFlowProtocol");
	ACSBulkDataError::AVProtocolErrorExImpl err = ACSBulkDataError::AVProtocolErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::getFlowProtocol");
	throw err;
	}
}


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::startSend(CORBA::ULong flowNumber, ACE_Message_Block *param_p)
{
    ACS_TRACE("BulkDataSender<>::startSend(ACE_Message_Block *)");

    try
	{
	int res = 0;

	CORBA::ULong dim = flowSpec_m.length();

	if(flowNumber < 1 || flowNumber > dim)
	    {
	    ACSBulkDataError::AVInvalidFlowNumberExImpl err = ACSBulkDataError::AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataSender::startSend");
	    throw err;
	    }

	flowNumber--;

	AVStreams::flowSpec locSpec(1);
	locSpec.length(1);
	ACE_CString flowname = TAO_AV_Core::get_flowname(flowSpec_m[flowNumber]);
	locSpec[0] = flowSpec_m[flowNumber];
		
	streamctrl_p->start(locSpec);
	
	TAO_AV_Protocol_Object *dp_p = 0;
	
	getFlowProtocol(flowname, dp_p);
	
	char tmp[255];
	
	if(param_p != 0)
	    {
	    ACE_OS::sprintf(tmp, "1 %u", (unsigned int)param_p->length());
	    }
	else
	    {
	    ACE_OS::sprintf(tmp, "1 0");
	    }      

	res = dp_p->send_frame(tmp, sizeof(tmp));
	if(res < 0)
	    {
	    ACSBulkDataError::AVSendFrameErrorExImpl err = ACSBulkDataError::AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::startSend");
	    throw err;
	    }
	
	streamctrl_p->stop(locSpec);
	
	if(param_p != 0)
	    {
	    res = dp_p->send_frame(param_p);
	    }
	else
	    {
	    ACE_Message_Block *locMb;
	    locMb = new ACE_Message_Block(1);
	    *locMb->wr_ptr()='\0';
	    locMb->wr_ptr(sizeof(char));
	    
	    res = dp_p->send_frame(locMb);
	
	    locMb->release();
	    }	
	if(res < 0)
	    {
	    ACSBulkDataError::AVSendFrameErrorExImpl err = ACSBulkDataError::AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::startSend");
	    throw err;
	    }

	streamctrl_p->stop(locSpec);	
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	ACSBulkDataError::AVSendFrameErrorExImpl err = ACSBulkDataError::AVSendFrameErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::startSend");
	throw err;
	}
    catch(CORBA::SystemException &ex)
	{
	ACSErrTypeCommon::CORBAProblemExImpl err = ACSErrTypeCommon::CORBAProblemExImpl(__FILE__,__LINE__,"BulkDataSender::startSend");
	err.setMinor(ex.minor());
	err.setCompletionStatus(ex.completed());
	err.setInfo(ex._info().c_str());
	ACSBulkDataError::AVSendFrameErrorExImpl err1 = ACSBulkDataError::AVSendFrameErrorExImpl(err,__FILE__,__LINE__,"BulkDataSender::startSend");
	throw err1;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataSender::startSend");
	ACSBulkDataError::AVSendFrameErrorExImpl err = ACSBulkDataError::AVSendFrameErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::startSend");
	throw err;
	}
}


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::startSend(CORBA::ULong flowNumber , const char *param_p, size_t len)
{
    ACS_TRACE("BulkDataSender<>::startSend(const char *)");

    try
	{
	int res = 0;

	CORBA::ULong dim = flowSpec_m.length();

	if(flowNumber < 1 || flowNumber > dim)
	    {
	    ACSBulkDataError::AVInvalidFlowNumberExImpl err = ACSBulkDataError::AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataSender::startSend");
	    throw err;
	    }

	flowNumber--;

	AVStreams::flowSpec locSpec(1);
	locSpec.length(1);
	ACE_CString flowname = TAO_AV_Core::get_flowname(flowSpec_m[flowNumber]);
	locSpec[0] = flowSpec_m[flowNumber];

	streamctrl_p->start(locSpec);

	TAO_AV_Protocol_Object *dp_p = 0;

	getFlowProtocol(flowname, dp_p);

	int locDim = 255;
	char tmp[locDim];
	ACE_OS::sprintf(tmp, "1 %u", (unsigned int)len); 
	res = dp_p->send_frame(tmp, locDim);
	if(res < 0)
	    {
	    ACSBulkDataError::AVSendFrameErrorExImpl err = ACSBulkDataError::AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::startSend");
	    throw err;
	    }

	streamctrl_p->stop(locSpec);
	
	res = dp_p->send_frame(param_p, len);
	if(res < 0)
	    {
	    ACSBulkDataError::AVSendFrameErrorExImpl err = ACSBulkDataError::AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::startSend");
	    throw err;
	    }

	streamctrl_p->stop(locSpec);
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	ACSBulkDataError::AVSendFrameErrorExImpl err = ACSBulkDataError::AVSendFrameErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::startSend");
	throw err;
	}
    catch(CORBA::SystemException &ex)
	{
	ACSErrTypeCommon::CORBAProblemExImpl err = ACSErrTypeCommon::CORBAProblemExImpl(__FILE__,__LINE__,"BulkDataSender::startSend");
	err.setMinor(ex.minor());
	err.setCompletionStatus(ex.completed());
	err.setInfo(ex._info().c_str());
	ACSBulkDataError::AVSendFrameErrorExImpl err1 = ACSBulkDataError::AVSendFrameErrorExImpl(err,__FILE__,__LINE__,"BulkDataSender::startSend");
	throw err1;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataSender::startSend");
	ACSBulkDataError::AVSendFrameErrorExImpl err = ACSBulkDataError::AVSendFrameErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::startSend");
	throw err;
	}
}


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::sendData(CORBA::ULong flowNumber, ACE_Message_Block *buffer_p)
{
    ACS_TRACE("BulkDataSender<>::startSend(ACE_Message_Block *)");
    int debug_location=0;

    try
	{
	int res = 0;

	CORBA::ULong dim = flowSpec_m.length();

	if(flowNumber < 1 || flowNumber > dim)
	    {
	    ACSBulkDataError::AVInvalidFlowNumberExImpl err = ACSBulkDataError::AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataSender::sendData");
	    throw err;
	    }

	flowNumber--;

	AVStreams::flowSpec locSpec(1);
	locSpec.length(1);
	ACE_CString flowname = TAO_AV_Core::get_flowname(flowSpec_m[flowNumber]);
	locSpec[0] = flowSpec_m[flowNumber];

	streamctrl_p->start(locSpec);
	debug_location=1; // after 1st start

	TAO_AV_Protocol_Object *dp_p = 0;
	getFlowProtocol(flowname, dp_p);

	char tmp[255];
	ACE_OS::sprintf(tmp, "2 %u", (unsigned int)buffer_p->length()); 

	res = dp_p->send_frame(tmp, sizeof(tmp));
	debug_location=2; // after 1st send_frame
	if(res < 0)
	    {
	    ACSBulkDataError::AVSendFrameErrorExImpl err = ACSBulkDataError::AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::sendData");
	    throw err;
	    }

	streamctrl_p->stop(locSpec);
	debug_location=3; // after 1st stop

//	cout << "TTTTTTTTTT: lenbuf: " << buffer_p->length() << endl;
	res = dp_p->send_frame(buffer_p);
	debug_location=4; // after 2nd send_frame
	if(res < 0)
	    {
	    ACSBulkDataError::AVSendFrameErrorExImpl err = ACSBulkDataError::AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::sendData");
	    throw err;
	    }

	// ACE_High_Res_Timer elapsed_timer;
	// elapsed_timer.start ();
 
	//	acsQoS::Timeout tim(timeout);
	
	streamctrl_p->stop(locSpec);
	debug_location=5; // after 2nd stop
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	ACSBulkDataError::AVSendFrameErrorExImpl err = ACSBulkDataError::AVSendFrameErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::sendData");
	throw err;
	}
    catch(CORBA::BAD_OPERATION & bad)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::sendData User Exception"));
	ACSBulkDataError::AVSendFrameErrorExImpl err = ACSBulkDataError::AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::sendData");
	throw err;
	}
    catch(CORBA::TIMEOUT & tim)
	{
    	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::sendData TIMEOUT expired at location: %d", debug_location));
    	ACSBulkDataError::AVSendFrameErrorExImpl err = ACSBulkDataError::AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::sendData");
    	err.addData("DEBUG LOCATION", debug_location);
    	throw err;
	}
    catch(CORBA::SystemException &ex)
	{
	ACSErrTypeCommon::CORBAProblemExImpl err = ACSErrTypeCommon::CORBAProblemExImpl(__FILE__,__LINE__,"BulkDataSender::sendData");
	err.setMinor(ex.minor());
	err.setCompletionStatus(ex.completed());
	err.setInfo(ex._info().c_str());
	ACSBulkDataError::AVSendFrameErrorExImpl err1 = ACSBulkDataError::AVSendFrameErrorExImpl(err,__FILE__,__LINE__,"BulkDataSender::sendData");
	throw err1;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataSender::sendData");
	ACSBulkDataError::AVSendFrameErrorExImpl err = ACSBulkDataError::AVSendFrameErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::sendData");
	throw err;
	}

    // elapsed_timer.stop ();
    // ACE_Time_Value elapsed_time;
    // elapsed_timer.elapsed_time (elapsed_time);
    // cout << "Elapsed_time in msec " << elapsed_time.msec () << endl;
}


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::sendData(CORBA::ULong flowNumber, const char *buffer_p, size_t len)
{
    ACS_TRACE("BulkDataSender<>::startSend(const char *)");
    int debug_location=0;

    try
	{
	int res = 0;

	CORBA::ULong dim = flowSpec_m.length();

	if(flowNumber < 1 || flowNumber > dim)
	    {
	    ACSBulkDataError::AVInvalidFlowNumberExImpl err = ACSBulkDataError::AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataSender::sendData");
	    throw err;
	    }
	flowNumber--;

	AVStreams::flowSpec locSpec(1);
	locSpec.length(1);
	ACE_CString flowname = TAO_AV_Core::get_flowname(flowSpec_m[flowNumber]);
	locSpec[0] = flowSpec_m[flowNumber];
	
	streamctrl_p->start(locSpec);
	debug_location=1; // after 1st start

	TAO_AV_Protocol_Object *dp_p = 0;
	getFlowProtocol(flowname, dp_p);

	int locDim = 255;
	char tmp[locDim];
	ACE_OS::sprintf(tmp, "2 %u", (unsigned int)len); 
	res = dp_p->send_frame(tmp, locDim);
	debug_location=2; // after 1st send_frame
	if(res < 0)
	    {
	    ACSBulkDataError::AVSendFrameErrorExImpl err = ACSBulkDataError::AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::sendData");
	    throw err;
	    }

	streamctrl_p->stop(locSpec);
	debug_location=3; // after 1st stop

	res = dp_p->send_frame(buffer_p, len);
	debug_location=4; // after 2nd send_frame
	if(res < 0)
	    {
	    ACSBulkDataError::AVSendFrameErrorExImpl err = ACSBulkDataError::AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::sendData");
	    throw err;
	    }
	
	streamctrl_p->stop(locSpec);
	debug_location=5; // after 2nd stop
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	ACSBulkDataError::AVSendFrameErrorExImpl err = ACSBulkDataError::AVSendFrameErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::sendData");
	throw err;
	}
    catch(CORBA::BAD_OPERATION & bad)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::sendData User Exception"));
	ACSBulkDataError::AVSendFrameErrorExImpl err = ACSBulkDataError::AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::sendData");
	throw err;
	}
    catch(CORBA::TIMEOUT & tim)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::sendData TIMEOUT expired at location: %d", debug_location));
	ACSBulkDataError::AVSendFrameErrorExImpl err = ACSBulkDataError::AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::sendData");
	err.addData("DEBUG LOCATION", debug_location);
	throw err;
	}
    catch(CORBA::SystemException &ex)
	{
	ACSErrTypeCommon::CORBAProblemExImpl err = ACSErrTypeCommon::CORBAProblemExImpl(__FILE__,__LINE__,"BulkDataSender::sendData");
	err.setMinor(ex.minor());
	err.setCompletionStatus(ex.completed());
	err.setInfo(ex._info().c_str());
	ACSBulkDataError::AVSendFrameErrorExImpl err1 = ACSBulkDataError::AVSendFrameErrorExImpl(err,__FILE__,__LINE__,"BulkDataSender::sendData");
	throw err1;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataSender::sendData");
	ACSBulkDataError::AVSendFrameErrorExImpl err = ACSBulkDataError::AVSendFrameErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::sendData");
	throw err;
	}
}


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::stopSend(CORBA::ULong flowNumber)
{
    ACS_TRACE("BulkDataSender<>::stopSend");

    try 
	{
	CORBA::ULong dim = flowSpec_m.length();
	if(flowNumber < 1 || flowNumber > dim)
	    {
	    ACSBulkDataError::AVInvalidFlowNumberExImpl err = ACSBulkDataError::AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataSender::stopSend");
	    throw err;
	    }
	flowNumber--;
    
	AVStreams::flowSpec locSpec(1);
	locSpec.length(1);
	ACE_CString flowname = TAO_AV_Core::get_flowname(flowSpec_m[flowNumber]);
	locSpec[0] = flowSpec_m[flowNumber];
	
	streamctrl_p->stop(locSpec);
	}
    catch(CORBA::SystemException &ex)
	{
	ACSErrTypeCommon::CORBAProblemExImpl err = ACSErrTypeCommon::CORBAProblemExImpl(__FILE__,__LINE__,"BulkDataSender::stopSend");
	err.setMinor(ex.minor());
	err.setCompletionStatus(ex.completed());
	err.setInfo(ex._info().c_str());
	ACSBulkDataError::AVStopSendErrorExImpl err1 = ACSBulkDataError::AVStopSendErrorExImpl(err,__FILE__,__LINE__,"BulkDataSender::stopSend");
	throw err1;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataSender::stopSend");
	ACSBulkDataError::AVStopSendErrorExImpl err = ACSBulkDataError::AVStopSendErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::stopSend");
	throw err;
	}
}


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::disconnectPeer()
{
    ACS_TRACE("BulkDataSender<>::disconnectPeer");

    try
	{
	deleteConnector();

	deleteHandler();

	ACE_OS::sleep(1);  // seems necessary to give time to remove the reactor

	deleteStreamCtrl();

	ACE_OS::sleep(1); // idem: seems necessary to give time to remove the streamCtrl

	deleteFepsA();

	deleteSepA();
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	ACSBulkDataError::AVDisconnectErrorExImpl err = ACSBulkDataError::AVDisconnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::disconnectPeer");
	throw err;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataSender::disconnectPeer");
	ACSBulkDataError::AVDisconnectErrorExImpl err = ACSBulkDataError::AVDisconnectErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::disconnectPeer");
	throw err;
	}

    disconnectPeerFlag = true;
}


template<class TSenderCallback>
const char * AcsBulkdata::BulkDataSender<TSenderCallback>::getFlowSpec(const ACE_CString & flowName)
{
    ACS_TRACE("BulkDataSender<>::getFlowSpec");

    ACE_CString flowSpec;

    CORBA::ULong dim = flowSpec_m.length();

    for(CORBA::ULong i = 0; i < dim; i++)
	{
	
	ACE_CString flw = TAO_AV_Core::get_flowname(flowSpec_m[i]);
	if (flw == flowName)
	    {
	    flowSpec = CORBA::string_dup(flowSpec_m[i]);
	    
	    return CORBA::string_dup (flowSpec.c_str());
	    }
	else
	    {
//TBD error handling
	    flowSpec="";
	    }
	}

    return CORBA::string_dup (flowSpec.c_str());

}


template<class TSenderCallback>
std::vector<std::string> AcsBulkdata::BulkDataSender<TSenderCallback>::getFlowNames()
{
    ACE_TRACE("BulkDataSender<>::getFlowNames");

    std::vector<std::string> flwNames;
    ACE_CString flowname;

    for(CORBA::ULong i = 0; i < senderFeps_m.length(); i++)
	{
	flowname = TAO_AV_Core::get_flowname(senderFeps_m[i]);
	flwNames.push_back(flowname.c_str());
	}

    return flwNames;
}


/* THE FOLLOWING METHODS ARE UNDER TESTING - PLEASE DO NOT USE THEM */
/********************************************************************/

template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::startStream(CORBA::ULong flowNumber)
{
    ACE_TRACE("BulkDataSender<>::startStream");

    CORBA::ULong dim = flowSpec_m.length();

    if(flowNumber < 1 || flowNumber > dim)
	{
	ACSBulkDataError::AVInvalidFlowNumberExImpl err = ACSBulkDataError::AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataSender::startStream");
	throw err;
	}
    flowNumber--;

    AVStreams::flowSpec locSpec(1);
    locSpec.length(1);
    ACE_CString flowname = TAO_AV_Core::get_flowname(flowSpec_m[flowNumber]);
    locSpec[0] = flowSpec_m[flowNumber];

    streamctrl_p->start(locSpec);
}

template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::sendStream(CORBA::ULong flowNumber, ACE_Message_Block *buffer)
{
    ACE_TRACE("BulkDataSender<>::sendStream");

    int res = 0;
    
    CORBA::ULong dim = flowSpec_m.length();
    if(flowNumber < 1 || flowNumber > dim)
	{
	ACSBulkDataError::AVInvalidFlowNumberExImpl err = ACSBulkDataError::AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataSender::sendStream");
	throw err;
	}
    flowNumber--;

    ACE_CString flowname = TAO_AV_Core::get_flowname(flowSpec_m[flowNumber]);

    TAO_AV_Protocol_Object *dp_p = 0;
    getFlowProtocol(flowname, dp_p);

    res = dp_p->send_frame(buffer);
    if(res < 0)
	{
	ACSBulkDataError::AVSendFrameErrorExImpl err = ACSBulkDataError::AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::sendStream");
	throw err;
	}
}

template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::stopStream(CORBA::ULong flowNumber)
{
    ACE_TRACE("BulkDataSender<>::stopStream");

    CORBA::ULong dim = flowSpec_m.length();
    if(flowNumber < 1 || flowNumber > dim)
	{
	ACSBulkDataError::AVInvalidFlowNumberExImpl err = ACSBulkDataError::AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataSender::stopStream");
	throw err;
	}
    flowNumber--;

    AVStreams::flowSpec locSpec(1);
    locSpec.length(1);
    ACE_CString flowname = TAO_AV_Core::get_flowname(flowSpec_m[flowNumber]);
    locSpec[0] = flowSpec_m[flowNumber];

    streamctrl_p->stop(locSpec);
}

/********************************************************************/


/************************************* private part ******************************************/


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::initPartA()
{
    ACE_TRACE("BulkDataSender<>::initPartA");

    CORBA::ORB_var testOrb = TAO_AV_CORE::instance()->orb();
    if(CORBA::is_nil(testOrb.in()))
	{
	TAO_AV_CORE::instance()->init(BACI_CORBA::getORB(), BACI_CORBA::getPOARoot());
	}

    int result = endpointStrategy_m.init(TAO_AV_CORE::instance()->orb(),
					 TAO_AV_CORE::instance()->poa());
    if (result != 0)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::initPartA endpoint_strategy init failed"));
	ACSBulkDataError::AVInitErrorExImpl err = ACSBulkDataError::AVInitErrorExImpl(__FILE__,__LINE__,"BulkDataSender::initPartA");
	throw err;
	}
}


template<class TSenderCallback>
AVStreams::StreamEndPoint_A_ptr AcsBulkdata::BulkDataSender<TSenderCallback>::createSepA()
{
    ACE_TRACE("BulkDataSender<>::createSepA");

    TAO_StreamEndPoint_A *localSepA_p = new TAO_StreamEndPoint_A();
    if(localSepA_p == 0)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::createSepA Stream Endpoint null"));
	ACSBulkDataError::AVStreamEndpointErrorExImpl err = ACSBulkDataError::AVStreamEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSender::createSepA");
	throw err;
	}

    sepRefCount_p = localSepA_p;

    AVStreams::StreamEndPoint_A_var localSepObj_p = localSepA_p->_this();
    if(CORBA::is_nil(localSepObj_p.in()))
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::createSepA unable to activate Stream Endpoint"));
	ACSBulkDataError::AVStreamEndpointErrorExImpl err = ACSBulkDataError::AVStreamEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSender::createSepA");
	throw err;
	}
    
    return localSepObj_p._retn();
}


template<class TSenderCallback>
AVStreams::FlowProducer_ptr AcsBulkdata::BulkDataSender<TSenderCallback>::createFepProducerA(ACE_CString &flowname, AVStreams::protocolSpec protocols, ACE_CString &format, TAO_StreamCtrl *strctrl_p)
{
    ACE_TRACE("BulkDataSender<>::createFepProducerA");

    BulkDataFlowProducer<TSenderCallback> *localFepA_p = new BulkDataFlowProducer<TSenderCallback>(flowname.c_str(), protocols, format.c_str(), strctrl_p);
    if(localFepA_p == 0)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::createFepProducerA Flow Producer null"));
	ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSender::createFepProducerA");
	throw err;
	}

    AVStreams::FlowProducer_var localFepObj_p = localFepA_p->_this(); 
    if (CORBA::is_nil(localFepObj_p.in()))
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::createFepProducerA unable to activate Flow Producer"));
	ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSender::createFepProducerA");
	throw err;
	}

    //fepMap_m.bind(flowname, AVStreams::FlowEndPoint::_duplicate(localFepObj_p.in())); 
    fepMap_m.bind(flowname, localFepA_p); 

    return localFepObj_p._retn();
}


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::addFepToSep(AVStreams::StreamEndPoint_A_ptr locSepA_p, AVStreams::FlowProducer_ptr locFepA_p)
{
    ACE_TRACE("BulkDataSender<>::addFepToSep");

    CORBA::String_var s1 = locSepA_p->add_fep(locFepA_p);
    if(s1 == 0)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::addFepToSep Flow Endpoint cannot be created"));
	ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSender::addFepToSep");
	throw err;
	}

    ACS_SHORT_LOG((LM_INFO,"BulkDataSender<>::addFepToSep Added flowendpoint named: %s", s1.in()));
}


template<class TSenderCallback>
TAO_StreamCtrl * AcsBulkdata::BulkDataSender<TSenderCallback>::createStreamCtrl()  
{
    ACE_TRACE("BulkDataSender<>::createStreamCtrl");

    TAO_StreamCtrl *locStrctrl_p = new TAO_StreamCtrl();
    if(locStrctrl_p == 0)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::createStreamCtrl locStrctrl_p not initialized."));
	ACSBulkDataError::AVStreamEndpointErrorExImpl err = ACSBulkDataError::AVStreamEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSender::createStreamCtrl");
	throw err;
	}

    return locStrctrl_p;
}


/*
  template<class TSenderCallback>
  AVStreams::streamQoS_ptr AcsBulkdata::BulkDataSender<TSenderCallback>::create_QoS()  
  {
  ACE_TRACE("BulkDataSender<>::create_QoS");

  AVStreams::streamQoS_var loc_qos(new AVStreams::streamQoS);

  if (CORBA::is_nil(loc_qos.in()))
  {
  ACS_SHORT_LOG((LM_INFO,"BulkDataSender<>::create_QoS loc_qos not initialized."));
  ACSBulkDataError::AVStreamEndpointErrorExImpl err = ACSBulkDataError::AVStreamEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSender::create_QoS");
  throw err;
  }

  return loc_qos._retn();
  }
*/


template<class TSenderCallback>
const char * AcsBulkdata::BulkDataSender<TSenderCallback>::createFwdFlowSpec(ACE_CString &flowname,
									     ACE_CString &direction,
									     ACE_CString &formatName,
									     ACE_CString &flowProtocol,
									     ACE_CString &carrierProtocol,
									     ACE_CString &localAddress,
									     ACE_CString &remoteAddress)
{
    ACE_INET_Addr locAddr(localAddress.c_str());
    ACE_INET_Addr remAddr(remoteAddress.c_str());


    TAO_Forward_FlowSpec_Entry entry(flowname.c_str(),
				     direction.c_str(),
				     formatName.c_str(),
				     flowProtocol.c_str(),
				     carrierProtocol.c_str(),
				     &locAddr);
  
    entry.set_peer_addr(&remAddr);

    return CORBA::string_dup(entry.entry_to_string());
}


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::setReceiverConfig(bulkdata::BulkDataReceiverConfig *recvConfig_p)
{
    ACS_TRACE("BulkDataSender<>:::setReceiverConfig");   

    sepB_p = recvConfig_p->streamendpoint_B;
    if(CORBA::is_nil(sepB_p.in()))
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::setReceiverConfig Stream Endpoint not initialized"));
	ACSBulkDataError::AVReceiverConfigErrorExImpl err = ACSBulkDataError::AVReceiverConfigErrorExImpl(__FILE__,__LINE__,"BulkDataSender::setReceiverConfig");
	throw err;
	}

    CORBA::ULong dim = recvConfig_p->fepsInfo.length();
    if(dim == 0)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::setReceiverConfig Flow Specifications empty"));
	ACSBulkDataError::AVReceiverConfigErrorExImpl err = ACSBulkDataError::AVReceiverConfigErrorExImpl(__FILE__,__LINE__,"BulkDataSender::setReceiverConfig");
	throw err;
	}
    
    recvFeps_p = new AVStreams::flowSpec;
    if (recvFeps_p == 0)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::setReceiverConfig error creating Flow Specifications"));
	ACSBulkDataError::AVReceiverConfigErrorExImpl err = ACSBulkDataError::AVReceiverConfigErrorExImpl(__FILE__,__LINE__,"BulkDataSender::setReceiverConfig");
	throw err;
	}

    recvFeps_p->length(dim);

    for(CORBA::ULong i = 0; i < dim; i++)
	{ 
	recvFeps_p[i] = recvConfig_p->fepsInfo[i];
	}
}


template<class TSenderCallback>
AVStreams::StreamEndPoint_A_ptr AcsBulkdata::BulkDataSender<TSenderCallback>::getStreamEndPointA()
{
    ACS_TRACE("BulkDataSender<>::getStreamEndPointA");   

    if(sepA_p.in() == 0)
	{
	return AVStreams::StreamEndPoint_A::_nil();
	}

    return sepA_p._retn();
}


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::deleteStreamCtrl()
{
    ACS_TRACE("BulkDataSender<>::deleteStreamCtrl");   

    if (streamctrl_p != 0)
	{
	AVStreams::flowSpec nilSpec;
	streamctrl_p->destroy(nilSpec);
	CORBA::Long dim = streamctrl_p->_refcount_value();

	for(CORBA::Long n = 1; n < dim; n++)
	    {
	    TAO_AV_Core::deactivate_servant(streamctrl_p);
	    }

	if(streamctrl_p != 0)
	    streamctrl_p->_remove_ref();

	streamctrl_p = 0;
	}
}


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::deleteFepsA()
{
    ACS_TRACE("BulkDataSender<>::deleteFepsA");   

    for(CORBA::ULong i = 0; i < senderFeps_m.length(); i++)
	{
	ACE_CString flowname = TAO_AV_Core::get_flowname(senderFeps_m[i]);

	if (sepRefCount_p != 0)
	    {
	    sepRefCount_p->remove_fep(flowname.c_str());
	    }

	BulkDataFlowProducer<TSenderCallback> *fep = 0;
	fepMap_m.find(flowname, fep);
	if(fep == 0)
	    {
	    disconnectPeerFlag = true; //necessary to avoid container crash
	    ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSender<>::deleteFepsA");
	    throw err;
	    }
	else
	    {
	    TSenderCallback *cb_p = fep->getBulkDataCallback();
	    if(cb_p == 0)
		{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::deleteFepsA callback null"));
		ACSBulkDataError::AVCallbackErrorExImpl err = ACSBulkDataError::AVCallbackErrorExImpl(__FILE__,__LINE__,"BulkDataSender<>::deleteFepsA");
		throw err;
		}
	    else
		{
		if(cb_p->isFepAlive())
		    fep->destroy();
		}

	    CORBA::Long dim = fep->_refcount_value/*_ref_count*/();
	    
	    for(CORBA::Long n = 1; n < dim; n++)
		{
		TAO_AV_Core::deactivate_servant(fep);
		}

// fep is always != NULL; TBC
	    if (fep != 0) fep->_remove_ref();

	    fepMap_m.unbind(flowname, fep);
	    }
	
	//TAO_AV_CORE::instance ()->remove_connector(flowname.c_str());
	}
}


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::deleteSepA()
{
    ACS_TRACE("BulkDataSender<>::deleteSepA");
    
    if (sepRefCount_p != 0)
	{
	CORBA::Long dim = sepRefCount_p->_refcount_value/*_ref_count*/();
	for(CORBA::Long n = 1; n < dim; n++)
	    {
	    TAO_AV_Core::deactivate_servant(sepRefCount_p);
	    }

// sepRefCount_p is always != NULL; TBC
	if (sepRefCount_p != 0) sepRefCount_p->_remove_ref();
	sepRefCount_p=0;
	}

    sepA_p = AVStreams::StreamEndPoint_A::_nil();
   
/* 
   PortableServer::ServantBase_var sep_a_servant =
   TAO_AV_CORE::instance()->poa()->reference_to_servant ( sep_a_.in() );
   
   int aresult =TAO_AV_Core::deactivate_servant (sep_a_servant.in());

   PortableServer::ServantBase_var sep_b_servant =
   TAO_AV_CORE::instance()->poa()->reference_to_servant ( sep_b_.in() );
   
   int bresult =TAO_AV_Core::deactivate_servant (sep_b_servant.in());
*/
}


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::deleteConnector()
{
    ACS_TRACE("BulkDataSender<>::deleteConnector");   

    for(CORBA::ULong i = 0; i <  senderFeps_m.length(); i++)
	{
	ACE_CString flowname = TAO_AV_Core::get_flowname(senderFeps_m[i]);
	TAO_AV_CORE::instance()->remove_connector(flowname.c_str());
	}
}



template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::deleteHandler()
{
    ACS_TRACE("BulkDataSender<>::deleteHandler");
  
    for(CORBA::ULong i = 0; i < senderFeps_m.length(); i++)
	{
	ACE_CString flowname = TAO_AV_Core::get_flowname(senderFeps_m[i]);
        BulkDataFlowProducer<TSenderCallback> *fep = 0;
    
        fepMap_m.find(flowname, fep);

	if(fep == 0)
	    {
	    ACSBulkDataError::AVFlowEndpointErrorExImpl err = ACSBulkDataError::AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSender::deleteHandler");
	    throw err;
	    }
        else
            {
	    ACE_HANDLE handle;
	    if((handleMap_m.find(flowname,handle)) == -1)
		{
		//ACS_SHORT_LOG((LM_INFO,"BulkDataSender<>::deleteHandler - handle not present in HandleMap"));
		//return; // to be better defined what to do here
		continue;
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
		    //ACS_SHORT_LOG((LM_INFO,"BulkDataSender<>::deleteHandler - handle not present in the ACE_Reactor"));
		    handleMap_m.unbind(flowname,handle);	    
		    continue;
		    }
		}
	    }
	}
}


template<class TSenderCallback>
const char * AcsBulkdata::BulkDataSender<TSenderCallback>::createFlowSpec(ACE_CString &flowname,
									  ACE_CString &fepProtocol)
{
    ACS_TRACE("BulkDataSender<>::createFlowSpec");

    ACE_CString direction = "";
    ACE_CString formatName = "";
    ACE_CString flowProtocol = "";

    TAO_Tokenizer address(fepProtocol.c_str(), '=');
    ACE_CString localAddress = CORBA::string_dup(address[1]);
    ACE_CString remoteAddress = "";
    ACE_CString carrierProtocol = CORBA::string_dup(address[0]);

    ACE_INET_Addr locAddr(localAddress.c_str());
    ACE_INET_Addr remAddr(remoteAddress.c_str());

    TAO_Forward_FlowSpec_Entry entry(flowname.c_str(),
				     direction.c_str(),
				     formatName.c_str(),
				     flowProtocol.c_str(),
				     carrierProtocol.c_str(),
				     &locAddr);
  
    entry.set_peer_addr(&remAddr);

    return CORBA::string_dup(entry.entry_to_string());
} 


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::mergeFlowSpecs()
{
    ACS_TRACE("BulkDataSender<>::mergeFlowSpecs");

    try
	{
	CORBA::ULong senderDim = senderFeps_m.length();
	CORBA::ULong receiverDim = recvFeps_p->length();

	if(senderDim != receiverDim)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::mergeFlowSpecs sender and receiver flow numbers not matching"));	
	    ACSBulkDataError::AVFlowNumbersNotMatchingErrorExImpl err = ACSBulkDataError::AVFlowNumbersNotMatchingErrorExImpl(__FILE__,__LINE__,"BulkDataSender::mergeFlowSpecs");
	    throw err;
	    }

	flowSpec_m.length(senderDim);

	ACE_CString flowname;  
	ACE_CString direction = "IN";
	ACE_CString formatName = "USER_DEFINED";
	ACE_CString flowProtocol = "";
	ACE_CString carrierProtocol;
	ACE_CString localAddress;
	ACE_CString remoteAddress;

	ACE_INET_Addr *peerAddr_p = new ACE_INET_Addr();
	char buf[BUFSIZ];

	for(CORBA::ULong i = 0; i < senderDim; i++)
	    {
	    TAO_Forward_FlowSpec_Entry senderEntry;
	    TAO_Forward_FlowSpec_Entry recvEntry;

	    int is = senderEntry.parse(senderFeps_m[i]);
	    if(is != 0)
		{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::mergeFlowSpecs sender_protocols[%d] CDB entry not correct",i));
		ACSBulkDataError::AVStreamBindErrorExImpl err = ACSBulkDataError::AVStreamBindErrorExImpl(__FILE__,__LINE__,"BulkDataSender::mergeFlowSpecs");
		throw err;	
		}

	    int ir = recvEntry.parse(recvFeps_p[i]);
	    if(ir != 0)
		{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::mergeFlowSpecs recv_protocols[%d] CDB entry not correct",i));
		ACSBulkDataError::AVStreamBindErrorExImpl err = ACSBulkDataError::AVStreamBindErrorExImpl(__FILE__,__LINE__,"BulkDataSender::mergeFlowSpecs");
		throw err;	
		}

	    flowname = CORBA::string_dup(senderEntry.flowname());
	    carrierProtocol = CORBA::string_dup(senderEntry.carrier_protocol_str());
	    localAddress = CORBA::string_dup(senderEntry.address_str());
      
	    peerAddr_p = dynamic_cast<ACE_INET_Addr *>(recvEntry.get_peer_addr());
	    peerAddr_p->addr_to_string(buf, BUFSIZ);
	    remoteAddress = buf;

	    const char *locEntry = createFwdFlowSpec(flowname,
						     direction,
						     formatName,
						     flowProtocol,
						     carrierProtocol,
						     localAddress,
						     remoteAddress);

	    flowSpec_m[i] = CORBA::string_dup(locEntry);
	    }

	//  delete peerAddr_p;

	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	ACSBulkDataError::AVStreamBindErrorExImpl err = ACSBulkDataError::AVStreamBindErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::mergeFlowSpecs");
	throw err;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataSender::mergeFlowSpecs");
	ACSBulkDataError::AVStreamBindErrorExImpl err = ACSBulkDataError::AVStreamBindErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::mergeFlowSpecs");
	throw err;
	}
}


template<class TSenderCallback>
ACE_HANDLE AcsBulkdata::BulkDataSender<TSenderCallback>::findHandle(ACE_CString &flowname)
{
    ACS_TRACE("BulkDataSender<>::findHandle");

    ACE_HANDLE handle;
    if((handleMap_m.find(flowname,handle)) == -1)
	{
	//ACS_SHORT_LOG((LM_DEBUG,"handle not in the handle map"));
	return -1;
	}
    else
	{
	if(TAO_AV_CORE::instance()->reactor()->find_handler(handle))
	    {
	    return handle;
	    }
	else
	    {
	    ACS_SHORT_LOG((LM_DEBUG,"handle in the handle map but not registered in the ACE Reactor"));
	    handleMap_m.unbind(flowname,handle);
	    return -1;
	    }
	}
}
