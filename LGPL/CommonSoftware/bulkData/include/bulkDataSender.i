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

    if(disconnectPeerFlag == false)
	{
//	disconnectPeer();

// we do not remove the handler; this will
// cause problems if the user forget to call disconnect,
// but at least the container will not crash when an exception occurs

	deleteConnector();
	deleteStreamCtrl();
	deleteFepsA();
	deleteSepA();

	}
}


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::initialize()
{
    ACE_TRACE("BulkDataSender<>::initialize");

    initPartA();
}


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::createSingleFlow()
{
    ACE_TRACE("BulkDataSender<>::createSingleFlow");

    streamctrl_p = createStreamCtrl();

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


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::createMultipleFlows(const char *fepsConfig)
{
    ACE_TRACE("BulkDataSender<>::createMultipleFlows");

    if(ACE_OS::strcmp(fepsConfig, "") == 0)
	{
	createSingleFlow();
	return;
	}

    streamctrl_p = createStreamCtrl();

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
	AVInvalidFlowNumberExImpl err = AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataSender::createMultipleFlows");
	throw err;	
	}

    senderFeps_m.length(numOtherFeps);

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

	fepObj_p = createFepProducerA(localStruct.fepsFlowname, defProt, localStruct.fepsFormat, streamctrl_p); 

	addFepToSep(sepA_p.in(), fepObj_p.in());

	const char *locEntry = createFlowSpec(localStruct.fepsFlowname, localStruct.fepsProtocol);

	senderFeps_m[j] = CORBA::string_dup(locEntry);
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
	    AVStreamBindErrorExImpl err = AVStreamBindErrorExImpl(__FILE__,__LINE__,"BulkDataSender::connectToPeer");
	    throw err;
	    }
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVConnectErrorExImpl err = AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataSender::connectToPeer");
	throw err;
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::connectToPeer UNKNOWN exception"));
	AVConnectErrorExImpl err = AVConnectErrorExImpl(__FILE__,__LINE__,"BulkDataSender::connectToPeer");
	throw err;
	}

    // it seems necessary beacuse the bind method is not compeltely synchronous, i.e. it exits even if the connection is not fully established. TBA
    ACE_OS::sleep(1);
}


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::getFlowProtocol(ACE_CString &flowname, TAO_AV_Protocol_Object *&currentProtocol_p)
{
//    ACS_TRACE("BulkDataSender<>::getFlowProtocol");   

    BulkDataFlowProducer<TSenderCallback> *fep_p = 0;
    fepMap_m.find(flowname, fep_p);
    
    if(fep_p == 0)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::getFlowProtocol Flow protocol null"));
	AVFlowEndpointErrorExImpl err = AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSender::getFlowProtocol");
	throw err;
	} 
    else
	currentProtocol_p = fep_p->getProtocolObject();

    if(currentProtocol_p == 0)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::getFlowProtocol Protocol object null"));
	AVProtocolErrorExImpl err = AVProtocolErrorExImpl(__FILE__,__LINE__,"BulkDataSender::getFlowProtocol");
	throw err;
	}
}


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::startSend(CORBA::ULong flowNumber, ACE_Message_Block *param_p)
{
    ACS_TRACE("BulkDataSender<>::startSend(ACE_Message_Block *)");

    int res = 0;

    CORBA::ULong dim = flowSpec_m.length();

    if(flowNumber < 1 || flowNumber > dim)
	{
	AVInvalidFlowNumberExImpl err = AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataSender::startSend");
	throw err;
	}

    flowNumber--;

    try
	{
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
	    AVSendFrameErrorExImpl err = AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::startSend");
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
	    AVSendFrameErrorExImpl err = AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::startSend");
	    throw err;
	    }

	streamctrl_p->stop(locSpec);	
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVSendFrameErrorExImpl err = AVSendFrameErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::startSend");
	throw err;
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::startSend UNKNOWN exception"));
	AVSendFrameErrorExImpl err = AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::startSend");
	throw err;
	}
}


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::startSend(CORBA::ULong flowNumber , const char *param_p, size_t len)
{
    ACS_TRACE("BulkDataSender<>::startSend(const char *)");

    int res = 0;

    CORBA::ULong dim = flowSpec_m.length();

    if(flowNumber < 1 || flowNumber > dim)
	{
	AVInvalidFlowNumberExImpl err = AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataSender::startSend");
	throw err;
	}

    flowNumber--;

    try
	{
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
	    AVSendFrameErrorExImpl err = AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::startSend");
	    throw err;
	    }

	streamctrl_p->stop(locSpec);
	
	res = dp_p->send_frame(param_p, len);
	if(res < 0)
	    {
	    AVSendFrameErrorExImpl err = AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::startSend");
	    throw err;
	    }

	streamctrl_p->stop(locSpec);
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVSendFrameErrorExImpl err = AVSendFrameErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::startSend");
	throw err;
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::startSend UNKNOWN exception"));
	AVSendFrameErrorExImpl err = AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::startSend");
	throw err;
	}
}


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::sendData(CORBA::ULong flowNumber, ACE_Message_Block *buffer_p)
{
    ACS_TRACE("BulkDataSender<>::startSend(ACE_Message_Block *)");

    int res = 0;

    CORBA::ULong dim = flowSpec_m.length();

    if(flowNumber < 1 || flowNumber > dim)
	{
	AVInvalidFlowNumberExImpl err = AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataSender::sendData");
	throw err;
	}

    flowNumber--;

    try
	{
	AVStreams::flowSpec locSpec(1);
	locSpec.length(1);
	ACE_CString flowname = TAO_AV_Core::get_flowname(flowSpec_m[flowNumber]);
	locSpec[0] = flowSpec_m[flowNumber];

	streamctrl_p->start(locSpec);

	TAO_AV_Protocol_Object *dp_p = 0;
	getFlowProtocol(flowname, dp_p);

	char tmp[255];
	ACE_OS::sprintf(tmp, "2 %u", (unsigned int)buffer_p->length()); 

	res = dp_p->send_frame(tmp, sizeof(tmp));
	if(res < 0)
	    {
	    AVSendFrameErrorExImpl err = AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::sendData");
	    throw err;
	    }

	streamctrl_p->stop(locSpec);

//	cout << "TTTTTTTTTT: lenbuf: " << buffer_p->length() << endl;
	res = dp_p->send_frame(buffer_p);
	if(res < 0)
	    {
	    AVSendFrameErrorExImpl err = AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::sendData");
	    throw err;
	    }

	// ACE_High_Res_Timer elapsed_timer;
	// elapsed_timer.start ();
 
	//	acsQoS::Timeout tim(timeout);
	
	streamctrl_p->stop(locSpec);
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVSendFrameErrorExImpl err = AVSendFrameErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::sendData");
	throw err;
	}
    catch(CORBA::BAD_OPERATION & bad)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::sendData User Exception catched!"));
	AVSendFrameErrorExImpl err = AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::sendData");
	throw err;
	}
    catch(CORBA::TIMEOUT & tim)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::sendData TIMEOUT expired"));
	AVSendFrameErrorExImpl err = AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::sendData");
	throw err;
	}
    catch(...)
	{
	AVSendFrameErrorExImpl err = AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::sendData");
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

    int res = 0;

    CORBA::ULong dim = flowSpec_m.length();

    if(flowNumber < 1 || flowNumber > dim)
	{
	AVInvalidFlowNumberExImpl err = AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataSender::sendData");
	throw err;
	}
    flowNumber--;

    try
	{
	AVStreams::flowSpec locSpec(1);
	locSpec.length(1);
	ACE_CString flowname = TAO_AV_Core::get_flowname(flowSpec_m[flowNumber]);
	locSpec[0] = flowSpec_m[flowNumber];
	
	streamctrl_p->start(locSpec);

	TAO_AV_Protocol_Object *dp_p = 0;
	getFlowProtocol(flowname, dp_p);

	int locDim = 255;
	char tmp[locDim];
	ACE_OS::sprintf(tmp, "2 %u", (unsigned int)len); 
	res = dp_p->send_frame(tmp, locDim);
	if(res < 0)
	    {
	    AVSendFrameErrorExImpl err = AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::sendData");
	    throw err;
	    }

	streamctrl_p->stop(locSpec);

	res = dp_p->send_frame(buffer_p, len);
	if(res < 0)
	    {
	    AVSendFrameErrorExImpl err = AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::sendData");
	    throw err;
	    }
	
	streamctrl_p->stop(locSpec);
	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	AVSendFrameErrorExImpl err = AVSendFrameErrorExImpl(ex,__FILE__,__LINE__,"BulkDataSender::sendData");
	throw err;
	}
    catch(CORBA::BAD_OPERATION & bad)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::sendData User Exception catched!"));
	AVSendFrameErrorExImpl err = AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::sendData");
	throw err;
	}
    catch(CORBA::TIMEOUT & tim)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::sendData TIMEOUT expired"));
	AVSendFrameErrorExImpl err = AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::sendData");
	throw err;
	}
    catch(...)
	{
	AVSendFrameErrorExImpl err = AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::sendData");
	throw err;
	}
}


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::stopSend(CORBA::ULong flowNumber)
{
    ACS_TRACE("BulkDataSender<>::stopSend");

    CORBA::ULong dim = flowSpec_m.length();
    if(flowNumber < 1 || flowNumber > dim)
	{
	AVInvalidFlowNumberExImpl err = AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataSender::stopSend");
	throw err;
	}
    flowNumber--;
    
    try 
	{
	AVStreams::flowSpec locSpec(1);
	locSpec.length(1);
	ACE_CString flowname = TAO_AV_Core::get_flowname(flowSpec_m[flowNumber]);
	locSpec[0] = flowSpec_m[flowNumber];
	
	streamctrl_p->stop(locSpec);
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::stopSend UNKNOWN exception"));
	AVStopSendErrorExImpl err = AVStopSendErrorExImpl(__FILE__,__LINE__,"BulkDataSender::stopSend");
	throw err;
	}
}


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::disconnectPeer()
{
    ACS_TRACE("BulkDataSender<>::disconnectPeer");

    //cout << "BulkDataSender<>::disconnectPeer" << endl;

    deleteConnector();
    //cout << "BulkDataSender<>::disconnectPeer after deleteConnector" << endl;
    deleteHandler();
    //cout << "BulkDataSender<>::disconnectPeer after deleteHandler" << endl;

    ACE_OS::sleep(1);  // seems necessary to give time to remove the reactor

    deleteStreamCtrl();

    ACE_OS::sleep(1); // idem: seems necessary to give time to remove the streamCtrl

    deleteFepsA();
    deleteSepA();

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
vector<string> AcsBulkdata::BulkDataSender<TSenderCallback>::getFlowNames()
{
    ACE_TRACE("BulkDataSender<>::getFlowNames");

    vector<string> flwNames;
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
	AVInvalidFlowNumberExImpl err = AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataSender::startStream");
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
	AVInvalidFlowNumberExImpl err = AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataSender::sendStream");
	throw err;
	}
    flowNumber--;

    ACE_CString flowname = TAO_AV_Core::get_flowname(flowSpec_m[flowNumber]);

    TAO_AV_Protocol_Object *dp_p = 0;
    getFlowProtocol(flowname, dp_p);

    res = dp_p->send_frame(buffer);
    if(res < 0)
	{
	AVSendFrameErrorExImpl err = AVSendFrameErrorExImpl(__FILE__,__LINE__,"BulkDataSender::sendStream");
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
	AVInvalidFlowNumberExImpl err = AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataSender::stopStream");
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

    int result = endpointStrategy_m.init(TAO_AV_CORE::instance()->orb (),
					 TAO_AV_CORE::instance()->poa ());
    if (result != 0)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::initPartA endpoint_strategy init failed"));
	AVInitErrorExImpl err = AVInitErrorExImpl(__FILE__,__LINE__,"BulkDataSender::initPartA");
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
	AVStreamEndpointErrorExImpl err = AVStreamEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSender::createSepA");
	throw err;
	}

    sepRefCount_p = localSepA_p;

    AVStreams::StreamEndPoint_A_var localSepObj_p = localSepA_p->_this();
    if (CORBA::is_nil(localSepObj_p.in()))
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::createSepA unable to activate Stream Endpoint"));
	AVStreamEndpointErrorExImpl err = AVStreamEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSender::createSepA");
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
	AVFlowEndpointErrorExImpl err = AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSender::createFepProducerA");
	throw err;
	}

    AVStreams::FlowProducer_var localFepObj_p = localFepA_p->_this(); 
    if (CORBA::is_nil(localFepObj_p.in()))
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::createFepProducerA unable to activate Flow Producer"));
	AVFlowEndpointErrorExImpl err = AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSender::createFepProducerA");
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
	AVFlowEndpointErrorExImpl err = AVFlowEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSender::addFepToSep");
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
	AVStreamEndpointErrorExImpl err = AVStreamEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSender::createStreamCtrl");
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
  AVStreamEndpointErrorExImpl err = AVStreamEndpointErrorExImpl(__FILE__,__LINE__,"BulkDataSender::create_QoS");
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
  
    //cout << "!!!!!!!!!!!!!!!!!!!!!!!" << flowname.c_str() << endl;
    //cout << "!!!!!!!!!!!!!!!!!!!!!!!" << direction.c_str() << endl;
    //cout << "!!!!!!!!!!!!!!!!!!!!!!!" << formatName.c_str() << endl;
    //cout << "!!!!!!!!!!!!!!!!!!!!!!!" << localAddress.c_str () << endl;
    //cout << "!!!!!!!!!!!!!!!!!!!!!!!" << remoteAddress.c_str () << endl;

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
	AVReceiverConfigErrorExImpl err = AVReceiverConfigErrorExImpl(__FILE__,__LINE__,"BulkDataSender::setReceiverConfig");
	throw err;
	}

    CORBA::ULong dim = recvConfig_p->fepsInfo.length();
    if(dim == 0)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::setReceiverConfig Flow Specifications empty"));
	AVReceiverConfigErrorExImpl err = AVReceiverConfigErrorExImpl(__FILE__,__LINE__,"BulkDataSender::setReceiverConfig");
	throw err;
	}
    
    recvFeps_p = new AVStreams::flowSpec;
    if (recvFeps_p == 0)
	{
	ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::setReceiverConfig error creating Flow Specifications"));
	AVReceiverConfigErrorExImpl err = AVReceiverConfigErrorExImpl(__FILE__,__LINE__,"BulkDataSender::setReceiverConfig");
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
	if (fep != 0)
	    {
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

        if (fep != 0)
            {
 
            TAO_AV_Flow_Handler * locHandler_p = fep->getFlowHandler();
            ACE_Event_Handler *event_handler = locHandler_p ->event_handler();
            TAO_AV_CORE::instance()->reactor()->remove_handler(event_handler,ACE_Event_Handler::READ_MASK );
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
  
    //cout << "!!!!!!!!!!!!!!!!!!!!!!!" << flowname.c_str() << endl;
    //cout << "!!!!!!!!!!!!!!!!!!!!!!!" << direction.c_str() << endl;
    //cout << "!!!!!!!!!!!!!!!!!!!!!!!" << formatName.c_str() << endl;
    //cout << "!!!!!!!!!!!!!!!!!!!!!!!" << localAddress.c_str () << endl;
    //cout << "!!!!!!!!!!!!!!!!!!!!!!!" << remoteAddress.c_str () << endl;

    entry.set_peer_addr(&remAddr);

    return CORBA::string_dup(entry.entry_to_string());
} 


template<class TSenderCallback>
void AcsBulkdata::BulkDataSender<TSenderCallback>::mergeFlowSpecs()
{
    ACS_TRACE("BulkDataSender<>::mergeFlowSpecs");

    CORBA::ULong dim = senderFeps_m.length();
    flowSpec_m.length(dim);

    ACE_CString flowname;  
    ACE_CString direction = "IN";
    ACE_CString formatName = "USER_DEFINED";
    ACE_CString flowProtocol = "";
    ACE_CString carrierProtocol;
    ACE_CString localAddress;
    ACE_CString remoteAddress;

    ACE_INET_Addr *peerAddr_p = new ACE_INET_Addr();
    char buf[BUFSIZ];

    for(CORBA::ULong i = 0; i < dim; i++)
	{
	TAO_Forward_FlowSpec_Entry senderEntry;
	TAO_Forward_FlowSpec_Entry recvEntry;

	int is = senderEntry.parse(senderFeps_m[i]);
	if(is != 0)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::mergeFlowSpecs sender_protocols[%d] CDB entry not correct",i));
	    AVStreamBindErrorExImpl err = AVStreamBindErrorExImpl(__FILE__,__LINE__,"BulkDataSender::mergeFlowSpecs");
	    throw err;	
	    }

	int ir = recvEntry.parse(recvFeps_p[i]);
	if(ir != 0)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataSender<>::mergeFlowSpecs recv_protocols[%d] CDB entry not correct",i));
	    AVStreamBindErrorExImpl err = AVStreamBindErrorExImpl(__FILE__,__LINE__,"BulkDataSender::mergeFlowSpecs");
	    throw err;	
	    }

	//cout << "JJJJJJJJJJJJJJJJJJJJJJJ: " << entry.format() << endl;
	//cout << "JJJJJJJJJJJJJJJJJJJJJJJ: " << entry.flowname() << endl;

	flowname = CORBA::string_dup(senderEntry.flowname());
	carrierProtocol = CORBA::string_dup(senderEntry.carrier_protocol_str());
	localAddress = CORBA::string_dup(senderEntry.address_str());
      
	peerAddr_p = ACE_dynamic_cast(ACE_INET_Addr *, recvEntry.get_peer_addr());
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
