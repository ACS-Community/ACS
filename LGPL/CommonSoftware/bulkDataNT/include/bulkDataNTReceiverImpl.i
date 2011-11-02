
template<class TCallback>
BulkDataNTReceiverImpl<TCallback>::BulkDataNTReceiverImpl(const ACE_CString& name,maci::ContainerServices* containerServices) :
	CharacteristicComponentImpl(name,containerServices)
{
	ACS_TRACE("BulkDataNTReceiverImpl<>::BulkDataNTReceiverImpl");
	containerServices_p = containerServices;
}//BulkDataNTReceiverImpl

template<class TCallback>
BulkDataNTReceiverImpl<TCallback>::~BulkDataNTReceiverImpl()
{
	ACS_TRACE("BulkDataNTReceiverImpl<>::~BulkDataNTReceiverImpl");
}//~BulkDataNTReceiverImpl


template<class TCallback>
void BulkDataNTReceiverImpl<TCallback>::initialize()
{
	ACS_TRACE("BulkDataNTReceiverImpl<>::initialize");

	char buf[BUFSIZ];

	// get the DAO and read the alma/ branch of the component out from the CDB
	CDB::DAL_ptr dal_p = containerServices_p->getCDB();
	if(CORBA::is_nil(dal_p))
	{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataNTReceiverImpl<>::initialize error getting DAL reference"));
		ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataNTReceiverImpl::openReceiver");
		throw err.getAVOpenReceiverErrorEx();
	}

	ACE_CString CDBpath="alma/";
	CDBpath += name();

	CDB::DAO_ptr dao_p = dal_p->get_DAO_Servant(CDBpath.c_str());
	if(CORBA::is_nil(dao_p))
	{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataNTReceiverImpl<>::initialize error getting DAO reference"));
		ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataNTReceiverImpl::openReceiver");
		throw err.getAVOpenReceiverErrorEx();
	}

	// Check which kind of configuration we have (the old "TCP"-based one, or the new schema-based one)
	char *recv_protocols = 0;
	bool useNewConfigMechanism = false;
	try {
		recv_protocols = dao_p->get_string("recv_protocols");
		ACE_OS::strcpy(buf,recv_protocols);
		// TODO: put log tating that old config mechanism is going to be used instead of the new one
	} catch(cdbErrType::CDBFieldDoesNotExistEx &ex) {
		useNewConfigMechanism = true;
	}

	if( useNewConfigMechanism ) {

		// Use the new mechanism to parse the XML document and create the streams/flows
		char *xmlNode = dal_p->get_DAO(CDBpath.c_str());
		BulkDataConfigurationParser parser;

		try {
			// Get the streams and put them on the map
			parser.parseReceiverConfig(xmlNode, recvConfigMap_m);
			if( recvConfigMap_m.size() == 0 ) {
				ACS_SHORT_LOG((LM_NOTICE,"BulkDataNTReceiverImpl<>::initialize No Receiver Streams configured, streams created in the future will have a default configuration"));
			}
		} catch(CDBProblemExImpl &ex) {
			ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataNTReceiverImpl::initialize");
			err.log(LM_DEBUG);
			throw err.getAVOpenReceiverErrorEx();
		} catch(...) {
			ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataNTReceiverImpl::initialize");
			ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataNTReceiverImpl::initialize");
			err.log(LM_DEBUG);
			throw err.getAVOpenReceiverErrorEx();
		}
	}
	else {

		// Use the old mechanism to create the 1 stream with its flows
		try
		{
         // Create default config objects for the stream and the necessary flows
			BulkDataConfigurationParser::ReceiverCfg cfg;
			if(ACE_OS::strcmp(buf, "") != 0) {

				TAO_Tokenizer addressToken(buf, '/');
				int numOtherFeps = addressToken.num_tokens();
				char strFlowNumber[2];

				if( numOtherFeps < 0 || numOtherFeps > 19 ) {
					ACS_SHORT_LOG((LM_ERROR, "BulkDataNTReceiverImpl<>::initialize too many flows specified - maximum 19"));
					ACSBulkDataError::AVInvalidFlowNumberExImpl err = ACSBulkDataError::AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataNTReceiverImpl<>::initialize");
					throw err;
				}

				// For each "TCP" create a default flow configuration object
				map<string, ReceiverFlowConfiguration> flowsCfgMap;
				for (int i=0; i<numOtherFeps; i++)
				{
					ReceiverFlowConfiguration flowConfig;
					sprintf(strFlowNumber,"%d",i);
				   flowsCfgMap[strFlowNumber] = flowConfig;
				}

				cfg.flowsCfgMap = flowsCfgMap;
			}

			recvConfigMap_m["DefaultStream"] = cfg;
		}
		catch(ACSErr::ACSbaseExImpl &ex)
		{
			ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataNTReceiverImpl::openReceiver");
			err.log(LM_DEBUG);
			throw err.getAVOpenReceiverErrorEx();
		}
		catch(...)
		{
			ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataNTReceiverImpl::openReceiver");
			ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataNTReceiverImpl::openReceiver");
			throw err.getAVOpenReceiverErrorEx();
		}
	} // if (useNewConfigMechanism)
}//cleanUp




template<class TCallback>
void BulkDataNTReceiverImpl<TCallback>::cleanUp()
{
	ACS_TRACE("BulkDataNTReceiverImpl<>::cleanUp");
	closeReceiver();
	recvConfigMap_m.clear();
   
}//cleanUp

template<class TCallback>
void BulkDataNTReceiverImpl<TCallback>::openReceiver()
{
	ACS_TRACE(__PRETTY_FUNCTION__);

	try {
		map<string, BulkDataConfigurationParser::ReceiverCfg>::iterator it;
		for(it = recvConfigMap_m.begin(); it != recvConfigMap_m.end(); it++) {

			// Double check that we don't re-create existing streams
			if( receiverStreams_m.find(it->first.c_str()) != receiverStreams_m.end() )
				continue;

			ACS_SHORT_LOG((LM_ERROR,"BulkDataNTReceiverImpl<>::openReceiver Opening receiver stream '%s' with configuration from CDB", it->first.c_str()));
			AcsBulkdata::BulkDataNTReceiverStream<TCallback>* stream = createReceiverStream(it->first.c_str(), it);
			receiverStreams_m[it->first.c_str()] = stream;
		}
	} catch(StreamCreateProblemExImpl &ex) {
		ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,__PRETTY_FUNCTION__);
		err.log(LM_DEBUG);
		throw err.getAVOpenReceiverErrorEx();
	} catch(FlowCreateProblemExImpl &ex) {
		ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,__PRETTY_FUNCTION__);
		err.log(LM_DEBUG);
		throw err.getAVOpenReceiverErrorEx();
	} catch(...) {
		ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,__PRETTY_FUNCTION__);
		ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,__PRETTY_FUNCTION__);
		throw err.getAVOpenReceiverErrorEx();
	}

}//openReceiver

template<class TCallback>
void BulkDataNTReceiverImpl<TCallback>::openReceiverStream(const char *stream_name)
{
	map<string, BulkDataConfigurationParser::ReceiverCfg>::iterator it;
	AcsBulkdata::BulkDataNTReceiverStream<TCallback> *stream = 0;

	// double-check that the stream is already opened
	if( receiverStreams_m.find(stream_name) != receiverStreams_m.end() ) {
		ACS_SHORT_LOG((LM_ERROR,"BulkDataNTReceiverImpl<>::openReceiverStream Receiver stream '%s' is already opened, won't try to open it again", stream_name));
		return;
	}

	try {

		// Look for the matching configuration. If found, use it
		for(it = recvConfigMap_m.begin(); it != recvConfigMap_m.end(); it++) {
			if( ACE_OS::strcmp(it->first.c_str(), stream_name) == 0 ) {
				ACS_SHORT_LOG((LM_ERROR,"BulkDataNTReceiverImpl<>::openReceiverStream Opening receiver stream '%s' with configuration from CDB", stream_name));
				stream = createReceiverStream(stream_name, it);
				receiverStreams_m[stream_name] = stream;
				break;
			}
		}

		// If no configurationw was found for the given stream name, use a default configuration
		if( stream == 0 ) {
			ACS_SHORT_LOG((LM_ERROR,"BulkDataNTReceiverImpl<>::openReceiverStream Opening receiver stream '%s' with default configuration", stream_name));
			stream = new AcsBulkdata::BulkDataNTReceiverStream<TCallback>(stream_name);
			receiverStreams_m[stream_name] = stream;
		}

	} catch(StreamCreateProblemExImpl &ex) {
		ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,__PRETTY_FUNCTION__);
		err.log(LM_DEBUG);
		throw err.getAVOpenReceiverErrorEx();
	} catch(FlowCreateProblemExImpl &ex) {
		ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,__PRETTY_FUNCTION__);
		err.log(LM_DEBUG);
		throw err.getAVOpenReceiverErrorEx();
	} catch(...) {
		ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,__PRETTY_FUNCTION__);
		ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,__PRETTY_FUNCTION__);
		throw err.getAVOpenReceiverErrorEx();
	}

}

template<class TCallback>
AcsBulkdata::BulkDataNTReceiverStream<TCallback>* BulkDataNTReceiverImpl<TCallback>::createReceiverStream(const char *stream_name, map<string, BulkDataConfigurationParser::ReceiverCfg>::iterator &it) {

	// Create the stream with the configuration pointed out by the iterator
	AcsBulkdata::BulkDataNTReceiverStream<TCallback> *stream = new AcsBulkdata::BulkDataNTReceiverStream<TCallback>(stream_name, it->second.streamCfg);

	// Create also all the necessary flows that have been configured in the CDB
	map<string, ReceiverFlowConfiguration>::iterator it2;
	for(it2 = it->second.flowsCfgMap.begin(); it2 != it->second.flowsCfgMap.end(); it2++) {
		stream->createFlow(it2->first.c_str(), it2->second);
	}

	return stream;
}

template<class TCallback>
void BulkDataNTReceiverImpl<TCallback>::closeReceiver()
{
	ACS_TRACE("BulkDataNTReceiverImpl<>::closeReceiver");

	try
	{
		typename StreamMap::iterator it;
		for( it = receiverStreams_m.begin(); it != receiverStreams_m.end(); it++ ) {
			ACS_SHORT_LOG((LM_ERROR,"BulkDataNTReceiverImpl<>::closeReceiver Closing receiver stream '%s'", it->first.c_str()));
			closeStream(it);
		}
	}
	catch(ACSErr::ACSbaseExImpl &ex)
	{
		ACSBulkDataError::AVCloseReceiverErrorExImpl err = ACSBulkDataError::AVCloseReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataNTReceiverImpl::closeReceiver");
		err.log(LM_DEBUG);
		throw err.getAVCloseReceiverErrorEx();
	}
	catch(...)
	{
		ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataNTReceiverImpl::closeReceiver");
		ACSBulkDataError::AVCloseReceiverErrorExImpl err = ACSBulkDataError::AVCloseReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataNTReceiverImpl::closeReceiver");
		throw err.getAVCloseReceiverErrorEx();
	}
}//closeReceiver

template<class TCallback>
void BulkDataNTReceiverImpl<TCallback>::closeReceiverStream(const char *stream_name)
{
	typename StreamMap::iterator it = receiverStreams_m.find(stream_name);

	// TODO: should we fail more miserably or is it OK to be good with people?
	if( it == receiverStreams_m.end() ) {
		ACS_SHORT_LOG((LM_ERROR,"BulkDataNTReceiverImpl<>::closeReceiverStream Receiver stream '%s' is not opened, won't try to close it", stream_name));
		return;
	}

	try
	{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataNTReceiverImpl<>::closeReceiverStream Closing receiver stream '%s'", stream_name));
		closeStream(it);
	}
	catch(ACSErr::ACSbaseExImpl &ex)
	{
		ACSBulkDataError::AVCloseReceiverErrorExImpl err = ACSBulkDataError::AVCloseReceiverErrorExImpl(ex,__FILE__,__LINE__,__PRETTY_FUNCTION__);
		err.log(LM_DEBUG);
		throw err.getAVCloseReceiverErrorEx();
	}
	catch(...)
	{
		ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,__PRETTY_FUNCTION__);
		ACSBulkDataError::AVCloseReceiverErrorExImpl err = ACSBulkDataError::AVCloseReceiverErrorExImpl(ex,__FILE__,__LINE__,__PRETTY_FUNCTION__);
		throw err.getAVCloseReceiverErrorEx();
	}
}

template<class TCallback>
void BulkDataNTReceiverImpl<TCallback>::closeStream(typename StreamMap::iterator &it) {
	delete it->second;
	receiverStreams_m.erase(it);
}

template<class TCallback>
bulkdata::BulkDataReceiverConfig * BulkDataNTReceiverImpl<TCallback>::getReceiverConfig() {
	//TBD
	return NULL;
}//getReceiverConfig

template<class TCallback>
void BulkDataNTReceiverImpl<TCallback>::setRecvName(const char *recvName)
{
	ACS_TRACE("BulkDataNTReceiverImpl<>::setRecvName");

	typename StreamMap::iterator it = receiverStreams_m.begin();
	for(; it != receiverStreams_m.end(); it++)
		(it->second)->setReceiverName(recvName);
}//setRecvName

template<class TCallback>
AcsBulkdata::BulkDataNTReceiverStream<TCallback>* BulkDataNTReceiverImpl<TCallback>::getReceiverStream(const char *streamName)
{
	// Implementation should change when IDL changes. It currently returns the first stream, whatever it is
	if( receiverStreams_m.size() != 0 )
	{
		typename StreamMap::iterator it;
		it = receiverStreams_m.find(streamName);
		if (it!=receiverStreams_m.end())
		{
			return it->second;
		}
	}//if
	//here we come just in case of an error
	StreamNotExistExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
	ex.setStreamName(streamName);
	throw ex;
}//getReceiverStream
