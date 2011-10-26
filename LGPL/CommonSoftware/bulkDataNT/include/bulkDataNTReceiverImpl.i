
#include "bulkDataNTConfigurationParser.h"

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
}//cleanUp




template<class TCallback>
void BulkDataNTReceiverImpl<TCallback>::cleanUp()
{
	ACS_TRACE("BulkDataNTReceiverImpl<>::cleanUp");

	typename StreamMap::iterator it = receiverStreams_m.begin();
	for(; it != receiverStreams_m.end(); it++)
		delete it->second;
	receiverStreams_m.clear();

}//cleanUp

template<class TCallback>
void BulkDataNTReceiverImpl<TCallback>::openReceiver()
{
	ACS_TRACE("BulkDataNTReceiverImpl<>::openReceiver");

	char buf[BUFSIZ];

	// here we read CDB just that we are backward compatible, this has to be changed !!!
	CDB::DAL_ptr dal_p = containerServices_p->getCDB();
	if(CORBA::is_nil(dal_p))
	{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataNTReceiverImpl<>::openReceiver error getting DAL reference"));
		ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataNTReceiverImpl::openReceiver");
		throw err.getAVOpenReceiverErrorEx();
	}

	ACE_CString CDBpath="alma/";
	CDBpath += name();

	CDB::DAO_ptr dao_p = dal_p->get_DAO_Servant(CDBpath.c_str());
	if(CORBA::is_nil(dao_p))
	{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataNTReceiverImpl<>::openReceiver error getting DAO reference"));
		ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataNTReceiverImpl::openReceiver");
		throw err.getAVOpenReceiverErrorEx();
	}

	// See first if we have an old configuration. If so, we use that to create the 1 stream with its flows
	char *recv_protocols = 0;
	bool createNew = false;
	try {
		recv_protocols = dao_p->get_string("recv_protocols");
		ACE_OS::strcpy(buf,recv_protocols);
		// TODO: put log about old config mechanism
	} catch(cdbErrType::CDBFieldDoesNotExistEx &ex) {
		createNew = true;
	}

	if( createNew ) {

		// Use the new mechanism to parse the XML document and create the streams/flows
		char *xmlNode = dal_p->get_DAO(CDBpath.c_str());
		BulkDataConfigurationParser parser;

		try {

			// Get the streams and put them on the map
			list<AcsBulkdata::BulkDataNTReceiverStream<TCallback> *> *recvStreams = parser.parseReceiverConfig<TCallback>(xmlNode);
			if( recvStreams->size() == 0 ) {
				ACS_SHORT_LOG((LM_NOTICE,"BulkDataNTReceiverImpl<>::openReceiver No Receiver Streams configured, no streams will be created"));
			}
			else {
				typename list<AcsBulkdata::BulkDataNTReceiverStream<TCallback> *>::iterator it = recvStreams->begin();
				for(; it != recvStreams->end(); it++)
					receiverStreams_m[(*it)->getName()] = (*it);
			}
			recvStreams->clear();
			delete recvStreams;

		} catch(CDBProblemExImpl &ex) {
			ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataNTReceiverImpl::openReceiver");
			err.log(LM_DEBUG);
			throw err.getAVOpenReceiverErrorEx();
		} catch(StreamCreateProblemExImpl &ex) {
			ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataNTReceiverImpl::openReceiver");
			err.log(LM_DEBUG);
			throw err.getAVOpenReceiverErrorEx();
		} catch(FlowCreateProblemExImpl &ex) {
			ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataNTReceiverImpl::openReceiver");
			err.log(LM_DEBUG);
			throw err.getAVOpenReceiverErrorEx();
		} catch(...) {
			ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataNTReceiverImpl::openReceiver");
			ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataNTReceiverImpl::openReceiver");
			err.log(LM_DEBUG);
			throw err.getAVOpenReceiverErrorEx();
		}
	}
	else {

		// Use the old mechanism to create the 1 stream with its flows
		try
		{
			//receiverStream_m->initialize();
			AcsBulkdata::BulkDataNTReceiverStream<TCallback> *recvStream = new AcsBulkdata::BulkDataNTReceiverStream<TCallback>("DefaultStream");
			recvStream->createMultipleFlowsFromConfig(buf); // actually here we need just number of flows !!!
			recvStream->setReceiverName(name());

			receiverStreams_m["DefaultStream"] = recvStream;
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
	} // if (createNew)

}//openReceiver

template<class TCallback>
void BulkDataNTReceiverImpl<TCallback>::openReceiverStream(const char *stream_name)
{
	//TBD implemented
}

template<class TCallback>
void BulkDataNTReceiverImpl<TCallback>::closeReceiver()
{
	ACS_TRACE("BulkDataNTReceiverImpl<>::closeReceiver");

	try
	{
//		receiverStream_m->closeReceiver();
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
	//TBD implemented
}


template<class TCallback>
bulkdata::BulkDataReceiverConfig * BulkDataNTReceiverImpl<TCallback>::getReceiverConfig()
{
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

