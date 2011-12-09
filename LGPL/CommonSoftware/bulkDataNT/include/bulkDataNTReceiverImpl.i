/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2011
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: bulkDataNTReceiverImpl.i,v 1.14 2011/12/09 17:11:45 rtobar Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/

#include <sstream>
#include <Tokenizer_T.h>
#include "bulkDataNTReceiverImpl.h"

template<class TCallback>
BulkDataNTReceiverImpl<TCallback>::BulkDataNTReceiverImpl(const ACE_CString& name,maci::ContainerServices* containerServices) :
	CharacteristicComponentImpl(name,containerServices),
	parser_m(0),
	defaultFlowsCount_m(-1)
{
	ACS_TRACE("BulkDataNTReceiverImpl<>::BulkDataNTReceiverImpl");
}//BulkDataNTReceiverImpl

template<class TCallback>
BulkDataNTReceiverImpl<TCallback>::~BulkDataNTReceiverImpl()
{
	ACS_TRACE("BulkDataNTReceiverImpl<>::~BulkDataNTReceiverImpl");
	delete parser_m;
}//~BulkDataNTReceiverImpl


template<class TCallback>
void BulkDataNTReceiverImpl<TCallback>::initialize()
{
	ACS_TRACE("BulkDataNTReceiverImpl<>::initialize");

	char buf[BUFSIZ];

	// get the DAO and read the alma/ branch of the component out from the CDB
	CDB::DAL_ptr dal_p = getContainerServices()->getCDB();
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

		try {
			// Parse the XML document and check if we got any configuration object
			parser_m = new AcsBulkdata::BulkDataConfigurationParser();
			parser_m->parseReceiverConfig(xmlNode);
			if( parser_m->getAllReceiverStreamNames().size() == 0 )
				ACS_SHORT_LOG((LM_NOTICE,"BulkDataNTReceiverImpl<>::initialize No Receiver Streams configured, streams created in the future will have a default configuration"));

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

		// Use the old mechanism and remember the number of flows
		try
		{
			// Create default config objects for the stream and the necessary flows
			if(ACE_OS::strcmp(buf, "") != 0) {

				ACE_Tokenizer addressToken(buf);
				defaultFlowsCount_m = 0;
				while( addressToken.next() != 0 )
					defaultFlowsCount_m++;

				// TODO: Does this "> 19" condition make any sense?
				if( defaultFlowsCount_m < 0 || defaultFlowsCount_m > 19 ) {
					ACS_SHORT_LOG((LM_ERROR, "BulkDataNTReceiverImpl<>::initialize too many flows specified - maximum 19"));
					ACSBulkDataError::AVInvalidFlowNumberExImpl err = ACSBulkDataError::AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataNTReceiverImpl<>::initialize");
					throw err;
				}
			}

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
bool BulkDataNTReceiverImpl<TCallback>::usesOldConfigurationMechanism() {
	return (parser_m == 0);
}


template<class TCallback>
void BulkDataNTReceiverImpl<TCallback>::cleanUp()
{
	ACS_TRACE("BulkDataNTReceiverImpl<>::cleanUp");
	closeReceiver();
   
}//cleanUp

template<class TCallback>
void BulkDataNTReceiverImpl<TCallback>::openReceiver()
{
	ACS_TRACE(__PRETTY_FUNCTION__);

	try {

		// With the old config mechanism only one stream, namely "DefaultStream", is allowed
		if( usesOldConfigurationMechanism() ) {

			if( receiverStreams_m.find("DefaultStream") != receiverStreams_m.end() )
				return;

			ACS_SHORT_LOG((LM_NOTICE,"BulkDataNTReceiverImpl<>::openReceiver Opening receiver stream 'DefaultStream' with '%d' flows", defaultFlowsCount_m));
			AcsBulkdata::BulkDataNTReceiverStream<TCallback>* stream = createDefaultReceiverStream();
			receiverStreams_m["DefaultStream"] = stream;
			return;
		}

		// With the new configuration mechanism check all the configured receiver streams
		// and open them all (if not already opened)
		std::set<const char *> streamNames = parser_m->getAllReceiverStreamNames();
		std::set<const char *>::iterator it;
		for(it = streamNames.begin(); it != streamNames.end(); it++) {

			// Double check that we don't re-create existing streams
			if( receiverStreams_m.find(*it) != receiverStreams_m.end() )
				continue;

			ACS_SHORT_LOG((LM_INFO,"BulkDataNTReceiverImpl<>::openReceiver Opening receiver stream '%s' with configuration from CDB", *it));
			AcsBulkdata::BulkDataNTReceiverStream<TCallback>* stream = createReceiverStream(*it);
			receiverStreams_m[*it] = stream;
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
	std::map<std::string, BulkDataConfigurationParser::ReceiverCfg>::iterator it;
	AcsBulkdata::BulkDataNTReceiverStream<TCallback> *stream = 0;

	// double-check that the stream is already opened
	if( receiverStreams_m.find(stream_name) != receiverStreams_m.end() ) {
		ACS_SHORT_LOG((LM_NOTICE,"BulkDataNTReceiverImpl<>::openReceiverStream Receiver stream '%s' is already opened, won't try to open it again", stream_name));
		return;
	}

	try {

		// With the old config mechanism only one stream, namely "DefaultStream", is allowed
		// If someone asked to open exactly that stream, check and open it
		if( usesOldConfigurationMechanism() && ACE_OS::strcmp("DefaultStream", stream_name) ) {
			if( receiverStreams_m.find("DefaultStream") != receiverStreams_m.end() )
				return;

			ACS_SHORT_LOG((LM_NOTICE,"BulkDataNTReceiverImpl<>::openReceiverStream Opening receiver stream 'DefaultStream' with '%d' flows", defaultFlowsCount_m));
			AcsBulkdata::BulkDataNTReceiverStream<TCallback>* stream = createDefaultReceiverStream();
			receiverStreams_m["DefaultStream"] = stream;
			return;
		}

		// With the new configuration mechanism, the parser may contains a configuration for the stream
		// If available, use it; otherwise, create a default stream
		std::set<const char*> streamNames = parser_m->getAllReceiverStreamNames();
		if( streamNames.find(stream_name) != streamNames.end() ) {
			ACS_SHORT_LOG((LM_INFO, "BulkDataNTReceiverImpl<>::openReceiverStream Opening receiver stream '%s' with configuration from CDB", stream_name));
			stream = createReceiverStream(stream_name);
		}
		else {
			ACS_SHORT_LOG((LM_NOTICE, "BulkDataNTReceiverImpl<>::openReceiverStream Opening receiver stream '%s' with default configuration", stream_name));
			stream = new AcsBulkdata::BulkDataNTReceiverStream<TCallback>(stream_name);
		}

		receiverStreams_m[stream_name] = stream;

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
AcsBulkdata::BulkDataNTReceiverStream<TCallback>* BulkDataNTReceiverImpl<TCallback>::createReceiverStream(const char *stream_name) {

	// Create the stream with the configuration pointed out by the iterator
	AcsBulkdata::BulkDataNTReceiverStream<TCallback> *stream = 0;
	stream = new AcsBulkdata::BulkDataNTReceiverStream<TCallback>(stream_name, *parser_m->getReceiverStreamConfiguration(stream_name));

	// Create also all the necessary flows that have been configured in the CDB
	std::set<const char *> flowNames = parser_m->getAllReceiverFlowNames(stream_name);
	std::set<const char *>::iterator it;
	for(it = flowNames.begin(); it != flowNames.end(); it++) {
		const char * flowName = *it;
		stream->createFlow(flowName, *parser_m->getReceiverFlowConfiguration(stream_name, flowName));
	}

	return stream;
}

template<class TCallback>
AcsBulkdata::BulkDataNTReceiverStream<TCallback>* BulkDataNTReceiverImpl<TCallback>::createDefaultReceiverStream() {

	// Create the default stream
	AcsBulkdata::BulkDataNTReceiverStream<TCallback> *stream = 0;
	stream = new AcsBulkdata::BulkDataNTReceiverStream<TCallback>("DefaultStream");

	// Add the specified of flows
	for(int i=0; i < defaultFlowsCount_m; i++) {
		std::stringstream s("Flow");
		s << i;
		stream->createFlow(s.str().c_str());
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
