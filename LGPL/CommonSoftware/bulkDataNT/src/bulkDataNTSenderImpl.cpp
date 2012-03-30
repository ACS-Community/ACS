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
*/


#include <ACS_BD_ErrorsC.h>
#include <ACSBulkDataError.h>   // error definition  ??
#include <Tokenizer_T.h>

#include "bulkDataNTSenderFlow.h"
#include "bulkDataNTSenderImpl.h"

using namespace ACS_BD_Errors;

BulkDataNTSenderImpl::BulkDataNTSenderImpl(const ACE_CString& name,maci::ContainerServices* containerServices):
    CharacteristicComponentImpl(name,containerServices),
    parser_m(0),
    defaultFlowsCount_m(-1)
{
	ACS_TRACE("BulkDataNTSenderImpl::BulkDataNTSenderImpl");
}


BulkDataNTSenderImpl::~BulkDataNTSenderImpl()
{
	ACS_TRACE("BulkDataNTSenderImpl::~BulkDataNTSenderImpl");
	delete parser_m;
}

void BulkDataNTSenderImpl::initialize()
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	CharacteristicComponentImpl::initialize();

	// Read the configuration from the CDB and parse it as necessary
	// This code has been copy/pasted from bulkDataNTReceiverImpl.i, so
	// it could probably be shared
	char buf[BUFSIZ];

	// get the DAO and read the alma/ branch of the component out from the CDB
	CDB::DAL_ptr dal_p = getContainerServices()->getCDB();
	if(CORBA::is_nil(dal_p))
	{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataNTSenderImpl::initialize error getting DAL reference"));
		ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataNTSenderImpl::initialize");
		throw err.getAVOpenReceiverErrorEx();
	}

	ACE_CString CDBpath="alma/";
	CDBpath += name();

	CDB::DAO_ptr dao_p = dal_p->get_DAO_Servant(CDBpath.c_str());
	if(CORBA::is_nil(dao_p))
	{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataNTSenderImpl::initialize error getting DAO reference"));
		ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataNTSenderImpl::initialize");
		throw err.getAVOpenReceiverErrorEx();
	}

	// Check which kind of configuration we have (the old "TCP"-based one, or the new schema-based one)
	char *sender_protocols = 0;
	bool useNewConfigMechanism = false;
	try {
		sender_protocols = dao_p->get_string("sender_protocols");
		ACE_OS::strcpy(buf,sender_protocols);
		// TODO: put log saying that old config mechanism is going to be used instead of the new one
	} catch(cdbErrType::CDBFieldDoesNotExistEx &ex) {
		useNewConfigMechanism = true;
	}

	if( useNewConfigMechanism ) {

		// Use the new mechanism to parse the XML document and create the streams/flows
		char *xmlNode = dal_p->get_DAO(CDBpath.c_str());

		try {
			// Parse the XML document and check if we got any configuration object
			parser_m = new AcsBulkdata::BulkDataConfigurationParser(name());
			parser_m->parseSenderConfig(xmlNode);
			if( parser_m->getAllSenderStreamNames().size() == 0 )
				ACS_SHORT_LOG((LM_NOTICE,"BulkDataNTSenderImpl::initialize No Sender Streams configured, streams created in the future will have a default configuration"));

		} catch(CDBProblemExImpl &ex) {
			ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataNTSenderImpl::initialize");
			err.log(LM_DEBUG);
			throw err.getAVOpenReceiverErrorEx();
		} catch(...) {
			ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataNTSenderImpl::initialize");
			ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataNTSenderImpl::initialize");
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
				addressToken.delimiter('/');
				defaultFlowsCount_m = 0;
				while( addressToken.next() != 0 )
					defaultFlowsCount_m++;

				// TODO: Does this "> 19" condition make any sense?
				if( defaultFlowsCount_m < 0 || defaultFlowsCount_m > 19 ) {
					ACS_SHORT_LOG((LM_ERROR, "BulkDataNTSenderImpl::initialize too many flows specified - maximum 19"));
					ACSBulkDataError::AVInvalidFlowNumberExImpl err = ACSBulkDataError::AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataNTSenderImpl::initialize");
					throw err;
				}
			}

		}
		catch(ACSErr::ACSbaseExImpl &ex)
		{
			ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataNTSenderImpl::initialize");
			err.log(LM_DEBUG);
			throw err.getAVOpenReceiverErrorEx();
		}
		catch(...)
		{
			ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataNTSenderImpl::initialize");
			ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataNTSenderImpl::initialize");
			throw err.getAVOpenReceiverErrorEx();
		}
	} // if (useNewConfigMechanism)

}

void BulkDataNTSenderImpl::cleanUp() {
	disconnect();
}

bool BulkDataNTSenderImpl::usesOldConfigurationMechanism() {
	return (parser_m == 0);
}

void BulkDataNTSenderImpl::connect(bulkdata::BulkDataReceiver_ptr receiverObj_p)
{
	ACS_TRACE("BulkDataNTSenderImpl::connect - deprecated");

	// check if initialize has been called
	if( parser_m == 0 && defaultFlowsCount_m == -1 ) {
		acsErrTypeLifeCycle::LifeCycleExImpl lcEx = acsErrTypeLifeCycle::LifeCycleExImpl(__FILE__,__LINE__,__PRETTY_FUNCTION__);
		lcEx.log(LM_DEBUG);
		throw lcEx.getacsErrTypeLifeCycleEx();
	}

	// Here we currently open all senders, new methods might be added later to the IDL interface
	openSenders();
}//connect

void BulkDataNTSenderImpl::openSenders() {
	ACS_TRACE(__PRETTY_FUNCTION__);

	try {

		// With the old config mechanism only one stream, namely "DefaultStream", is allowed
		if( usesOldConfigurationMechanism() ) {

			if( senderStreams_m.find("DefaultStream") != senderStreams_m.end() )
				return;

			ACS_SHORT_LOG((LM_NOTICE,"BulkDataNTSenderImpl::openSenders Opening sender stream 'DefaultStream' with '%d' flows", defaultFlowsCount_m));
			AcsBulkdata::BulkDataNTSenderStream* stream = createDefaultSenderStream();
			senderStreams_m["DefaultStream"] = stream;
			return;
		}

		// With the new configuration mechanism check all the configured sender streams
		// and open them all (if not already opened)
		std::set<const char *> streamNames = parser_m->getAllSenderStreamNames();
		std::set<const char *>::iterator it;
		for(it = streamNames.begin(); it != streamNames.end(); it++) {

			// Double check that we don't re-create existing streams
			if( senderStreams_m.find(*it) != senderStreams_m.end() )
				continue;

			ACS_SHORT_LOG((LM_INFO,"BulkDataNTSenderImpl::openSenders Opening sender stream '%s' with configuration from CDB", *it));
			AcsBulkdata::BulkDataNTSenderStream* stream = createSenderStream(*it);
			senderStreams_m[*it] = stream;
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

AcsBulkdata::BulkDataNTSenderStream* BulkDataNTSenderImpl::createSenderStream(const char *stream_name) {

	// Create the stream with the configuration pointed out by the iterator
	AcsBulkdata::BulkDataNTSenderStream *stream = 0;
	stream = new AcsBulkdata::BulkDataNTSenderStream(stream_name, *parser_m->getSenderStreamConfiguration(stream_name));

	// Create also all the necessary flows that have been configured in the CDB
	std::set<const char *> flowNames = parser_m->getAllSenderFlowNames(stream_name);
	std::set<const char *>::iterator it;
	for(it = flowNames.begin(); it != flowNames.end(); it++) {
		const char * flowName = *it;
		stream->createFlow(flowName, *parser_m->getSenderFlowConfiguration(stream_name, flowName));
	}

	return stream;
}

AcsBulkdata::BulkDataNTSenderStream* BulkDataNTSenderImpl::createDefaultSenderStream() {

	// Create the default stream
	AcsBulkdata::BulkDataNTSenderStream *stream = 0;
	stream = new AcsBulkdata::BulkDataNTSenderStream("DefaultStream");

	// Add the specified of flows
	for(int i=0; i < defaultFlowsCount_m; i++) {
		std::stringstream s;
		s << "Flow" << i;
		stream->createFlow(s.str().c_str());
	}

	return stream;
}

void BulkDataNTSenderImpl::disconnect()
{
	ACS_TRACE("BulkDataNTSenderImpl::disconnect - deprecated");

	try
	{
		StreamMap::iterator it;
		for( it = senderStreams_m.begin(); it != senderStreams_m.end(); it++ ) {
			ACS_SHORT_LOG((LM_ERROR,"BulkDataNTSenderImpl::disconnect Closing sender stream '%s'", it->first.c_str()));
			closeStream(it);
		}
	}
	catch(ACSErr::ACSbaseExImpl &ex)
	{
		ACSBulkDataError::AVCloseReceiverErrorExImpl err = ACSBulkDataError::AVCloseReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataNTSenderImpl::disconnect");
		err.log(LM_DEBUG);
		throw err.getAVCloseReceiverErrorEx();
	}
	catch(...)
	{
		ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataNTSenderImpl::disconnect");
		ACSBulkDataError::AVCloseReceiverErrorExImpl err = ACSBulkDataError::AVCloseReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataNTSenderImpl::disconnect");
		throw err.getAVCloseReceiverErrorEx();
	}
}

void BulkDataNTSenderImpl::closeStream(StreamMap::iterator &it) {
	delete it->second;
	senderStreams_m.erase(it);
}

AcsBulkdata::BulkDataNTSenderStream* BulkDataNTSenderImpl::getSenderStream() {

	if( senderStreams_m.size() != 0 )
		return senderStreams_m.begin()->second;

	//here we come just in case of an error
	StreamNotExistExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
	throw ex;
}

AcsBulkdata::BulkDataNTSenderStream* BulkDataNTSenderImpl::getSenderStream(const char *streamName) {

	if( senderStreams_m.size() != 0 ) {
		StreamMap::iterator it;

		it = senderStreams_m.find(streamName);
		if (it!=senderStreams_m.end())
			return it->second;
	}

	//here we come just in case of an error
	StreamNotExistExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
	ex.setStreamName(streamName);
	throw ex;
}
