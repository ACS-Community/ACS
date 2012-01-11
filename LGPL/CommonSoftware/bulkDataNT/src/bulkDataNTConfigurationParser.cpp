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
* "@(#) $Id: bulkDataNTConfigurationParser.cpp,v 1.25 2012/01/11 10:22:16 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/

#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/framework/psvi/XSValue.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/framework/MemBufInputSource.hpp>
#include <xercesc/framework/MemBufFormatTarget.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <stdio.h>

#include "ACS_BD_Errors.h"
#include "bulkDataNTConfigurationParser.h"

using namespace std;
using namespace xercesc;
using namespace AcsBulkdata;
using namespace ACS_BD_Errors;

const char* const BulkDataConfigurationParser::SENDER_STREAM_NODENAME       = "SenderStream";
const char* const BulkDataConfigurationParser::SENDER_STREAM_QOS_NODENAME   = "DDSSenderStreamQoS";
const char* const BulkDataConfigurationParser::SENDER_FLOW_NODENAME         = "SenderFlow";
const char* const BulkDataConfigurationParser::SENDER_FLOW_QOS_NODENAME     = "DDSSenderFlowQoS";

const char* const BulkDataConfigurationParser::RECEIVER_STREAM_NODENAME     = "ReceiverStream";
const char* const BulkDataConfigurationParser::RECEIVER_STREAM_QOS_NODENAME = "DDSReceiverStreamQoS";
const char* const BulkDataConfigurationParser::RECEIVER_FLOW_NODENAME       = "ReceiverFlow";
const char* const BulkDataConfigurationParser::RECEIVER_FLOW_QOS_NODENAME   = "DDSReceiverFlowQoS";

const char* const BulkDataConfigurationParser::DYNAMIC_LIBRARY_NAME         = DDSConfiguration::DEFAULT_LIBRARY;

const struct BulkDataConfigurationParser::ParsingInfo BulkDataConfigurationParser::SENDER_PARSING_INFO = {
	SENDER,
	SENDER_STREAM_NODENAME,
	SENDER_STREAM_QOS_NODENAME,
	SENDER_FLOW_NODENAME,
	SENDER_FLOW_QOS_NODENAME,
	DDSConfiguration::DEFAULT_SENDER_STREAM_PROFILE,
	DDSConfiguration::DEFAULT_SENDER_FLOW_PROFILE
};

const struct BulkDataConfigurationParser::ParsingInfo BulkDataConfigurationParser::RECEIVER_PARSING_INFO = {
	RECEIVER,
	RECEIVER_STREAM_NODENAME,
	RECEIVER_STREAM_QOS_NODENAME,
	RECEIVER_FLOW_NODENAME,
	RECEIVER_FLOW_QOS_NODENAME,
	DDSConfiguration::DEFAULT_RECEIVER_STREAM_PROFILE,
	DDSConfiguration::DEFAULT_RECEIVER_FLOW_PROFILE
};

XMLChSP::XMLChSP(char* xmlCh) {
	xmlch_m = xmlCh;
}

XMLChSP::~XMLChSP() {
	XMLString::release(&xmlch_m);
}

char * XMLChSP::get() {
	return xmlch_m;
}


BulkDataConfigurationParser::BulkDataConfigurationParser(const char *baseLibraryName) :
   m_baseLibrary(ACE_OS::strdup(baseLibraryName)),
   m_writer(0),
   m_parser(0)
{
	try {
		XMLPlatformUtils::Initialize();
	}
	catch (const XMLException& toCatch) {
		fprintf(stderr, "mmm... something went's super-wrong");
	}

	static const XMLCh gLS[] = { chLatin_L, chLatin_S, chNull };
	DOMImplementation *impl = DOMImplementationRegistry::getDOMImplementation(gLS);
	m_writer = impl->createDOMWriter();

	m_parser = new XercesDOMParser();
	m_parser->useScanner(XMLUni::fgWFXMLScanner); // just this, we don't need more than well-formedness checking
	m_parser->setValidationScheme(XercesDOMParser::Val_Always);

}

BulkDataConfigurationParser::~BulkDataConfigurationParser() {

	// Clean all the objects on the heap
	cleanSenderConfigs();
	cleanReceiverConfigs();

	// Terminate the XML parser objects
	m_writer->release();
	delete m_parser;
	ACE_OS::free((void *)m_baseLibrary);
	XMLPlatformUtils::Terminate();
}

void BulkDataConfigurationParser::cleanSenderConfigs() {
	for(SenderCfgMap::iterator it = senderConfigMap_m.begin(); it!= senderConfigMap_m.end(); it++)
		cleanConfig(0, &it->second, SENDER);
	senderConfigMap_m.clear();
}

void BulkDataConfigurationParser::cleanReceiverConfigs() {
	for(ReceiverCfgMap::iterator it = receiverConfigMap_m.begin(); it!= receiverConfigMap_m.end(); it++)
		cleanConfig(&it->second, 0, RECEIVER);
	receiverConfigMap_m.clear();
}

void BulkDataConfigurationParser::cleanConfig(ReceiverCfg *recvConfig, SenderCfg *senderConfig, ParsingType type) {
	if( type == SENDER ) {
		delete senderConfig->streamCfg;
		map<string, SenderFlowConfiguration *>::iterator it2;
		for(it2 = senderConfig->flowsCfgMap.begin(); it2 != senderConfig->flowsCfgMap.end(); it2++)
			delete it2->second;
		senderConfig->flowsCfgMap.clear();
	}
	else {
		delete recvConfig->streamCfg;
		map<string, ReceiverFlowConfiguration *>::iterator it2;
		for(it2 = recvConfig->flowsCfgMap.begin(); it2 != recvConfig->flowsCfgMap.end(); it2++)
			delete it2->second;
		recvConfig->flowsCfgMap.clear();
	}
}

void BulkDataConfigurationParser::parseSenderConfig(const char *config) {
	cleanSenderConfigs();
	parseConfig(config, SENDER_PARSING_INFO);
}

void BulkDataConfigurationParser::parseReceiverConfig(const char *config) {
	cleanReceiverConfigs();
	parseConfig(config, RECEIVER_PARSING_INFO);
}

void BulkDataConfigurationParser::parseConfig(const char *config, const struct ParsingInfo &parsingInfo)
{

	// Parse the XML string and get the DOM Document
	m_parser->reset();
	try {
		MemBufInputSource is((const XMLByte *)config, strlen(config), "id");
		m_parser->parse(is);
	} catch(const XMLException& toCatch) {

		XMLChSP m = XMLString::transcode(toCatch.getMessage());

		CDBProblemExImpl cdbProblemEx(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		cdbProblemEx.setDetail(m.get());

		throw cdbProblemEx;
	} catch(const DOMException& toCatch) {

		char *m = XMLString::transcode(toCatch.getMessage());

		CDBProblemExImpl cdbProblemEx(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		cdbProblemEx.setDetail(m);

		XMLString::release(&m);

		throw cdbProblemEx;
	}

	DOMDocument* doc = m_parser->getDocument();
	DOMNode *bdSenderNode = doc->getFirstChild();
	if( bdSenderNode == 0 ) {

		CDBProblemExImpl cdbProblemEx(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		cdbProblemEx.setDetail("Configuration XML string seems to be empty, no children from the root element");

		throw cdbProblemEx;
	}

	// For each stream node, process the information
	DOMNodeList *streamNodesList = bdSenderNode->getChildNodes();
	for(unsigned int i=0; i!=streamNodesList->getLength(); i++) {

		DOMNode *streamNode = streamNodesList->item(i);
		if( streamNode->getNodeType() != DOMNode::ELEMENT_NODE )
			continue;

		XMLChSP nodeName = XMLString::transcode(streamNode->getNodeName());
		if( ACE_OS::strcmp(nodeName.get(),parsingInfo.reqStreamNodeName) != 0 ) {
			// Don't throw exception, just continue parsing
			// The XML document might specify other things which are of no interest to us
			continue;
		}

		const XMLCh* streamNameCh = getAttrValue(streamNode, "Name");
		if( streamNameCh == 0 ) {

			string s("Node '");
			s.append(parsingInfo.reqStreamNodeName);
			s.append("' doesn't have attribute 'Name'");

			CDBProblemExImpl cdbProblemEx(__FILE__, __LINE__, __PRETTY_FUNCTION__);
			cdbProblemEx.setDetail(s.c_str());

			throw cdbProblemEx;
		}
		XMLChSP streamName = XMLString::transcode(streamNameCh);

		// Check that the sender stream isn't repeated
		bool streamExists;
		if( parsingInfo.type == SENDER )
			streamExists = (senderConfigMap_m.find(streamName.get()) != senderConfigMap_m.end());
		else
			streamExists = (receiverConfigMap_m.find(streamName.get()) != receiverConfigMap_m.end());

		if( streamExists ) {

			string s("Repeated stream: '");
			s.append(streamName.get());
			s.append("'");

			CDBProblemExImpl cdbProblemEx(__FILE__, __LINE__, __PRETTY_FUNCTION__);
			cdbProblemEx.setDetail(s.c_str());

			throw cdbProblemEx;
		}

		// Create the configuration structures, we fill them afterwards
		SenderCfg   senderCfg;
		ReceiverCfg receiverCfg;
		bool anyProfileFound = false;
		if( parsingInfo.type == SENDER )
			senderCfg.streamCfg = new SenderStreamConfiguration();
		else
			receiverCfg.streamCfg = new ReceiverStreamConfiguration();

		// For each sender stream, check the QoS and the underlying flow nodes
		DOMNodeList *streamChildrenNodesList = streamNode->getChildNodes();
		for(unsigned int j=0; j!= streamChildrenNodesList->getLength(); j++) {

			DOMNode *streamChildNode = streamChildrenNodesList->item(j);
			if( streamChildNode->getNodeType() != DOMNode::ELEMENT_NODE )
				continue;

			XMLChSP childNodeName = XMLString::transcode(streamChildNode->getNodeName());

			// The Sender/ReceiverStreamQoS is appended to the str:// URI
			if( ACE_OS::strcmp(childNodeName.get(), parsingInfo.reqStreamQoSNodeName) == 0 ) {

				try {
					if( parsingInfo.type == SENDER )
						senderCfg.streamCfg->participantPerStream = getBooleanFromAttribute(streamChildNode, "ParticipantPerStream", false);
					else
						receiverCfg.streamCfg->participantPerStream = getBooleanFromAttribute(streamChildNode, "ParticipantPerStream", false);
				} catch(CDBProblemExImpl &ex) {
					cleanConfig(&receiverCfg, &senderCfg, parsingInfo.type);
					throw ex;
				}

				// Now take the rest of the QoS node children and build up the profile string
				string profileQoS = getQosProfile(streamName.get(), parsingInfo.defaultStreamProfileName, streamChildNode);
				if( parsingInfo.type == SENDER ) {
					senderCfg.streamCfg->libraryQos       = (m_baseLibrary != 0 ? m_baseLibrary : DYNAMIC_LIBRARY_NAME);
					senderCfg.streamCfg->profileQos       = streamName.get();
					senderCfg.streamCfg->stringProfileQoS = profileQoS;
				}
				else {
					receiverCfg.streamCfg->libraryQos       = (m_baseLibrary != 0 ? m_baseLibrary : DYNAMIC_LIBRARY_NAME);
					receiverCfg.streamCfg->profileQos       = streamName.get();
					receiverCfg.streamCfg->stringProfileQoS = profileQoS;
				}
				anyProfileFound = true;
			}

			// Process the Flow nodes
			else if( ACE_OS::strcmp(childNodeName.get(), parsingInfo.reqFlowNodeName) == 0 ) {

				const XMLCh * flowNameCh = getAttrValue(streamChildNode, "Name");
				if( flowNameCh == 0 ) {

					string s("Node '");
					s.append(parsingInfo.reqFlowNodeName);
					s.append("' doesn't have attribute 'Name'");

					CDBProblemExImpl cdbProblemEx(__FILE__, __LINE__, __PRETTY_FUNCTION__);
					cdbProblemEx.setDetail(s.c_str());

					cleanConfig(&receiverCfg, &senderCfg, parsingInfo.type);
					throw cdbProblemEx;
				}
				XMLChSP flowName = XMLString::transcode(flowNameCh);

				// Check for repeated flows inside the stream
				bool flowExists;
				if( parsingInfo.type == SENDER )
					flowExists = (senderCfg.flowsCfgMap.find(flowName.get()) != senderCfg.flowsCfgMap.end());
				else
					flowExists = (receiverCfg.flowsCfgMap.find(flowName.get()) != receiverCfg.flowsCfgMap.end());

				if( flowExists ) {

					string s("Repeated flow in stream '");
					s.append(streamName.get());
					s.append("': '");
					s.append(flowName.get());
					s.append("'");

					CDBProblemExImpl cdbProblemEx(__FILE__, __LINE__, __PRETTY_FUNCTION__);
					cdbProblemEx.setDetail(s.c_str());

					cleanConfig(&receiverCfg, &senderCfg, parsingInfo.type);
					throw cdbProblemEx;
				}

				// Insert the new flow configuration
				if( parsingInfo.type == SENDER )
					senderCfg.flowsCfgMap[flowName.get()] = new SenderFlowConfiguration();
				else
					receiverCfg.flowsCfgMap[flowName.get()] = new ReceiverFlowConfiguration();

				// SenderFlow has two extra attributes, let's check them
				try {
					if( parsingInfo.type == SENDER ) {
						senderCfg.flowsCfgMap[flowName.get()]->setFrameTimeout(getDoubleFromAttribute(streamChildNode, "SendFrameTimeoutSec", 5.0));
						senderCfg.flowsCfgMap[flowName.get()]->setACKsTimeout(getDoubleFromAttribute(streamChildNode, "ACKsTimeoutSec", 1.0));
					}else {
						receiverCfg.flowsCfgMap[flowName.get()]->setCbReceiveProcessTimeout(getDoubleFromAttribute(streamChildNode, "cbReceiveProcessTimeoutSec", 0.010));
					}

				} catch(CDBProblemExImpl &ex) {
					cleanConfig(&receiverCfg, &senderCfg, parsingInfo.type);
					throw ex;
				}

				// Flow nodes contain Sender/ReceiverFlowQoS
				DOMNodeList *flowNodesList = streamChildNode->getChildNodes();
				for(unsigned int k=0; k!= flowNodesList->getLength(); k++) {

					DOMNode *childFlowNode = flowNodesList->item(k);
					if( childFlowNode->getNodeType() != DOMNode::ELEMENT_NODE )
						continue;

					XMLChSP childFlowNodeName = XMLString::transcode(childFlowNode->getNodeName());
					if( ACE_OS::strcmp(childFlowNodeName.get(),parsingInfo.reqFlowQoSNodeName) != 0 ) {

						stringstream s;
						s << "Node name is different from '" << parsingInfo.reqFlowQoSNodeName << "': " << childFlowNodeName.get();

						CDBProblemExImpl cdbProblemEx(__FILE__, __LINE__, __PRETTY_FUNCTION__);
						cdbProblemEx.setDetail(s.str().c_str());

						cleanConfig(&receiverCfg, &senderCfg, parsingInfo.type);
						throw cdbProblemEx;
					}

					string profileName(streamName.get());
					profileName.append("#");
					profileName.append(flowName.get());
					string profileQoS = getQosProfile(profileName.c_str(), parsingInfo.defaultFlowProfileName, childFlowNode);

					// and let the flow have it's right profile name configured
					// NOTE: We need to set the libraryQoS into the *stream* configuration object, since the
					// *stream* configuration object is later used to fill up the DDS Factory QoS.
					if( parsingInfo.type == SENDER ) {
						SenderFlowConfiguration *flowConfig = senderCfg.flowsCfgMap[flowName.get()];
						flowConfig->libraryQos = (m_baseLibrary != 0 ? m_baseLibrary : DYNAMIC_LIBRARY_NAME);
						flowConfig->profileQos = profileName;
						flowConfig->stringProfileQoS = profileQoS;

//						senderCfg.streamCfg->libraryQos = (m_baseLibrary != 0 ? m_baseLibrary : DYNAMIC_LIBRARY_NAME);
					}
					else {
						ReceiverFlowConfiguration *flowConfig = receiverCfg.flowsCfgMap[flowName.get()];
						flowConfig->libraryQos = (m_baseLibrary != 0 ? m_baseLibrary : DYNAMIC_LIBRARY_NAME);
						flowConfig->profileQos = profileName;
						flowConfig->stringProfileQoS = profileQoS;

//						receiverCfg.streamCfg->libraryQos = (m_baseLibrary != 0 ? m_baseLibrary : DYNAMIC_LIBRARY_NAME);
					}

					anyProfileFound = true;
				}
			}

		} // for(streamChildrenNodesList)


		// Finally, we add the configuration to our map
		if( parsingInfo.type == SENDER )
		    senderConfigMap_m[streamName.get()] = senderCfg;
		else
			receiverConfigMap_m[streamName.get()] = receiverCfg;

	} // for(streamNodesList)

}

double BulkDataConfigurationParser::getDoubleFromAttribute(DOMNode *node, const char * attribute, double defaultVal) {

	double returnVal;
	const XMLCh* sft = getAttrValue(node, attribute);
	if( sft == 0 )
		returnVal = defaultVal;
	else {
		XSValue::Status status;
		auto_ptr<XSValue> val(XSValue::getActualValue(sft, XSValue::dt_double, status));

		if( status != XSValue::st_Init ) {

			stringstream s;
			s << "Invalid value for '" << attribute << "' on stream QoS configuration";

			CDBProblemExImpl cdbProblemEx(__FILE__, __LINE__, __PRETTY_FUNCTION__);
			cdbProblemEx.setDetail(s.str().c_str());
			throw cdbProblemEx;
		}

		returnVal = val->fData.fValue.f_doubleType.f_double;
	}

	return returnVal;
}

bool BulkDataConfigurationParser::getBooleanFromAttribute(DOMNode *node, const char * attribute, bool defaultVal) {

	bool returnVal;
	const XMLCh* sft = getAttrValue(node, attribute);
	if( sft == 0 )
		returnVal = defaultVal;
	else {
		XSValue::Status status;
		auto_ptr<XSValue> val(XSValue::getActualValue(sft, XSValue::dt_boolean, status));

		if( status != XSValue::st_Init ) {

			stringstream s;
			s << "Invalid value for '" << attribute << "' on stream QoS configuration";

			CDBProblemExImpl cdbProblemEx(__FILE__, __LINE__, __PRETTY_FUNCTION__);
			cdbProblemEx.setDetail(s.str().c_str());
			throw cdbProblemEx;
		}

		returnVal = val->fData.fValue.f_bool;
	}

	return returnVal;
}

const XMLCh* BulkDataConfigurationParser::getAttrValue(DOMNode *node, const char* name) {

	DOMNamedNodeMap *attrs = node->getAttributes();
	if( attrs  == 0 )
		return 0;

	XMLCh* nameXmlCh = XMLString::transcode(name);
	DOMNode *attrAsNode = attrs->getNamedItem( nameXmlCh );
	XMLString::release(&nameXmlCh);

	if( attrAsNode == 0 )
		return 0;

	DOMAttr *attr = dynamic_cast<DOMAttr *>(attrAsNode);
	return attr->getValue();

}

string BulkDataConfigurationParser::getQosProfile(const char *profileName, const char *baseProfile, DOMNode *node) {

	stringstream s;
	s << "<qos_profile name=\"" << profileName << "\" base_name=\"" << DDSConfiguration::DEFAULT_LIBRARY << "::" << baseProfile << "\">";

	DOMNodeList *children = node->getChildNodes();
	for(unsigned int i=0; i!= children->getLength(); i++) {
		DOMNode *child = children->item(i);
		if( child->getNodeType() == DOMNode::ELEMENT_NODE ) {
			MemBufFormatTarget *formatTarget = new MemBufFormatTarget();
			m_writer->writeNode(formatTarget, *child);
			s << (char*)formatTarget->getRawBuffer();
			delete formatTarget;
		}
	}

	s << "</qos_profile>";
	return s.str();
}

string BulkDataConfigurationParser::getStrURIforStream(list<string> profiles) {

	list<string>::iterator it = profiles.begin();

	stringstream s;

	for(; it != profiles.end(); it++)
		s << *it;

	string string = s.str();
	return string;
}

std::set<char const *> BulkDataConfigurationParser::getAllSenderStreamNames() {
	std::set<char const *> names;
	SenderCfgMap::iterator it;
	for(it = senderConfigMap_m.begin(); it != senderConfigMap_m.end(); it++)
		names.insert(it->first.c_str());
	return names;
}

std::set<char const *> BulkDataConfigurationParser::getAllReceiverStreamNames() {
	std::set<char const *> names;
	ReceiverCfgMap::iterator it;
	for(it = receiverConfigMap_m.begin(); it != receiverConfigMap_m.end(); it++) {
		names.insert(it->first.c_str());
	}
	return names;
}

std::set<char const *> BulkDataConfigurationParser::getAllSenderFlowNames(char const *streamName) {
	std::set<char const *> names;
	SenderCfgMap::iterator it;
	for(it = senderConfigMap_m.begin(); it != senderConfigMap_m.end(); it++) {

		if( ACE_OS::strcmp(it->first.c_str(), streamName) == 0 ) {

			std::map<std::string, SenderFlowConfiguration *> flows = it->second.flowsCfgMap;
			std::map<std::string, SenderFlowConfiguration *>::iterator it2;
			for(it2 = flows.begin(); it2 != flows.end(); it2++)
				names.insert(it2->first.c_str());

			break;
		}
	}

	return names;
}

std::set<char const *> BulkDataConfigurationParser::getAllReceiverFlowNames(char const *streamName) {

	std::set<char const *> names;
	ReceiverCfgMap::iterator it;
	for(it = receiverConfigMap_m.begin(); it != receiverConfigMap_m.end(); it++) {

		if( ACE_OS::strcmp(it->first.c_str(), streamName) == 0 ) {

			std::map<std::string, ReceiverFlowConfiguration *> flows = it->second.flowsCfgMap;
			std::map<std::string, ReceiverFlowConfiguration *>::iterator it2;
			for(it2 = flows.begin(); it2 != flows.end(); it2++)
				names.insert(it2->first.c_str());

			break;
		}
	}

	return names;
}

SenderStreamConfiguration * BulkDataConfigurationParser::getSenderStreamConfiguration(char const *streamName) {
	SenderCfgMap::iterator it;
	for(it = senderConfigMap_m.begin(); it != senderConfigMap_m.end(); it++)
		if( ACE_OS::strcmp(it->first.c_str(), streamName) == 0 )
			return it->second.streamCfg;

	return 0;
}

SenderFlowConfiguration * BulkDataConfigurationParser::getSenderFlowConfiguration(char const *streamName, char const *flowName) {

	SenderCfgMap::iterator it;
	for(it = senderConfigMap_m.begin(); it != senderConfigMap_m.end(); it++) {

		if( ACE_OS::strcmp(it->first.c_str(), streamName) == 0 ) {

			std::map<std::string, SenderFlowConfiguration *> flows = it->second.flowsCfgMap;
			std::map<std::string, SenderFlowConfiguration *>::iterator it2;
			for(it2 = flows.begin(); it2 != flows.end(); it2++)
				if( ACE_OS::strcmp(it2->first.c_str(), flowName) == 0 )
					return it2->second;

		}
	}

	return 0;
}

ReceiverStreamConfiguration * BulkDataConfigurationParser::getReceiverStreamConfiguration(char const *streamName) {
	ReceiverCfgMap::iterator it;
	for(it = receiverConfigMap_m.begin(); it != receiverConfigMap_m.end(); it++)
		if( ACE_OS::strcmp(it->first.c_str(), streamName) == 0 ) {
			ReceiverStreamConfiguration *config = it->second.streamCfg;
			return config;
		}

	return 0;
}

ReceiverFlowConfiguration * BulkDataConfigurationParser::getReceiverFlowConfiguration(char const *streamName, char const *flowName) {

	ReceiverCfgMap::iterator it;
	for(it = receiverConfigMap_m.begin(); it != receiverConfigMap_m.end(); it++) {

		if( ACE_OS::strcmp(it->first.c_str(), streamName) == 0 ) {

			std::map<std::string, ReceiverFlowConfiguration *> flows = it->second.flowsCfgMap;
			std::map<std::string, ReceiverFlowConfiguration *>::iterator it2;
			for(it2 = flows.begin(); it2 != flows.end(); it2++)
				if( ACE_OS::strcmp(it2->first.c_str(), flowName) == 0 )
					return it2->second;

		}
	}

	return 0;
}
