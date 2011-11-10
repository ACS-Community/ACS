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
* "@(#) $Id: bulkDataNTConfigurationParser.cpp,v 1.11 2011/11/10 11:10:33 rtobar Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/

#include <xercesc/util/PlatformUtils.hpp>
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
const char* const BulkDataConfigurationParser::SENDER_FLOW_NODENAME         = "Flow";
const char* const BulkDataConfigurationParser::SENDER_FLOW_QOS_NODENAME     = "DDSSenderFlowQoS";

const char* const BulkDataConfigurationParser::RECEIVER_STREAM_NODENAME     = "ReceiverStream";
const char* const BulkDataConfigurationParser::RECEIVER_STREAM_QOS_NODENAME = "DDSReceiverStreamQoS";
const char* const BulkDataConfigurationParser::RECEIVER_FLOW_NODENAME       = "ReceiverFlow";
const char* const BulkDataConfigurationParser::RECEIVER_FLOW_QOS_NODENAME   = "DDSReceiverFlowQoS";

const char* const BulkDataConfigurationParser::DYNAMIC_LIBRARY_NAME         = "DynamicLib";

BulkDataConfigurationParser::BulkDataConfigurationParser() :
   m_profiles(),
   m_entities(),
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
	m_parser->setValidationScheme(XercesDOMParser::Val_Always);
}

BulkDataConfigurationParser::~BulkDataConfigurationParser() {
	clearCollections();
	m_writer->release();

	delete m_parser;
	XMLPlatformUtils::Terminate();
}

void BulkDataConfigurationParser::clearCollections() {

	map<char*, set<char *> >::iterator mit2;
	set<char*>::iterator sit;

	m_profiles.clear();

	for(mit2 = m_entities.begin(); mit2 != m_entities.end(); mit2++) {
		for(sit = mit2->second.begin(); sit != mit2->second.end(); sit++)
			XMLString::release((char **)&(*sit));
		(*mit2).second.clear();
		XMLString::release((char **)&mit2->first);
	}
	m_entities.clear();
}

void BulkDataConfigurationParser::printEntities() {
	map<char *, set<char*> >::iterator it;
	set<char *>::iterator it2;

	for(it = m_entities.begin(); it != m_entities.end(); it++) {
		printf("Stream '%s'\n", it->first);
		for(it2 = (*it).second.begin(); it2 != (*it).second.end(); it2++) {
			printf("  Flow '%s'\n", (*it2));
		}
	}
}

void BulkDataConfigurationParser::parseSenderConfig(const char *config, map<string, SenderCfg> &configMap) {
	parseConfig(config, SENDER_STREAM_NODENAME, SENDER_FLOW_NODENAME, SENDER_STREAM_QOS_NODENAME, SENDER_FLOW_QOS_NODENAME,
			DDSConfiguration::DEFAULT_SENDER_STREAM_PROFILE, DDSConfiguration::DEFAULT_SENDER_FLOW_PROFILE);
	populateConfiguration<SenderCfg, SenderStreamConfiguration, SenderFlowConfiguration>(configMap);
}

void BulkDataConfigurationParser::parseReceiverConfig(const char *config, map<string, ReceiverCfg> &configMap) {
	parseConfig(config, RECEIVER_STREAM_NODENAME, RECEIVER_FLOW_NODENAME, RECEIVER_STREAM_QOS_NODENAME, RECEIVER_FLOW_QOS_NODENAME,
			DDSConfiguration::DEFAULT_RECEIVER_STREAM_PROFILE, DDSConfiguration::DEFAULT_RECEIVER_FLOW_PROFILE);
	populateConfiguration<ReceiverCfg, ReceiverStreamConfiguration, ReceiverFlowConfiguration>(configMap);
}

void BulkDataConfigurationParser::parseConfig(const char *config,
	const char* const reqStreamNodeName,
	const char* const reqFlowNodeName,
	const char* const reqStreamQoSNodeName,
	const char* const reqFlowQoSNodeName,
	const char* const defaultStreamProfileName,
	const char* const defaultFlowProfileName)
{

	clearCollections();

	// Parse the XML string and get the DOM Document
	m_parser->reset();
	try {
		MemBufInputSource is((const XMLByte *)config, strlen(config), "id");
		m_parser->parse(is);
	} catch(const XMLException& toCatch) {

		char *m = XMLString::transcode(toCatch.getMessage());

		CDBProblemExImpl cdbProblemEx(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		cdbProblemEx.setDetail(m);

		XMLString::release(&m);

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

		char *nodeName = XMLString::transcode(streamNode->getNodeName());
		if( strcmp(nodeName,reqStreamNodeName) != 0 ) {

			string s("Node name is different from '");
			s.append(reqStreamNodeName);
			s.append("': ");
			s.append(nodeName);

			CDBProblemExImpl cdbProblemEx(__FILE__, __LINE__, __PRETTY_FUNCTION__);
			cdbProblemEx.setDetail(s.c_str());

			XMLString::release(&nodeName);

			throw cdbProblemEx;
		}
		XMLString::release(&nodeName);

		char* streamName = getAttrValue(streamNode, "Name");
		if( streamName == 0 ) {

			string s("Node '");
			s.append(reqStreamNodeName);
			s.append("' doesn't have attribute 'Name'");

			CDBProblemExImpl cdbProblemEx(__FILE__, __LINE__, __PRETTY_FUNCTION__);
			cdbProblemEx.setDetail(s.c_str());

			XMLString::release(&streamName);

			throw cdbProblemEx;
		}

		// Check for repeated streams
		if( m_entities.find(streamName) != m_entities.end() ) {

			string s("Repeated stream: '");
			s.append(streamName);
			s.append("'");

			CDBProblemExImpl cdbProblemEx(__FILE__, __LINE__, __PRETTY_FUNCTION__);
			cdbProblemEx.setDetail(s.c_str());

			XMLString::release(&streamName);

			throw cdbProblemEx;
		}
		m_entities[streamName] = set<char*>();

		// For each sender stream, check the QoS and the underlying flow nodes
		DOMNodeList *streamChildrenNodesList = streamNode->getChildNodes();
		for(unsigned int j=0; j!= streamChildrenNodesList->getLength(); j++) {

			DOMNode *streamChildNode = streamChildrenNodesList->item(j);
			if( streamChildNode->getNodeType() != DOMNode::ELEMENT_NODE )
				continue;

			char *childNodeName = XMLString::transcode(streamChildNode->getNodeName());

			// The Sender/ReceiverStreamQoS is appended to the str:// URI
			if( strcmp(childNodeName, reqStreamQoSNodeName) == 0 )
				addQoSToProfile(streamName, streamName, defaultStreamProfileName, streamChildNode);

			// Process the Flow nodes
			else if( strcmp(childNodeName, reqFlowNodeName) == 0 ) {

				char* flowName = getAttrValue(streamChildNode, "Name");
				if( flowName == 0 ) {

					string s("Node '");
					s.append(reqFlowNodeName);
					s.append("' doesn't have attribute 'Name'");

					CDBProblemExImpl cdbProblemEx(__FILE__, __LINE__, __PRETTY_FUNCTION__);
					cdbProblemEx.setDetail(s.c_str());

					XMLString::release(&childNodeName);

					throw cdbProblemEx;
				}

				// Check for repeated flows inside the stream
				if( m_entities[streamName].find(flowName) != m_entities[streamName].end() ) {

					string s("Repeated flow in stream '");
					s.append(streamName);
					s.append("': '");
					s.append(flowName);
					s.append("'");

					CDBProblemExImpl cdbProblemEx(__FILE__, __LINE__, __PRETTY_FUNCTION__);
					cdbProblemEx.setDetail(s.c_str());

					XMLString::release(&childNodeName);

					throw cdbProblemEx;
				}
				m_entities[streamName].insert(flowName);

				// Flow nodes contain Sender/ReceiverFlowQoS
				DOMNodeList *flowNodesList = streamChildNode->getChildNodes();
				for(unsigned int k=0; k!= flowNodesList->getLength(); k++) {

					DOMNode *childFlowNode = flowNodesList->item(k);
					if( childFlowNode->getNodeType() != DOMNode::ELEMENT_NODE )
						continue;

					char *childFlowNodeName = XMLString::transcode(childFlowNode->getNodeName());
					if( strcmp(childFlowNodeName,reqFlowQoSNodeName) != 0 ) {

						string s("Node name is different from '");

						s.append(reqFlowQoSNodeName);
						s.append("': ");
						s.append(childFlowNodeName);

						CDBProblemExImpl cdbProblemEx(__FILE__, __LINE__, __PRETTY_FUNCTION__);
						cdbProblemEx.setDetail(s.c_str());

						XMLString::release(&childNodeName);
						XMLString::release(&childFlowNodeName);

						throw cdbProblemEx;
					}
					XMLString::release(&childFlowNodeName);

					string profileName(streamName);
					profileName.append("#");
					profileName.append(flowName);
					addQoSToProfile(streamName, profileName.c_str(), defaultFlowProfileName,childFlowNode);
				}
			}

			XMLString::release(&childNodeName);
		}
	}

}

char* BulkDataConfigurationParser::getAttrValue(DOMNode *node, const char* name) {

	DOMNamedNodeMap *attrs = node->getAttributes();
	if( attrs  == 0 )
		return 0;

	XMLCh* nameXmlCh = XMLString::transcode(name);
	DOMNode *attrAsNode = attrs->getNamedItem( nameXmlCh );
	XMLString::release(&nameXmlCh);

	if( attrAsNode == 0 )
		return 0;

	DOMAttr *attr = dynamic_cast<DOMAttr *>(attrAsNode);
	return XMLString::transcode( attr->getValue() );

}

void BulkDataConfigurationParser::addQoSToProfile(const char *stream, const char *profileName, const char *baseProfile, DOMNode *node) {

	map<string, string> streamProfileMap = m_profiles[stream];

	string profileStr("<qos_profile name=\"");
	profileStr.append(profileName);
	profileStr.append("\" base_name=\"");
	profileStr.append(DDSConfiguration::DEFAULT_LIBRARY);
	profileStr.append("::");
	profileStr.append(baseProfile);
	profileStr.append("\">");

	DOMNodeList *children = node->getChildNodes();
	for(unsigned int i=0; i!= children->getLength(); i++) {
		DOMNode *child = children->item(i);
		if( child->getNodeType() == DOMNode::ELEMENT_NODE ) {
			MemBufFormatTarget *formatTarget = new MemBufFormatTarget();
			m_writer->writeNode(formatTarget, *child);
			profileStr.append((char*)formatTarget->getRawBuffer());
			delete formatTarget;
		}
	}

	profileStr.append("</qos_profile>");
	streamProfileMap[profileName] = profileStr;
	m_profiles[stream] = streamProfileMap;
}

string BulkDataConfigurationParser::getStrURIforStream(char * streamName) {

	map<string, string> profiles = m_profiles[streamName];
	map<string, string>::iterator it = profiles.begin();

	string s("<dds><qos_library name=\"");
	s.append(DYNAMIC_LIBRARY_NAME);
	s.append("\">");
	for(; it != profiles.end(); it++)
		s.append(it->second);
	s.append("</qos_library></dds>");

	return s;
}
