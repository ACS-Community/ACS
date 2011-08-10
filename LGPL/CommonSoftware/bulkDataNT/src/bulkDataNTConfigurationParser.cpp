#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/framework/MemBufInputSource.hpp>
#include <xercesc/framework/MemBufFormatTarget.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <stdio.h>
#include <list>

#include "ACS_BD_Errors.h"
#include "bulkDataNTConfigurationParser.h"

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

BulkDataConfigurationParser::BulkDataConfigurationParser() :
   m_profiles(),
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

	list<const char*> l;
	map<const char*, string>::iterator mit;
	list<const char*>::iterator lit;

	// collect keys, then free them -- maybe there's an easier way?
	for(mit = m_profiles.begin(); mit!=m_profiles.end(); mit++)
		l.push_back((*mit).first);
	for(lit = l.begin(); lit!=l.end(); lit++)
		free((void*)*lit); // counterpart for strdup()
	l.clear();

	m_profiles.clear();
	m_writer->release();

	delete m_parser;
	XMLPlatformUtils::Terminate();
}

void BulkDataConfigurationParser::parseSenderConfig(const char *config) {
	parseConfig(config, SENDER_STREAM_NODENAME, SENDER_FLOW_NODENAME, SENDER_STREAM_QOS_NODENAME, SENDER_FLOW_QOS_NODENAME);
}

void BulkDataConfigurationParser::parseReceiverConfig(const char *config) {
	parseConfig(config, RECEIVER_STREAM_NODENAME, RECEIVER_FLOW_NODENAME, RECEIVER_STREAM_QOS_NODENAME, RECEIVER_FLOW_QOS_NODENAME);
}

void BulkDataConfigurationParser::parseConfig(const char *config,
	const char* const reqStreamNodeName,
	const char* const reqFlowNodeName,
	const char* const reqStreamQoSNodeName,
	const char* const reqFlowQoSNodeName)
{

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

		printf("Processing %s '%s'\n", reqStreamNodeName, streamName);

		// For each sender stream, check the QoS and the underlying flow nodes
		DOMNodeList *streamChildrenNodesList = streamNode->getChildNodes();
		for(unsigned int j=0; j!= streamChildrenNodesList->getLength(); j++) {

			DOMNode *streamChildNode = streamChildrenNodesList->item(j);
			if( streamChildNode->getNodeType() != DOMNode::ELEMENT_NODE )
				continue;

			char *childNodeName = XMLString::transcode(streamChildNode->getNodeName());

			// The Sender/ReceiverStreamQoS is appended to the str:// URI
			if( strcmp(childNodeName, reqStreamQoSNodeName) == 0 ) {
				printf("  Processing %s\n", reqStreamQoSNodeName);
				addQoSToProfile(streamName, streamChildNode);
			}

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
					XMLString::release(&streamName);
					XMLString::release(&flowName);
					throw cdbProblemEx;
				}
				printf("  Processing %s '%s'\n", reqFlowNodeName, flowName);

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
						XMLString::release(&flowName);
						XMLString::release(&childNodeName);
						XMLString::release(&streamName);
						XMLString::release(&childFlowNodeName);
						throw cdbProblemEx;
					}
					XMLString::release(&childFlowNodeName);

					string profileName(streamName);
					profileName.append("-");
					profileName.append(flowName);
					addQoSToProfile(profileName.c_str(), childFlowNode);
				}

				XMLString::release(&flowName);
			}

			XMLString::release(&childNodeName);
		}

		XMLString::release(&streamName);
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

void BulkDataConfigurationParser::addQoSToProfile(const char *profile, DOMNode *node) {
	if( m_profiles.find(profile) == m_profiles.end() )
		profile = strdup(profile);
	string s = m_profiles[profile];
	getSerializedElement(node, s);
}

void BulkDataConfigurationParser::getSerializedElement(DOMNode *node, std::string &s) {

	DOMNodeList *children = node->getChildNodes();
	for(unsigned int i=0; i!= children->getLength(); i++) {
		DOMNode *child = children->item(i);
		if( child->getNodeType() == DOMNode::ELEMENT_NODE ) {
			MemBufFormatTarget *formatTarget = new MemBufFormatTarget();
			m_writer->writeNode(formatTarget, *child);
			s.append((const char*)formatTarget->getRawBuffer());
			delete formatTarget;
		}
	}

}
