#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/framework/MemBufInputSource.hpp>
#include <xercesc/framework/MemBufFormatTarget.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <stdio.h>

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

list<BulkDataNTSenderStream *>* BulkDataConfigurationParser::parseSenderConfig(const char *config) {

	// Collect the information
	parseConfig(config, SENDER_STREAM_NODENAME, SENDER_FLOW_NODENAME, SENDER_STREAM_QOS_NODENAME, SENDER_FLOW_QOS_NODENAME);

	// Create the senders from the collected information
	list<BulkDataNTSenderStream *>* senders = new list<BulkDataNTSenderStream *>();
	try {
		map<char*, set<char *> >::iterator mit;
		set<char *>::iterator sit;
		for(mit = m_entities.begin(); mit != m_entities.end(); mit++) {

			SenderStreamConfiguration *streamCfg = new SenderStreamConfiguration();
			if( m_profiles.find(mit->first) != m_profiles.end() ) {
				streamCfg->libraryQos    = "DynamicLib";
				streamCfg->profileQos    = mit->first;
				streamCfg->urlProfileQoS = m_profiles[mit->first];
				printf("Profile %s is: ==== %s ====\n", mit->first, m_profiles[mit->first].c_str());
			}
			else {
				printf("Entity %s has no profile\n", mit->first);
			}
			BulkDataNTSenderStream *senderStream = new BulkDataNTSenderStream(mit->first, *streamCfg);
			for(sit = mit->second.begin(); sit != mit->second.end(); sit++) {
				SenderFlowConfiguration *flowCfg = new SenderFlowConfiguration();
				// TODO: how do we set the profile URL here? I think we don't...
				senderStream->createFlow(*sit, *flowCfg);
			}
			senders->push_back(senderStream);
		}
	} catch(StreamCreateProblemExImpl &ex) {
		// delete all senders that we could possibly created
		while(senders->size() > 0) {
			delete senders->back();
			senders->pop_back();
		}
		throw ex;
	} catch(FlowCreateProblemExImpl &ex) {
		// delete all senders that we could possibly created
		while(senders->size() > 0) {
			delete senders->back();
			senders->pop_back();
		}
		throw ex;
	}

	return senders;
}

void BulkDataConfigurationParser::parseReceiverConfig(const char *config) {
	parseConfig(config, RECEIVER_STREAM_NODENAME, RECEIVER_FLOW_NODENAME, RECEIVER_STREAM_QOS_NODENAME, RECEIVER_FLOW_QOS_NODENAME);
	printEntities();
}

void BulkDataConfigurationParser::clearCollections() {

	map<char*, set<char *> >::iterator mit2;
	set<char*>::iterator sit;

	m_profiles.clear();

	for(mit2 = m_entities.begin(); mit2 != m_entities.end(); mit2++) {
		for(sit = (*mit2).second.begin(); sit != (*mit2).second.end(); sit++)
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

void BulkDataConfigurationParser::parseConfig(const char *config,
	const char* const reqStreamNodeName,
	const char* const reqFlowNodeName,
	const char* const reqStreamQoSNodeName,
	const char* const reqFlowQoSNodeName)
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
			if( strcmp(childNodeName, reqStreamQoSNodeName) == 0 ) {
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
					profileName.append("-");
					profileName.append(flowName);
					addQoSToProfile(profileName.c_str(), childFlowNode);
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

void BulkDataConfigurationParser::addQoSToProfile(const char *profile, DOMNode *node) {

	if( m_profiles.find(profile) == m_profiles.end() ) {
		string s("str://\"<dds><qos_library name=\"DynamicLib\"><qos_profile name=\"");
		s.append(profile);
		s.append("\">");
		m_profiles[profile] = s;
	}
	string profileString = m_profiles[profile];

	DOMNodeList *children = node->getChildNodes();
	for(unsigned int i=0; i!= children->getLength(); i++) {
		DOMNode *child = children->item(i);
		if( child->getNodeType() == DOMNode::ELEMENT_NODE ) {
			MemBufFormatTarget *formatTarget = new MemBufFormatTarget();
			m_writer->writeNode(formatTarget, *child);
			profileString.append((char*)formatTarget->getRawBuffer());
			delete formatTarget;
		}
	}

	profileString.append("</qos_profile></qos_library></dds>\"");
	m_profiles[profile] = profileString;
}
