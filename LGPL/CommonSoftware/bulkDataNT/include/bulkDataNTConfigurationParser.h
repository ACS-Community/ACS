#ifndef _BULKDATA_NT_CONFIGURATION_PARSER_H_
#define _BULKDATA_NT_CONFIGURATION_PARSER_H_

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <map>
#include <string>
#include <set>

#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMWriter.hpp>
#include <xercesc/parsers/AbstractDOMParser.hpp>

#include "bulkDataNTSenderStream.h"
#include "bulkDataNTReceiverStream.h"

using namespace std;
using namespace xercesc;

namespace AcsBulkdata
{

	class BulkDataConfigurationParser {

	public:
		BulkDataConfigurationParser();
		~BulkDataConfigurationParser();
		list<BulkDataNTSenderStream *>* parseSenderConfig(char const *config);
		void parseReceiverConfig(char const *config);

	private:
		char* getAttrValue(DOMNode *node, const char * name);
		void getSerializedElement(DOMNode *node, string &s);
		void addQoSToProfile(const char *profile, DOMNode *node);
		void parseConfig(const char *config,
			const char* const reqStreamNodeName,
			const char* const reqFlowNodeName,
			const char* const reqStreamQoSNodeName,
			const char* const reqFlowQoSNodeName);
		void clearCollections();
		void printEntities();

		map<string, string> m_profiles;
		map<char *, set<char*> > m_entities;
		DOMWriter *m_writer;
		AbstractDOMParser *m_parser;

		static const char* const SENDER_STREAM_NODENAME;
		static const char* const SENDER_STREAM_QOS_NODENAME;
		static const char* const SENDER_FLOW_NODENAME;
		static const char* const SENDER_FLOW_QOS_NODENAME;
		static const char* const RECEIVER_STREAM_NODENAME;
		static const char* const RECEIVER_STREAM_QOS_NODENAME;
		static const char* const RECEIVER_FLOW_NODENAME;
		static const char* const RECEIVER_FLOW_QOS_NODENAME;
	};

};

#endif
