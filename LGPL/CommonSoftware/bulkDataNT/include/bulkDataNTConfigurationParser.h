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

		typedef struct {
			SenderStreamConfiguration streamCfg;
			map<string, SenderFlowConfiguration> flowsCfgMap;
		} SenderCfg;

		typedef struct {
			ReceiverStreamConfiguration streamCfg;
			map<string, ReceiverFlowConfiguration> flowsCfgMap;
		} ReceiverCfg;

		/**
		 * Constructor
		 */
		BulkDataConfigurationParser();

		/**
		 * Destructor
		 */
		~BulkDataConfigurationParser();

		/**
		 * Given an XML document, parses it an retrieves the list of sender streams
		 * that can be found. Each stream, and its corresponding flows, are properly
		 * configured depending on the QoS settings coming from the XML document
		 */
		void parseSenderConfig(char const *configXML, map<string, SenderCfg> &configMap);

		/**
		 * Given an XML document, parses it an retrieves the list of receiver streams
		 * that can be found. Each stream, and its corresponding flows, are properly
		 * configured depending on the QoS settings coming from the XML document
		 */
		void parseReceiverConfig(char const *configXML, map<string, ReceiverCfg> &configMap);

	private:

		char* getAttrValue(DOMNode *node, const char * name);

		void getSerializedElement(DOMNode *node, string &s);

		void addQoSToProfile(const char *stream, const char *profileName, const char* baseProfile, DOMNode *node);

		void parseConfig(const char *config,
			const char* const reqStreamNodeName,
			const char* const reqFlowNodeName,
			const char* const reqStreamQoSNodeName,
			const char* const reqFlowQoSNodeName,
			const char* const defaultStreamProfile,
			const char* const defaultFlowProfile);

		template<class CfgS, class StreamConfigT, class FlowConfigT>
		void populateConfiguration(map<string, CfgS> &configMap);

		void clearCollections();

		void printEntities();

		string getStrURIforStream(char *streamName);

		// map<streamName, map<flowName, strURLPiece>>
		map<string, map<string, string> > m_profiles;
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

		static const char* const DYNAMIC_LIBRARY_NAME;
	};

};

#include "bulkDataNTConfigurationParser.i"

#endif
