#ifndef _BULKDATA_NT_CONFIGURATION_PARSER_H_
#define _BULKDATA_NT_CONFIGURATION_PARSER_H_

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
* "@(#) $Id: bulkDataNTConfigurationParser.h,v 1.7 2011/11/10 11:10:33 rtobar Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/

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

namespace AcsBulkdata
{

	class BulkDataConfigurationParser {

	public:

		typedef struct {
			SenderStreamConfiguration streamCfg;
			std::map<std::string, SenderFlowConfiguration> flowsCfgMap;
		} SenderCfg;

		typedef struct {
			ReceiverStreamConfiguration streamCfg;
			std::map<std::string, ReceiverFlowConfiguration> flowsCfgMap;
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
		void parseSenderConfig(char const *configXML, std::map<std::string, SenderCfg> &configMap);

		/**
		 * Given an XML document, parses it an retrieves the list of receiver streams
		 * that can be found. Each stream, and its corresponding flows, are properly
		 * configured depending on the QoS settings coming from the XML document
		 */
		void parseReceiverConfig(char const *configXML, std::map<std::string, ReceiverCfg> &configMap);

	private:

		char* getAttrValue(xercesc::DOMNode *node, const char * name);

		void getSerializedElement(xercesc::DOMNode *node, std::string &s);

		void addQoSToProfile(const char *stream, const char *profileName, const char* baseProfile, xercesc::DOMNode *node);

		void parseConfig(const char *config,
			const char* const reqStreamNodeName,
			const char* const reqFlowNodeName,
			const char* const reqStreamQoSNodeName,
			const char* const reqFlowQoSNodeName,
			const char* const defaultStreamProfile,
			const char* const defaultFlowProfile);

		template<class CfgS, class StreamConfigT, class FlowConfigT>
		void populateConfiguration(std::map<std::string, CfgS> &configMap);

		void clearCollections();

		void printEntities();

		std::string getStrURIforStream(char *streamName);

		// map<streamName, map<flowName, strURLPiece>>
		std::map<std::string, std::map<std::string, std::string> > m_profiles;
		std::map<char *, std::set<char*> > m_entities;
		xercesc::DOMWriter *m_writer;
		xercesc::AbstractDOMParser *m_parser;

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
