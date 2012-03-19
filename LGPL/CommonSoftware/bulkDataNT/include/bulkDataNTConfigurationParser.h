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
* "@(#) $Id: bulkDataNTConfigurationParser.h,v 1.12 2012/03/19 18:28:28 rtobar Exp $"
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

	class XMLChSP {

	public:
		XMLChSP();
		XMLChSP(char* xmlCh);
		~XMLChSP();
		char * get();
		XMLChSP& operator=(const char*);
		void release();

	private:
		char* xmlch_m;

	};

	class BulkDataConfigurationParser {

	private:

		const char                 *m_baseLibrary;
		xercesc::DOMWriter         *m_writer;
		xercesc::AbstractDOMParser *m_parser;

		typedef struct _SenderCfg {
			SenderStreamConfiguration *streamCfg;
			std::map<std::string, SenderFlowConfiguration *> flowsCfgMap;
		} SenderCfg;

		typedef struct _ReceiverCfg {
			ReceiverStreamConfiguration *streamCfg;
			std::map<std::string, ReceiverFlowConfiguration *> flowsCfgMap;
		} ReceiverCfg;

		typedef std::map<std::string, ReceiverCfg> ReceiverCfgMap;
		typedef std::map<std::string, SenderCfg>   SenderCfgMap;

		ReceiverCfgMap receiverConfigMap_m;
		SenderCfgMap   senderConfigMap_m;

		static const char* const SENDER_STREAM_NODENAME;
		static const char* const SENDER_STREAM_QOS_NODENAME;
		static const char* const SENDER_FLOW_NODENAME;
		static const char* const SENDER_FLOW_QOS_NODENAME;
		static const char* const RECEIVER_STREAM_NODENAME;
		static const char* const RECEIVER_STREAM_QOS_NODENAME;
		static const char* const RECEIVER_FLOW_NODENAME;
		static const char* const RECEIVER_FLOW_QOS_NODENAME;

		static const char* const DYNAMIC_LIBRARY_NAME;

		enum ParsingType {
			SENDER,
			RECEIVER
		};

		struct ParsingInfo {
			ParsingType type;
			const char* const reqStreamNodeName;
			const char* const reqStreamQoSNodeName;
			const char* const reqFlowNodeName;
			const char* const reqFlowQoSNodeName;
			const char* const defaultStreamProfileName;
			const char* const defaultFlowProfileName;
		};

		const static struct ParsingInfo SENDER_PARSING_INFO;
		const static struct ParsingInfo RECEIVER_PARSING_INFO;


		void parseConfig(const char *config, const struct ParsingInfo &parsingInfo);

		const XMLCh* getAttrValue(xercesc::DOMNode *node, const char * name);

		double getDoubleFromAttribute(xercesc::DOMNode *node, const char * attribute, double defaultVal);

		std::string getStringFromAttribute(xercesc::DOMNode *node, const char * attribute, std::string defaultVal);

		bool getBooleanFromAttribute(xercesc::DOMNode *node, const char * attribute, bool defaultVal);

		std::string getQosProfile(const char *profileName, const char *baseProfile, xercesc::DOMNode *node);

		std::string getStrURIforStream(std::list<std::string> profiles);

		void cleanReceiverConfigs();

		void cleanSenderConfigs();

		void cleanConfig(ReceiverCfg *recvConfig, SenderCfg *senderConfig, ParsingType type);

	public:

		/**
		 * Parser constructor. Its creation needs a base library as a mandatory argument, which
		 * will prevent name duplication among configured entities. An example of potential
		 * danger would be a process that creates the same streams/flows combinations in two
		 * separate places. Such a process would get a failure when creating the second set
		 * of streams/flows, since the "QoS profiles" (internal representations of the DDS
		 * configuration entities) would belong to the same "library", and would already
		 * exists on the RTI world.
		 *
		 * @param baseLibraryName The base library name to be used by all the configuration
		 *   objects that this parser creates. An example would be the ACS component name,
		 *   or any other particular identification string.
		 */
		BulkDataConfigurationParser(const char *baseLibraryName);

		/**
		 * Destructor
		 */
		~BulkDataConfigurationParser();

		/**
		 * Given an XML document, parses it an retrieves the list of sender streams
		 * that can be found. Each stream, and its corresponding flows, are properly
		 * configured depending on the QoS settings coming from the XML document
		 */
		void parseSenderConfig(char const *configXML);
		/**
		 * Given an XML document, parses it an retrieves the list of receiver streams
		 * that can be found. Each stream, and its corresponding flows, are properly
		 * configured depending on the QoS settings coming from the XML document
		 */
		void parseReceiverConfig(char const *configXML);

		/**
		 * Returns a set of strings containing all Sender Stream names found in
		 * the parsed XML document. If none was found, an empty set is returned
		 */
		std::set<char const *> getAllSenderStreamNames();

		/**
		 * Returns a set of strings containing all Receiver Stream names found in
		 * the parsed XML document. If none was found, an empty set is returned
		 */
		std::set<char const *> getAllReceiverStreamNames();

		/**
		 * Returns, for the given Sender stream name, a set of strings containing all
		 * Sender Flow names found in the parsed XML document. If none was found, an
		 * empty set is returned
		 *
		 * @param streamName The name of the sender stream
		 * @return A set containing all flow names for the given stream name
		 */
		std::set<char const *> getAllSenderFlowNames(char const *streamName);

		/**
		 * Returns, for the given Receiver stream name, a set of strings containing all
		 * Receiver Flow names found in the parsed XML document. If none was found, an
		 * empty set is returned
		 */
		std::set<char const *> getAllReceiverFlowNames(char const *streamName);

		/**
		 * Returns, if found during parsing, the configuration object for the given
		 * Sender stream.
		 *
		 * @param streamName The name of the Sender stream
		 * @return The configuration for the given Sender stream, NULL if it doesn't exist
		 */
		SenderStreamConfiguration * getSenderStreamConfiguration(char const *streamName);

		/**
		 * Returns, if found during parsing, the configuration object for the given
		 * Sender flow.
		 *
		 * @param streamName The name of the Sender stream where the given flow resides
		 * @param flowName The name of the Sender flow
		 * @return The configuration for the given Sender flow, NULL if it doesn't exist
		 */
		SenderFlowConfiguration * getSenderFlowConfiguration(char const *streamName, char const *flowName);

		/**
		 * Returns, if found during parsing, the configuration object for the given
		 * Receiver stream.
		 *
		 * @param streamName The name of the Receiver stream
		 * @return The configuration for the given Receiver stream, NULL if it doesn't exist
		 */
		ReceiverStreamConfiguration * getReceiverStreamConfiguration(char const *streamName);

		/**
		 * Returns, if found during parsing, the configuration object for the given
		 * Receiver flow.
		 *
		 * @param streamName The name of the Receiver stream where the given flow resides
		 * @param flowName The name of the Receiver flow
		 * @return The configuration for the given Receiver flow, NULL if it doesn't exist
		 */
		ReceiverFlowConfiguration * getReceiverFlowConfiguration(char const *streamName, char const *flowName);
	};

};

#endif
