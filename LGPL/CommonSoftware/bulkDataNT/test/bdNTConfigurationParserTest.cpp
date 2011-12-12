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
* "@(#) $Id: bdNTConfigurationParserTest.cpp,v 1.14 2011/12/12 18:09:02 rtobar Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/
#include <stdio.h>

#include "bulkDataNTConfiguration.h"
#include "bulkDataNTConfigurationParser.h"
#include "ACS_BD_Errors.h"
#include <iostream>
#include <fstream>

using namespace std;
using namespace xercesc;
using namespace AcsBulkdata;
using namespace ACS_BD_Errors;

void printConfigMap(BulkDataConfigurationParser &parser,
		set<const char *> (BulkDataConfigurationParser::*streamNamesGetter)(void),
		set<const char *> (BulkDataConfigurationParser::*flowNamesGetter)(const char *)) {

	set<const char *> streamNames = (parser.*streamNamesGetter)();
	if( streamNames.size() == 0 ) {
		cout << "No streams found" << endl;
		return;
	}

	cout << "Number of streams: " << streamNames.size() << endl;

	set<const char *>::iterator it;
	for(it = streamNames.begin(); it != streamNames.end(); it++) {

		set<const char *> flowNames = (parser.*flowNamesGetter)(*it);

		cout << "   Stream name: '" << *it << "'" << endl;
		cout << "   Number of flows: " << flowNames.size() << endl;

		set<const char *>::iterator it2;
		for(it2 = flowNames.begin(); it2 != flowNames.end(); it2++)
			cout << "     Flow name: " << *it2 << endl;
	}

}

void parseFiles(const char *directory, BulkDataConfigurationParser &parser,
		void (BulkDataConfigurationParser::*parserMethod)(const char *),
		set<const char *> (BulkDataConfigurationParser::*streamNamesGetter)(void),
		set<const char *> (BulkDataConfigurationParser::*flowNamesGetter)(const char *)) {

	ACE_DIRENT **nameList;
	int fileCount;

	// Parse all receiver configs
	fileCount = ACE_OS::scandir(directory, &nameList, 0, 0);
	for(int i=0; i<fileCount; i++) {
		ACE_DIRENT *dirent = nameList[i];

		if( ACE_OS::strcmp(dirent->d_name, ".") == 0 ||
		    ACE_OS::strcmp(dirent->d_name, "..") == 0 ||
		    ACE_OS::strcmp(dirent->d_name, "CVS") == 0 ) {
			free(dirent);
			continue;
		}


		string realFilename(directory);
		realFilename.append("/");
		realFilename.append(dirent->d_name);

		cout << "Parsing file: " << realFilename << endl;

		ifstream file(realFilename.c_str());
		string s((std::istreambuf_iterator<char>(file)), istreambuf_iterator<char>());
		file.close();

		try {
			BulkDataConfigurationParser parser;
			(parser.*parserMethod)(s.c_str());
			printConfigMap(parser, streamNamesGetter, flowNamesGetter);
		} catch(CDBProblemExImpl &ex) {
			ex.log(LM_ERROR);
		}

		free(dirent);
	}
	free(nameList);

}

void parseSenderFiles() {
	BulkDataConfigurationParser parser;
	parseFiles("bdSenderConfigs", parser,
			&BulkDataConfigurationParser::parseSenderConfig,
			&BulkDataConfigurationParser::getAllSenderStreamNames,
			&BulkDataConfigurationParser::getAllSenderFlowNames);

	set<const char*> streams = parser.getAllSenderStreamNames();
	for(set<const char*>::iterator it = streams.begin(); it != streams.end(); it++) {
		SenderStreamConfiguration *streamConfig = parser.getSenderStreamConfiguration(*it);
		if( streamConfig == 0 )
			cerr << "SenderStreamConfiguration shouldn't be null!" << endl;

		set<const char*> flows = parser.getAllSenderFlowNames(*it);
		for(set<const char*>::iterator it2 = flows.begin(); it2 != flows.end(); it2++) {
			SenderFlowConfiguration *flowConfig = parser.getSenderFlowConfiguration(*it, *it2);
			if( flowConfig == 0 )
				cerr << "SenderFlowConfiguration shouldn't be null!" << endl;
		}
	}
}

void parseReceiverFiles() {
	BulkDataConfigurationParser parser;
	parseFiles("bdReceiverConfigs", parser,
			&BulkDataConfigurationParser::parseReceiverConfig,
			&BulkDataConfigurationParser::getAllReceiverStreamNames,
			&BulkDataConfigurationParser::getAllReceiverFlowNames);

	set<const char*> streams = parser.getAllReceiverStreamNames();
	for(set<const char*>::iterator it = streams.begin(); it != streams.end(); it++) {
		ReceiverStreamConfiguration *streamConfig = parser.getReceiverStreamConfiguration(*it);
		if( streamConfig == 0 )
			cerr << "ReceiverStreamConfiguration shouldn't be null!" << endl;

		set<const char*> flows = parser.getAllReceiverFlowNames(*it);
		for(set<const char*>::iterator it2 = flows.begin(); it2 != flows.end(); it2++) {
			ReceiverFlowConfiguration *flowConfig = parser.getReceiverFlowConfiguration(*it, *it2);
			if( flowConfig == 0 )
				cerr << "ReceiverFlowConfiguration shouldn't be null!" << endl;
		}
	}
}

int main(int args, char *argv[]) {

	LoggingProxy m_logger(0, 0, 31);
	LoggingProxy::init (&m_logger);
    ACS_CHECK_LOGGER;

    parseSenderFiles();
    parseReceiverFiles();

	m_logger.done();
	LoggingProxy::done();
	return 0;

}
