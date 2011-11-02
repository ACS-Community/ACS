#include <stdio.h>

#include "bulkDataNTConfiguration.h"
#include "bulkDataNTConfigurationParser.h"
#include "ACS_BD_Errors.h"
#include <iostream>

using namespace AcsBulkdata;
using namespace ACS_BD_Errors;

template<class T, class F>
void printConfigMap(map<string, T> &configMap) {

	printf("Number of streams: %d\n", configMap.size());

	typename map<string, T>::iterator it;
	for(it = configMap.begin(); it != configMap.end(); it++) {
		printf("   Stream name: '%s'\n", it->first.c_str());
		printf("   Number of flows: %d\n", it->second.flowsCfgMap.size());

		typename map<string, F>::iterator it2;
		for(it2 = it->second.flowsCfgMap.begin(); it2 != it->second.flowsCfgMap.end(); it2++)
			printf("     Flow name: %s\n", it2->first.c_str());
	}

	printf("\n");
}

int main(int args, char *argv[]) {

	LoggingProxy m_logger(0, 0, 31);
	LoggingProxy::init (&m_logger);
    ACS_CHECK_LOGGER;

	map<string, BulkDataConfigurationParser::SenderCfg> senderCfgMap;
	map<string, BulkDataConfigurationParser::ReceiverCfg> receiverCfgMap;

	char *correctSenderConfig = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><BulkDataNTSender xmlns:baci=\"urn:schemas-cosylab-com:BACI:1.0\" xmlns=\"urn:schemas-eso-org:BulkDataNTSender:1.0\" xmlns:cdb=\"urn:schemas-cosylab-com:CDB:1.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:schemas-eso-org:BulkDataNTSender:1.0 file:/D:/eclipseWorkshop/bulkDataNT/config/CDB/schemas/BulkDataNTSender.xsd\" recentCommand=\"\" recentTimeStamp=\"\" actionThreadStackSize=\"1024\" monitoringThreadStackSize=\"2048\"><SenderStream Name=\"Name7\">        <DDSSenderStreamQoS>            <participant_qos name=\"name5\">            </participant_qos>        </DDSSenderStreamQoS><Flow Name=\"Name3\">            <DDSSenderFlowQoS>                <datawriter_qos name=\"name1\">                </datawriter_qos>            </DDSSenderFlowQoS>        </Flow>        <Flow Name=\"Name5\">            <DDSSenderFlowQoS>                <datawriter_qos name=\"name3\">                 </datawriter_qos>            </DDSSenderFlowQoS>        </Flow>    </SenderStream><SenderStream Name=\"Name1\">        <Flow Name=\"Name3\">            <DDSSenderFlowQoS>                <datawriter_qos name=\"name1\">                </datawriter_qos>            </DDSSenderFlowQoS>        </Flow>        <Flow Name=\"Name5\">            <DDSSenderFlowQoS>                <datawriter_qos name=\"name3\">                 </datawriter_qos>            </DDSSenderFlowQoS>        </Flow>    </SenderStream>    </BulkDataNTSender>";
	try {
		BulkDataConfigurationParser parser;
		parser.parseSenderConfig(correctSenderConfig, senderCfgMap);
	} catch(CDBProblemExImpl &ex) {
		std::cout << ex.getDetail() << std::endl;
	}
	printConfigMap<BulkDataConfigurationParser::SenderCfg, SenderFlowConfiguration>(senderCfgMap);

	char *correctReceiverConfig = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><BulkDataNTReceiver xmlns:baci=\"urn:schemas-cosylab-com:BACI:1.0\" xmlns=\"urn:schemas-eso-org:BulkDataNTSender:1.0\" xmlns:cdb=\"urn:schemas-cosylab-com:CDB:1.0\"  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:schemas-eso-org:BulkDataNTSender:1.0 file:/D:/eclipseWorkshop/bulkDataNT/config/CDB/schemas/BulkDataNTSender.xsd\" recentCommand=\"\" recentTimeStamp=\"\" actionThreadStackSize=\"1024\" monitoringThreadStackSize=\"2048\">    <ReceiverStream Name=\"Name1\">        <ReceiverFlow Name=\"Name3\">            <DDSReceiverFlowQoS>                <datawriter_qos name=\"name1\">                </datawriter_qos>            </DDSReceiverFlowQoS>        </ReceiverFlow>        <ReceiverFlow Name=\"Name5\">            <DDSReceiverFlowQoS>                <datawriter_qos name=\"name3\">                 </datawriter_qos>            </DDSReceiverFlowQoS>        </ReceiverFlow>    </ReceiverStream>    <ReceiverStream Name=\"Name7\">           <ReceiverFlow Name=\"Name3\">            <DDSReceiverFlowQoS>                <datawriter_qos name=\"name1\">                </datawriter_qos>            </DDSReceiverFlowQoS>        </ReceiverFlow>        <ReceiverFlow Name=\"Name5\">            <DDSReceiverFlowQoS>                <datawriter_qos name=\"name3\">                 </datawriter_qos>            </DDSReceiverFlowQoS>        </ReceiverFlow>      <DDSReceiverStreamQoS>            <participant_qos name=\"name5\">            </participant_qos>        </DDSReceiverStreamQoS>    </ReceiverStream></BulkDataNTSender>";
	try {
		BulkDataConfigurationParser parser;
		parser.parseReceiverConfig(correctReceiverConfig, receiverCfgMap);
	} catch(CDBProblemExImpl &ex) {
		std::cout << ex.getDetail() << std::endl;
	}
	printConfigMap<BulkDataConfigurationParser::ReceiverCfg, ReceiverFlowConfiguration>(receiverCfgMap);

	m_logger.done();
	LoggingProxy::done();
	return 0;

}
