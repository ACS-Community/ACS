#include <stdio.h>

#include "bulkDataNTConfigurationParser.h"
#include "ACS_BD_Errors.h"
#include <iostream>

using namespace AcsBulkdata;
using namespace ACS_BD_Errors;

class MyCallback : public BulkDataCallback {
	int cbStart(unsigned char* userParam_p = 0, unsigned  int size=0) { return 0; }
	int cbReceive(unsigned char * frame_p, unsigned  int size) { return 0; }
	int cbStop() { return 0; }
};

int main(int args, char *argv[]) {

	LoggingProxy m_logger(0, 0, 31);
	LoggingProxy::init (&m_logger);
    ACS_CHECK_LOGGER;

    list<BulkDataNTSenderStream *>* senders = 0;
    list<BulkDataNTReceiverStream<MyCallback> *>* receivers = 0;

	char *correctSenderConfig = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><BulkDataNTSender xmlns:baci=\"urn:schemas-cosylab-com:BACI:1.0\" xmlns=\"urn:schemas-eso-org:BulkDataNTSender:1.0\" xmlns:cdb=\"urn:schemas-cosylab-com:CDB:1.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:schemas-eso-org:BulkDataNTSender:1.0 file:/D:/eclipseWorkshop/bulkDataNT/config/CDB/schemas/BulkDataNTSender.xsd\" recentCommand=\"\" recentTimeStamp=\"\" actionThreadStackSize=\"1024\" monitoringThreadStackSize=\"2048\"><SenderStream Name=\"Name7\">        <DDSSenderStreamQoS>            <participant_qos name=\"name5\">            </participant_qos>        </DDSSenderStreamQoS>    </SenderStream><SenderStream Name=\"Name1\">        <Flow Name=\"Name3\">            <DDSSenderFlowQoS>                <datawriter_qos name=\"name1\">                </datawriter_qos>            </DDSSenderFlowQoS>        </Flow>        <Flow Name=\"Name5\">            <DDSSenderFlowQoS>                <datawriter_qos name=\"name3\">                 </datawriter_qos>            </DDSSenderFlowQoS>        </Flow>    </SenderStream>    </BulkDataNTSender>";
	try {
		BulkDataConfigurationParser parser;
		senders = parser.parseSenderConfig(correctSenderConfig);
	} catch(CDBProblemExImpl &ex) {
		std::cout << ex.getDetail() << std::endl;
	} catch(StreamCreateProblemExImpl &ex) {
		std::cout << "Error while creating stream '" << ex.getStreamName() << "'" << std::endl;
	} catch(FlowCreateProblemExImpl &ex) {
		std::cout << "Error while creating stream '" << ex.getStreamName() << "', flow '" << ex.getFlowName() << "': " << ex.getDescription() << std::endl;
	}

	std::cout << std::endl << std::endl << "Now let's check the Receiver configuration!" << std::endl << std::endl;

	char *correctReceiverConfig = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><BulkDataNTReceiver xmlns:baci=\"urn:schemas-cosylab-com:BACI:1.0\" xmlns=\"urn:schemas-eso-org:BulkDataNTSender:1.0\" xmlns:cdb=\"urn:schemas-cosylab-com:CDB:1.0\"  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:schemas-eso-org:BulkDataNTSender:1.0 file:/D:/eclipseWorkshop/bulkDataNT/config/CDB/schemas/BulkDataNTSender.xsd\" recentCommand=\"\" recentTimeStamp=\"\" actionThreadStackSize=\"1024\" monitoringThreadStackSize=\"2048\">    <ReceiverStream Name=\"Name1\">        <ReceiverFlow Name=\"Name3\">            <DDSReceiverFlowQoS>                <datawriter_qos name=\"name1\">                </datawriter_qos>            </DDSReceiverFlowQoS>        </ReceiverFlow>        <ReceiverFlow Name=\"Name5\">            <DDSReceiverFlowQoS>                <datawriter_qos name=\"name3\">                 </datawriter_qos>            </DDSReceiverFlowQoS>        </ReceiverFlow>    </ReceiverStream>    <ReceiverStream Name=\"Name7\">        <DDSReceiverStreamQoS>            <participant_qos name=\"name5\">            </participant_qos>        </DDSReceiverStreamQoS>    </ReceiverStream></BulkDataNTSender>";
	try {
		BulkDataConfigurationParser parser;
		receivers = parser.parseReceiverConfig<MyCallback>(correctReceiverConfig);
	} catch(CDBProblemExImpl &ex) {
		std::cout << ex.getDetail() << std::endl;
	} catch(StreamCreateProblemExImpl &ex) {
		std::cout << "Error while creating stream '" << ex.getStreamName() << "'" << std::endl;
	} catch(FlowCreateProblemExImpl &ex) {
		std::cout << "Error while creating stream '" << ex.getStreamName() << "', flow '" << ex.getFlowName() <<"'" << std::endl;
	}

	// cleanup the senders and receivers
	if( senders != 0 ) {
		list<BulkDataNTSenderStream *>::iterator it;
		for( it = senders->begin(); it != senders->end(); it++)
			delete *it;
		delete senders;
	}
	if( receivers != 0 ) {
		list<BulkDataNTReceiverStream<MyCallback> *>::iterator it;
		for( it = receivers->begin(); it != receivers->end(); it++)
			delete *it;
		delete receivers;
	}

	m_logger.done();
	LoggingProxy::done();
	return 0;

}
