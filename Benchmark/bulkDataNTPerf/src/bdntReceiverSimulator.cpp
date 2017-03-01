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
* "@(#) $Id: bulkDataNTGenReceiver.cpp,v 1.16 2013/03/16 21:01:30 rtobar Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/
#include "bulkDataNTReceiverStream.h"
#include "ReceiverFlowSimCallback.h"
#include <iostream>
#include <iosfwd>
#include <ace/Get_Opt.h>
#include <ace/Tokenizer_T.h>

using namespace std;

void print_usage(char *argv[]) {
	cout << "Usage: " << argv[0] << " [-s streamName] -f flow1Name[,flow2Name,flow3Name...] [-d cbReceive delay(sleep) in usec] [-u[unicast port] unicast mode] [-m multicast address] [-w output filename prefix] [-q qosFileName] [-x qosLibName]" << endl;
	exit(1);
}


int main(int argc, char *argv[])
{

	char c;
	ReceiverStreamConfiguration streamCfg;
	ReceiverFlowConfiguration flowCfg;
	char *streamName = "DefaultStream";
	char *outputFilenamePrefix = 0;
	string qosLibFileName="BulkDataQoSLibrary";
	string qosFileName;
	/*char unicastPortQoS[250];
	unsigned int unicastPort=24000;
	*/
	//char multicastAdd[100];
	list<char *> flowNames;

	// Parse the args
	ACE_Get_Opt get_opts (argc, argv, "q:x:s:f:d:m:u::nw:");

	while(( c = get_opts()) != -1 ) {

		switch(c) {
			case 'm':
			{
				flowCfg.setMulticastAddress(get_opts.opt_arg());
				break;
			}
			case 'u':
			{
				flowCfg.setEnableMulticast(false);
				char *op=get_opts.opt_arg();
				if (op!=NULL)
				{
					flowCfg.setUnicastPort(atoi(op));
				}
				break;
			}
			case 's':
			{
				streamName = get_opts.opt_arg();
				break;
			}
			case 'f':
			{
				ACE_Tokenizer tok(get_opts.opt_arg());
				tok.delimiter_replace(',', 0);
				for(char *p = tok.next(); p; p = tok.next())
					flowNames.push_back(p);
				break;
			}
			case 'd':
			{
				ReceiverFlowSimCallback::cbDealy = atoi(get_opts.opt_arg());
				break;
			}
			case 'w':
			{
				outputFilenamePrefix = strdup(get_opts.opt_arg());
				break;
			}
			case 'q':
			{
				qosFileName = get_opts.opt_arg();
				break;
			}
			case 'x':
			{
				qosLibFileName = get_opts.opt_arg();
				break;
			}
		}//case

	}//while

	if( flowNames.size() == 0 )
		print_usage(argv);

	LoggingProxy m_logger(0, 0, 31, 0);
	LoggingProxy::init (&m_logger);
	ACS_CHECK_LOGGER;

	vector<BulkDataNTReceiverFlow*> flows;
	vector<ReceiverFlowSimCallback*> callbacks;

	cout << "Will receive data through " << flowNames.size() << " flows of the " << streamName << " stream:" << endl;
	list<char *>::iterator it;
	for (it = flowNames.begin(); it != flowNames.end(); it++) {
		cout << "\t" << *it << endl;
	}
	if (!qosFileName.empty()) cout << "Load QoS settings from " <<  qosFileName << endl;
	if (!qosLibFileName.empty()) cout << "Load QoS library from " <<  qosLibFileName << endl;

	//streamCfg.setUseIncrementUnicastPort(false);
	//streamCfg.setParticipantPerStream(true);
	if (!qosFileName.empty()) {
		streamCfg.setQosProfile(qosFileName);
	}
	if (!qosLibFileName.empty()) {
		streamCfg.setQosLibrary(qosLibFileName);
	}

/*	ReceiverStreamConfiguration streamCfg100;
	streamCfg100.setQosLibrary("XBulkDataQoSLibrary");
	AcsBulkdata::BulkDataNTReceiverStream<TestCB> receiverStream100("TEST", streamCfg100);
*/
	AcsBulkdata::BulkDataNTReceiverStream<ReceiverFlowSimCallback> receiverStream(streamName, streamCfg);


	//flowCfg.setUnicastPort(47000);
	flowCfg.setQosLibrary(qosLibFileName.c_str());//"TCPBulkDataQoSLibrary");

	//unsigned int j=0;
	for(it = flowNames.begin(); it != flowNames.end(); it++) {
		/*
		sprintf(unicastPortQoS, "<datareader_qos><unicast><value><element><receive_port>%ud</receive_port></element></value></unicast></datareader_qos>", unicastPort++);
		flowCfg.setDDSReceiverFlowQoS((*it), unicastPortQoS);
		 */
		/*
		sprintf(multicastAdd, "225.3.2.%d", j++);
		flowCfg.setMulticastAddress(multicastAdd);
		*/
		ReceiverFlowSimCallback *cb = new ReceiverFlowSimCallback();
		cb->setStoreData(outputFilenamePrefix != 0);
		BulkDataNTReceiverFlow *flow = receiverStream.createFlow((*it), flowCfg, cb);
		flows.push_back(flow);
		callbacks.push_back(cb);
//		flow->getCallback<TestCB>();
		//flowCfg.setProfileQos("TCPDefaultStreamQosProfile");
//		BulkDataNTReceiverFlow *flow100 = receiverStream100.createFlow((*it), flowCfg);
	}

	std::vector<string> actflowNames = receiverStream.getFlowNames();
	std::cout << "Waiting on the following " << receiverStream.getFlowNumber() << " flow(s):[ ";
	for(unsigned int i=0;i<actflowNames.size(); i++)
		std::cout << actflowNames[i] << " ";
	std::cout << "] of stream: " <<  streamName << std::endl;

	std::cout << "Press a key to exit.." << std::endl;
	getchar();

	unsigned int numOfCreatedFlows = receiverStream.getFlowNumber();
	for(unsigned int i=0; i<numOfCreatedFlows; i++)
	{
		flows[i]->dumpStatistics();
		if( outputFilenamePrefix != 0 ) {
			string filename;
			filename += outputFilenamePrefix;
			filename += "_";
			filename += flows[i]->getName();

			list<unsigned char> data = callbacks[i]->getData();
			std::ofstream ofs(filename.c_str());
			std::ostream_iterator<unsigned char> it(ofs);
			std::copy(data.begin(), data.end(), it);
			ofs.close();
			cout << "Dumped contents for flow " << flows[i]->getName() << " to " << filename << endl;
		}
		delete callbacks[i];
	 }

}
