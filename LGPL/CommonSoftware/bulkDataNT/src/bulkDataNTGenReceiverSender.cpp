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
 * "@(#) $Id: bulkDataNTGenReceiverSender.cpp,v 1.1 2013/02/06 15:07:07 rbourtem Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * bjeram  2011-04-19  created
 */
#include "bulkDataNTReceiverStream.h"
#include "bulkDataNTCallback.h"
#include "bulkDataNTSenderFlow.h"
#include <iostream>
#include <ace/Get_Opt.h>
#include <ace/Tokenizer_T.h>

using namespace AcsBulkdata;
using namespace std;

class  TestReceiverCB:  public BulkDataNTCallback
{
public:
	TestReceiverCB()
	{
		totalRcvData=0;
	}

	virtual ~TestReceiverCB()
	{
		std::cout << "Total received data for: " << getStreamName() << "#" << getFlowName()  << " : " << totalRcvData << std::endl;
	}

	int cbStart(unsigned char* userParam_p, unsigned  int size)
	{
		// we cannot initialize flow name and flow stream in constructor, because they are after CB object is created
		fn = getFlowName();
		sn = getStreamName();

		std::cout << "cbStart[ " << sn << "#" << fn  << " ]: got a parameter: ";
		for(unsigned int i=0; i<size; i++)
		{
			std::cout <<  *(char*)(userParam_p+i);
		}
		std::cout << " of size: " << size << std::endl;
		return 0;
	}

	int cbReceive(unsigned char* data, unsigned  int size)
	{
		if (cbReceivePrint)
		{
			std::cout << "cbReceive[ " << sn << "#" << fn << " ]: got data of size: " << size << " :";
			/*		for(unsigned int i=0; i<frame_p->length(); i++)
		{
			std::cout <<  *(char*)(frame_p->base()+i);
		}
			 */
			std::cout << std::endl;
		}
		if (cbDelay>0)
		{
			ACE_Time_Value start_time, elapsed_time;
			start_time =  ACE_OS::gettimeofday();
			elapsed_time =  ACE_OS::gettimeofday() - start_time;
			// usleep(cbDealy);
			while (elapsed_time.usec() <  cbDelay)
			{
				elapsed_time = ACE_OS::gettimeofday() - start_time;
			}

		}
		totalRcvData+=size;
		return 0;
	}

	int cbStop()
	{
		std::cout << "cbStop[ " << sn << "#" << fn << " ]" << std::endl;
		return 0;
	}

	static long cbDelay;
	static bool cbReceivePrint;
private:
	std::string fn; ///flow Name
	std::string sn; ///stream name
	unsigned int totalRcvData; ///total size of all received data
};

long TestReceiverCB::cbDelay = 0;
bool TestReceiverCB::cbReceivePrint=true;

void print_usage(char *argv[]) {
	cout << "Usage: " << argv[0] << endl;
	cout << "\t[-rs \t receiverStreamName]" << endl;
	cout << "\t-rf \t receiverFlow1Name[,receiverFlow2Name,receiverFlow3Name...]" << endl;
	cout << "\t[-rd \t cbReceive delay(sleep) in usec]" << endl;
	cout << "\t[-ru \t [receiver unicast port] receiver unicast mode]" << endl;
	cout << "\t[-rm \t receiver multicast address]" << endl;
	cout << "\t[-rv \t additional printing in cbReceive]" << endl;
	cout << "\t[-ss \t senderStreamName]" << endl;
	cout << "\t[-sf \t senderFlow1Name[,senderFlow2Name,senderFlow3Name...]]" << endl;
	cout << "\t[-sb \t sender data size in bytes]. Default: 65000" << endl;
	cout << "\t[-sp \t sender parameter (startSend())]. Default: 'defaultParameter'" << endl;
	cout << "\t[-sl \t # of loops/iterations for the sender]. Default: 1" << endl;
	cout << "\t[-sn] \t sender does not wait for a key" << endl;
	cout << "\t[-st \t send frame timeout in sec]. Default: 5.0" << endl;
	cout << "\t[-sa \t send ACK timeout in sec]. Default: 2.0" << endl;
	cout << "\t[-so \t send throttling in MBytes/sec]. Default: 0.0 (no throttling)" << endl;
	exit(1);
}


int main(int argc, char *argv[])
{
	int option;
	bool waitForKey=true;
	bool sendData=true;
	bool recreate=true;
	unsigned int loop=1;
	double throttling=0.0;
	double send_time, sendTimeout=5.0, ACKtimeout=2.0;
	ACE_Time_Value start_time, elapsed_time;
	ReceiverStreamConfiguration recvStreamCfg;
	ReceiverFlowConfiguration recvFlowCfg;
	SenderStreamConfiguration sendStreamCfg;
	SenderFlowConfiguration sendFlowCfg;
	const int RECEIVER_STREAM_NAME_OPTION 	= 1;
	const int RECEIVER_FLOWS_NAMES_OPTION	= 2;
	const int RECEIVER_DELAY_OPTION			= 3;
	const int RECEIVER_UNICAST_OPTION       = 4;
	const int RECEIVER_MULTICAST_OPTION		= 5;
	const int SENDER_STREAM_NAME_OPTION 	= 6;
	const int SENDER_FLOWS_NAMES_OPTION 	= 7;
	const int SENDER_DATA_SIZE_OPTION		= 8;
	const int SENDER_START_SEND_PARAM_OPTION = 9;
	const int SENDER_NBLOOPS_OPTION			= 10;
	const int SENDER_NO_KEY_WAIT_OPTION		= 11;
	const int SENDER_FRAME_TIMEOUT_OPTION	= 12;
	const int SENDER_ACK_TIMEOUT_OPTION		= 13;
	const int SENDER_THROTTLING_OPTION		= 14;

	string recvStreamName = "DefaultRcvStream";
	string senderStreamName = "DefaultSendStream";
	string param="defaultParameter";
	/*char unicastPortQoS[250];
	unsigned int unicastPort=24000;
	 */
	//char multicastAdd[100];
	list<char *> recvFlows;
	list<char *> sendFlows;
	unsigned int dataSize=65000;

	// Parse the args
	ACE_Get_Opt get_opts (argc, argv, ":v",0,1,ACE_Get_Opt::PERMUTE_ARGS,1);
	// Check mandatory options
	// receiver stream name (rs) option
	if(get_opts.long_option(ACE_TEXT ("rs"), RECEIVER_STREAM_NAME_OPTION, ACE_Get_Opt::ARG_REQUIRED) == -1)
	{		return -1;  	}
	// receiver flows names list (rf) option
	if (get_opts.long_option(ACE_TEXT ("rf"), RECEIVER_FLOWS_NAMES_OPTION, ACE_Get_Opt::ARG_REQUIRED) == -1)
	{		return -1;	}
	// Receiver cbReceive delay (rd) option
	if (get_opts.long_option(ACE_TEXT ("rd"), RECEIVER_DELAY_OPTION, ACE_Get_Opt::ARG_REQUIRED) == -1)
	{		return -1;	}
	// receiver unicast option
	if (get_opts.long_option(ACE_TEXT ("ru"), RECEIVER_UNICAST_OPTION, ACE_Get_Opt::ARG_OPTIONAL) == -1)
	{		return -1;	}
	// receiver multicast (rm) option
	if (get_opts.long_option(ACE_TEXT ("rm"), RECEIVER_MULTICAST_OPTION, ACE_Get_Opt::ARG_REQUIRED) == -1)
	{		return -1;	}
	get_opts.long_option(ACE_TEXT ("rv"), 'v');

	// sender flows names list (sf) option
	if(get_opts.long_option(ACE_TEXT ("sf"), SENDER_FLOWS_NAMES_OPTION, ACE_Get_Opt::ARG_REQUIRED) == -1)
	{		return -1; 	}
	// sender stream name (ss) option
	if (get_opts.long_option(ACE_TEXT ("ss"), SENDER_STREAM_NAME_OPTION, ACE_Get_Opt::ARG_REQUIRED) == -1)
	{		return -1;  	}
	// Sender data size (sb) option
	if (get_opts.long_option(ACE_TEXT ("sb"), SENDER_DATA_SIZE_OPTION, ACE_Get_Opt::ARG_REQUIRED) == -1)
	{		return -1;	}
	// Sender start parameter (sp) option
	if (get_opts.long_option(ACE_TEXT ("sp"), SENDER_START_SEND_PARAM_OPTION, ACE_Get_Opt::ARG_REQUIRED) == -1)
	{		return -1;	}
	// Sender nb loops (sl) option
	if (get_opts.long_option(ACE_TEXT ("sl"), SENDER_NBLOOPS_OPTION, ACE_Get_Opt::ARG_REQUIRED) == -1)
	{		return -1;	}
	// Sender does not wait for key option
	if (get_opts.long_option(ACE_TEXT ("sn"), SENDER_NO_KEY_WAIT_OPTION, ACE_Get_Opt::NO_ARG) == -1)
	{		return -1;	}
	// Sender frame timeout (st) option
	if (get_opts.long_option(ACE_TEXT ("st"), SENDER_FRAME_TIMEOUT_OPTION, ACE_Get_Opt::ARG_REQUIRED) == -1)
	{		return -1;	}
	// Sender ACK timeout (sa) option
	if (get_opts.long_option(ACE_TEXT ("sa"), SENDER_ACK_TIMEOUT_OPTION, ACE_Get_Opt::ARG_REQUIRED) == -1)
	{		return -1;	}
	// Sender Throttling (so) option
	if (get_opts.long_option(ACE_TEXT ("so"), SENDER_THROTTLING_OPTION, ACE_Get_Opt::ARG_REQUIRED) == -1)
	{		return -1;	}

	while(( option = get_opts()) != -1 )
	{
		switch(option) {
		case 'v':
		{
			TestReceiverCB::cbReceivePrint=false;
			break;
		}
		case RECEIVER_MULTICAST_OPTION:
		{
			recvFlowCfg.setMulticastAddress(get_opts.opt_arg());
			break;
		}
		case RECEIVER_UNICAST_OPTION:
		{
			// Todo Give possibility to set 1 unicast port per receiver flow
			recvFlowCfg.setEnableMulticast(false);
			char *op=get_opts.opt_arg();
			if (op!=NULL)
			{
				recvFlowCfg.setUnicastPort(atoi(op));
			}
			break;
		}
		case SENDER_STREAM_NAME_OPTION:
		{
			senderStreamName = get_opts.opt_arg();
			cout << "sender Stream Name = " << senderStreamName << endl;
			break;
		}
		case RECEIVER_STREAM_NAME_OPTION:
		{
			recvStreamName = get_opts.opt_arg();
			cout << "receiver Stream Name = " << recvStreamName << endl;
			break;
		}
		case SENDER_FLOWS_NAMES_OPTION:
		{
			ACE_Tokenizer tok(get_opts.opt_arg());
			tok.delimiter_replace(',', 0);
			for(char *p = tok.next(); p; p = tok.next())
				sendFlows.push_back(p);
			break;
		}
		case RECEIVER_FLOWS_NAMES_OPTION:
		{
			ACE_Tokenizer tok(get_opts.opt_arg());
			tok.delimiter_replace(',', 0);
			for(char *p = tok.next(); p; p = tok.next())
				recvFlows.push_back(p);
			break;
		}
		case RECEIVER_DELAY_OPTION:
		{
			TestReceiverCB::cbDelay = atoi(get_opts.opt_arg());
			break;
		}
		case SENDER_START_SEND_PARAM_OPTION:
		{
			param = get_opts.opt_arg();
			break;
		}
		case SENDER_DATA_SIZE_OPTION:
		{
			dataSize = atoi(get_opts.opt_arg());
			break;
		}
		case SENDER_NO_KEY_WAIT_OPTION:
		{
			waitForKey = false;
			break;
		}
		case SENDER_NBLOOPS_OPTION:
		{
			loop = atoi(get_opts.opt_arg());
			break;
		}
		case SENDER_FRAME_TIMEOUT_OPTION:
		{
			sendTimeout = atof(get_opts.opt_arg());
			break;
		}
		case SENDER_ACK_TIMEOUT_OPTION:
		{
			ACKtimeout = atof(get_opts.opt_arg());
			break;
		}
		case SENDER_THROTTLING_OPTION:
		{
			throttling = atof(get_opts.opt_arg());
			break;
		}
		}// switch (option)
	}//while

	if( recvFlows.size() == 0 )
		print_usage(argv);

	LoggingProxy m_logger(0, 0, 31, 0);
	LoggingProxy::init (&m_logger);
	ACS_CHECK_LOGGER;

	//streamCfg.setUseIncrementUnicastPort(false);
	AcsBulkdata::BulkDataNTReceiverStream<TestReceiverCB> receiverStream(recvStreamName.c_str(), recvStreamCfg);

	//flowCfg.setUnicastPort(47000);
	list<char *>::iterator it;
	//unsigned int j=0;
	// Create Receiver flows
	for(it = recvFlows.begin(); it != recvFlows.end(); it++) {
		/*
		sprintf(unicastPortQoS, "<datareader_qos><unicast><value><element><receive_port>%ud</receive_port></element></value></unicast></datareader_qos>", unicastPort++);
		flowCfg.setDDSReceiverFlowQoS((*it), unicastPortQoS);
		 */
		/*
		sprintf(multicastAdd, "225.3.2.%d", j++);
		flowCfg.setMulticastAddress(multicastAdd);
		 */
		BulkDataNTReceiverFlow *flow = receiverStream.createFlow((*it), recvFlowCfg);
		flow->getCallback<TestReceiverCB>();
	}

	std::vector<string> flowNames = receiverStream.getFlowNames();
	std::cout << "Waiting on the following " << receiverStream.getFlowNumber() << " flow(s):[ ";
	for(unsigned int i=0;i<flowNames.size(); i++)
		std::cout << flowNames[i] << " ";
	std::cout << "] of stream: " <<  recvStreamName << std::endl;

	ACS_SHORT_LOG((LM_INFO, "Is new bulk data enabled (ENABLE_BULKDATA_NT) %d", isBulkDataNTEnabled()));
	while(recreate)
	{
		unsigned int numOfCreatedFlows=0;
		vector<BulkDataNTSenderFlow*> flows;
		try
		{
			double throuhgput=0;
			double sumThrouhgput=0;
			vector<double> 	throughputSums;
			// first we need a stream
			BulkDataNTSenderStream senderStream(senderStreamName.c_str());

			// let's create flows
			list<char *>::iterator it;
			for(it = sendFlows.begin(); it != sendFlows.end(); it++) {
				SenderFlowConfiguration cfg;
				cfg.setACKsTimeout(ACKtimeout);
				cfg.setSendFrameTimeout(sendTimeout);
				cfg.setThrottling(throttling);
				BulkDataNTSenderFlow *flow = senderStream.createFlow((*it), cfg);
				flows.push_back(flow);
			}

			// print out what we have created
			std::vector<string> tmpFlowNames = senderStream.getFlowNames();
			std::cout << "The following " << senderStream.getFlowNumber() << " flow(s) has/have been created:[ ";
			for(unsigned int i=0;i<tmpFlowNames.size(); i++)
				std::cout << tmpFlowNames[i] << " ";
			std::cout << "] on stream: " << senderStreamName << std::endl;

			numOfCreatedFlows = senderStream.getFlowNumber();

			std::cout << "press ENTER to send data (start/data/stop) to connected receivers ..." << std::endl;
			if (waitForKey) getchar();
			sendData=true;

			throughputSums.resize(numOfCreatedFlows);

			while(sendData)
			{
				// first startSend
				for(unsigned int i=0; i<numOfCreatedFlows; i++)
				{
					ACS_SHORT_LOG((LM_INFO, "Going to send parameter: '%s' to flow: '%s' to %d receiver(s)", param.c_str(), tmpFlowNames[i].c_str(), flows[i]->getNumberOfReceivers()));
					flows[i]->startSend((const unsigned char*)param.c_str(), param.size());
					throughputSums[i]=0.0;
				}//for
				sumThrouhgput=0.0;
				// then sendData
				unsigned char *data= new unsigned char[dataSize];
				for (unsigned int i=0; i<dataSize; i++)	data[i]=i;

				for(unsigned int j=1; j<=loop; j++)
				{
					std::cout << "Loop: " << j << " of " << loop << std::endl;
					for(unsigned int i=0; i<numOfCreatedFlows; i++)
					{
						ACS_SHORT_LOG((LM_INFO, "Going to send [%d/%d]: %d Bytes of data to flow: '%s' to %d receiver(s)", j, loop, dataSize, tmpFlowNames[i].c_str(), flows[i]->getNumberOfReceivers()));
						start_time = ACE_OS::gettimeofday();
						flows[i]->sendData(data, dataSize);
						elapsed_time = ACE_OS::gettimeofday() - start_time;
						send_time = (elapsed_time.sec()+( elapsed_time.usec() / 1000000. ));
						throuhgput = (dataSize/(1024.0*1024.0))/send_time;
						ACS_SHORT_LOG((LM_INFO, "Transfer rate for flow '%s': %f MBytes/sec",
								tmpFlowNames[i].c_str(), throuhgput));
						sumThrouhgput+=throuhgput;
						throughputSums[i]+=throuhgput;
					}//for i
				}//for j


				// and stopSend
				for(unsigned int i=0; i<numOfCreatedFlows; i++)
				{
					ACS_SHORT_LOG((LM_INFO, "Average transfer rate for flow '%s': %f MBytes/sec",
							tmpFlowNames[i].c_str(), throughputSums[i]/loop));
					ACS_SHORT_LOG((LM_INFO, "Going to send stop to flow: '%s' to %d receiver(s)",
							tmpFlowNames[i].c_str(), flows[i]->getNumberOfReceivers()));
					flows[i]->stopSend();
				}//for

				ACS_SHORT_LOG((LM_INFO, "Average transfer rate for all the flow(s): %f MBytes/sec",
						sumThrouhgput/(loop*numOfCreatedFlows)));

				if (!waitForKey) //we exit both loops
				{
					recreate=false;
					break;
				}

				std::cout << "press 'r' for re-send data, 'c' for re-create stream+flow(s), and any other key for exit + ENTER" << std::endl;
				int c=getchar();
				switch(c)
				{
				case 'r':
				{
					getchar();
					sendData=true;
					break;
				}
				case 'c':
				{
					getchar();
					sendData=false;
					recreate=true;
					break;
				}
				default:
				{
					sendData=false;
					recreate=false;
					break;
				}
				}//switch
			}//while(sendData)

		}catch(ACSErr::ACSbaseExImpl &ex)
		{
			recreate=false; //in case of an error we exit the while loop
			for(unsigned int i=0; i<numOfCreatedFlows; i++)
				flows[i]->dumpStatistics();

			ex.log();
		}
	}//while(recreate)
	m_logger.flush();
}
