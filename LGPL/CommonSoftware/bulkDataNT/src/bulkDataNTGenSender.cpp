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
* "@(#) $Id: bulkDataNTGenSender.cpp,v 1.17 2013/03/16 21:01:30 rtobar Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/
#include "bulkDataNTSenderFlow.h"
#include <iostream>
#include <ace/Get_Opt.h>
#include <ace/Tokenizer_T.h>

using namespace AcsBulkdata;
using namespace std;

void print_usage(char *argv[]) {
	cout << "Usage: " << argv[0] << ":" << endl;
	cout << "\t[-s] \t streamName. Default: 'DefaultStream'" << endl;
	cout << "\t-f \t flow1Name[,flow2Name,flow3Name..." << endl;
	cout << "\t[-b] \t data size in bytes. Default: 65000" << endl;
	cout << "\t[-r] \t read data from this file instead of sending fake data" << endl;
	cout << "\t[-p] \t parameter (startSend()). Default: 'defalutParamter'" << endl;
	cout << "\t[-l] \t # of loops/iterations. Default: 1" << endl;
	cout << "\t[-n] \t no wait for a key" << endl;
	cout << "\t[-t] \t send frame timeout in sec. Default: 5.0" << endl;
	cout << "\t[-a] \t ACK timeout in sec. Default: 2.0" << endl;
	cout << "\t[-o] \t throttling in MBytes/sec. Default: 0.0 (no throttling)" << endl;
	cout << "\t[-c] \t CDP protocol compatibility (-p option ignored and parameter = data size as unsigned int)" << endl;
	exit(1);
}

int main(int argc, char *argv[])
{
	char c;
	bool sendData=true;
	bool recreate=true;
	bool waitForKey=true;
	bool cdpProtocolCompatible = false;
	double send_time, sendTimeout=5.0, ACKtimeout=2.0;
	ACE_Time_Value start_time, elapsed_time;
	char *streamName = "DefaultStream";
	char *inputFilename = 0;
	std::string param="defaultParameter";
	unsigned int dataSize=65000;
	unsigned int totalDataSize=65000;
	unsigned int loop=1;
	double throttling=0.0;
	string qosLib="BulkDataQoSLibrary";
	list<char *> flowNames;


	// Parse the args
    ACE_Get_Opt get_opts (argc, argv, "o:f:s:b:p:l:t:a:ncr:");

    if(get_opts.long_option(ACE_TEXT ("qos_lib"), 0, ACE_Get_Opt::ARG_REQUIRED) == -1)
    {
    	cerr << "long option: qos_lib can not be added" << endl;
    	return -1;
    }

    while(( c = get_opts()) != -1 ) {
    	switch(c) {

    	case 0: //long option (qos_lib)
    	{
    		qosLib = get_opts.opt_arg();
    		break;
    	}
    	case 'l':
    	{
    		loop = atoi(get_opts.opt_arg());
    		break;
    	}
    	case 't':
    	{
    		sendTimeout = atof(get_opts.opt_arg());
    		break;
    	}
    	case 'a':
    	{
    		ACKtimeout = atof(get_opts.opt_arg());
    		break;
    	}
    	case 'o':
    	{
    		throttling = atof(get_opts.opt_arg());
    		break;
    	}
    	case 'p':
    	{
    		param = get_opts.opt_arg();
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
    	case 'b':
    	{
    		dataSize = atoi(get_opts.opt_arg());
    		break;
    	}
    	case 'n':
    	{
    		waitForKey = false;
    		break;
    	}
    	case 'c':
    	{
    		cdpProtocolCompatible = true;
    		break;
    	}
    	case 'r':
    	{
    		inputFilename = strdup(get_opts.opt_arg());
    		break;
    	}
    	default:
    	{
    		print_usage(argv);
    		break;
    	}
    	}
    }//while

    if( flowNames.size() == 0 )
    		print_usage(argv);

	LoggingProxy m_logger(0, 0, 31, 0);
	LoggingProxy::init (&m_logger);
    ACS_CHECK_LOGGER;

    if(cdpProtocolCompatible)
    {
    	totalDataSize = dataSize * loop;
    }


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
    		SenderStreamConfiguration scfg;
    		//scfg.setParticipantPerStream(true);
    		scfg.setQosLibrary(qosLib.c_str()); //"TCPBulkDataQoSLibrary");
    		//scfg.setQosProfile("TCPDefaultStreamQosProfile");
    		BulkDataNTSenderStream senderStream(streamName, scfg);

    		// let's create flows
    		list<char *>::iterator it;
    		SenderFlowConfiguration cfg;
    		cfg.setACKsTimeout(ACKtimeout);
        	cfg.setSendFrameTimeout(sendTimeout);
        	cfg.setThrottling(throttling);
    		cfg.setQosLibrary(qosLib.c_str());//"TCPBulkDataQoSLibrary");

    		for(it = flowNames.begin(); it != flowNames.end(); it++) {
    			//std::cout << cfg.getQosProfile() << std::endl;
    			BulkDataNTSenderFlow *flow = senderStream.createFlow((*it), cfg);
    			flows.push_back(flow);
    		//	cfg.setProfileQos("TCPDefaultStreamQosProfile");
    		}

/*
    		SenderStreamConfiguration scfg100;
    		scfg100.setParticipantPerStream(true);
    		scfg100.setQosLibrary("XBulkDataQoSLibrary");
    		BulkDataNTSenderStream senderStream100("TEST", scfg100);
    		cfg.setQosLibrary("XBulkDataQoSLibrary");
    		BulkDataNTSenderFlow *flow = senderStream100.createFlow("55", cfg);
    		flows.push_back(flow);
    		flowNames.push_back("55");
*/

    		// print out what we have created
    		std::vector<string> tmpFlowNames = senderStream.getFlowNames();
//    		tmpFlowNames.push_back("55");
    		std::cout << "The following " << senderStream.getFlowNumber() << " flow(s) has/have been created:[ ";
    		for(unsigned int i=0;i<tmpFlowNames.size(); i++)
    			std::cout << tmpFlowNames[i] << " ";
    		std::cout << "] on stream: " << streamName << std::endl;

    		numOfCreatedFlows = senderStream.getFlowNumber();
  //  		numOfCreatedFlows++;

    		std::cout << "press ENTER to send data (start/data/stop) to connected receivers ..." << std::endl;
    		if (waitForKey) getchar();
    		sendData=true;

    		throughputSums.resize(numOfCreatedFlows);

    		while(sendData)
    		{
    			// Check if we're using a file to send data or we're
    			// creating fake data
    			unsigned char *data =0;
    			if( inputFilename == 0 )
    			{
    				data = new unsigned char[dataSize];
    				for (unsigned int i=0; i<dataSize; i++)	data[i]=i;
    			}
    			else
    			{
    				cout << "Reading flow data from " << inputFilename << endl;
    				std::ifstream inputFile(inputFilename, std::ios::binary);
    				if( !inputFile.is_open() ) {
    					recreate = false;
    					cerr << "ERROR: File " << inputFilename << " could not be read" << endl;
    					break;
    				}

    				std::vector<char> fileContents((std::istreambuf_iterator<char>(inputFile)), std::istreambuf_iterator<char>());
    				std::vector<char>::size_type size = fileContents.size();
    				data = new unsigned char[size];
    				memcpy(data, (unsigned char *)fileContents.data(), size);
    				uint16_t paramSize = data[0];
    				param = std::string((char *)data+1, paramSize);
    				data = data + (1 + paramSize);
    				dataSize = size - 1 - paramSize;
    			}

    			// first startSend
    			for(unsigned int i=0; i<numOfCreatedFlows; i++)
    			{
    				if(cdpProtocolCompatible)
    				{
        				ACS_SHORT_LOG((LM_INFO, "Going to send parameter (= dataSize*nb_loops): '%d' to flow: '%s' to %d receiver(s)", totalDataSize, tmpFlowNames[i].c_str(), flows[i]->getNumberOfReceivers()));
        				flows[i]->startSend((const unsigned char*)&totalDataSize,sizeof(unsigned int));
    				}
    				else
    				{
        				ACS_SHORT_LOG((LM_INFO, "Going to send parameter: '%s' to flow: '%s' to %d receiver(s)", param.c_str(), tmpFlowNames[i].c_str(), flows[i]->getNumberOfReceivers()));
        				flows[i]->startSend((const unsigned char*)param.c_str(), param.size());
    				}
    				throughputSums[i]=0.0;
    			}//for
    			sumThrouhgput=0.0;

    			// then sendData
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
    	/* problem by TCP ???
    		for(unsigned int i=0; i<numOfCreatedFlows; i++)
    		    flows[i]->dumpStatistics();
    		    */

    		ex.log();
    	}

    }//while(recreate)
    m_logger.flush();
};//main
