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
* "@(#) $Id: bulkDataNTGenSender.cpp,v 1.3 2012/05/02 14:12:20 bjeram Exp $"
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
	cout << "Usage: " << argv[0] << " [-s streamName] -f flow1Name[,flow2Name,flow3Name...] [-b data size in bytes] [-p parameter]" << endl;
	exit(1);
}

int main(int argc, char *argv[])
{
	char c;
	double send_time;
	ACE_Time_Value start_time, elapsed_time;
	char *streamName = "DefaultStream";
	std::string param="defalutParamter";
	unsigned int dataSize=65000;
	list<char *> flowNames;
	vector<BulkDataNTSenderFlow*> flows;

	// Parse the args
    ACE_Get_Opt get_opts (argc, argv, "f:s:b:p:");
    while(( c = get_opts()) != -1 ) {
    	switch(c) {
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

    ACS_SHORT_LOG((LM_INFO, "Is new bulk data enabled (ENABLE_BULKDATA_NT) %d", isBulkDataNTEnabled()));
    try
    {
    	// first we need a stream
    	BulkDataNTSenderStream senderStream(streamName);

    	// let's create flows
    	list<char *>::iterator it;
    	for(it = flowNames.begin(); it != flowNames.end(); it++) {
    		SenderFlowConfiguration cfg;
    		BulkDataNTSenderFlow *flow = senderStream.createFlow((*it), cfg);
    		flows.push_back(flow);
    	}

    	// print out what we have created
    	std::vector<string> tmpFlowNames = senderStream.getFlowNames();
    	std::cout << "The following " << senderStream.getFlowNumber() << " flow(s) has/have been created:[ ";
    	for(unsigned int i=0;i<tmpFlowNames.size(); i++)
    		std::cout << tmpFlowNames[i] << " ";
    	std::cout << "] on stream: " << streamName << std::endl;

    	unsigned int numOfCreatedFlows = senderStream.getFlowNumber();

    	sleep(2);

    	// first startSend
    	for(unsigned int i=0; i<numOfCreatedFlows; i++)
    	{
    		ACS_SHORT_LOG((LM_INFO, "Going to send paramter: %s to flow: %s to %d receiver(s)", param.c_str(), tmpFlowNames[i].c_str(), flows[i]->getNumberOfReceivers()));
    		flows[i]->startSend((const unsigned char*)param.c_str(), param.size());
    	}//for

    	// then sendData
    	unsigned char *data= new unsigned char[dataSize];
    	for (unsigned int i=0; i<dataSize; i++)	data[i]=i;
    	for(unsigned int i=0; i<numOfCreatedFlows; i++)
    	{
        	ACS_SHORT_LOG((LM_INFO, "Going to send: %d Bytes of data on flow: %s to %d receiver(s)", dataSize, tmpFlowNames[i].c_str(), flows[i]->getNumberOfReceivers()));
        	start_time = ACE_OS::gettimeofday();
        	flows[i]->sendData(data, dataSize);
        	elapsed_time = ACE_OS::gettimeofday() - start_time;
        	send_time = (elapsed_time.sec()+( elapsed_time.usec() / 1000000. ));
        	ACS_SHORT_LOG((LM_INFO, "Transfer rate for flow %s: %f MBytes/sec",
        			tmpFlowNames[i].c_str(),
        			(dataSize/(1024.0*1024.0))/send_time));
    	}//for

    	// and stopSend
    	for(unsigned int i=0; i<numOfCreatedFlows; i++)
    	{
    		ACS_SHORT_LOG((LM_INFO, "Going to send stop to flow: %s to %d receiver(s)", tmpFlowNames[i].c_str(), flows[i]->getNumberOfReceivers()));
    		flows[i]->stopSend();
    	}//for

    }catch(ACSErr::ACSbaseExImpl &ex)
    {
    	ex.log();
    }

    std::cout << "press a key to exit.." << std::endl;
    getchar();


    m_logger.flush();
};//main
