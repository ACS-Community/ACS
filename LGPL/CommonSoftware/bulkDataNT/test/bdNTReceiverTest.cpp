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
* "@(#) $Id: bdNTReceiverTest.cpp,v 1.19 2012/04/25 13:48:51 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/
#include "bulkDataNTReceiverStream.h"
#include "bulkDataNTCallback.h"
#include <iostream>
#include <string>
#include <ace/Get_Opt.h>
#include <ace/Tokenizer_T.h>

using namespace std;

class  TestCB:  public BulkDataNTCallback
{
	long totBytesReceived_m;
public:
	int cbStart(unsigned char* userParam_p, unsigned  int size)
	{
		totBytesReceived_m=0;
		std::cout << "cbStart " << getFlowName()<<"@" << getStreamName();
		std::cout << ": got " << size << " :";
		for(unsigned int i=0; i<size; i++)
		{
			std::cout <<  *(char*)(userParam_p+i);
		}
		std::cout << std::endl;
		return 0;
	}

	int cbReceive(unsigned char* data, unsigned  int size)
	{

		//std::cout << "cbReceive " << getFlowName()<<"@" << getStreamName();
		//std::cout << ": received " << size << " bytes "<< std::endl;
		totBytesReceived_m+=size;
		if (cbDealy>0) usleep(cbDealy);
		return 0;
	}

	int cbStop()
	{
		std::cout << "cbStop " << getFlowName()<<"@" << getStreamName();
		std::cout << ": received data size =" << totBytesReceived_m << std::endl;
		return 0;
	}

	int cbReset()
	{
		std::cout << "cbReset " << getFlowName()<<"@" << getStreamName();
		return 0;
	}

	static unsigned long cbDealy;
};


void print_usage(char *argv[]) {
	cout << "Usage: " << argv[0] << " [-s streamName] -f flow1Name[,flow2Name,flow3Name...] [-d sleep delay in cbReceive] [-w end_test_wait_period]" << endl;
	exit(1);
}


unsigned long TestCB::cbDealy = 0;


int main(int argc, char *argv[])
{

	char c;
	unsigned int sleepPeriod=0;
	ReceiverFlowConfiguration cfg; //just
	//cfg.setEnableMulticast(false);
	char *streamName = "DefaultStream";
	list<char *> flows;

	// Parse the args
	ACE_Get_Opt get_opts (argc, argv, "s:f:w:d:");
	while(( c = get_opts()) != -1 ) {

		switch(c) {
			case 's':
			{
				streamName = get_opts.opt_arg();
				break;
			}
			case 'w':
			{
				sleepPeriod = atoi(get_opts.opt_arg());
				break;
			}
			case 'f':
			{
				ACE_Tokenizer tok(get_opts.opt_arg());
				tok.delimiter_replace(',',0);
				for(char *p = tok.next(); p; p = tok.next()) {
					flows.push_back(p);
				}
				break;
			}
			case 'd':
			{
				TestCB::cbDealy = atoi(get_opts.opt_arg());
				break;
			}
			default:
			{
				print_usage(argv);
				break;
			}
		}

	}

	if( flows.size() == 0 )
		print_usage(argv);

	if( !strcmp("DefaultStream", streamName) )
		cerr << "Warning: using default stream name \"DefaultStream\"" << endl;

	LoggingProxy m_logger(0, 0, 31, 0);
	LoggingProxy::init (&m_logger);
	ACS_CHECK_LOGGER;

	AcsBulkdata::BulkDataNTReceiverStream<TestCB> receiverStream(streamName);

	list<char *>::iterator it;
	for(it = flows.begin(); it != flows.end(); it++) {
		ReceiverFlowConfiguration cfg;
		BulkDataNTReceiverFlow *flow = receiverStream.createFlow((*it), cfg);
		flow->getCallback<TestCB>();
		std::vector<string> flowNames = receiverStream.getFlowNames();
		std::cout << receiverStream.getFlowNumber() << ":[ ";
		for(unsigned int i=0;i<flowNames.size(); i++)
		  std::cout << flowNames[i] << " ";
		std::cout << "]" << std::endl;
	}

	if (sleepPeriod>0)
	{
		sleep(sleepPeriod);
	}
	else
	{
		std::cout << "press a key to exit.." << std::endl;
		getchar();
	}

}
