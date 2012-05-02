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
* "@(#) $Id: bulkDataNTGenReceiver.cpp,v 1.3 2012/05/02 14:06:30 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/
#include "bulkDataNTReceiverStream.h"
#include "bulkDataNTCallback.h"
#include <iostream>
#include <ace/Get_Opt.h>
#include <ace/Tokenizer_T.h>

using namespace std;

class  TestCB:  public BulkDataNTCallback
{
public:
	int cbStart(unsigned char* userParam_p, unsigned  int size)
	{
		std::cout << "cbStart: got a paramter: ";
		for(unsigned int i=0; i<size; i++)
		{
			std::cout <<  *(char*)(userParam_p+i);
		}
		std::cout << " of size: " << size << std::endl;
		return 0;
	}

	int cbReceive(unsigned char* data, unsigned  int size)
	{
		std::cout << "cbReceive: got data of size: " << size << " :";
/*		for(unsigned int i=0; i<frame_p->length(); i++)
		{
			std::cout <<  *(char*)(frame_p->base()+i);
		}
	*/
		std::cout << std::endl;

		if (cbDealy>0) usleep(cbDealy);
		return 0;
	}

	int cbStop()
	{
		std::cout << "cbStop" << std::endl;
		return 0;
	}

	static unsigned long cbDealy;
};

unsigned long TestCB::cbDealy = 0;

void print_usage(char *argv[]) {
	cout << "Usage: " << argv[0] << " [-s streamName] -f flow1Name[,flow2Name,flow3Name...] [-d sleep delay in cbReceive]" << endl;
	exit(1);
}


int main(int argc, char *argv[])
{

	char c;
	ReceiverFlowConfiguration cfg; //just
	//cfg.setEnableMulticast(false);
	char *streamName = "DefaultStream";
	list<char *> flows;

	// Parse the args
	ACE_Get_Opt get_opts (argc, argv, "s:f:d:");
	while(( c = get_opts()) != -1 ) {

		switch(c) {
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
					flows.push_back(p);

				break;
			}
			case 'd':
			{
				TestCB::cbDealy = atoi(get_opts.opt_arg());
				break;
			}
		}

	}

	if( flows.size() == 0 )
		print_usage(argv);

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
		std::cout << "Wating on the following " << receiverStream.getFlowNumber() << " flow(s):[ ";
		for(unsigned int i=0;i<flowNames.size(); i++)
		  std::cout << flowNames[i] << " ";
		std::cout << "] of stream: " <<  streamName << std::endl;
	}


	std::cout << "Press a key to exit.." << std::endl;
	getchar();

}
