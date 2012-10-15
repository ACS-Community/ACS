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
* "@(#) $Id: bulkDataNTGenReceiver.cpp,v 1.10 2012/10/15 10:09:09 bjeram Exp $"
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
	TestCB()
	{
		totalRcvData=0;
	}

	virtual ~TestCB()
	{
		std::cout << "Total received data for: " << getStreamName() << "#" << getFlowName()  << " : " << totalRcvData << std::endl;
	}

	int cbStart(unsigned char* userParam_p, unsigned  int size)
	{
		// we cannot initialize flow name and flow stream in ctor, because they are after CB object is created
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
		if (cbDealy>0) 
		  {
		    ACE_Time_Value start_time, elapsed_time;
                    start_time =  ACE_OS::gettimeofday(); 
		    elapsed_time =  ACE_OS::gettimeofday() - start_time;
		    // usleep(cbDealy);
		    while (elapsed_time.usec() <  cbDealy)
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

	static long cbDealy;
	static bool cbReceivePrint;
private:
	std::string fn; ///flow Name
	std::string sn; ///stream name
	unsigned int totalRcvData; ///total size of all received data
};

long TestCB::cbDealy = 0;
bool TestCB::cbReceivePrint=true;

void print_usage(char *argv[]) {
	cout << "Usage: " << argv[0] << " [-s streamName] -f flow1Name[,flow2Name,flow3Name...] [-d cbReceive delay(sleep) in usec] [-u unicast mode] [-m multicast address] [-n suppers printing in cbReceive]" << endl;
	exit(1);
}


int main(int argc, char *argv[])
{

	char c;
	ReceiverStreamConfiguration streamCfg;
	ReceiverFlowConfiguration flowCfg;
	char *streamName = "DefaultStream";
	/*char unicastPortQoS[250];
	unsigned int unicastPort=24000;
	*/
	//char multicastAdd[100];
	list<char *> flows;

	// Parse the args
	ACE_Get_Opt get_opts (argc, argv, "s:f:d:m:un");
	while(( c = get_opts()) != -1 ) {

		switch(c) {
			case 'n':
			{
				TestCB::cbReceivePrint=false;
				break;
			}
			case 'm':
			{
				flowCfg.setMulticastAddress(get_opts.opt_arg());
				break;
			}
			case 'u':
			{
				flowCfg.setEnableMulticast(false);
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
					flows.push_back(p);
				break;
			}
			case 'd':
			{
				TestCB::cbDealy = atoi(get_opts.opt_arg());
				break;
			}
		}//case

	}//while

	if( flows.size() == 0 )
		print_usage(argv);

	LoggingProxy m_logger(0, 0, 31, 0);
	LoggingProxy::init (&m_logger);
	ACS_CHECK_LOGGER;

	//streamCfg.setUseIncrementUnicastPort(false);
	AcsBulkdata::BulkDataNTReceiverStream<TestCB> receiverStream(streamName, streamCfg);

	//flowCfg.setUnicastPort(47000);
	list<char *>::iterator it;
	//unsigned int j=0;
	for(it = flows.begin(); it != flows.end(); it++) {
		/*
		sprintf(unicastPortQoS, "<datareader_qos><unicast><value><element><receive_port>%ud</receive_port></element></value></unicast></datareader_qos>", unicastPort++);
		flowCfg.setDDSReceiverFlowQoS((*it), unicastPortQoS);
		 */
		/*
		sprintf(multicastAdd, "225.3.2.%d", j++);
		flowCfg.setMulticastAddress(multicastAdd);
		*/
		BulkDataNTReceiverFlow *flow = receiverStream.createFlow((*it), flowCfg);
		flow->getCallback<TestCB>();
	}

	std::vector<string> flowNames = receiverStream.getFlowNames();
	std::cout << "Waiting on the following " << receiverStream.getFlowNumber() << " flow(s):[ ";
	for(unsigned int i=0;i<flowNames.size(); i++)
		std::cout << flowNames[i] << " ";
	std::cout << "] of stream: " <<  streamName << std::endl;



	std::cout << "Press a key to exit.." << std::endl;
	getchar();

}
