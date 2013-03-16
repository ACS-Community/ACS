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
* "@(#) $Id: bulkDataNTGenReceiver.cpp,v 1.15 2013/03/16 20:25:02 rtobar Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/
#include "bulkDataNTReceiverStream.h"
#include "bulkDataNTCallback.h"
#include <iostream>
#include <iosfwd>
#include <ace/Get_Opt.h>
#include <ace/Tokenizer_T.h>

using namespace std;

class  TestCB:  public BulkDataNTCallback
{
public:
	TestCB() :
		dataToStore(0)
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
		if( storeData ) {
			dataToStore.push_back(size); // TODO: this trims an uint into an unsigned char (2/4 bytes to 1)
		}
		for(unsigned int i=0; i<size; i++)
		{
			std::cout <<  *(char*)(userParam_p+i);
			if( storeData ) {
				dataToStore.push_back(*(userParam_p+i));
			}
		}
		std::cout << " of size: " << size << std::endl;
		frameCount = 0;
		rcvDataStartStop = 0;

		return 0;
	}//cbStart

	int cbReceive(unsigned char* data, unsigned  int size)
	{

		rcvDataStartStop+=size;
		totalRcvData+=size;
		frameCount++;
		if (cbReceivePrint)
		{
			std::cout << "cbReceive[ " << sn << "#" << fn << " ]: got data of size: " << size << " (";
			std::cout <<  rcvDataStartStop << ", " << frameCount << ") :";
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

		if( storeData ) {
			for(unsigned int i=0; i<size; i++) {
				dataToStore.push_back(*(data + i));
			}
		}
		return 0;
	}

	int cbStop()
	{
		std::cout << "cbStop[ " << sn << "#" << fn << " ]" << std::endl;
		return 0;
	}

	void setStoreData(bool shouldStoreData) {
		storeData = shouldStoreData;
	}

	list<unsigned char> getData() {
		return dataToStore;
	}

	uint16_t getUserParamSize() {
		return userParamSize;
	}

	static long cbDealy;
	static bool cbReceivePrint;

private:
	std::string fn; ///flow Name
	std::string sn; ///stream name
	unsigned int totalRcvData; ///total size of all received data
	unsigned int rcvDataStartStop; ///size of received data between Start/stop
	unsigned int frameCount; ///frame count
	bool storeData;
	uint16_t userParamSize;
	list<unsigned char> dataToStore;
};

long TestCB::cbDealy = 0;
bool TestCB::cbReceivePrint=true;

void print_usage(char *argv[]) {
	cout << "Usage: " << argv[0] << " [-s streamName] -f flow1Name[,flow2Name,flow3Name...] [-d cbReceive delay(sleep) in usec] [-u[unicast port] unicast mode] [-m multicast address] [-n suppers printing in cbReceive] [-w output filename prefix]" << endl;
	exit(1);
}


int main(int argc, char *argv[])
{

	char c;
	ReceiverStreamConfiguration streamCfg;
	ReceiverFlowConfiguration flowCfg;
	char *streamName = "DefaultStream";
	char *outputFilenamePrefix = 0;
	string qosLib="BulkDataQoSLibrary";
	/*char unicastPortQoS[250];
	unsigned int unicastPort=24000;
	*/
	//char multicastAdd[100];
	list<char *> flowNames;

	// Parse the args
	ACE_Get_Opt get_opts (argc, argv, "s:f:d:m:u::nw:");
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
				TestCB::cbDealy = atoi(get_opts.opt_arg());
				break;
			}
			case 'w':
			{
				outputFilenamePrefix = strdup(get_opts.opt_arg());
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
	vector<TestCB*> callbacks;

	//streamCfg.setUseIncrementUnicastPort(false);
	//streamCfg.setParticipantPerStream(true);
	streamCfg.setQosLibrary(qosLib.c_str());//"TCPBulkDataQoSLibrary");

/*	ReceiverStreamConfiguration streamCfg100;
	streamCfg100.setQosLibrary("XBulkDataQoSLibrary");
	AcsBulkdata::BulkDataNTReceiverStream<TestCB> receiverStream100("TEST", streamCfg100);
*/
	AcsBulkdata::BulkDataNTReceiverStream<TestCB> receiverStream(streamName, streamCfg);


	//flowCfg.setUnicastPort(47000);
	flowCfg.setQosLibrary(qosLib.c_str());//"TCPBulkDataQoSLibrary");
	list<char *>::iterator it;
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
		TestCB *cb = new TestCB();
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
		}
		delete callbacks[i];
	 }

}
