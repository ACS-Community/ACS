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
* "@(#) $Id: bdNTMultiReceiversTest.cpp,v 1.1 2012/07/10 10:43:16 bjeram Exp $"
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
		std::cout << "cbStart: got " << size << " :";
		for(unsigned int i=0; i<size; i++)
		{
			std::cout <<  *(char*)(userParam_p+i);
		}
		std::cout << std::endl;
		return 0;
	}

	int cbReceive(unsigned char* data, unsigned  int size)
	{
		// std::cout << "cbReceive: got " << size << " :";
/*		for(unsigned int i=0; i<frame_p->length(); i++)
		{
			std::cout <<  *(char*)(frame_p->base()+i);
		}
	*/
		//std::cout << std::endl;
		return 0;
	}

	int cbStop()
	{
		std::cout << "cbStop" << std::endl;
		return 0;
	}

};


int main(int argc, char *argv[])
{
	int numOfIter=32;
	ReceiverFlowConfiguration cfg; //just

	LoggingProxy m_logger(0, 0, 31, 0);
	LoggingProxy::init (&m_logger);
	ACS_CHECK_LOGGER;

	if (argc>1)
		numOfIter= atoi(argv[1]);

	std::cout << "Going to create " << numOfIter << " streams." << std::endl;

	char buf[]="00";
	AcsBulkdata::BulkDataNTReceiverStream<TestCB>* receiverStreams[numOfIter];
	for (int i=0; i<numOfIter; i++)
	  {
	    sprintf(buf, "%d", i);
	    std::string streamName("Stream");
	    streamName += buf;
	    std::cout << "Going to create stream: " << streamName << std::endl;
	    receiverStreams[i] = new AcsBulkdata::BulkDataNTReceiverStream<TestCB>(streamName.c_str());
	    std::cout << "Stream: " << streamName << " has been created. Going to create a flow inside the stream" << std::endl;
	   receiverStreams[i]->createFlow("00");
	  }

	//getchar();

	std::cout << "Going to destroy all " << numOfIter << " streams." << std::endl;
	for (int i=0; i<numOfIter; i++)
	  {
	    std::cout << "Going to destroy stream: " << receiverStreams[i]->getName() << std::endl;
	    delete receiverStreams[i];
	  }


}
