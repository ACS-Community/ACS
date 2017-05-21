/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2016 
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
*
* who       when      what
* --------  --------  ----------------------------------------------
* almadev  2016-02-26  created 
*/

#include "ReceiverFlowSimCallback.h"

long ReceiverFlowSimCallback::cbDealy = 0;

ReceiverFlowSimCallback::ReceiverFlowSimCallback() :
	dataToStore(0), iterationsNum(0)
{
	totalRcvData=0;
}

ReceiverFlowSimCallback::~ReceiverFlowSimCallback()
{
	std::cout << "Total received data for: " << getStreamName() << "#" << getFlowName()  << " : " << totalRcvData << std::endl;
}

int ReceiverFlowSimCallback::cbStart(unsigned char* userParam_p, unsigned  int size)
	{
	// Record the start time of this iteration (start-receive-stop)
	cbStartTime = ACE_OS::gettimeofday();
	// we cannot initialize flow name and flow stream in ctor, because they are after CB object is created
	fn = getFlowName();
	sn = getStreamName();

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

int ReceiverFlowSimCallback::cbReceive(unsigned char* data, unsigned  int size)
	{

	rcvDataStartStop+=size;
	totalRcvData+=size;
	frameCount++;
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

int ReceiverFlowSimCallback::cbStop()
{
	// Time between start and stop
	ACE_Time_Value iterationDuration = ACE_OS::gettimeofday() - cbStartTime;

	double duration = (iterationDuration.sec()+( iterationDuration.usec() / 1000000. ));
	double throuhgput = (rcvDataStartStop/(1024.0*1024.0))/duration;

	std::cout << "cbStop[ " << fn << "@" << sn << " ] Iteration=" << ++iterationsNum;
	std::cout << " tot. bytes received: " << rcvDataStartStop;
	std::cout << " @ " << throuhgput << "Mb/s" <<std::endl;
	return 0;
}

void ReceiverFlowSimCallback::onError(ACSErr::CompletionImpl &error)
{
	error.log();
	std:: cout << "An error occurred to flow " << fn << ": type=" << error.getType() << " code=" << error.getCode() << std::endl;
}

void ReceiverFlowSimCallback::onSenderConnect(unsigned short totalSenders)
{
	std:: cout << "A new sender connected to " << fn << ": number of connected senders " << totalSenders << std::endl;
}

void ReceiverFlowSimCallback::onSenderDisconnect(unsigned short totalSenders)
{
	std:: cout << "A sender disconnected from " << fn << ": number of connected senders " << totalSenders << std::endl;
}

void ReceiverFlowSimCallback::onDataLost(unsigned long frameCount, unsigned long totalFrames, ACSErr::CompletionImpl &error)
{
	std:: cout << fn << ": frame " << frameCount << " out of a total of "<< totalFrames << "did not arrive!" << std::endl;
	error.log();
}



/*___oOo___*/
