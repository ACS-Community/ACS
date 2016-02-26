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
* almadev  2016-02-18  created 
*/
#include "BDNTSenderSimulatorFlow.h"

BDNTSenderSimulatorFlow::BDNTSenderSimulatorFlow(
		const char* streamName,
		const char* flowName,
		unsigned int sizeOfDataToSend,
		ACE_Barrier& theStartBarrier,
		ACE_Barrier& theDoneBarrier,
		LoggingProxy& logger,
		AcsBulkdata::BulkDataNTSenderFlow* theFlow):
	size(sizeOfDataToSend),
	numOfIterations(0),
	startSendBarrier(theStartBarrier),
	doneSendBarrier(theDoneBarrier),
	m_logger(logger),
	flow(theFlow)
{
	// Build the name of the flow
	name.append(flowName);
	name.append("@");
	name.append(streamName);

	// Allocate and initialize the bytes to send
	charsToSend = new unsigned char[size];
	for (unsigned int i=0; i<size; i++)	{
		charsToSend[i]=i%256;
	}

	// Start the thread
	this->activate(THR_NEW_LWP | THR_JOINABLE | THR_SUSPENDED | THR_SCHED_DEFAULT);
	cout  << "Thread for "<< name << " started with Id=" << this->grp_id() << ": will send "<< size << " bytes (thread count " << this->thr_count() << ')'<< endl;
	this->resume();
}

BDNTSenderSimulatorFlow::~BDNTSenderSimulatorFlow() {
	// Ensure the thread terminated
	this->wait();
	delete charsToSend;
	charsToSend = NULL;
	cout << "BDNTSenderSimulatorFlow " << name << " destroyed" << endl;
}

int BDNTSenderSimulatorFlow::svc(void) {
	cout << "Sender thread " << name << " started. Now entering the loop." << endl;
	while (startSendBarrier.wait()!=-1) {
		cout << "Sender thread " << name << " iteration #" << (++numOfIterations) << endl;

		// startSend
		//
		// the param is the name of this object (i.e flow@stream)
		cout << name << " is going to call startSend with parameter [" << name << "] to " << flow->getNumberOfReceivers() << " receiver(s)" << endl;
		startSendExecTime = ACE_OS::gettimeofday();
		flow->startSend((const unsigned char*)name.c_str(), name.size());
		startSendExecTime = ACE_OS::gettimeofday() - startSendExecTime;

		// sendData
		cout << name << " is going to send " << size << " bytes to " << flow->getNumberOfReceivers() << " receiver(s)" << endl;
		sendDataExecTime = ACE_OS::gettimeofday();
		flow->sendData(charsToSend, size);
		sendDataExecTime = ACE_OS::gettimeofday() - sendDataExecTime;
		double send_time = (sendDataExecTime.sec()+( sendDataExecTime.usec() / 1000000. ));
		throuhgput = (size/(1024.0*1024.0))/send_time;
		cout << name << " sent " << size << " bytes to " << flow->getNumberOfReceivers() << " receiver(s)" << endl;

		// stopSend
		cout << name << " is going to call stopSend to " << flow->getNumberOfReceivers() << " receiver(s)" << endl;
		stopSendExecTime = ACE_OS::gettimeofday();
		flow->stopSend();
		stopSendExecTime = ACE_OS::gettimeofday() - stopSendExecTime;

		// Signal that the iteration terminated
		doneSendBarrier.wait();
	}
	cout << "Sender thread " << name << " now exiting." << endl;
	return 0;
}

/*___oOo___*/
