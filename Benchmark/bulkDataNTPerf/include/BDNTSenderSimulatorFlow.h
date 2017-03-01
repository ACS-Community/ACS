#ifndef BDNTSENDERSIMULATORFLOW_H
#define BDNTSENDERSIMULATORFLOW_H
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

#include <string>
#include <ace/Barrier.h>
#include <ace/Task.h>
#include <ace/Thread_Manager.h>
#include <ace/Thread.h>
#include <logging.h>
#include "bulkDataNTSenderFlow.h"


/************************************************************************
 *
 * BDNTSenderSimulatorFlow sends the data though the passed DDS flow.
 *
 * Objects of this class do not instantiate any DDS flow but simply use
 * the passed one. It means that if the flow is recreated a new
 * BDNTSenderSimulatorFlow must be created to send data through such a flow.
 *
 * Sending of data must be done through a dedicated thread because we
 * want all the data to be sent through the flows at the very same moment
 * to generate the desired load on the network.
 *
 * Sleeping cannot be done on a flow basis because the amount of data
 * sent through each flow can be substantially different so it would be
 * the time required to complete the sending.
 */
class BDNTSenderSimulatorFlow : public ACE_Task_Base {
	public:
	/**
	 * Constructor
	 *
	 * @param streamName The name of the stream
	 * @param flowName The name of the flow
	 * @param sizeOfDataToSend The number of bytes to send
	 */
	BDNTSenderSimulatorFlow(
			const char* streamName,
			const char* flowName,
			unsigned int sizeOfDataToSend,
			ACE_Barrier& theStartBarrier,
			ACE_Barrier& theDoneBarrier,
			LoggingProxy& logger,
			AcsBulkdata::BulkDataNTSenderFlow* theFlow);

	/**
	 * Send the data through the flow.
	 * This method must be executed by the thread.
	 *
	 * The method waits for the barrier to start then sends data through the DDS flow.
	 * When done blocks again in the barrier, ready to start the next iteration.
	 *
	 * This method should not be called directly.
	 */
	void sendDataThread();

	/**
	 * Destructor
	 */
	virtual ~BDNTSenderSimulatorFlow();

	/**
	 * This is the function that will be running in the spawned ACE thread.
	 * It sends data through the flow: it waits for the barrier to start
	 * then sends data through the DDS flow. When done blocks again in the barrier,
	 * ready to start the next iteration
	 */
	virtual int svc(void);

	/**
	 * Getter
	 */
	AcsBulkdata::BulkDataNTSenderFlow* getFlow() const {
		return flow;
	}

	/**
	 * Getter
	 */
	std::string getName() const {
		return name;
	}

	/**
	 * Getter
	 */
	ACE_Time_Value getSendDataExecTime() const {
		return sendDataExecTime;
	}

	/**
	 * Getter
	 */
	unsigned int getSize() const {
		return size;
	}

	/**
	 * Getter
	 */
	ACE_Time_Value getStartSendExecTime() const {
		return startSendExecTime;
	}

	/**
	 * Getter
	 */
	ACE_Time_Value getStopSendExecTime() const {
		return stopSendExecTime;
	}

	/**
	 * Getter
	 */
	double getSumTotalThroughput() const {
		return sumTotalThroughput;
	}

	/**
	 * Getter
	 */
	double getThrouhgput() const {
		return throuhgput;
	}

	unsigned int getNumOfIterations() const {
		return numOfIterations;
	}

	private:
	/**
	 * The name of the stream and flow
	 */
	std::string name;

	// The data to send through the flow
	unsigned char* charsToSend;

	// Number of bytes to send
	unsigned int size;

	// The number of iterations executed so far.
	unsigned int numOfIterations;

	/**
	 * The barrier used to start all the sending at the same time.
	 *
	 * When the barrier is shutdown, the wait method return -1
	 * that signals the thread to terminate
	 */
	ACE_Barrier& startSendBarrier;

	/**
	 * The barrier to signal that all the threads terminate
	 * to send data at a given iteration
	 */
	ACE_Barrier& doneSendBarrier;

	// Thread ID
	ACE_thread_t threadId;

	// The logger
	LoggingProxy& m_logger;

	// The flow to send that to receivers
	AcsBulkdata::BulkDataNTSenderFlow* flow;

	// Time to execute startSend of the last iteration
	ACE_Time_Value startSendExecTime;

	// Time to execute sendData of the last iteration
	ACE_Time_Value sendDataExecTime;

	// Time to execute stopSend of the last iteration
	ACE_Time_Value stopSendExecTime;

	// Throughput (i.e. transfer rate in MBytes/sec) of the last iteration
	double throuhgput;

	// Sum of all the throughput for all the sendData run by this flow
	// (to calculate the average)
	double sumTotalThroughput;

};

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif



#endif /*!BDNTSENDERSIMULATORFLOW_H*/
