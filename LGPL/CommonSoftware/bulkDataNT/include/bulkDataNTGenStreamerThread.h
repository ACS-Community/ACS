#ifndef bulkDataNTStreamerThread_H
#define bulkDataNTStreamerThread_H
/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) AUI - Associated Universities Inc., 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************
 * 
 * "@(#) $Id: bulkDataNTGenStreamerThread.h,v 1.1 2013/02/11 18:37:33 rbourtem Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * ramestic  2011-08-01  created
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

//
// System stuff
//
#include <pthread.h>
#include <semaphore.h>

//
// ACS stuff
//
#include <acsThread.h>
#include <maciContainerServices.h>

//
// CORR stuff
//
//#include <MasterBulkDataSender.h>

//
// Local stuff
//
#include "bulkDataNTSenderFlow.h"

/** Stream out BDF blobs as they are produced. Taking care of blobs ending
 ** after the next one has started receiving data.
 */
namespace AcsBulkdata
{
class StreamerThread : public ACS::Thread
{
public:
	/** constructor.
	 */
	StreamerThread(	const char *threadName,
			const std::string & streamName,
			const std::string &sendFlowName,
			const std::string &qosLib,
			const double & throttling,
			const double & sendTimeout,
			const double & ACKTimeout);

	/** Destructor flushes remaining data and stops the thread.
	 */
	~StreamerThread();

	/** Send data.
	 */
	void send(uint8_t * data, size_t size);

	/** Stop flow.
	 */
	//  void abort(uint8_t * data, size_t size);
	void abort();
	/** Waits until a given blob is completed or aborted, it
	 ** simply invokes wait() on base Streamer class which is
	 ** actually private.
	 */
	void wait(const char *uid, const ACS::TimeInterval timeout)
	{
		// TODO
		//Streamer<Corr::CDP::Master::BulkData::SenderTemporal>::wait(uid, timeout);
	}

	/** Get flow number.
	 */ // TODO?
	/*SpectralResolutionTypeMod::SpectralResolutionType getFlow() const
	{
		// TODO
		//return Streamer<Corr::CDP::Master::BulkData::SenderTemporal>::getFlow();
	}*/

	/** worker function that handles the incoming data.
	 */
	void run();


private:
	/** Copy constructor is private, it does not make sense
	 ** to 'copy' one of these threads.
	 */
	StreamerThread(const StreamerThread &src);

	/** Assigment operator is private, it does not make sense
	 ** to make one of these threads the same as a second one.
	 */
	StreamerThread &operator =(const StreamerThread &src);

	/** Use this flag to signal that the thread is being
	 ** commanded to terminate. Once this flag is unset
	 ** and the thread gets once more the data-ready semaphore
	 ** the thread will break its main while-loop.
	 */
	bool m_isThreadRunning;

	/** Use this flag to signal that the data flow is being aborted.
	 */
	bool m_isAbort;

	std::list< std::pair<uint8_t *, size_t> > data_buffers;

	/** Data to send is kept as a list of pointers to
	 ** stl vectors. The order in the list is the order
	 ** in which data should be streamed out.
	 */
	//std::list<DataType> m_data;
	std::list< std::pair<uint8_t *, size_t> > m_data;

	/** Data available for retrieval every time this semaphore is
	 ** available.
	 */
	sem_t m_dataReadySem;

	/** Abort method waits for thread completion by means
	 ** of this semaphore.
	 */
	sem_t m_abortReadySem;

	/** Data pointers are kept in a container that must
	 ** be accessed (insert or remove) only while holding
	 ** this mutex.
	 */
	pthread_mutex_t m_accessMutex;

	/** Timeout for guarded access operations.
	 */
	static const ACS::TimeInterval m_accessTimeout = 50000000LLU;
	//static const ACS::TimeInterval m_accessTimeout = 5000000000LLU;
	std::string m_streamName;

	std::string m_sendFlowName;

	std::string m_qosLib;

	BulkDataNTSenderStream * m_senderStream;

	BulkDataNTSenderFlow *m_sendFlow;

	double m_throttling;
	
	double m_sendTimeout;
	
	double m_ACKTimeout;
};
};

#endif

/*___oOo___*/
