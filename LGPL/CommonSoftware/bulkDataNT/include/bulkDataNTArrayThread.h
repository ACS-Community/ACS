#ifndef BD_ARRAY_THREAD_H
#define BD_ARRAY_THREAD_H
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
 * "@(#) $Id: bulkDataNTArrayThread.h,v 1.1 2013/02/11 18:37:36 rbourtem Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 */

//
// System stuff
//
#include <set>
#include <list>
//#include <boost/tuple/tuple.hpp>
#include <utility>
#include <System_Time.h>
#include <pthread.h>

//
// ACS stuff
//
#include <acsThread.h>
/*#include <bulkDataDistributerC.h>
#include <acsncRTSupplier.h>
#include <acsncSimpleConsumer.h>
#include <RepeatGuard.h>*/

//
// ICD stuff
//
/*#include <ObservationControlC.h>
#include <SpectralResolutionType.h>
#include <ControlBasicInterfacesC.h>*/

//
// CORR stuff
//
/*#include <CorrEx.h>
#include <CorrMemoryHeap.h>
#include <CorrThreadSyncGuard.h>
#include <CorrTimingStats.h>
#include <DataCollector.h>
#include <NodeC.h>*/

//
// Local stuff
//
/*#include "MasterAlarm.h"
#include "MasterBulkDataSender.h"
#include "ConfigBasket.h"
#include "NodesCluster.h"
#include "Interferometer.h"
#include "BlobStreamerThread.h"*/
#include "bulkDataNTGenStreamerThread.h"


namespace AcsBulkdata
{
/** This class handles incoming data from cdp nodes and its final packeting
 ** into an SDM document to be sent to the Archive. It also takes care of
 ** transmitting related data to the data-capture component associated with
 ** the on going sub-scan. This class spawns a thread that takes care of
 ** the on-the-fly actions for handling the data and each instance of this
 ** class is associate to an specific sub-array.
 */
class BulkDataNTArrayThread : public ACS::Thread
{
public:
	/** constructor.
	 * TODO: Add ACK Timeout send timeout, throttling
	 */
	BulkDataNTArrayThread(const ACE_CString &name,
			const std::string &streamName,
			const std::string &sendFlowName,
			const std::string &qosLib,
			const double &throttling,
			const double &sendTimeout,
			const double &ACKTimeout);

	/** destructor.
	 */
	~BulkDataNTArrayThread();

	/** thread's work function which handles data produced by sub-scans.
	 */
	void run();

	ACE_CString getName() const { return name; };

	/** add a data event to the thread handler's queue for deferred
	 ** processing.
	 ** @param data event data structure received from the data source
	 ** (normally a cdp node)
	 ** @return true on successful insertion in the queue.
	 */
	bool addDataEvent(const uint8_t *buffer, const size_t size);

	/** start a sub-scan sequence.
	 ** @exception BDNTEx
	 */
	void startSequence();

	/** stop a sub-scan sequence.
         ** @exception BDNTEx
         */
        void stopSequence();


private:

	ACE_CString name;

	std::string m_streamName;

	std::string m_sendFlowName;

	std::string m_qosLib;

	/** End of sequence status as returned by the loop handler method.
	 */
	enum SequenceEndStatus
	{
		SequenceEndStatus_OK,
		SequenceEndStatus_EXCEPTION,
		SequenceEndStatus_TIMEOUT,
		SequenceEndStatus_STOPPED
	};

	//
	// lower limit for data packages sent through a blob streamer
	//
	//static const unsigned int m_blobThresholdSize = 41943040;

	//
	// copies not allowed
	//
	BulkDataNTArrayThread(const BulkDataNTArrayThread &toCopy);

	//
	// assignment not allowed
	//
	BulkDataNTArrayThread &operator =(const BulkDataNTArrayThread &);

	/** Timeout for guarded access operations.
	 */
	static const ACS::TimeInterval m_accessTimeout = 50000000LLU;
	//static const ACS::TimeInterval m_accessTimeout = 5000000000LLU;
	/** Memory heap reference to request/release memory.
	 */
	//Corr::CDP::MemoryHeap *m_mh_p;

	/** BDF streamers. Each sequence utilize just one streamer
	 ** per data type.
	 */
	//TODO
	StreamerThread * m_Streamer;
	//std::map<SpectralResolutionTypeMod::SpectralResolutionType, Corr::CDP::Master::Blob::StreamerThread *> m_blobStreamer;

	/** list for data events from nodes.
	 */
	std::list< std::pair<uint8_t *, size_t> > m_frontEndBuffer;

	/** Guard event lists with this mutex.
	 */
	pthread_mutex_t m_eventListMutex;

	/** Mutex used for synchronizing the configuration, startup and
	 ** stopping of sequences. It must be a recursive mutex, for example,
	 ** clean calls stop.
	 */
	pthread_mutex_t m_accessMutex;

    /** condition variable object for running a sequence.
     ** The thread blocks until requested to terminate or start a sequence,
     ** that is, the condition is signaled by either a command to terminate
     ** the thread or by a request to start a sequence.
     */
    pthread_cond_t m_condition;

	/** If an error has occurred while handling data from nodes
	 ** then this variable will set to error.
	 */
	//CorrEx m_eventListErr;

	/** Condition variable use to signal array thread that
	 ** new data has been delivered into the front-end-buffer.
	 */
	pthread_cond_t m_newDataCondition;

	/** In case of error while ingesting new data to the
	 ** event's help (within the AV callback handler) report
	 ** the error as a log, but do that only once for one
	 ** given sub-scan (avoid logging cascade.)
	 */
	bool m_addDataEventLogFlag;

	/** condition variable object for stopping a sequence.
         ** The stop sequence method waits on this variable for feedback from
         ** thread after it has actually stopped. The method itself raises
         ** the stop flag that's check by the thread. The associated mutex is
         ** the one declared above as m_accessMutex.
         ** Note: seems to be convenient to use a conditional variable for this
         ** purpose given that the 'wait' call helps releasing the access mutex
         ** that needs to be released any how.
         */
         pthread_cond_t m_stopCondition;

	/** flag used to synchronize the stopping of a sequence.
	 ** The stopSequence method sets the stop flag and then it waits for the
	 ** thread to acknowledge by signaling this condition variable and checking
	 ** that the flag was actually reset by the thread.
	 */
	bool m_sequenceStopFlag;

	/** flag used to know whether a sequence
	 ** is already running
	 */
	bool sequenceAlreadyRunningFlag;

	double m_throttling;

	double m_sendTimeout;

	double m_ACKTimeout;

	/** signals the thread to exit it's main running loop.
	 ** This is a re-implementation of the base method. We need
	 ** to do this as to broadcast the condition variable and
	 ** forcing that way the thread to recheck its running status.
	 */
	virtual void exit()
	{
		ACS::Thread::exit();

		pthread_cond_broadcast(&m_condition);
	}

	/** Method that implements a loop to receive data for every
	 ** sub-scan in a sequence until all those sub-scans have
	 ** completed.
	 */
	SequenceEndStatus handleSequenceLoop();

	/** Wait for new data in the front end buffer. Before blocking
	 ** on the event list semaphore the access mutex is unlocked
	 ** and after getting the semaphore or timing out on the
	 ** semaphore the mutex is reacquired. If new data is signaled
	 ** during the alloted timeout then the function return true.
	 */
	void waitForDataEvent(const ACS::TimeInterval to, std::list< std::pair<uint8_t *, size_t> > &out);

	/** Remove available items from front-end-buffer and return
	 ** their sizes and memory addresses. The caller now owns
	 ** those memory addresses.
	 */
	void deleteFrontEndBuffer(const ACS::TimeInterval _to, std::list< std::pair<uint8_t *, size_t> > &out);

	void relyDataToStreamer(std::list< std::pair<uint8_t *, size_t> > &data);

	/** Abort sequence by aborting streaming
         */
        void abort(const std::string &reason);
};
};

#endif /* BD_ARRAY_THREAD_H */

/*___oOo___*/
