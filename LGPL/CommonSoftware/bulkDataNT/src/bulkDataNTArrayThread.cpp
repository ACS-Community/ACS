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
 * "@(#) $Id: bulkDataNTArrayThread.cpp,v 1.1 2013/02/11 18:37:36 rbourtem Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * ramestic  2009/10/07  created
 */

//
// System stuff
//
#define __STDC_FORMAT_MACROS
#include <inttypes.h>
#include <iterator>
#include <System_Time.h>
#include <pthread.h>

//
// ACS stuff
//
#include <acsThreadBase.h>
#include <acsutilTimeStamp.h>
#include <acstimeTimeUtil.h>
#include <acstimeC.h>
#include <ACSErrTypeOK.h>

//
// ICD stuff
//
/*#include <CBasebandName.h>
#include <IntegrationEventC.h>
#include <CorrErr.h>*/

//
// Local stuff
//
//#include "MasterAlarm.h"
#include "bulkDataNTConfiguration.h"
#include "bulkDataNTArrayThread.h"
#include <bulkDataNTPosixHelper.h>
#include <bulkDataNTThreadSyncGuard.h>


using namespace std;
using namespace AcsBulkdata;

//
// Static variables
//
const ACS::TimeInterval BulkDataNTArrayThread::m_accessTimeout;
/*const string ArrayThread::ARCHIVE_IDENTIFIER_COMPONENT_NAME = "ARCHIVE_IDENTIFIER";
const ACS::TimeInterval ArrayThread::m_wvrIntegrationTimeUnit;
const ACS::TimeInterval ArrayThread::m_wvrIntegrationTimeMax;*/

//----------------------------------------------------------------------------------
BulkDataNTArrayThread::BulkDataNTArrayThread(const ACE_CString &name,
		const string &streamName,
		const string &sendFlowName,
		const string &qosLib,
		const double &throttling,
		const double &sendTimeout,
		const double &ACKTimeout):
		ACS::Thread(name, m_accessTimeout + 10000000LLU),
		m_streamName(streamName),
		m_sendFlowName(sendFlowName),
		m_qosLib(qosLib),
		m_Streamer(0),
		m_addDataEventLogFlag(false),
		m_sequenceStopFlag(false),
		sequenceAlreadyRunningFlag(false),
		m_throttling(throttling),
		m_sendTimeout(sendTimeout),
		m_ACKTimeout(ACKTimeout)
{
	int rc = 0;

	// initialize event's list mutex as a normal one (non recursive)
	Pthread::Mutex::init(m_eventListMutex, false);
	//
	// initialize access mutex (recursive)
	Pthread::Mutex::init(m_accessMutex, true);

	// initialize condition variable use to signal array thread
	// that new data is available
	pthread_cond_init(&m_newDataCondition, NULL);

	// create conditional variable used by thread to synchronize sequence
	// start/stop and access to the data list.
	if ( (rc = pthread_cond_init(&m_condition, NULL)) )
	{
		BDNT_EX_THROW_EX("failed to initialize condition variable (err=%d)", rc);
	}
	//
	// create conditional variable used for stop-sequence synchronization
	//
	if ( (rc = pthread_cond_init(&m_stopCondition, NULL)) )
	{
		BDNT_EX_THROW_EX("failed to initialize stop-sequence condition variable (err=%d)", rc);
	}

}

//----------------------------------------------------------------------------------
BulkDataNTArrayThread::~BulkDataNTArrayThread()
{
    //
    // disconnect AV sender (the destructor implements the real work)
    //
    /*if ( m_bdSender_p != NULL )
    {
        delete m_bdSender_p;
    }*/

	if(m_Streamer != 0)
		delete m_Streamer;
    //
    // destroy resources associated to the access mutex
    //
    pthread_mutex_destroy(&m_accessMutex);

    //
    // destroy resources associated to condition variable
    //
    pthread_cond_destroy(&m_condition);
    
    //
    // delete stop-sequence condition variable
    //
    pthread_cond_destroy(&m_stopCondition);

    //
    // delete new data condition variable
    //
    pthread_cond_destroy(&m_newDataCondition);
}

//----------------------------------------------------------------------------------
void BulkDataNTArrayThread::run()
{
    SequenceEndStatus status;
    bool threadStopRequested = false;
    //map<SpectralResolutionTypeMod::SpectralResolutionType, Corr::CDP::Master::Blob::StreamerThread *> blobStreamer;

    ACS_SHORT_LOG((LM_DEBUG, "sub-array thread has started (%s)", name.c_str()));
    cout << "sub-array thread has started" << endl;

    // infinite loop (until requested to stop)
    while ( check() )
    {
        // get rid of previously allocated blob streaming threads
        /*for ( map<SpectralResolutionTypeMod::SpectralResolutionType, Corr::CDP::Master::Blob::StreamerThread *>::iterator i = blobStreamer.begin();
              i != blobStreamer.end();
              ++i )
        {
            delete i->second;
        }*/

        try
        {
            // proceed only after locking the access mutex, because
            // we are accessing shared variables.
        	//cout << "run loop getting ready for next sequence" << endl;
            if ( !Pthread::Mutex::lock("run loop getting ready for next sequence", m_accessMutex, m_accessTimeout) )
            {
                // this is bad but let's simply try again
                ACS_SHORT_LOG((LM_ERROR,
                               "thread's loop timeout on access semaphore (%s)",
                               getName().c_str()));
                
                continue;
            }
        }
        catch ( BDNTEx &ex )
        {
            // raise an alarm and go away
            ACS_SHORT_LOG((LM_ERROR, "%s", ex.asString().c_str()));
            ACS_SHORT_LOG((LM_ERROR, "access mutex invalid (%s)", getName().c_str()));

            goto TheThreadEnd;
        }
        catch ( ... )
        {
            ACS_SHORT_LOG((LM_ERROR, "thread caught ellipses and re-thrown"));
                
            throw;
        }
        
        ACS_SHORT_LOG((LM_DEBUG, "array thread waiting for sequence (%s)",name.c_str()));
       // cout << "array thread waiting for sequence" << endl;

        // if no sequence running then wait for it, the loop must break
        // when data has arrived or when the thread is being requested 
        // to stop
        threadStopRequested = false;
        // TODO? Wait for a flag to start collecting and sending data
        //while ( m_basket.getRunningMap().size() == 0 && !(threadStopRequested = !check()) )
        while(!sequenceAlreadyRunningFlag && !(threadStopRequested = !check()))
        {
            if ( Pthread::CondVar::wait(m_condition, m_accessMutex, m_accessTimeout) )
            {
                ACS_SHORT_LOG((LM_DEBUG, "condition variable is set"));
                cout << "Condition variable is set" << endl;
            }
        }

       // cout << "no longer waiting..." << endl;
        // if we were requested to terminate then break the
        // big loop now, otherwise a sequence was just started
        if ( threadStopRequested )
        {
            ACS_SHORT_LOG((LM_DEBUG,
                           "sub-array thread responding to stop request (%s)",
                           name.c_str()));
            cout << "sub-array thread responding to stop request" << endl;

            goto TheThreadEnd;
        }

        /*ACS_SHORT_LOG((LM_DEBUG,
                       "sub-array thread started sequence of length %zd",
                       m_basket.getRunningMap().size()));*/

//        cout << "sub-array thread started sequence" << endl;

        // in case of any exception in the loop then record it in
        // this variable
        BDNTEx ex;

        // handle all sub-scans in the sequence.
        try
        {
            status = handleSequenceLoop();
        }
        catch ( BDNTEx &_ex )
        {
            BDNT_EX(ex, "sequence loop threw exception: %s", _ex.asCString());
            status = SequenceEndStatus_EXCEPTION;
        }
        catch ( ... )
        {
            BDNT_EX(ex, "sequence loop threw unknown exception");
            status = SequenceEndStatus_EXCEPTION;
        }

        // decode ended status
        switch ( status )
        {
        case SequenceEndStatus_OK:
            
            // TODO?
        	/*ACS_SHORT_LOG((LM_DEBUG,
                           "sub-array thread completed sequence of lenght %zd ",
                           m_basket.getRunningMap().size()));*/

            break;

        case SequenceEndStatus_TIMEOUT:

            // TODO
        	///abort("data from cluster to master node has timed out");

            break;

        case SequenceEndStatus_STOPPED:

        	// TODO
        	abort("sub-scan sequence has been willfully stopped");

            //
            // reset the flag for letting the stopSequence method know
            // that we are correctly acknowledging
            //
            m_sequenceStopFlag = false;
            // cout << "Signal the stopCondition" << endl; 
            //
            // signal the condition, the stopSequence method is waiting
            // on it.
            pthread_cond_broadcast(&m_stopCondition);

            break;

        case SequenceEndStatus_EXCEPTION:
            
        	// TODO
        	abort(ex.asString());

            break;
        }

        //
        // after using the cb object we must drop it. Next time we come here the
        // object should be a new one.
        //
        //m_subscanCb = Correlator::SubscanSeqCB::_nil();

        //
        // while a sequence is not running we do not ingest any WVR event
        //
        //m_ingestWVRData = false;

        //
        // destroy data-collector objects used during this sequence
        //
        /*for ( map<ACS::Time, DataCollector>::iterator i = m_dataCollector.begin();
              i != m_dataCollector.end();*/
        //      /* empty update */ )
        /*{
            ACS::Time startTime = i->second.getSubScanStartTime();

            m_dataCollector.erase(i++);

            ACS_SHORT_LOG((LM_DEBUG, "erased data collector (st=%s)", Corr::Time::acs2string(startTime).c_str()));
        }*/

        //
        // delete blob streamer threads
        //
	
        /*for ( map<SpectralResolutionTypeMod::SpectralResolutionType, Corr::CDP::Master::Blob::StreamerThread *>::iterator i = m_blobStreamer.begin();
              i != m_blobStreamer.end();*/
             // /* empty update */ )
       /* {
            delete i->second;
            
            ACS_SHORT_LOG((LM_DEBUG, "deleted blob streamer thread (%s)", CSpectralResolutionType::name(i->first).c_str()));

            m_blobStreamer.erase(i++);
        }*/

        //
        // cleanup extract-from-collcetor timing statistics
        //
        //m_extractResultStats.clear();

        //
        // cleanup insert results into collector timing statistics
        //
        //m_insertResultStats.clear();

        //
        // cleanup integrations timing statistics
        //
        //m_integrationLatency.clear();
        
        //
        // clear all identifiers from running list, that is, the sequence
        // is not running any more.
        //
        //m_basket.clearRunningList();
        //
        // now that we are finished with a sub-scan we must be sure to unlock
        // the access mutex.
        //
        pthread_mutex_unlock(&m_accessMutex);

    } /* while ( check() ) */

TheThreadEnd:

    //
    // unlock the access mutex, we already read the vector size
    //
    pthread_mutex_unlock(&m_accessMutex);
        
    //
    // flag the thread as stopped
    //
    //setStopped();

    ACS_SHORT_LOG((LM_DEBUG, "sub-array thread has stopped (%s)", name.c_str()));
}

//---------------------------------------------------------------------------------
void BulkDataNTArrayThread::abort(const string &reason)
{
	std::cout << reason << std::endl;
	if(m_Streamer != 0)
	{
		m_Streamer->abort();
		// cout << "Streamer thread aborted" << endl;
		m_Streamer->stop();
		// cout << "Streamer thread had been stopped" << endl;
		delete m_Streamer;
		m_Streamer = 0;
	}
	sequenceAlreadyRunningFlag = false;
	//cout << "BulkDataNTArrayThread::abort(): exiting" << endl;
}

//----------------------------------------------------------------------------------
bool BulkDataNTArrayThread::addDataEvent(const uint8_t *buffer, const size_t size)
{
	//cout << "addDataEvent started (" << name << ")" <<  endl;

	///ThreadSyncGuard guard(__PRETTY_FUNCTION__, &obj_p->m_eventListMutex);
	ThreadSyncGuard guard(__PRETTY_FUNCTION__, &m_eventListMutex);

	// must first lock the mutex that protects the front-end-buffer
    try
    {
        guard.acquire(m_accessTimeout);
    }
    catch ( BDNTEx &ex )
    {
        ACS_STATIC_SHORT_LOG((LM_ERROR, "%s", ex.asString().c_str()));

        if ( m_addDataEventLogFlag )
        {
            m_addDataEventLogFlag = false;
        }
        return false;
    }

    if(!sequenceAlreadyRunningFlag)
    {
    	// ignore received data
    	cout << "ignore received data" << endl;
    	delete [] buffer;
    }
    else
    {
    	// push pointer to data into the list,
    	m_frontEndBuffer.push_back(pair<uint8_t *, size_t>(const_cast<uint8_t *>(buffer), size));
    }

    // signal consumer thread that data is now available
    pthread_cond_signal(&m_newDataCondition);

    //cout << "addDataEvent ended (" << name << ")" <<  endl;
    // descoping will now unlock the mutex
    return true;
}

//---------------------------------------------------------------------------------
/*void ArrayThread::cleanDataEventLists()
{
    //
    // wvr events are also cleaup in this method, need to guard access
    // to its stl container
    //
    Corr::ThreadSyncGuard guard(getName().c_str(), &m_wvrEventSetMutex, m_accessTimeout);

    //
    // drop all events currently present in the set
    //
    m_wvrEventSet.clear();

    if ( !m_frontEndBuffer.empty() )
    {
        ACS_SHORT_LOG((LM_WARNING, "data buffer list found not empty"));

        //
        // free memory before discarding buffers in the list
        //
        for ( list< pair<uint8_t *, size_t> >::const_iterator i = m_frontEndBuffer.begin();
              i != m_frontEndBuffer.end();
              ++i )
        {
            m_mh_p->free(i->first);
        }

        ACS_SHORT_LOG((LM_DEBUG, "event lists semaphore reset %zd times", m_frontEndBuffer.size()));

        m_frontEndBuffer.clear();
    }

    //
    // no data receive errors (default constructor defaults to 'empty')
    //
    m_eventListErr = CorrEx();
}*/

//----------------------------------------------------------------------------------
BulkDataNTArrayThread::SequenceEndStatus BulkDataNTArrayThread::handleSequenceLoop()
{
	// cout << "handleSequenceLoop started" << endl;
	ACS_SHORT_LOG((LM_DEBUG, "handleSequenceLoop started (%s)", name.c_str()));

	ACS::TimeInterval dataTimeout;
	list< pair<uint8_t *, size_t> > buffers;

	//
	// timeout for data arrival
	//
	dataTimeout = m_accessTimeout;
	// wait for data event, note that this method releases the access
	// mutex and reacquires it before returning
	waitForDataEvent(dataTimeout, buffers);
	//
	// if sequence stop command received then return
	//
	if ( m_sequenceStopFlag )
	{
		sequenceAlreadyRunningFlag = false;
		return SequenceEndStatus_STOPPED;
	}

	//
	// if no data was available in time then return with error
	//
	if ( buffers.empty() )
	{
		//cerr << "data did not arrive on time (to=" << dataTimeout << ")" << endl;
		return SequenceEndStatus_TIMEOUT;
	}

	//
	// pass data to streamer threads
	relyDataToStreamer(buffers);

	//
	// check if the sub-scan in the head is finished and in that case
	// wait until all its data has been streamed out and when done
	// trigger its ended callback, log timing statistics and remove
	// the sub-scan from the map. On the other hand, if the first
	// sub-scan in the map is not actually finished then check
	// that its current integration has not timed out.
	//
	//checkSubscanEndedCallbackTrigger();


 /*   ACS::TimeInterval dataTimeout;
    list< pair<uint8_t *, size_t> > buffers;
    set<Corr::CDP::IntegrationMainHeader *> imhs;
    ExtractedBlobs blobs;
    Corr::TimingStats extractChannelAverageStats, extractFullResolutionStats;
 
    //
    // loop until there are no more sub-scans running in the sequence
    //
    while ( !m_dataCollector.empty() )
    {
        //
        // timeout for data arrival
        //
        dataTimeout = computeDataReceiveTimeout();

        //
        // wait for data event, note that this method releases the access
        // mutex and reacquires it before returning
        //
        waitForDataEvent(dataTimeout, buffers);
        
        //
        // if sequence stop command received then return
        //
        if ( m_sequenceStopFlag )
        {
            return SequenceEndStatus_STOPPED;
        }

        //
        // if no data was available in time then return with error
        //
        if ( buffers.empty() )
        {
            ACS_SHORT_LOG((LM_ERROR, "data did not arrive on time (to=%" PRIi64 ")", dataTimeout));

            return SequenceEndStatus_TIMEOUT;
        }
        
        //
        // split all IMHs in the many buffers as individual entities
        //
        splitIntegrationMainHeaders(buffers, imhs);

        //
        // check for any first integration and trigger started callback
        // Note that sub-scans are reported started as soon as we receive
        // data for them from nodes, not when we first send data out though
        // bulk-data.
        //
        checkSubscanStartedCallbackTrigger(imhs);

        //
        // ingest data to its corresponding data collector instance
        //
        ingestDataIntoCollector(imhs);

        //
        // ingest currently available WVR events
        //
        ingestDataIntoCollector(m_wvrEventSet);

        //
        // check for any completed integration and extract formatted
        // bdf data from collector, if the just extracted integration
        // is the last one in the sub-scan then also extract the mime
        // trailer and include it as one extra stream blob in the 
        // output parameter.
        //
        extractDataFromCollector(blobs);

        // 
        // pass blobs to streamer threads
        //
        relyBlobsToStreamer(blobs);

        //
        // check if the sub-scan in the head is finished and in that case
        // wait until all its data has been streamed out and when done
        // trigger its ended callback, log timing statistics and remove
        // the sub-scan from the map. On the other hand, if the first
        // sub-scan in the map is not actually finished then check
        // that its current integration has not timed out. 
        //
        checkSubscanEndedCallbackTrigger();

    }*/
	ACS_SHORT_LOG((LM_DEBUG, "handleSequenceLoop ended (%s)", name.c_str()));
    return SequenceEndStatus_OK;
}

//---------------------------------------------------------------------------------
void BulkDataNTArrayThread::deleteFrontEndBuffer(const ACS::TimeInterval _to, list< pair<uint8_t *, size_t> > &out)
{
	//cout << __PRETTY_FUNCTION__ << ": started" << endl;
    ACS::TimeInterval to = _to;
    ACS::Time now = getTimeStamp();
    ThreadSyncGuard guard(__PRETTY_FUNCTION__, &m_eventListMutex);

    //
    // the input list must be empty, because the semantic is to 
    // signal a timeout with an empty list as return value.
    //
    if ( !out.empty() )
    {
        BDNT_EX_THROW_EX("input list of pointers is not empty (size=%zd)", out.size());
    }

    //
    // must first lock the mutex that protects the front-end-buffer
    //
    try
    {
        guard.acquire(to);
    }
    catch ( BDNTEx &ex )
    {
        ACS_SHORT_LOG((LM_ERROR, "%s", ex.asString().c_str()));

        return;
    }

    //
    // adjust user timeout to account for the time taken by actually
    // locking the data mutex
    //
    to -= (getTimeStamp() - now);

    //
    // conditional variable waiting status
    //
    bool waitStatus = true;

   // cout << __PRETTY_FUNCTION__ << "keep waiting until data available or timeout" << endl;

    //
    // keep waiting until data available or timeout
    //
    while ( m_frontEndBuffer.empty() && waitStatus )
    {
        waitStatus = Pthread::CondVar::wait(m_newDataCondition, m_eventListMutex, to);
    }

    //
    // if an error was signaled by the data receiving callback then
    // trigger an error
    // TODO
    /*if ( m_eventListErr.isError() )
    {
        throw m_eventListErr;
    }*/

    //
    // if no data then just return leaving the input variable as it was (empty)
    //
    if ( m_frontEndBuffer.empty() )
    {
        //
        // descoping now will release the events list's mutex
        //
        return;
    }

   // cout << __PRETTY_FUNCTION__ << "Buffer no longer empty!" << endl;

    //
    // make a copy of the complete list, all buffers in the
    // list are going to be processed now.
    //
    out = m_frontEndBuffer;

    //
    // clear input list, we are now in charge of its original content
    //
    m_frontEndBuffer.clear();

    //
    // descoping now will release the events list's mutex
    //
    pthread_mutex_unlock(&m_eventListMutex);
}

//----------------------------------------------------------------------------------
void BulkDataNTArrayThread::waitForDataEvent(const ACS::TimeInterval to, list< pair<uint8_t *, size_t> > &out)
{
	//cout << __PRETTY_FUNCTION__ << " started" << endl;
    //
    // before going into waiting for data we unlock
    // the global access mutex
    //
    pthread_mutex_unlock(&m_accessMutex);

    //
    // wait until buffer not empty and remove data pointers from it
    //
    deleteFrontEndBuffer(to, out);

    //
    // independently on whether there was a timeout or not we now need
    // to reacquire the access mutex before any other action could take place
    //
    if ( !Pthread::Mutex::lock("waitForDataEvent reacquiring access mutex", m_accessMutex, m_accessTimeout) )
    {
    	BDNT_EX_THROW_EX("timeout on reacquiring access mutex (to=%" PRIi64 ")", m_accessTimeout);
    }
    // cout << __PRETTY_FUNCTION__ << " stopped" << endl;
}

void BulkDataNTArrayThread::startSequence()
{
    //cout << "startSequence() started" << endl;

    //
    // guard access
    //
    ThreadSyncGuard guard(getName().c_str(), &m_accessMutex, m_accessTimeout);

    //
    // if there is a sequence already running then complain
    // TODO
    if(sequenceAlreadyRunningFlag)
    {
        // cause of the this failure
        //BDNT_COMPLETION_EX(InvalidSubScanTiming, c, "there is a sequence already running");

        // report back to client that started the sequence
        // TODO
        //callback->subscanEnded(subScans[0].scanNumber, subScans[0].subScanNumber, getSubscanMetadata(), c);

        BDNT_EX_THROW_EX("there is a sequence already running");
    }


    //
    // connect to bdd
    // TODO
    ///m_bdSender_p->connect();

    //
    // check that there is at least one receviver listening out there
    // TODO !!
   /* if ( AcsBulkdata::isBulkDataNTEnabled() )
    {
    	map<SpectralResolutionType, unsigned int> receivers;

    	receivers[SpectralResolutionTypeMod::CHANNEL_AVERAGE] = m_bdSender_p->getNumberOfReceivers(SpectralResolutionTypeMod::CHANNEL_AVERAGE);

    	receivers[SpectralResolutionTypeMod::FULL_RESOLUTION] = m_bdSender_p->getNumberOfReceivers(SpectralResolutionTypeMod::FULL_RESOLUTION);

    	receivers[SpectralResolutionTypeMod::BASEBAND_WIDE] = m_bdSender_p->getNumberOfReceivers(SpectralResolutionTypeMod::BASEBAND_WIDE);

    	//
    	// if no WVR data is expected then it does not matter if there is
    	// no receiver listening out there for that data type.
    	//
    	if ( receivers[SpectralResolutionTypeMod::CHANNEL_AVERAGE] == 0 ||
    			receivers[SpectralResolutionTypeMod::FULL_RESOLUTION] == 0 ||
    			(m_expectWVRData && receivers[SpectralResolutionTypeMod::BASEBAND_WIDE] == 0) )
    	{
    		//
    		// cause of the this failure
    		//
    		CORR_COMPLETION_EX(CorrResourceConflict,
    				c,
    				"bulk-data receivers are not listening (ca/fr/wvr=%u/%u/%u)",
    				receivers[SpectralResolutionTypeMod::CHANNEL_AVERAGE],
    				receivers[SpectralResolutionTypeMod::FULL_RESOLUTION],
    				receivers[SpectralResolutionTypeMod::BASEBAND_WIDE]);

    		//
    		// report back to client that started the sequence
    		//
    		callback->subscanEnded(subScans[0].scanNumber, subScans[0].subScanNumber, getSubscanMetadata(), c);

    		CORR_EX_THROW_EX(c.getCause().c_str());
    	}
    }*/

    // TODO
    ///m_blobStreamer.clear();

    //
    // streamer thread name variable
    //
    string threadName;

    //
    // instantiate channel average blob streamer
    // TODO
    //threadName = string("BST_") + m_basket.getArrayId() + string("_CA");
    threadName = "streamerThread";
    try
    {
        m_Streamer = new StreamerThread(threadName.c_str(),m_streamName,m_sendFlowName,
					m_qosLib, m_throttling, m_sendTimeout, m_ACKTimeout);
    }
	catch(ACS_BD_Errors::StreamCreateProblemExImpl &ex)
	{
		cerr << "Problem creating sender stream " << m_streamName << endl;
		throw ex;
	}
	catch ( ... )
    {
        //
        // cleanup those data-collectors created before in this same method
        //
        //m_dataCollector.clear();
    	cerr << "Error creating streamer thread" << endl;
        throw;
    }

    //
    // command nodes to start the sequence
    // TODO?
    /*try
    {
        m_nodesCluster->startSubscanSequence(m_basket.getArray().arrayID.in(),
                                            startTime,
                                            subScans);
    }
    catch ( CorrErr::InvalidArrayEx &ex )
    { ...
    }*/

    //
    // clean data event list (there should be nothing there)
    // TODO?
    ///cleanDataEventLists();


    //
    // retain reference to callback object, to be used later on when the
    // sub-scan has ended or failed.
    // Note: it is important to do this here at the very end, because
    // copying into the var variable takes ownership and, therefore, it will
    // be bad to take the ownershipt and return before this point with an
    // exception and let the caller use the callback from his side without
    // ownership.
    // TODO?
    ///m_subscanCb = Correlator::SubscanSeqCB::_duplicate(callback);

    sequenceAlreadyRunningFlag = true;
    //
    // signal the thread to check again the size of m_runningConfigId. The
    // mutex associated to the condition variable is released as soon as
    // this method goes out of scope.
    //
    pthread_cond_broadcast(&m_condition);
    //cout << "start_sequence() ended" << endl;
}


void BulkDataNTArrayThread::relyDataToStreamer(list< pair<uint8_t *, size_t> > &data)
{

	//cout << "relyDataToStreamer() start" << endl;

    BDNTEx ex;

    //
    // use a rather big try-statement to catch any error and free
    // remaining blob pointers in the input variable
    //
    try
    {

    	//
    	// go through each blob and rely it to the streamer.
    	// Note: use an empty update statement because we want to
    	// remove iterators (blob's pointer) as soon as we have
    	// successfully passed the pointer to the streamer, making
    	// this possible to keep in local scope only those pointers
    	// that have not yet been relied to the streamer and which
    	// would eventually need to be freed here id an error occurs.
    	//
    	for ( list< pair<uint8_t *, size_t> >::iterator it = data.begin();
    			it != data.end();
    			/* empty updated */ )
    	{
    		//
    		// pass data to streamer object
    		//
    		m_Streamer->send(it->first, it->second);

    		//
    		// done with this blob, memory ownership has been transfered to
    		// the streamer, remove the blob's pointer from our scope and
    		// increment the iterator
    		//
    		data.erase(it++);
    	}

    	//
    	// if those blobs were the last ones for this given
    	// stream and sub-scan and it happens that this is the
    	// heading sub-scan then wait for the streamer to report
    	// that the streaming has completed. We want to do this here
    	// because we want the heading sub-scan ready as soon as is
    	// possible. It also implies that outside this method we can
    	// detect that a collector is not needed any more by simply
    	// verifying that both streams are reported as completed,
    	// that is, in that scope we can be sure that all data was
    	// already extracted AND streamed out to completion.
    	//
    	/*if ( u->first == dc.getDataUID(t->first) )
    	{
    		if ( dc.isSubscanComplete(t->first) )
    		{
    			ACS::Time startTime = dc.getSubScanStartTime();
    			ACS::TimeInterval duration = dc.getSubScanDuration();
    			ACS::TimeInterval processing;

    			//
    			// use the sub-scan's assumed processing time as timeout
    			//
    			processing = m_basket.getRunningItem(startTime).m_processing;

    			//
    			// wait for data to be fully streamed out
    			//
    			m_blobStreamer[t->first]->wait(u->first.c_str(),
    					startTime  +
    					duration   +
    					processing -
    					getTimeStamp());

    			ACS_SHORT_LOG((LM_DEBUG, "sub-scan data streaming completed (type/uid=%s/%s)", CSpectralResolutionType::name(t->first).c_str(), u->first.c_str()));
    		}
        }*/
    }
    catch ( BDNTEx &_ex )
    {
        ex = _ex;
    }
    catch ( ... )
    {
        BDNT_EX(ex, "caught an unknown exception");
    }

    //
    // no error means that we can simple just return now
    if ( !ex.isError() )
    {
        return;
    }

    //
    // clean up data pointers that still remain in the input parameter
    //
    for ( list< pair<uint8_t *, size_t> >::iterator it = data.begin();
        			it != data.end();
        			++it )
    {
    	delete it->first;
    	ACS_SHORT_LOG((LM_WARNING, "erased blob"));
    }

    /*for ( ExtractedBlobs::iterator t = blobs.begin();
          t != blobs.end();
          ++t )
    {
        for ( map< std::string, std::list<Corr::CDP::Master::BDFBlob *> >::iterator u = t->second.begin();
              u != t->second.end();
              ++u )
        {
            for ( list<Corr::CDP::Master::BDFBlob *>::iterator b = u->second.begin();
                      b != u->second.end();
                  ++b )
            {
                delete *b;

                ACS_SHORT_LOG((LM_WARNING, "erased blob (type/uid/ptr=%s/%s/%p)", CSpectralResolutionType::name(t->first).c_str(), u->first.c_str(), *b));
            }
        }
    }*/

    //
    // this method always leaves the input variable empty, its content
    // was successfully passed to streamers or explicitly freed whenever
    // an error has been detected. What managed to be passed to the streamer
    // is not our responsibility any more.
    //
    data.clear();

    BDNT_EX_THROW_EX("%s", ex.asString().c_str());
}

//----------------------------------------------------------------------------------
void BulkDataNTArrayThread::stopSequence()
{
    BDNTEx ex;

    //
    // guard access
    //
    ThreadSyncGuard guard(getName().c_str(), &m_accessMutex, m_accessTimeout);

    //
    // well, if there is nothing running then complain
    //
    /*if ( m_basket.getRunningMap().empty() )
    {
        ACS_SHORT_LOG((LM_DEBUG,
                       "no sequence currently running (%d/%s)",
                       m_basket.getArray().arrayIndex,
                       m_basket.getArray().arrayID.in()));

        return;
    }*/

    //
    // as to eventually declare a timeout while waiting for the thread
    // to stop processing the current sub-scan we need to know the time
    // length of the integrations for the executing sub-scan.
    // Note: it is expensive to figure out which exact sub-scan in the sequence
    // is actually running. For the time being take the maximum integration
    // time.
    // Note: remember that the expected behavior for 'stop' is to stop at the
    // end of the current integration.
    //
    ACS::TimeInterval timeout = 0;
    /*for ( ConfigBasket::RunningMap::const_iterator i = m_basket.getRunningMap().begin();
          i != m_basket.getRunningMap().end();
          ++i )
    {
        //
        // keep track of the maximum value
        //
        if ( m_basket.getConfig(ConfigBasket::ConfigType(i->second.m_calibId().second)).integrationDuration > timeout )
        {
            timeout = m_basket.getConfig(ConfigBasket::ConfigType(i->second.m_calibId().second)).integrationDuration;
        }
    }*/

    //
    // provide 1 extra second of grace (avoid latency jittering)
    //
    //timeout += 10000000LLU;
	timeout = 30000000LLU; // 3 sec

    //
    // set sequence stop-flag, this is the flag being check by the thread
    //
    m_sequenceStopFlag = true;

    //
    // unfortunatelly, at the beginning of a sub-scan the thread is waiting
    // for data. In order to make it wake up and check the stop flag we need
    // lock the list's mutex, add a dummy pointer to the list, signal
    // the condition and then unlock the mutex.
    //
    if ( !Pthread::Mutex::lock(__PRETTY_FUNCTION__, m_eventListMutex, m_accessTimeout, true) )
    {
        BDNT_EX_THROW_EX("failed to lock front-end-buffer's mutex (to=%" PRIi64 ")", m_accessTimeout);
    }

    //
    // we are holding the mutex, add a dummy
    //
    m_frontEndBuffer.push_back(pair<uint8_t *, size_t>(NULL, 0));

    //
    // signal new data to force the thread to recheck for the stop flag.
    // If at this moment the thread is not waiting for the condition then
    // next time it come to check for data it will immediately discover
   // the dummy and check for the stop flag.
    //
    pthread_cond_signal(&m_newDataCondition);

    //
    // after signaling the condition then remember to unlock the mutex
    //
    pthread_mutex_unlock(&m_eventListMutex);

//	cout << "stopSequence(): Waiting for the thread to acknowlegde" << endl;
    //
    // wait for the thread to acknowledge
    //
    try
    {
        bool waitStatus = Pthread::CondVar::wait(m_stopCondition, m_accessMutex, timeout);

        if ( !waitStatus )
        {
            BDNT_EX_THROW_EX("thread loop did not acknowledge stop within %" PRId64 " [acs] (%s)", timeout, getName().c_str());
        }
        if ( waitStatus && m_sequenceStopFlag )
        {
            BDNT_EX_THROW_EX("wait status is true but flag was not unset (%s)", getName().c_str());
        }

        //ACS_SHORT_LOG((LM_INFO, "thread acknowledged stop successfully (%s)", m_basket.getArray().arrayID.in()));
//	cout << "thread acknowledged stop successfully" << endl;
    }
    catch ( BDNTEx &_ex )
    {
        ex = _ex;
    }

    //ACS_SHORT_LOG((LM_INFO, "commanding nodes to stop sequence (%s)", m_basket.getArray().arrayID.in()));
	sequenceAlreadyRunningFlag = false;
    //
    // command nodes to stop sequence. We stop the nodes last because we
   // are supposed to stop at the end of the current integration and not
    // immediately.
    //
    //m_nodesCluster_p->stopSubscanSequence(m_basket.getArrayId().c_str());

    //
    // if there was an error while waiting the thread to stop then complain
    //
    /*if ( ex.isError() )
    {
        throw ex;
    }*/

    //ACS_SHORT_LOG((LM_INFO, "sequence stopped successfully (%s)", m_basket.getArray().arrayID.in()));
//	cout << "sequence stopped successfully" << endl;
}


/*___oOo___*/
