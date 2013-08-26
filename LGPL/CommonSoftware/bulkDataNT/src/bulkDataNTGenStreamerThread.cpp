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
 * "@(#) $Id: bulkDataNTGenStreamerThread.cpp,v 1.1 2013/02/11 18:37:33 rbourtem Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * ramestic  2011/08/04  created
 */

//
// System stuff
//
#include <inttypes.h>
#include <sstream>
#include <iterator>
#include <System_Time.h>

//
// ACS stuff
//
#include <acsThreadBase.h>
#include <acsutilTimeStamp.h>
#include <acstimeTimeUtil.h>
#include <acstimeC.h>

//
// ICD stuff
//

//
// bulkDataNT stuff
//
#include <bulkDataNTPosixHelper.h>
#include <bulkDataNTGenEx.h>
//#include <CorrTimedExecute.h>

//
// Local stuff
//
#include "bulkDataNTGenStreamerThread.h"

using namespace std;
using namespace AcsBulkdata;

//
// Static variables
//
const ACS::TimeInterval StreamerThread::m_accessTimeout;

//----------------------------------------------------------------------------------
StreamerThread::StreamerThread( const char *threadName,
				const string & streamName,
				const string & sendFlowName,
				const string & qosLib,
				const double & throttling,
				const double &sendTimeout,
				const double &ACKTimeout):
    ACS::Thread(threadName, m_accessTimeout + 10000000LLU),
    m_isThreadRunning(true),
    m_isAbort(false),
    m_streamName(streamName),
    m_sendFlowName(sendFlowName),
    m_qosLib(qosLib),
    m_senderStream(0),
    m_sendFlow(0),
    m_throttling(throttling),
    m_sendTimeout(sendTimeout),
    m_ACKTimeout(ACKTimeout)
{
    int rc;

    //
    // initialize data access mutex as a normal one (non recursive)
    //
    if ( (rc = pthread_mutex_init(&m_accessMutex, NULL)) )
    {
        BDNT_EX_THROW_EX("failed to initialize access mutex (err=%d)", rc);
    }
    
    //
    // initialize data-ready semaphore
    //
    if ( sem_init(&m_dataReadySem, 0, 0) )
    {
        BDNT_EX_THROW_EX("failed to initialize data-ready semaphore (err=%d)", errno);
    }

    //
    // initialize data-ready semaphore
    //
    if ( sem_init(&m_abortReadySem, 0, 0) )
    {
        BDNT_EX_THROW_EX("failed to initialize abort-ready semaphore (err=%d)", errno);
    }

    // Create the stream
	SenderStreamConfiguration scfg;
	scfg.setParticipantPerStream(true);
	scfg.setQosLibrary(m_qosLib.c_str());
	m_senderStream = new BulkDataNTSenderStream(streamName.c_str(),scfg);

    // let's create flows
    list<char *>::iterator it;
    SenderFlowConfiguration cfg;
	cfg.setQosLibrary(m_qosLib.c_str());
	cfg.setACKsTimeout(m_ACKTimeout);
    	cfg.setSendFrameTimeout(m_sendTimeout);
    	cfg.setThrottling(m_throttling);
    m_sendFlow = m_senderStream->createFlow(m_sendFlowName.c_str(), cfg);

    // print out what we have created
    std::vector<string> tmpFlowNames = m_senderStream->getFlowNames();
    std::cout << "The following " << m_senderStream->getFlowNumber() << " flow(s) has/have been created:[ ";
    for(unsigned int i=0;i<tmpFlowNames.size(); i++)
    	std::cout << tmpFlowNames[i] << " ";
    std::cout << "] on stream: " << m_streamName << std::endl;

    //numOfCreatedFlows = senderStream.getFlowNumber();

    //
    // resume the accompanying thread
    //
    resume();
}

//----------------------------------------------------------------------------------
StreamerThread::~StreamerThread()
{
    //
    // to speed up shutdown transit unset the running flag and
    // give the data-ready semaphore once.
    //
    m_isThreadRunning = false;
    Pthread::Semaphore::post(__func__, m_dataReadySem);

    ACS_SHORT_LOG((LM_DEBUG, "terminating thread... (%s)", getName().c_str()));
    cout << "terminating thread..." << getName() << endl;

    //
    // make sure the thread is not running any more before proceeding
    //
    if ( !terminate() )
    {
        ACS_SHORT_LOG((LM_DEBUG, "blob streamer failed to terminate thread (%s)", getName().c_str()));
        cerr << "streamer failed to terminate thread " << getName() << endl;
    }

    ACS_SHORT_LOG((LM_DEBUG, "thread terminated (%s)", getName().c_str()));
    cout << "thread " << getName() << " terminated" << endl;

    delete m_sendFlow;
    delete m_senderStream;

    //
    // free memory used by unsent data buffers
    //
    for ( list<pair<uint8_t *, size_t> >::iterator i = m_data.begin();
          i != m_data.end();
          ++i )
    {
        delete i->first;

        //ACS_SHORT_LOG((LM_ERROR, "forcibly deleting queued data (%s/%s)", i->first.c_str(), getName().c_str()));
        cout << getName() << ": forcibly deleting queued data" << endl;
    }

    ACS_SHORT_LOG((LM_DEBUG, "streamer thread destructor done (%s)", getName().c_str()));
    cout << getName() << "streamer thread destructor done" << endl;
}

//----------------------------------------------------------------------------------
void StreamerThread::send(uint8_t * data, size_t size)
{
    //
    // if in abort process then refuse to send more data
    //
    if ( m_isAbort )
    {
        BDNT_EX_THROW_EX("already aborting flow (%s)", getName().c_str());
    }

    //
    // guarded access to event list
    //
    if ( !Pthread::Mutex::lock(__func__, m_accessMutex, m_accessTimeout, true) )
    {
        BDNT_EX_THROW_EX("failed to lock access (%s)", getName().c_str());
    }

    //
    // add data pointer to list
    //
    m_data.push_back(pair<uint8_t *,size_t>(data,size));

    //
    // signal that the list contains new data
    //
    Pthread::Semaphore::post(__func__, m_dataReadySem);

    //
    // unlock data access, note that this is better to keep as the last
    // action in this method because 'abort' holds the mutex itself.
    //
    pthread_mutex_unlock(&m_accessMutex);
}

//----------------------------------------------------------------------------------
//void StreamerThread::abort(uint8_t * data, size_t size)
void StreamerThread::abort()
{
    string isError("");

    //
    // please only one abort
    //
    if ( m_isAbort )
    {
        BDNT_EX_THROW_EX("already aborting flow (%s)", getName().c_str());
    }

    //
    // lock access mutex to set aborting flag 
    //
    if ( !Pthread::Mutex::lock(__func__, m_accessMutex, m_accessTimeout, true) )
    {
        ACS_SHORT_LOG((LM_ERROR, "failed to lock access (%s)", getName().c_str()));

        return;
    }

    //
    // add data pointer to list
    //
    ///m_data.push_back(pair<uint8_t *, size_t>(data, size));

    //
    // signal that the list contains new data
    //
    Pthread::Semaphore::post(__func__, m_dataReadySem);

    //
    // after setting the abortion flag no more data is accepted and
    // the thread knows that after exhausting the actual list content
    // the flow is finished.
    //
    m_isAbort = true;

    //
    // ready with shared data
    //
    pthread_mutex_unlock(&m_accessMutex);

    //
    // wait until all data was streamed out, note that the aborted data must
    // follow the already queue data because the actual abort's content is
    // a function of what was sent before.
    //
    Pthread::Semaphore::lock("waiting for abort flushing", m_abortReadySem, m_accessTimeout * 2, true);
}

//----------------------------------------------------------------------------------
void StreamerThread::run()
{
    bool isAbort;
    ACE_Time_Value start_time, elapsed_time;
    double throuhgput=0;
    double send_time = 0.0;
    double sumThrouhgput=0.0;
    unsigned int dataSize = 0;

    ACS_SHORT_LOG((LM_DEBUG, "streamer thread has started (%s)", getName().c_str()));
    cout << getName() << ": streamer thread has started" << endl;

    //
    // infinite loop (until requested to stop)
    //
    while ( check() && m_isThreadRunning )
    {
        list<pair<uint8_t *, size_t> > data;

        // cout << getName() << ": Wait for data available" << endl;

        //
        // wait for data available
        //
        if ( !Pthread::Semaphore::lock("data event lists", m_dataReadySem, m_accessTimeout) )
        {
            continue;
        }

        //
        // we got the semaphore but that could be because the destructor
        // is unrolling everything down
        //
        if ( !m_isThreadRunning )
        {
            break;
        }

       // cout << getName() << ": data available!" << endl;

        //
        // lock the list before accessing it
        //
        if ( !Pthread::Mutex::lock(__func__, m_accessMutex, m_accessTimeout, true) )
        {
            //
            // this should not happen very often but still it could happen...
            //
            ACS_SHORT_LOG((LM_ERROR, "failed to lock access (%s)", getName().c_str()));

            //
            // we already took the semaphore once, therefore, give it once
            // back in order to give the unhandled data a chance next time
            //
            if ( !Pthread::Semaphore::post(__func__, m_dataReadySem) )
            {
                ACS_SHORT_LOG((LM_ERROR, "failed to to re-post semaphore (%s)", getName().c_str()));
            }

            continue;
        }

        //
        // make a copy of the data which is available to transfer
        //
        data = m_data;

        //
        // all items are in this scope now ('data' vatiable), leave list empty
        //
        m_data.clear();

        //
        // copy the actual abort flag status, we must read it only
        // while holding the mutex.
        //
        isAbort = m_isAbort;

        //
        // ready with the list and unlock
        //
        pthread_mutex_unlock(&m_accessMutex);

       // cout << getName() << ": Ready to send via DDS" << endl;

        sumThrouhgput=0.0;
        //
        // iterate through items in the list streaming them out,
        // the last item is to be 'abort' if the flag is set
        //
        /*for ( list<pair<uint8_t *, size_t> >::iterator i = data.begin();
              i != (isAbort ? --data.end() : data.end());
              ++i )*/
        for ( list<pair<uint8_t *, size_t> >::iterator i = data.begin();
              i != data.end();
              ++i )
        {
            try
            {
            	// send through DDS
            	dataSize = i->second;
            	m_sendFlow->startSend((const unsigned char*)&dataSize, sizeof(unsigned int));
            	//ACS_SHORT_LOG((LM_INFO, "Going to send : %d Bytes of data to flow: '%s' to %d receiver(s)", dataSize, m_sendFlowName.c_str(), m_sendFlow->getNumberOfReceivers()));
            	ACS_LOG(LM_RUNTIME_CONTEXT,__FUNCTION__,(LM_INFO,"Going to send : %d Bytes of data to flow: '%s' to %d receiver(s)", dataSize, m_sendFlowName.c_str(), m_sendFlow->getNumberOfReceivers()));
            	//cout << "Going to send  " << dataSize << " Bytes of data to flow " << m_sendFlowName << endl;
            	start_time = ACE_OS::gettimeofday();
            	m_sendFlow->sendData(i->first,i->second);
            	elapsed_time = ACE_OS::gettimeofday() - start_time;
            	send_time = (elapsed_time.sec()+( elapsed_time.usec() / 1000000. ));
            	throuhgput = (dataSize/(1024.0*1024.0))/send_time;
            	ACS_SHORT_LOG((LM_INFO, "Transfer rate for flow '%s': %f MBytes/sec",
            			m_sendFlowName.c_str(), throuhgput));
            	sumThrouhgput+=throuhgput;
            	m_sendFlow->stopSend();
            	//Blob::Streamer<BulkData::SenderTemporal>::send(i->first, *(i->second));
            }
            catch ( BDNTEx &ex )
            {
                ACS_SHORT_LOG((LM_ERROR, "%s", ex.asString().c_str()));
            }

            //
            // get rid of memory that was allocated before us
            //
            delete i->first;
        }

        //
        // the last item is 'abort' then send it now (see en limit
        // in previous for-loop)
        //
        if ( isAbort )
        {
            ACS_SHORT_LOG((LM_DEBUG, "streamer thread aborting "));
		cout << "streamer thread aborting" << endl;
            /*try
            {
            	// send through DDS
            	dataSize = --data.end()->second;
            	m_sendFlow->startSend((const unsigned char*)&dataSize, sizeof(unsigned int));
            	//ACS_SHORT_LOG((LM_INFO, "Going to send : %d Bytes of data to flow: '%s' to %d receiver(s)", dataSize, m_sendFlowName.c_str(), m_sendFlow->getNumberOfReceivers()));
            	ACS_LOG(LM_RUNTIME_CONTEXT,__FUNCTION__,(LM_INFO,"Going to send : %d Bytes of data to flow: '%s' to %d receiver(s)", dataSize, m_sendFlowName.c_str(), m_sendFlow->getNumberOfReceivers()));
            	cout << "Going to send  " << dataSize << " Bytes of data to flow " << m_sendFlowName << endl;
            	start_time = ACE_OS::gettimeofday();
            	m_sendFlow->sendData(--data.end()->first,--data.end()->second);
            	elapsed_time = ACE_OS::gettimeofday() - start_time;
            	send_time = (elapsed_time.sec()+( elapsed_time.usec() / 1000000. ));
            	throuhgput = (dataSize/(1024.0*1024.0))/send_time;
            	ACS_SHORT_LOG((LM_INFO, "Transfer rate for flow '%s': %f MBytes/sec",
            			m_sendFlowName.c_str(), throuhgput));
            	sumThrouhgput+=throuhgput;
            	m_sendFlow->stopSend();
            	//Blob::Streamer<BulkData::SenderTemporal>::send(i->first, *(i->second));
            }
            catch ( BDNTEx &ex )
            {
                ACS_SHORT_LOG((LM_ERROR, "%s", ex.asString().c_str()));
            }*/

            //
            // get rid of memory that was allocated before us
            //
            //delete ((--data.end())->first);

            //
            // announce that abort has completed
            //
            Pthread::Semaphore::post(__func__, m_abortReadySem);
        }
        
    } /* while ( check() ) */


    ACS_SHORT_LOG((LM_DEBUG, "streamer thread has stopped (%s)", getName().c_str()));
    cout << getName() << ": streamer thread has stopped" << endl;

    //
    // flag the thread as stopped
    //
    setStopped();
}

/*
void StreamerThread::startSend(unsigned int & datasize)
{
	ACS_SHORT_LOG((LM_INFO, "Going to send parameter: '%s' to flow: '%s' to %d receiver(s)", param.c_str(), m_sendFlowName.c_str(), m_sendFlow->getNumberOfReceivers()));
	m_sendFlow->startSend((const unsigned char*)&datasize, sizeof(unsigned int));
}*/

/*___oOo___*/
