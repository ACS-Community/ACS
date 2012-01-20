/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration),
*    All rights reserved
*
*    This library is free software; you can redistribute it and/or
*    modify it under the terms of the GNU Lesser General Public
*    License as published by the Free Software Foundation; either
*    version 2.1 of the License, or (at your option) any later version.
*
*    This library is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*    Lesser General Public License for more details.
*
*    You should have received a copy of the GNU Lesser General Public
*    License along with this library; if not, write to the Free Software
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: acsThreadBase.cpp,v 1.48 2012/01/20 23:18:16 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran  17/02/01  created
*/

#include <vltPort.h>

/////////////////////////////////////////////////
// COS Library Thread Manipulation
/////////////////////////////////////////////////

#include <logging.h>
#include <acsutilTimeStamp.h>
#include "acsThreadBase.h"

using namespace ACS;

TimeInterval ACS::getTime()
{
    //forward this call to getTime() defined within acsutilTimeStamp.h
    return ::getTime();
}//ACS::getTime

/////////////////////////////////////////////////
// ThreadBase
// former BACIThread
/////////////////////////////////////////////////

TimeInterval ThreadBase::defaultResponseTime = 1000*1000*10;	   // 1s
TimeInterval ThreadBase::defaultSleepTime = 100*1000*10;		   // 100ms

ThreadBase* ThreadBase::NullThreadBase = 0;

InitThreadFunc ThreadBase::InitThread = 0;
DoneThreadFunc ThreadBase::DoneThread = 0;

ThreadBase::ThreadBase(const ACE_CString& _name,
		       ACE_Thread_Manager* _threadManager,
		       void* _threadProcedure,
		       void* _parameter,
		       const TimeInterval& _responseTime,
		       const TimeInterval& _sleepTime,
		       const bool _create,
		       const long _thrFlags,
		       const size_t _stackSize) :
    Logging::Loggable(_name.c_str()),
    threadProcedure_mp(_threadProcedure), parameter_mp(_parameter),
    responseTime_m(_responseTime), sleepTime_m(_sleepTime), suspendStatus_m(0),
    exitRequest_m(false), stopped_m(false),
    name_m(_name), threadID_m(0),
    thrFlags_m(_thrFlags),
    stackSize_m(_stackSize),
    threadManager_mp(_threadManager),
    m_suspendSemaphore(1, 0, 0, 1),  // Starts unlocked. An acquire will not block. Max 1
#ifdef __CYGWIN__
    m_sleepSemaphore(0, 0, 0 , 0, 1)  // Starts locked. An acquire will block. Max 1
#else
    m_sleepSemaphore(0, 1, 0 , 0, 1)  // Starts locked. An acquire will block. Max 1
#endif
{

    ACS_TRACE("ACS::ThreadBase::ThreadBase");

    suspend();

    if (_create)
	{
	if (!create(_thrFlags))
	    {
	    acsthreadErrType::CanNotSpawnThreadExImpl ex(__FILE__, __LINE__, "ACS::ThreadBase::ThreadBase");
	    ex.setThreadName(_name);
	    throw ex;
	    }
	}
}

ThreadBase::~ThreadBase() {
    ACS_TRACE("ACS::ThreadBase::~ThreadBase");
    terminate();
}

bool ThreadBase::create(const long _thrFlags) {
    ACS_TRACE("ACS::ThreadBase::create");

    timeStamp_m = getTime();

    stopped_m = exitRequest_m = false;

    thrFlags_m = _thrFlags;

    ThreadBaseParameter *param_p =  new ThreadBaseParameter(this, parameter_mp);

    int succ = threadManager_mp->spawn((ACE_THR_FUNC)threadProcedure_mp,
				      param_p,
				      _thrFlags,
				      &threadID_m,
				      0/*t_handle*/,
				      ACE_DEFAULT_THREAD_PRIORITY/*priority*/,
				  	  -1/*grp_id*/,
				      0/*stack*/,
				      stackSize_m
					);
    return (succ!=-1);
}


bool ThreadBase::suspend() {
    ACS_TRACE("ACS::ThreadBase::suspend");

    if (stopped_m==true)
	{
	return false;
	}

    if(suspendStatus_m == true)
	{
	return true;
	}

    /*
     * Set the member that keeps track of suspend status.
     * This is used in the thread sleep function
     * to determine if it has to suspend or if it can just
     * sleep for the requested amount of time.
     */
    suspendStatus_m = true;
    m_suspendSemaphore.acquire();

    return true;
}

bool ThreadBase::resume() {
    ACS_TRACE("ACS::ThreadBase::resume");

    if (stopped_m==true)
	{
	return false;
	}

    if(suspendStatus_m == false)
	{
	return true;
	}

    /*
     * We now reset the suspendStatus and
     * then we can release the sepaphore
     * effectively unlocking any Thread::sleep
     */
    /**
     * @todo: we might have some concurrency problems here.
     *        to be thought over
     */
    suspendStatus_m=0;
    m_suspendSemaphore.release(1);
    makeTimeInterval();

    return true;
};

bool ThreadBase::cancel() {

    ACE_CString thread_name           = getName();
    ACE_thread_t threadID             = threadID_m;
    ACE_Thread_Manager *threadManager = threadManager_mp;

    if (stopped_m==true)
	{
	return true;
	}

    ACS_TRACE("ACS::ThreadBase::cancel");

    /*
     * @attention
     * Do not use any data member after this line.
     * An autodelete object might start
     * deleting as soon as the thread is cancelled.
     */

    /*
     * GCH 2006-02-02
     * How can we force a cancel of the thread?
     * Actually I do not think there is really a good way.
     * You might think to use:
     * - threadManager_mp->cancel(threadID_m)
     *   I tried, I do not get ENOTSUP, but nevertheless
     *   I do not see any action beeing taken.
     * - threadManager_mp->kill(threadID_m, SIGINT)!=-1
     *   But this would kill the whole process, as
     *   by posix threads specs.
     * The only thing that remains is to
     * release the m_runSemaphore to unlock any suspend or sleep
     * and hope that the tread developer will use check()
     * and exit the thread as soon as possible.
     *
     * I can improve the chances of success by
     * putting directly a thread exit in the implementation
     * of sleep.
     * See comments there.
     * I need to add a flag.
     */

    /**
     * @todo GCH Code to be factored out.
     * The code that follows is essentially identical to stop()
     * and should be factored out.
     * We just do loop only twice
     */

    TimeInterval rs = responseTime_m;  // Unit: 100ns
    if (rs<1000000)
	{
	rs=1000000;               // minimum is 100ms
	}

    /*
     * This flags the object for exiting, if not done already.
     * The thread service function has to check periodically
     * if this is the case (at least at responseTime_m frequency)
     * and ensure that it terminates as soon as possible
     */
    exit();

    /*
     * Now I force releasing the semaphore.
     * This will unblock any thread sleep
     */
    m_suspendSemaphore.release(1);
    m_sleepSemaphore.release(1);

    /*
     * We loop for a total of 0.5 response time
     * If in that time the thread is terminated we return TRUE.
     * To decide if the thread is terminated we perform two tests:
     *  - the thread is terminated according to the ACE_Thread_Manager
     *  - the thread is not even any more under control of the ACE_Thread_Manager
     */
    ACE_Time_Value tv (0, 10000);  // sleep for 10ms
    /* rs is in 100ns and we have to find out how many loops for waiting 0.5 response time*/
    int n = static_cast<int>((rs * 0.5) /  100000 /* = 10 ms*/);

    while ( 0 < --n )
	{
	if (threadManager->testterminate(threadID) == true ||
	    threadManager->thread_within(threadID) == false   )
	    {
	    /*
	     * At this point the thread object might have been destroyed already
	     * (see the auto delete ACS::Thread), therefore I cannot safely
	     * use the logging macros, bacuse they would point me to object
	     * storage that has been already released.
	     * Therefore I explicitly get the glogal logger.
	     */
	    ACE_CString thread_message = "Thread stopped:" + thread_name;
	    ACS_CHECK_LOGGER;
	    ::getLogger()->log(Logging::Logger::LM_DEBUG,
			       thread_message.c_str(),
			       __FILE__, __LINE__,
			       "ThreadBase::cancel");
	    return true;
	    }
	  ACE_OS::sleep(tv);
	}

    /*
     * If we are here, the thread did not stop in the allotted time
     */
    ACS_LOG(LM_SOURCE_INFO,"ThreadBase::cancel",
	    (LM_ERROR,"Thread %s did not stop within %d 100ns",
	     thread_name.c_str(),
	     rs));

    return false;
}

bool ThreadBase::restart() {
    ACS_TRACE("ACS::ThreadBase::restart");

    if( terminate() == false )
      return false;

//  ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
    //GUARD;
    /*
     * Here call create passing the same thrFlags
     * that had been set in a previous call to create.
     * If the thread was never created before,
     * then thrFlags_m contains the default initialisation
     * value.
     */
    return create(thrFlags_m);
}

bool ThreadBase::terminate() {
    ACS_TRACE("ACS::ThreadBase::terminate");
    if (stopped_m==true)
	{
	return true;
	}

    if (stop(true)==false)
	{
	return cancel();
	}
    else
	{
	return true;
	}
}


bool ThreadBase::stop( bool terminating ) {
    ACE_CString thread_name           = getName();
    ACE_thread_t threadID             = threadID_m;
    ACE_Thread_Manager *threadManager = threadManager_mp;

    ACS_LOG(LM_SOURCE_INFO,"ThreadBase::stop",
	    (LM_DEBUG,"Thread %s",
	     thread_name.c_str()));
    if (stopped_m==true)
	{
	return true;
	}


    TimeInterval rs = responseTime_m;  // Unit: 100ns
    if (rs<1000000)
	{
	rs=1000000;               // minimum is 100ms
	}

    /**
     * @todo GCH: look at the commented code. The guard is commented out.
     * Why? Think about it.
     *
     * // ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
     */

    /*
     * This flags the object for exiting.
     * The thread service function has to check periodically
     * if this is the case (at least at responseTime_m frequency)
     * and ensure that it terminates as soon as possible
     */
    exit();

    /*
     * Now I force releasing the semaphore.
     */
    m_suspendSemaphore.release(1);

    /**
     * From now on, I should not use any more
     * data members of the thread object.
     * It might be that the object itself is automatically
     * destroyed as a consequence of the exit and therefore
     * I cannot risk accessing released memory here.
     */

    /*
     * I check if the thread service function has exited and the thread
     * is terminated.
     * I do it at a frequency 1ms
     * for a total time of 1.5*response time.
     * After that I bail out
     */

    ACE_Time_Value tv (0, 10000);  // sleep for 10ms
    /* rs is in 100ns and we have to find out how many loops for waiting 1.5 response time*/
    int n = static_cast<int>((rs * 1.5) /  100000 /* = 10 ms*/);

    while ( 0 < --n )
	{
	if (threadManager->testterminate(threadID) == true ||
	    threadManager->thread_within(threadID) == false   )
	    {
	    /*
	     * At this point the thread object might have been destroyed already
	     * (see the auto delete ACS::Thread), therefore I cannot safely
	     * use the logging macros, bacuse they would point me to object
	     * storage that has been already released.
	     * Therefore I explicitly get the glogal logger.
	     */
	    ACE_CString thread_message = "Thread stopped:" + thread_name;
	    ACS_CHECK_LOGGER;
	    ::getLogger()->log(Logging::Logger::LM_DEBUG,
			       thread_message.c_str(),
			       __FILE__, __LINE__,
			       "ThreadBase::stop");
	    return true;
	    }
	  ACE_OS::sleep(tv);
	}

    /*
     * If we are here, the thread did not stop in the allotted time
     */
    if( !terminating ) {
      ACS_LOG(LM_SOURCE_INFO,"ThreadBase::stop",
  	      (LM_ERROR,"Thread %s did not stop within %d 100ns",
	       thread_name.c_str(),
	       rs));
    }

    return false;
}

void ThreadBase::makeTimeInterval()
{
    // ACS_TRACE("ACS::ThreadBase::makeTimeInterval");
    timeStamp_m=getTime();
}


bool ThreadBase::isResponding() const
{
    ACS_TRACE("ACS::ThreadBase::isResponding");
    return ((getTime()-timeStamp_m)<responseTime_m);
}


bool ThreadBase::check()
{
    // ACS_TRACE("ACS::ThreadBase::check");

    // optimized
    timeStamp_m=getTime();

    return (stopped_m==false && exitRequest_m==false);
}

/*
 * Debugging logs are commented for performance reasons
 * Uncomment them if needed for debugging.
 */
ThreadBase::SleepReturn ThreadBase::sleep(TimeInterval timeIn100ns) const
{
    int acquireRet;

    TimeInterval timeToSleep = timeIn100ns ? timeIn100ns : getSleepTime();

    /*
     * If the thread is suspended, we have to wait forever,
     * i.e. until it is resumed.
     * Therefore we acquire the suspendSemaphore without a timeout.
     * But if we get resumed and acquire the semaphore
     * we have to release immediately, otherwise the next call to
     * suspend will block.
     */
    if ( isSuspended() )
	{
    	while ( ((acquireRet = m_suspendSemaphore.acquire())) == -1 && errno == EINTR )
    			continue; // Restart if interrupted by handler

    	/*
         * The semaphore acquire returns:
         * 0 (errno: x)
         *      if it managed to acquire.
	 *      This happens only when the thread is resumed
         * -1 (errno: x)
	 *      in case of .problems, to be identified
	 *      An error log is submitted.
         */
	if(acquireRet == 0)
	    {
   	    // ACS_LOG(LM_SOURCE_INFO,"ThreadBase::sleep",
	    //	(LM_ERROR,"Acquire %d (errno: %d) - suspended",
	    //	 acquireRet, errno));
	    m_suspendSemaphore.release(1);
	    return SLEEP_SUSPEND;
	    }
	else
	    {
	    ACS_LOG(LM_SOURCE_INFO,"ThreadBase::sleep",
		    (LM_ERROR,"Acquire %d (errno: %d - %s) - unexpected acquire in suspended",
		     acquireRet, errno, strerror(errno)));
	    return SLEEP_ERROR;
	    }
	}
    /*
     * Otherwise we have to wait for the requested time.
     * To do this, we acquire the runSemaphore (that is NOT
     * available) with the specified time as timeout.
     * When the time will be expired, the thread will gain
     * control again.
     * Releasing the semaphore will allow to "interrupt the sleep"
     */
    else if( timeToSleep > 0 )
	{

	/*
	 * Gets the sleep time in TimeInterval units,
	 * converts to us dividing by 10 and initializes an ACS_Time_Value
	 */
	ACE_Time_Value rs_timevalue(0, timeToSleep / 10);

	/*
	 * Now goes from relative to absolute time by adding
	 * the sleep time to the current time
	 */
	ACE_Time_Value sleepTime(ACE_OS::gettimeofday() + rs_timevalue);
	while ( ((acquireRet = m_sleepSemaphore.acquire(&sleepTime))) == -1 && errno == EINTR )
		continue; // Restart if interrupted by handler

        /*
         * The semaphore acquire returns:
         * -1 (errno: 62)
	 *      if it timed out.
	 *      This is for us a "good" sleep.
         * 0 (errno: x)
         *      if it managed to acquire.
	 *      This happens only if interrupted.
         */
	// ACS_LOG(LM_SOURCE_INFO,"ThreadBase::sleep",
	//	(LM_ERROR,"Acquire %d (errno: %d) - true sleep",
	//	 acquireRet, errno));
	if(acquireRet == 0)
	    {
	    return SLEEP_INTERRUPTED;
	    }
	else if(acquireRet == -1 && errno!=ETIME)
	    {
	    ACS_LOG(LM_SOURCE_INFO,"ThreadBase::sleep",
	    	(LM_ERROR,"Acquire %d (errno: %d - %s) - unexpected acquire in sleep",
	    	acquireRet, errno, strerror(errno)));
	    return SLEEP_ERROR;
	    }

	}

    return SLEEP_OK;

}

void ThreadBase::yield()
{
	ACS_TRACE("ACS::ThreadBase::yield");
	ACE_Thread::yield();
}//yield


void ThreadBase::setPriority(int _priority)
{
    ACS_TRACE("ACS::ThreadBase::setPriority");
    ACE_OS::thr_setprio(getThreadID(), _priority);
}//setPriority

int ThreadBase::getPriority()
{
    ACS_TRACE("ACS::ThreadBase::getPriority");
    int prio;
    ACE_OS::thr_getprio(getThreadID(), prio);
    return prio;
}//getPriority

/////////////////////////////////////////////////
// ThreadManagerBase
/////////////////////////////////////////////////

ThreadManagerBase::~ThreadManagerBase() {
    ACS_TRACE("ACS::ThreadManagerBase::~ThreadManagerBase");

    ACE_CString thrName;

    /* get mutex lock before starting terminate
     * since we need to read from threads_m
     * in case some thread is finishing its job, threads_m will change
     */
    ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
    terminateAll();

    /* after terminating threads, auto-delete threads will wait for
     * mutex lock in destructor, so we release here
     */
    guard.release();

    /* Let's give a chance to the other threads to get the mutex.
     */
    ACE_Thread::yield();

    /* acquire mutex lock again, since thrNum will change if some
     * threads are in destructor, threads_m will change in such case
     */
    guard.acquire();
    unsigned int thrNum = getThreadCount();

    /* we remove from the last, since threads_m is a vector, it cost less
     * in ~Threads, it will remove itself from threads_m and threadMap
     * so, no need to call removeFromMap again
     * problems?
     *   1. ~BaseThread don't call removeFromMap, could it happen
     *      some thread in thread_m is a BaseThread ?
     *   2. double deleting could happen, if some threads' removeFromMap
     *      is not finished yet, but, during test, it seems no problem?
     */
    for (unsigned int n=thrNum; n > 0 ; n--)
	{
	  delete threads_m[n-1];
	} /* end for n */
//    for (unsigned int n=0; n < thrNum ; n++)
//	{
//	thrName = threads_m[0]->getName();
//
//      /*
//	 * we delete always the first thread in the vector because
//	 * the thread removes itself from the thread manager
//	 * (and therefore the vector) when it is deleted.
//	 * But old threads do not remove themself from the thread
//	 * manager, so we remove them by hand in any case
//	 * to be sure everything is cleaned up.
//	 */
//      delete threads_m[0];
//	removeFromMap(thrName);
//	}
}

ThreadBase* ThreadManagerBase::create(const ACE_CString& name,
				      void* threadProc,
				      void* parameter,
				      const TimeInterval& responseTime,
				      const TimeInterval& sleepTime,
				      const long _thrFlags,
				      const size_t _stackSize)
{
    ACS_TRACE("ACS::ThreadManagerBase::create");
    ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex); //  GUARD;
    if (getThreadByName(name)!=NULL)
	{
	return NULL;
	}
    /*
     * Here hard codes
     * create    = true
     */
    ThreadBase* thread_p = new ThreadBase(name, threadManager_mp, threadProc, parameter,
					  responseTime, sleepTime,
					  true,
					  _thrFlags,
					  _stackSize);
    add2map(name, thread_p);
    return thread_p;
}



bool ThreadManagerBase::add(const ACE_CString& name, ThreadBase* thread)
{
    ACS_TRACE("ACS::ThreadManagerBase::add");
    ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex); //   GUARD;
    if (getThreadByName(name)!=NULL)
	{
	return false;
	}
    add2map(name, thread);
    return true;
}

bool ThreadManagerBase::stop(const ACE_CString& name) {
//  ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
//  GUARD;
  ACS_TRACE("ACS::ThreadManagerBase::stop");
  ThreadBase* thread_p=getThreadByName(name);
  if (thread_p==NULL)
      {
      return false;
      }
  return thread_p->stop();
}

bool ThreadManagerBase::stopAll() {
// ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
//  GUARD;

  ACS_TRACE("ACS::ThreadManagerBase::stopAll");
  int  failedCount = 0;
  ACE_CString thread_name;

  for (int n=0; n < getThreadCount(); n++)
      {
      thread_name = threads_m[n]->getName();
      if (threads_m[n]->stop()==false)
	  {
	  ACS_LOG(LM_SOURCE_INFO,"ThreadManagerBase::stopAll",
		  (LM_ERROR,"Failed to stop thread %s", thread_name.c_str()));
	  failedCount++;
	  }
      }
  if(failedCount != 0)
      {
      ACS_LOG(LM_SOURCE_INFO,"ThreadManagerBase::stopAll",
	      (LM_INFO,"Failed to stop %d threads", failedCount));
      }
  return failedCount==0 ? true : false;
}

void ThreadManagerBase::exit(const ACE_CString& name) {
// ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
//  GUARD;
  ACS_TRACE("ACS::ThreadManagerBase::exit");
  ThreadBase* thread_p=getThreadByName(name);
  if (thread_p==NULL)
      {
      return;
      }
  thread_p->exit();
}

void ThreadManagerBase::exitAll() {
// ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
//  GUARD;
  ACS_TRACE("ACS::ThreadManagerBase::exitAll");
  for (int n=0; n < getThreadCount(); n++)
      {
      threads_m[n]->exit();
      }
}

bool ThreadManagerBase::terminate(const ACE_CString& name) {
// ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
//  GUARD;
  ACS_TRACE("ACS::ThreadManagerBase::terminate");
  ThreadBase* thread_p=getThreadByName(name);
  if (thread_p==NULL)
      {
      return false;
      }
  return thread_p->terminate();
}

bool ThreadManagerBase::terminateAll() {
// ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
//  GUARD;
  ACS_TRACE("ACS::ThreadManagerBase::terminateAll");
  bool ok = true;
  unsigned int thrNum = getThreadCount();

  /* terminating starting from last threads,
   * since threads_m is vector, it is easier to remove from last
   */
  for (int n=thrNum; n > 0; n--)
      {
	  if (threads_m[n-1]->terminate()==false)
	      {
	      ok = false;
	      }
      }
  return ok;
}

bool ThreadManagerBase::cancel(const ACE_CString& name) {
// ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
//  GUARD;
  ACS_TRACE("ACS::ThreadManagerBase::cancel");
  ThreadBase* thread_p=getThreadByName(name);
  if (thread_p==NULL)
      {
      return false;
      }
  return thread_p->cancel();
}

bool ThreadManagerBase::cancelAll() {
// ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
//  GUARD;
  ACS_TRACE("ACS::ThreadManagerBase::cancelAll");
  bool ok = true;
  for (int n=0; n < getThreadCount(); n++)
      {
      if (threads_m[n]->cancel()==false)
	  {
	  ok = false;
	  }
      }
  return ok;
}

bool ThreadManagerBase::restart(const ACE_CString& name) {
// ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
//  GUARD;
  ACS_TRACE("ACS::ThreadManagerBase::restart");
  ThreadBase* thread_p=getThreadByName(name);
  if (thread_p==NULL)
      {
      return false;
      }
  return thread_p->restart();
}

bool ThreadManagerBase::restartAll() {
// ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
//  GUARD;
  ACS_TRACE("ACS::ThreadManagerBase::restartAll");
  bool ok = true;
  for (int n=0; n < getThreadCount(); n++)
      {
      if (threads_m[n]->restart()==false)
	  {
	  ok = false;
	  }
      }
  return ok;
}

bool ThreadManagerBase::suspend(const ACE_CString& name) {
// ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
//  GUARD;
  ACS_TRACE("ACS::ThreadManagerBase::suspend");
  ThreadBase* thread_p=getThreadByName(name);
  if (thread_p==NULL)
      {
      return false;
      }
  return thread_p->suspend();
}

bool ThreadManagerBase::suspendAll() {
// ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
//  GUARD;
  ACS_TRACE("ACS::ThreadManagerBase::suspendAll");
  bool ok = true;
  for (int n=0; n < getThreadCount(); n++)
      {
      if (threads_m[n]->suspend()==false)
	  {
	  ok = false;
	  }
      }
  return ok;
}

bool ThreadManagerBase::resume(const ACE_CString& name) {
// ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
//  GUARD;
  ACS_TRACE("ACS::ThreadManagerBase::resume");
  ThreadBase* thread_p=getThreadByName(name);
  if (thread_p==NULL)
      {
      return false;
      }
  return thread_p->resume();
}

bool ThreadManagerBase::resumeAll() {
// ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
//  GUARD;
  ACS_TRACE("ACS::ThreadManagerBase::resumeAll");
  bool ok = true;
  for (int n=0; n < getThreadCount(); n++)
      {
      if (threads_m[n]->resume()==false)
	  {
	  ok = false;
	  }
      }
  return ok;
}

bool ThreadManagerBase::isAlive(const ACE_CString& name) {
// ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
//  GUARD;
  ACS_TRACE("ACS::ThreadManagerBase::isAlive");
  ThreadBase* thread_p=getThreadByName(name);
  if (thread_p==NULL)
      {
      return false;
      }
  return thread_p->isAlive();
}

bool ThreadManagerBase::areAllAlive() {
// ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
//  GUARD;
  ACS_TRACE("ACS::ThreadManagerBase::areAlive");
  bool ok = true;
  for (int n=0; n < getThreadCount(); n++)
      {
      if (threads_m[n]->isAlive()==false)
	  {
	  ok = false;
	  }
      }
  return ok;
}


bool ThreadManagerBase::restartDead() {
// ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
//  GUARD;
  ACS_TRACE("ACS::ThreadManagerBase::restartDead");
  bool ok = true;
  ThreadBase* thread_p;
  for (int n=0; n < getThreadCount(); n++) {
	  thread_p = threads_m[n];
	  if (thread_p->isAlive()==true)
	      {
		  if (thread_p->restart()==false)
		      {
		      ok = false;
		      }
	      }
  }
  return ok;
}

int ThreadManagerBase::join(const ACE_thread_t& tid) {

  ACS_TRACE("ACS::ThreadManagerBase::join");
#ifndef MAKE_VXWORKS
  return threadManager_mp->join(tid);
#else
  ACE_UNUSED_ARG(tid);
#warning VxWorks does not support join!!
  return -1;
#endif
}//join

int ThreadManagerBase::join(const ThreadBase *th) {

  ACS_TRACE("ACS::ThreadManagerBase::join");
  return join(const_cast<ThreadBase*>(th)->getThreadID());
}//join

/////////////////////////////////////////////////
// ThreadSyncGuard
/////////////////////////////////////////////////

ThreadSyncGuard::ThreadSyncGuard(ACE_Recursive_Thread_Mutex * mutex_, bool block) {
        mutex_mp = mutex_;

        if (block==true)
        {
	  acquired_m = true;
	  mutex_mp->acquire();
        }

	//ThreadSyncGuardLock.release();
};

void ThreadSyncGuard::acquire() {
	if (acquired_m==false) {
		mutex_mp->acquire();
		acquired_m=true;
	}
};

void ThreadSyncGuard::release() {
	if (acquired_m==true) {
		mutex_mp->release();
		acquired_m=false;
	}
};


ThreadSyncGuard::~ThreadSyncGuard() {
	release();
}


