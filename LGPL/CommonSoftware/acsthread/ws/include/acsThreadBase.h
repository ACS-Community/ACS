
#ifndef acsThreadBase_h
#define acsThreadBase_h

/*******************************************************************************
 * ALMA - Atacama Large Millimiter Array
 * (c) European Southern Observatory, 2004
 *
 *This library is free software; you can redistribute it and/or
 *modify it under the terms of the GNU Lesser General Public
 *License as published by the Free Software Foundation; either
 *version 2.1 of the License, or (at your option) any later version.
 *
 *This library is distributed in the hope that it will be useful,
 *but WITHOUT ANY WARRANTY; without even the implied warranty of
 *MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *Lesser General Public License for more details.
 *
 *You should have received a copy of the GNU Lesser General Public
 *License along with this library; if not, write to the Free Software
 *Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *
 * "@(#) $Id: acsThreadBase.h,v 1.37 2012/02/29 12:50:09 tstaig Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * almamgr   2000-12-03  created
 */

/**
 * @file
 * Header file for ThreadBase former BACI Thread.
 */

#include <acsutil.h>
#include <acsutilThreadInit.h>

#include <map>
#include <vector>

#include <ace/Thread.h>
#include <ace/Thread_Manager.h>
#include <ace/Synch.h>

#include <acscommonC.h>
#include <ace/SString.h>
#include <logging.h>
#include <loggingLoggable.h>

#include "acsthreadErrType.h"
#include "acsThreadExport.h"

namespace ACS {

    /**
     * @deprecated  Use getTime() from acsutilTimeStamp.h instead.
     * @todo Remove this deprecated function with ACS 5.1 or 6.0
     */
    TimeInterval getTime();

/**
 * @class ThreadBase
 *
 * @brief Provides a ACS aware wrapper for ACE threads.
 *
 * This class provides a minimum interface to ACE threads.
 * Note, however, that it is
 * generally a better idea to use the <ThreadManagerBase>
 * programming API rather than the <ThreadBase> API since the
 * thread manager is more powerful.
 * An example of thread function implementation performin a loop:
 * <pre>
 *
 *   static void worker (void* param)
 *      {
 *              // pointer to ThreadBaseParameter object is passed as parameter
 *              // it contains pointer to ThreadBase (self) object and optional extra parameter
 * 		ThreadBase* myself = ((ThreadBaseParameter*)param)->threadBase;
 *
 *              // initialize thread
 *              if (ThreadBase::InitThread) ThreadBase::InitThread(myself->getName());
 *
 *              // enter the loop
 *              // check() method ACK heartbeat and returns true unless there is an exit request
 * 		while (myself->check())
 *              {
 *                 // simulate suspend() for systems not supporting it
 * 	           if (!myself->isSuspended())
 * 		   {
 * 		        // do something meaningful here
 * 		   }
 *
 *                 // sleep for default thread sleep time
 * 		   myself->sleep();
 * 		}
 *
 *              // ACK exit state
 * 		myself->setStopped();
 *
 *              // cleanup thread
 *              if (ThreadBase::DoneThread) ThreadBase::DoneThread();
 * 	}
 * </pre>
 */

    class acsThread_EXPORT ThreadBase : public Logging::Loggable
    {
      public:

	/**
	 * Default heartbeat response time in 100ns unit.
	 * A thread is responsive if the check() method is called
	 * at most every defaultResponseTime 100ns.
	 * Responsiveness can be verified by calling
	 * isResponding().
	 * @see ThreadBase#check
	 * @see ThreadBase#isResponsive
	 */
	static TimeInterval defaultResponseTime;

	/**
	 * Default default sleep time in 100ns unit.
	 * Defines the time the thread will sleep if sleep() is
	 * called without any parameter.
	 * It is typically used when implementing a periodic loop
	 * in the thread to define the time the thread shall sleep between
	 * two iterations of the loop.
	 */
	static TimeInterval defaultSleepTime;

	/// Pointer to NULL ThreadBase object.
	static ThreadBase * NullThreadBase;

	/// External thread initialization function.
	static InitThreadFunc InitThread;

	/// External thread cleanup function.
	static DoneThreadFunc DoneThread;

      public:

	/**
	 * Return values from sleep() method.
	 * SLEEP_OK means that the sleep() completed in the requested time
	 * SLEEP_SUSPEND that the thread was suspended during the sleep and then resumed.
	 *               The actual sleep time can be therefore shorter or longer than the
	 *               value requested
	 * SLEEP_INTERRUPTED that the sleep has been interrupted, for example by a cancel() request.
	 *                   The sleep time has been therefore shorter than requested.
	 * SLEEP_ERROR   an unexpected error occurred. See error logs.
	 */
	enum SleepReturn {
	    SLEEP_OK=0,
	    SLEEP_SUSPEND,
	    SLEEP_INTERRUPTED,
	    SLEEP_ERROR
	};

	/**
	 * Constructor.
	 * Creates a new thread and starts it automatically.
	 * @param _name name of the thread
	 * @param _threadManager reference to the acsthreadManagerBase that will manage the thread
	 * @param _threadProcedure thread worker function. Signature shall be compatible with ACE_THR_FUNC
	 * @param _parameter an optional parameter to the thread
	 * @param _responseTime heartbeat response time in 100ns unit
	 * @param _sleepTime default sleep time in 100ns unit
	 * @param _create should the thread be created in the constructoror not.
	 *                Default is yes. It is used if constructor is called from a subclass.
	 *                and this will take care of creating the thread or if we want
	 *                to call create ourselves.
	 * @param _thrFlags Thread creation flags to be used if _create = TRUE.
	 *        See ACE threads and pthread documentation for details.
	 *        Default is now THR_NEW_LWP | THR_DETACHED
	 * @param _stackSize The size of the stack of the thread. If ACE_DEFAULT_THREAD_STACKSIZE the default
	 * 		stack size is taken.
	 *
	 * @see the create method for important details about joinable threads.
	 */
	ThreadBase(const ACE_CString& _name, ACE_Thread_Manager* _threadManager,
		   void* _threadProcedure, void* _parameter,
		   const TimeInterval& _responseTime=ThreadBase::defaultResponseTime,
		   const TimeInterval& _sleepTime=ThreadBase::defaultSleepTime,
		   const bool _create=true,
		   const long _thrFlags= THR_NEW_LWP | THR_DETACHED,
		   const size_t _stackSize=ACE_DEFAULT_THREAD_STACKSIZE);

	/**
	 * Destructor.
	 * Also terminates the thread.
	 * @attention
	 * Always call terminate() in the destructor of user implemented
	 * ThreadBase classes.<br>
	 * If the user thread service function uses resources of the Thread
	 * object (like accessing member variables) we have to be sure
	 * that it is terminated before the object itself is destroyed.
	 * This is job of terminate().
	 * @see ThreadBase#terminate
	 */
	virtual ~ThreadBase();

	/**
	 * Set external thread initialization and cleanup functions for all threads
	 * This allows users of the thread library to define
	 * what initialisation and thread functions will have to be called, to
	 * customize the behavior of threads based on the rest of the infrastructure.
	 * For example, ACS Containers use setInitialisers to control
	 * initialisatoin and cleanup of the logging system for each thread.
	 *
	 * @param InitThread_ thread initialization function
	 * @param DoneThread_ thread cleanup function
	 */
	static void setInitializers(InitThreadFunc InitThread_, DoneThreadFunc DoneThread_) {
	    InitThread=InitThread_;
	    DoneThread=DoneThread_;
	}

	/**
	 * Get name of the thread.
	 * @return name of the thread
	 */
	ACE_CString getName() const { return name_m; }

	/**
	 * Get thread worker function.
	 * @return function pointer to the thread worker function.
	 *         Signature shall be compatible with ACE_THR_FUNC
	 */
	void* getThreadProcedure() const { return threadProcedure_mp; }

	/**
	 * Get heartbeat response time in 100ns unit.
	 * @return heartbeat response time in 100ns unit
	 * @see ThreadBase#isResponding
	 */
	TimeInterval getResponseTime() const { return responseTime_m; }

	/**
	 * Set heartbeat response time in 100ns unit.
	 * @param _responseTime heartbeat response time in 100ns unit
	 * @see ThreadBase#isResponding
	 */
	void setResponseTime(const TimeInterval& _responseTime) { responseTime_m=_responseTime; };

	/**
	 * Get default sleep time in 100ns unit.
	 * @return default sleep time in 100ns unit
	 * @see ThreadBase#sleep
	 */
	TimeInterval getSleepTime() const { return sleepTime_m; }

	/**
	 * Set default sleep time in 100ns unit.
	 * @param _sleepTime default sleep time in 100ns unit
	 * @see ThreadBase#sleep
	 */
	void setSleepTime(const TimeInterval& _sleepTime) {
	    sleepTime_m=_sleepTime;
	};

	/**
	 * Set thread priority.
	 * @param _priority (OS dependent)
	 */
	void setPriority(int _priority);

	/**
	 * Get thread priority.
	 * @return priority (OS dependent)
	 */
	int getPriority();

	/**
	 * Suspend the execution of a particular thread.
	 * If the suspend function is not supported by the underlying thread implementation,
	 * the suspend is simulated.<br>
	 * See example of usage.
	 * @return true if operation was successful
	 */
	bool suspend();

	/**
	 * Continue the execution of a previously suspended thread.
	 * @return true if operation was successful
	 */
	virtual bool resume();

	/**
	 * Check if thread is already suspended.
	 * @return true if thread is suspended
	 */
	bool isSuspended() const { return suspendStatus_m != 0; }

	/**
	 * Check if thread is already stopped.
	 * @return true if thread is stoped
	 */
	bool isStopped() const { return stopped_m; }

	/**
	 * Notify thread to exit thread worker function.
	 * Thread worker function should always exit when this notification was issued.
	 * See example of usage.
	 */
	virtual void exit() { exitRequest_m=true; }

	/**
	 * Has thread already received an exit request
	 * @return true if thread has already received an exit request
	 */
	bool exitRequested() const { return exitRequest_m; }

	/**
	 * Set thread state as stopped.
	 * This function should be called in the thread worker function.
	 * See example of usage.
	 */
	void setStopped() { stopped_m=true; }
	// !!! this can be done by ThreadBase function automatically - using wrapper method

	/**
	 * Stop the thread.
	 * Stopping means notifying thread by calling exit() method and then waiting
	 * for a while thread to stop, i.e. checking until thread gets in stopped state
	 * If after some time the thread is not exited, the method timeouts
	 * and returns an error
	 * See example of usage.
	 * @param terminating , set to true when called by terminate(), for control the "did not stop.." message
	 * @return true if thread has been stopped
	 */
	bool stop( bool terminating = false );


	/**
	 * Cancel (forceful termination) named thread.
	 * It tries to immediately stop the thread, without waiting for the thread to gracefully
	 * exit.
	 * But also this can fail is the thread never goes into a sleep or suspend function.
	 * Therefore is the thread is still running after a ceetain time, the function
	 * timeouts and return an error.
	 * Avoid using this kind of thread termination.
	 *
	 * @return true if thread has been stopped
	 * @see ThreadBase#terminate
	 */
	bool cancel();

	/**
	 * Terminate the thread.
	 * Terminating means calling stop() method and if even then the thread does not
	 * stop, then the cancel() method is called.
	 * @return true if thread has been stopped
	 * @see ThreadBase#terminate
	 * @see ThreadBase#create
	 */
	bool terminate();

	/**
	 * Restart the thread.
	 * Restarting means terminate and
	 * recreate a new thread (i.e. calling terminate() and create() methods).
	 * @see ThreadBase#terminate
	 * @see ThreadBase#create
	 */
	bool restart();

	/**
	 * Update last heartbeat time.
	 * To be used inside the thread function.
	 * It is called in check() and can be called by the thread developer
	 * to provide an "intermediate checkpoint" if check()
	 * cannot be called at the desired frequency.
	 */
	void makeTimeInterval();

	/**
	 * Checks if named thread is alive (has heartbeat).
	 * Having a hearbeat means that <ThreadBase::check()> was called
	 * within last <ThreadBase::defaultResponseTime> time.
	 * @param name name of the thread
	 * @return true if named thread is alive (has heartbeat)
	 * @see ThreadBase#getDefaultResponseTime
	 */
	bool isResponding() const;

	/**
	 * Checks if named thread is alive (not terminated/stopped).
	 * @return true if named thread is alive
	 */
	bool isAlive() const { return !stopped_m; }

	/**
	 * Check the state of thread and update heartbeat.
	 * This method is meant to a <pre>while(threadBase->check()) {}</pre> condition,
	 * automatically updating heartbeat and checking for exit status of the thread.
	 * The developer of a thread is responsible for calling check()
	 * at least at heartbeat time intervals.
	 * This is very important to ensure that we can effectively stop
	 * a thread.
	 * See example of usage.
	 * @return true unless a thread has received an exit request
	 */
	bool check();

	/**
	 * Sleep for given amount of time in 100ns units.
	 * If 0 time is given, then default sleep time is used.
	 *
	 * This method shall be used INSIDE a thread service function
	 * and not outside.
	 *
	 * It takes care of handling suspend conditions and of waking
	 * up the thread if requested, as if a signal was sent.
	 * Instead of an operating system sleep, this method
	 * is implemented trying to acquire a busy semaphore
	 * with a timeout equal to the sleep time.
	 * This allows to "interrupt the sleep" releasing the semaphore
	 * from another thread.
	 *
	 * Since the sleep can be interrupted, the user shall always
	 * call check() after returning from sleep() to verify
	 * is the sleep simply completed or was interrupted by a request
	 * to resume or exit.
	 *
	 * @param timeIn100ns time to sleep in 100ns unit.
	 * @return The reason for returning from the sleep method.
	 *
	 */
	SleepReturn sleep(TimeInterval timeIn100ns = 0) const;


	/**
	   Returns ACE specific thread ID of the base thread
	*/
	ACE_thread_t getThreadID() { return  threadID_m; }

      protected:
	/**
	 * Create a thread.
	 * @param thrFlags_ what kind of thread should be created.
	 * Default is kernel space thread (THR_NEW_LWP) detached thread (THR_DETACHED)
	 * For a list of the available thread flags, see the documentation for
	 * the underlying ACE Threads
	 * anr/or the Linux documentation for the behavior of ACE Threads under Linux.
	 *
	 * @attention A joinable thread must be joined, or we loose system resources.
	 * If a joinable thread is not joined after completion, some system resources
	 * will remain allocated (see pthread documentation).
	 * After the creation of a few hundred threads, the system will be unable
	 * to allocate more threads.
	 */
	bool create(const long _thrFlags= THR_NEW_LWP | THR_DETACHED);

	/**
	 * Yield the thread to another another ready-to-run, active thread.
	 * This method shall (and can) be called just from inside the thread,
	 * because we can yield just from actaul/current thread! We can not ask another thread to yield!
	 */
	virtual void yield();

      private:

	/// thread worker function. Signature shall be compatible with ACE_THR_FUNC
	void* threadProcedure_mp;

	/// thread optional parameter.
	const void* parameter_mp;

	/// heartbeat response time in 100ns unit.
	TimeInterval responseTime_m;

	/// thread default sleep time in 100ns unit.
	TimeInterval sleepTime_m;

	/// thread last heartbeat time in 100ns unit.
	TimeInterval timeStamp_m;

	/// true if thread suspended, false otherwise.
	volatile int suspendStatus_m;

	/// is thread received exit request?
	volatile bool exitRequest_m;

	/// is thread stopped ?
	volatile bool stopped_m;

	/// name of the thread
	ACE_CString name_m;

	// ACE speciic
	/// thread ID
	ACE_thread_t threadID_m;

	/// Thread flags used in create, to be reused for restart.
	long thrFlags_m;

	/// Thread stack size.
	size_t stackSize_m;

	/// thread manager
	ACE_Thread_Manager * threadManager_mp;

	/// semaphore used for running loop
	mutable ACE_Thread_Semaphore m_suspendSemaphore;
	mutable ACE_Semaphore m_sleepSemaphore;
    };



/// thread <map> typedef
    typedef std::map<ACE_CString, ThreadBase*> ThreadMap;

// fwd
/**
 * @todo GCH
 * Why a forward declaration for the Thread class here?
 * Thread Base should be sufficient !!!!!
 * Try to remove it and see what happens.
 */
//    class Thread;


/**
 * @class ThreadManagerBase
 *
 * @brief Manages a pool of ACS Base threads.
 */
    class ThreadManagerBase
    {
	/**
	 * @todo GCH
	 * Why a friend declaration for the Thread class here?
	 * Thread Base should be sufficient !!!!!
	 * Try to remove it and see what happens.
	 */
//    friend class ACS::Thread;

      public:

	/**
	 * Constructor.
	 */
	ThreadManagerBase()
	    {
		threadManager_mp = ACE_Thread_Manager::instance();
	    }

	/**
	 * Destructor.
	 * Also terminates and deletes all threads.
	 */
	~ThreadManagerBase();

	/**
	 * Get number of threads in the pool.
	 * @return number of threads in the pool
	 */
	int getThreadCount() const {return threads_m.size();}

	/**
	 * Get name of the thread at the specified position.
	 * @param pos position of the thread
	 * @return name of the thread
	 */
	ACE_CString getThreadName(const int pos) const { return threads_m[pos]->getName(); }


	/**
	 * Get thread at the specified position.
	 * @param pos position of the thread
	 * @return pointer to ThreadBase object
	 */
	ThreadBase* getThreadAt(const int pos) const { return static_cast<ThreadBase*>(threads_m[pos]); }


	/**
	 * Get thread with specified named.
	 * @param name name of the thread
	 * @return pointer to ThreadBase object, NULL otherwise
	 */
	ThreadBase* getThreadByName(const ACE_CString& name) {
	    ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);

	    ThreadMap::iterator i = threadMap_m.find(name);
	    if (i!=threadMap_m.end())
		return static_cast<ThreadBase*>((*i).second);
	    else
		return NULL;
	}

	/**
	 * Get thread with specified ID.
	 * @param id ID of the thread
	 * @return pointer to ThreadBase object, NULL otherwise
	 */
	ThreadBase* getThreadByID(ACE_thread_t id)
	    {

		for(unsigned int i=0UL; i < threads_m.size(); i++)
		    if (threads_m[i]->getThreadID() == id)
			return threads_m[i];
		return NULL;
	    }


	/**
	 * Create a new thread and add it to the pool.
	 * The thread is immediately created (create=true) and
	 * suspended
	 * @param name of the thread
	 * @param threadProc thread worker function
	 * @param parameter an optional parameter to the thread
	 * @param responseTime heartbeat response time in 100ns unit
	 * @param sleepTime default sleep time in 100ns unit
	 * @param _thrFlags Thread creation flags to be used if _create = TRUE.
	 *        See ACE threads and pthread documentation for details.
	 *        Default is now THR_NEW_LWP | THR_DETACHED
	 *
	 * @return pointer to ThreadBase object, NULL otherwise
	 * @see ThreadBase
	 */
	ThreadBase * create(const ACE_CString& name, void * threadProc, void * parameter,
			    const TimeInterval& responseTime=ThreadBase::defaultResponseTime,
			    const TimeInterval& sleepTime=ThreadBase::defaultSleepTime,
			    const long _thrFlags= THR_NEW_LWP | THR_DETACHED,
			    const size_t _stackSize=ACE_DEFAULT_THREAD_STACKSIZE);

	/**
	 * Add a thread to the ThreadBaseManger thread pool.
	 * <b>WARNING: after this operation ThreadManagerBase owns the pointer of ThreadBase.
	 * ThreadBase object is deleted on destruction of ThreadManagerBase object.
	 * Therefore the given ThreadBase must have been allocated on the heap</b>
	 * @param name name of the thread
	 * @param ThreadBase pointer to the thread
	 */
	bool add(const ACE_CString& name, ThreadBase * acsBaseThread);

	/**
	 * Stop named thread.
	 * @param name name of the thread
	 * @return true if thread has been stopped
	 * @see ThreadBase#stop
	 */
	bool stop(const ACE_CString& name);

	/**
	 * Stop all the threads_m.
	 * @return true if thread has been stopped
	 * @see ThreadManagerBase#stop
	 */
	bool stopAll();

	/**
	 * Notify named thread to exit thread worker function.
	 * @param name name of the thread
	 * @see ThreadBase#exit
	 */
	void exit(const ACE_CString& name);

	/**
	 * Notify all the threads to exit thread worker function.
	 * @see ThreadManagerBase#exit
	 */
	void exitAll();

	/**
	 * Cancel (forceful termination) named thread.
	 * Avoid using this kind of thread termination.
	 * @param name name of the thread
	 * @see ThreadBase#cancel
	 * @see ThreadManagerBase#terminate
	 */
	bool cancel(const ACE_CString& name);

	/**
	 * Cancel (forceful termination) all threads.
	 * @see ThreadManagerBase#cancel
	 */
	bool cancelAll();

	/**
	 * Terminate named thread.
	 * @param name name of the thread
	 * @see ThreadBase#terminate
	 */
	bool terminate(const ACE_CString& name);

	/**
	 * Terminate all threads.
	 * @see ThreadManagerBase#terminate
	 */
	bool terminateAll();

	/**
	 * Restart named thread.
	 * Restarting means treminate and recreate a new thread.
	 * @param name name of the thread
	 * @see ThreadBase#restart
	 */
	bool restart(const ACE_CString& name);

	/**
	 * Restart all threads.
	 * @see ThreadManagerBase#restart
	 */
	bool restartAll();

	/**
	 * Restart all dead threads, i.e. all terminated threads
	 * @see ThreadManagerBase#restart
	 * @see ThreadManagerBase#isAlive
	 */
	bool restartDead();

	/**
	 * Suspend the execution of a particular thread.
	 * @param name name of the thread
	 * @see ThreadBase#suspend
	 */
	bool suspend(const ACE_CString& name);

	/**
	 * Suspend the execution of all running threads.
	 * @see ThreadManagerBase#suspend
	 */
	bool suspendAll();

	/**
	 * Continue the execution of a previously suspended thread.
	 * @param name name of the thread
	 * @see ThreadBase#resume
	 */
	bool resume(const ACE_CString& name);

	/**
	 * Continue the execution of all suspended threads.
	 * @see ThreadManagerBase#resume
	 */
	bool resumeAll();

	/**
	 * Checks if named thread is alive (not terminated).
	 * @param name name of the thread
	 * @return true if named thread is alive
	 * @see ThreadBase#isAlive
	 */
	bool isAlive(const ACE_CString& name);

	/**
	 * Checks if all threads are alive (not terminated).
	 * @param name name of the thread
	 * @return true if all threads are alive
	 * @see ThreadManagerBase#isAlive
	 */
	bool areAllAlive();

	/**
	 * join joinable thread
	 * @param tid  thread id
	 * @return -1 if fail
	 */
	int join(const ACE_thread_t& tid);

	/**
     *join joinable thread
	 * @param pointer to thread
	 * @return -1 if fail
	 */
	int join(const ThreadBase *th);

	/**
	 * Returns pointer to the ACE Thread Manager
	 * @return pointer to  ACE_Thread_Manager object
	 */
	ACE_Thread_Manager* getACEThreadManager() { return threadManager_mp; }

      protected:

	/**
	 * Add a thread to the ThreadManagerBase data store.
	 * @param name name of the thread
	 * @param thread pointer to the thread
	 */
	void add2map(const ACE_CString& name, ThreadBase* thread)
	    {

		threadMap_m[name]=thread;
		threads_m.push_back(thread);
	    }

	void removeFromMap(const ACE_CString& name)
	    {
		ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);

		ThreadMap::iterator i = threadMap_m.find(name);
		if (i!=threadMap_m.end())
		    {
		    for (std::vector<ThreadBase*>::iterator thr = threads_m.begin(); thr!=threads_m.end(); ++thr)
			{
			if (static_cast<ThreadBase*>((*i).second) == *thr)
			    {
			    threads_m.erase(thr);
			    break;
			    }//if
			}//for
		    }//if
		threadMap_m.erase(name);
	    }//removeFromMap

      protected:
	/// mutex used by add and remove method
	ACE_Recursive_Thread_Mutex m_addRemoveMutex;

      private:

	/// pointer to ACE_Threads_Manager
	ACE_Thread_Manager * threadManager_mp;

	/// Thread name-thread map data store
	ThreadMap threadMap_m;

	/// Vector of all threads
	std::vector<ThreadBase*> threads_m;

	/**
	 * ALMA C++ coding standards state assignment operators should be disabled.
	 */
	void operator=(const ThreadManagerBase&);

	/**
	 * ALMA C++ coding standards state copy constructors should be disabled.
	 */
	ThreadManagerBase(const ThreadManagerBase&);

    };

/**
 * @class ThreadBaseParameter
 *
 * @brief ThreadBase parameter type definition.
 *
 * ThreadBase thread parameter contains pointer to ThreadBase (self) object and optional extra parameter.
 */
    class ThreadBaseParameter {

      public:

	/**
	 * Constructor.
	 * @param thread reference to ThreadBase (self) object
	 * @param parameter void pointer to an extra parameter
	 */
	ThreadBaseParameter(ThreadBase * thread,
			    const void * parameter = 0) :
	    thread_mp(thread),  parameter_mp(parameter) {}

	/**
	 * Accessor method to an optional extra parameter.
	 * @return void pointer to an optional extra parameter
	 */
	const void * getParameter() const { return parameter_mp; }

	/**
	 * Accessor method to ThreadBase (self) object.
	 * @return pointer to ThreadBase (self) object
	 */
	ThreadBase * getThreadBase() const { return thread_mp; }

	/**
	 * This function is equivalent to ThreadBaseParameter#getThreadBase
	 * and it is here just for backward compatibility reason.
	 */
//    ThreadBase * getBACIThread() const { return getThreadBase(); }
      private:

	/// pointer to ThreadBase (self) object
	ThreadBase * thread_mp;

	/// void pointer to an optional extra parameter
	const void * parameter_mp;
    };


/**
 * @class ThreadSyncGuard
 *
 * @brief Class implementing scope locking synchronization pattern.
 *
 * ThreadSyncGuard implements scope locking synchronization pattern,
 * which is very usefull not to forget to release (or relase can be by-passed by an exception)
 * and so avoiding deadlocks.
 * Mutex is automatically released when out of scope.<br>
 * ThreadSyncGuard uses recursive thread mutex.
 * An example of usage:
 * <pre>
 *      void PowerSupplyImpl::shutdown()
 *      {
 *		//ThreadSyncGuard guard("gizmo0::propertyVector");
 *		ThreadSyncGuard guard(mutex_m);
 *
 *		// do something
 *
 *		// optional lock release
 *		guard.release();
 *
 *		// do something
 *
 *		// optional locking
 *		guard.acquire();
 *
 *		// do something
 *
 *		// lock is automatically released when out of scope
 *
 *      }
 * </pre>
 */
    class ThreadSyncGuard
    {
      public:

	/**
	 * Constructor.
	 * Creates a guard and acquires mutex if <block> is true.
	 * @param mutex mutex to be used by guard
	 * @param block if true, lock is acuired, otherwise not
	 */
	ThreadSyncGuard(ACE_Recursive_Thread_Mutex * mutex, bool block=true);

	/**
	 * Destructor.
	 * Relases mutex if necessary.
	 */
	~ThreadSyncGuard();

	/**
	 * Blocks the thread until the lock is acquired.
	 */
	void acquire();

	/**
	 * Releases the lock.
	 */
	void release();

	/// mutex used by guard
	ACE_Recursive_Thread_Mutex * mutex_mp;

	/// state of mutex
	bool acquired_m;
    };

}; //namespace ACS

#endif





