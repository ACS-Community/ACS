#ifndef _ACS_THREAD_H
#define _ACS_THREAD_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2005
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
* "@(#) $Id: acsThread.h,v 1.32 2009/08/28 09:53:54 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2005-02-15  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif


#include "acsThreadBase.h"
#include <logging.h>

namespace ACS
{
    class ThreadManager;


    /**
       Thread class is a base class for the ACS based threads.
       If a user wants to define his/her own thread,
       it has to create a class which derives from ACS::hread class
       and the override run or runLoop methods.

       The Thread class is ment to be used with the support
       of a ThreaManager.

       It is possible to use it without a ThreadManager, but
       you have to be careful and read accurately this documentation
       to be aware of things you have to be careful with.
    */
    class Thread : public ACS::ThreadBase
    {
	friend class ThreadManager;

      public:
	/**
	   Constructor of the Thread class.
	   If someone wants to set thread parameters, s/he has to send these
	   parameters from the constructor of the derived class up to the base class.
       @throw acsthreadErrType::CanNotSpawnThreadExImpl
	   @param name thread's name
	   @param responsTime thread's heartbeat response time in 100ns unit.
	   Default value is 1s.
	   @param sleepTime thread's sleep time in 100ns unit Default value is 100ms.
	   @param del should the thread object be deleted automatically
           after the thread exits.<br>
	   Default is not (=false).<br>
	   If the thread object has to be deleted after the thread exits (i.e. del=true),
	   a detached instead of a joinable thread is created.<br>
           @attention This means that the thread will call 'delete this' after the
	   thread function execution is completed.<br>
           This can work only for threads
           allocated on the heap and the user has to be careful not to use
           the pointer after thread execution is started, because the object could be
           deleted at any time.<br>
	   A ThreadManager always creates a thread with new (on a heap) !!!
           It is not possible to call restart() on auto delete threads,
           because the thread object would be deleted when the thread is stopped.
	   @attention all of thread will be
           suspended.<br>
           This has been done to protect the code from creation concurrency problems.<br>
           If we would start execution of the thread service function in the
	   constructor of the Thread class, the user thread function
	   would be potentially executed BEFORE the object is fully constructed
           and data members and virtual tables are allocated and initialised.
           Therefore we have decided to always suspend the thread.
           The developer of a subclass can use the parameter to resume()
	   the thread at the end of its own constructor, when everything has been
	   allocated and initialised or can choose to do it afterwards.<br>
           The ThreadManager does exactly this:
	   <ul>
	      <li> creates the Thread object suspended
              <li> if was to be not-suspended calls release() after construction
	   </ul>
	   @attention autodelete threads are created as non joinable, but
	   all other threads are created joinable and the developer has to
	   take care of joining them, otherwise resourses will be lost
	   and it will be possible to allocate only a few
	   hundred tests before crashing the machine

           2006/07/06 merge two constructor, since there is almost no difference between them

		@param _stackSize The size of the stack of the thread. If ACE_DEFAULT_THREAD_STACKSIZE the default ACE stack size is taken.

	 */
	Thread(const ACE_CString & name,
	       const TimeInterval& responseTime=ThreadBase::defaultResponseTime,
	       const TimeInterval& sleepTime=ThreadBase::defaultSleepTime,
	       const bool del=false,
	       const long _thrFlags=THR_NEW_LWP | THR_DETACHED,
	       const size_t stackSize = ACE_DEFAULT_THREAD_STACKSIZE);


	/*
	 * This additional constructor allows to explicitly pass
	 * thread creation flags.
	 * In this case all other parameters have to be passed
	 * as well and we do not provide default values.
	 * @param _thrFlags Thread creation flags to be used if _create = TRUE.
	 *        See ACE threads and pthread documentation for details.
         *  2006/07/06 merge two constructor, since there is almost no difference between them
 	 */
//	Thread(const ACE_CString & name,
//	       const TimeInterval& responseTime,
//	       const TimeInterval& sleepTime,
//	       const bool del,
//	       const long _thrFlags);

	/**
	   Thread destructor.
	 */
	virtual ~Thread();

	/**
	   Method which is executed before the body of the thread.
	   It is the hook method for a user that wants to do some initialization
	   before the thread execution (thread initialization),
	   s/he has to override/implement this method in the implementation class.
	   It is called by #commonStart
	 */
	virtual void onStart(){};

	/**
	   Method which is executed after the body of the thread, i.e.
	   after the thread execution.
	   It is the hook method for a user that wants to do some cleanup after the thread
	   execution (thread cleanup),
	   s/he has to override/implement this method in the implementation class.
	   It is called by #commonStop
	*/
	virtual void onStop(){};

	/**
	   This method performs basic thread initialization, common
	   to all threads.
	   It is called by #threadSvc and call the user hook #onStart.
	   The common initialization performs the initialization of the
	   logging system, which has to be done on a per thread base.
	   It shall not be overridden unless it is really necessary
	   (for example. user does not like what is done for common initialization).
	   In most cases if should be sufficient to override of #onStart.
	*/
	virtual void commonStart();

	/**
	   This method performs basic thread cleanup, common
	   to all threads.
	   It is called by #threadSvc and call the user hook #onStop.
	   The common cleanup performs the cleanup of the
	   logging system, which has to be done on a per thread base.
	   It shall not be overridden unless it is really necessary
	   (for example. user does not like what is done for common cleanup).
	   In most cases if should be sufficient to override of #onStop.
	 */
	virtual void commonStop();

	/**
	   Basic used hook to implement the thread functionality.
	   The provided default implementation executes
           the method #runLoop in a loop, to provide periodic loops.
	   If a user wants that his/her thread function is executed
           periodically, s/he has to override the #runLoop.
	   Between two iterations the #runLoop sleeps
	   for #responseTime 100ns
	   If a user wants that his/her thread function is executed
           only once, s/he has to override this method.
	*/
	virtual void run();

	/**
	   Basic used hook to implement the thread functionality in
           a periodic loop.
	   The method is executed in a loop until the thread is alive
	   and every #responseTime 100ns by the #run method.
	   The thread can be exited from the lop by calling ACS::ThreadBase::stop
	   or ACS::ThreadBase::exit command.
	   If the #run method is overridden than runLoop is not executed.
	*/
	virtual void runLoop(){}

      protected:

	/**
	   Static method which is passed to the ACE thread manager
	   which than spawns the thread.
           This is the actual thread service function.
	   It executes:
	      - commonStart() (which by default implementation executes onStart()),
	      - run()         (which by default implementation executes runLoop()),
	      - commonStop()  (which by default implementation executes onStop()).
	*/
	static  void threadSvc(void *param);

	/**
	   Returns pointer to the thread manager responsible for this thread.
	*/
	ACS::ThreadManager*  getThreadManager() const;

	LoggingProxy *logger_mp; ///< ponter to Logging proxy

	ACS::ThreadManager* thrMgr_mp; ///< Pointer to thread manager.
      private:

	/**
	 * ALMA C++ coding standards state assignment operators should be disabled.
	 */
	void operator=(const Thread&);

	/**
	 * ALMA C++ coding standards state copy constructors should be disabled.
	 */
	Thread(const Thread&);

	/**
	 * flag that indicates if thread should be deleted after the thread exits
	 */
	bool delete_m;

    };//class ACS::Thread

};//namespace ACS

#endif
