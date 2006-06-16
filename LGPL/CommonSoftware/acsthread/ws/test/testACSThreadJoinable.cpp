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
* "@(#) $Id: testACSThreadJoinable.cpp,v 1.6 2006/06/16 11:35:43 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2005-02-15  created
*/

#define NO_SHORT_LOG

#include "acsThreadManager.h"
#include "acsThread.h"
#include <logging.h>

#include "acsThreadTest.h"

static char *rcsId="@(#) $Id: testACSThreadJoinable.cpp,v 1.6 2006/06/16 11:35:43 bjeram Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

/**
   @file
   On my machine, this program will fail around 1510 threads.

   Besides, if I add "join" in acsThreaBase destructor , this program
   can run much longer ( more than 3500 threads till now ).
*/

#define MAX_THREADS 2000

/**
 * This is just a minimal thread class that executes an empty loop
 * at the specified sleepTime frequncy
 */
class FastACSThread :public ACS::Thread
{
  public:
    FastACSThread(const ACE_CString& name, 
		  const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime, 
		  const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime,
		  bool del=false
	) :
	ACS::Thread(name, responseTime, sleepTime, del)
	{
	    ACS_TRACE("FastACSThread::FastACSThread");
	}

    FastACSThread(const ACE_CString& name, 
		  const ACS::TimeInterval& responseTime, 
		  const ACS::TimeInterval& sleepTime,
		  bool del,
		  const long _thrFlags
	) :
	ACS::Thread(name, responseTime, sleepTime, del, _thrFlags)
	{
	    ACS_TRACE("FastACSThread::FastACSThread");
	}

    /**
     * This is the method executed in the thread loop.
     * It is just an empty method.
     */
    virtual void runLoop() 
	{ 
#ifndef MAKE_VXWORKS
	std::cout << pthread_self() << " runLoop()" << std::endl; 
#else
	std::cout << taskIdSelf() << " runLoop()" << std::endl; 	
#endif
	}
};

int main(int argc, char *argv[])
{
    /*
     * Unset the ACS_LOG_STDOUT environment variable,
     * to make sure only LM_INFO messages go on stdout.
     * The test tat environment would otherwise
     * set it to 0, slooding with messages.
     * Under VxWorks we will "unset" it from the script.
     */
#ifndef MAKE_VXWORKS
    unsetenv("ACS_LOG_STDOUT");
#endif

    LoggingProxy logger_m(0, 0, 31);
    LoggingProxy::init(&logger_m);

    ACS::ThreadManager tm;
    int l = 0;

    ACE_thread_t threadId;
    ACE_thread_t threadIdVec[MAX_THREADS];

   /*
     * In the first test we create and destroy MAX_THREADS0
     * DETACHED threads.
     * This should work without problems.
     */
    ACS_SHORT_LOG(( LM_INFO, "===== 1 - create DETACHED threads"));
    try 
	{
	for( l=0; l<MAX_THREADS; l++)
	    {
	    FastACSThread *a = tm.create<FastACSThread>("TestThreadA",
							5000000, /* 500ms */
							1000000, /* 100ms */
							false,
							THR_NEW_LWP | THR_DETACHED);
	    // ACS_SHORT_LOG(( LM_INFO, "Progress: %ld thread created", l+1 ));
            a->resume();
	    threadId = a->getThreadID();
	    tm.destroy(a);
	    }
	}
    catch(acsthreadErrType::CanNotCreateThreadExImpl) 
	{
	ACS_SHORT_LOG(( LM_INFO, "acsthreadErrType::CanNotCreateThreadExImpl catched. Counter is: %ld", l ));
	}
    catch(...) 
	{
	ACS_SHORT_LOG(( LM_INFO, "UNKNOWN exception catched, program stop" ));
	}
    ACS_SHORT_LOG(( LM_INFO, "Created threads: %ld", l ));

    /*
     * In the second test we TRY to create and destroy MAX_THREADS
     * JOINABLE threads, but we do not join them.
     * This should fail with an exception when resources are exausted.
     */
    ACS_SHORT_LOG(( LM_INFO, "===== 2 - create JOINABLE threads"));
    try 
	{
	for( l=0; l<MAX_THREADS ; l++)
	    {
	    FastACSThread *a = tm.create<FastACSThread>("TestThreadA",
							5000000, /* 500ms */
							1000000, /* 100ms */
							false,
							THR_NEW_LWP | THR_JOINABLE);
            a->resume();
	    // ACS_SHORT_LOG(( LM_INFO, "Progress: %ld thread created", l+1 ));
	    threadIdVec[l] = a->getThreadID();
	    tm.destroy(a);
	    }
	}
    catch(acsthreadErrType::CanNotCreateThreadExImpl) 
	{
	ACS_SHORT_LOG(( LM_INFO, "acsthreadErrType::CanNotCreateThreadExImpl catched. Counter is: %ld", l ));
	}
    catch(...) 
	{
	ACS_SHORT_LOG(( LM_INFO, "UNKNOWN exception catched, program stop" ));
	}
     
    /*
     * Then we try to join all threads we had managed to create.
     */
    /***********************************************************
     * GCH
     * With this join, the test sefgault exiting at the end.
     * If I comment it out, it does not segfault.
     * This is very strange, but I cannot understand why!!!!
     ***********************************************************/
    int totalCreated = l;
    ACS_SHORT_LOG(( LM_INFO, "===== 3 - join the threads"));
    for( l=0; l<totalCreated; l++)
	{
	ACE_thread_t tid = threadIdVec[l];
	ACE_Thread::join(tid);
	}

    /*
     * In the second test we TRY to create and destroy MAX_THREADS
     * JOINABLE threads, but we join them.
     * This should not fail.
     */
    ACS_SHORT_LOG(( LM_INFO, "===== 4 - create, destroy and JOIN threads"));
    try 
	{
	for( l=0; l<MAX_THREADS; l++)
	    {
	    FastACSThread *a = tm.create<FastACSThread>("TestThreadA",
							5000000, /* 500ms */
							1000000, /* 100ms */
							false,
							THR_NEW_LWP | THR_JOINABLE);
            a->resume();
	    // ACS_SHORT_LOG(( LM_INFO, "Progress: %ld thread created", l+1 ));
	    threadId = a->getThreadID();
	    tm.destroy(a);
	    ACE_Thread::join(threadId);
	    }
	}
    catch(acsthreadErrType::CanNotCreateThreadExImpl) 
	{
	ACS_SHORT_LOG(( LM_INFO, "acsthreadErrType::CanNotCreateThreadExImpl catched. Counter is: %ld", l ));
	}
    catch(...) 
	{
	ACS_SHORT_LOG(( LM_INFO, "UNKNOWN exception catched, program stop" ));
	}

    sleep(5);
    ACS_SHORT_LOG(( LM_INFO, "Done" ));
    return 0;

}













