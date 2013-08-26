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
* "@(#) $Id: testACSThreadGuard.cpp,v 1.3 2006/03/24 12:42:31 vwang Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* vwwang   2006-02-22 created
*/

#include "acsThread.h"
#include "acsThreadManager.h"

using ACS::ThreadSyncGuard;

class TestThreadGuard :public ACS::Thread
{
  public:
    TestThreadGuard(const ACE_CString& name,
	 const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime, 
	 const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime )
        :  ACS::Thread(name, responseTime, sleepTime), loopCounter_m(0)
    {
       ACS_TRACE("TestThreadGuard::TestThreadGuard");
    }

    ~TestThreadGuard() 
    { 
       ACS_TRACE("TestThreadGuard::~TestThreadGuard"); 

       terminate();
    }

    /**
     * This is the method executed in the thread loop
     * And, show the simpliest way of using ThreadSyncGuard
     */
    virtual void runLoop()
    {
       ThreadSyncGuard guard(&loopCounterMutex_m);
         
       ACS_LOG(LM_SOURCE_INFO,
               "TestThreadGuard::runLoop", 
               (LM_INFO, "runLoop (%d)", loopCounter_m++));
       
    }

    /* this function will race with runLoop */
    void testGuard()
    { 
       ThreadSyncGuard guard(&loopCounterMutex_m, false);

       ACS_LOG(LM_SOURCE_INFO, "TestThreadGuard::testGuard",
               (LM_INFO, "enter TestThreadGuard::testGuard"));

       /* sleep to show runLoop is not blocked */      
       ACE_OS::sleep(3);

       /*
	* It is necessary to aquire the guard BEFORE
	* the while check to protect atomically
	* access to the loopCounter_m both in the
	* test and in the assignment.
	* Therefore we aquire BEFORE entering the loop
	* AND again at the end of the block just 
	* before going back up to check again for the
	* while condition.
	*/
       guard.acquire();
       while( loopCounter_m < 10 )
       {
         loopCounter_m -= 3;
         ACS_LOG(LM_SOURCE_INFO, "TestThreadGuard::testGuard",
                 (LM_INFO, "set loopCounter_m to (%d)", loopCounter_m));

         guard.release();
         ACE_OS::sleep(3);
         guard.acquire();
       }
      
       /* exit without release, to show that no need to release before leaving scope */ 
       ACS_LOG(LM_SOURCE_INFO, "TestThreadGuard::testGuard",
               (LM_INFO, "exit TestThreadGuard::testGuard"));

    };

  private:

   int loopCounter_m;
   ACE_Recursive_Thread_Mutex loopCounterMutex_m;

};

int main(int argc, char *argv[])
{
    LoggingProxy logger_m(0, 0, 31);
    LoggingProxy::init(&logger_m);

    ACS::ThreadManager tm;

    /*
     * Creates an thread and try to restart it before one loop finish
     */
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Creating thread"));
    TestThreadGuard *a = 
	tm.create<TestThreadGuard>("TestA", 
			 5 * 1000000 /* 0.5s */, 5 * 1000000 /* 0.5s */ );
    a->resume();

    a->testGuard();

    ACE_OS::sleep(3);

    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Done"));
    return 0;

}








