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
* "@(#) $Id: testSelfDelThread.cpp,v 1.3 2006/03/24 12:42:31 vwang Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2005-07-22  created
*/

#include "acsThreadManager.h"


static char *rcsId="@(#) $Id: testSelfDelThread.cpp,v 1.3 2006/03/24 12:42:31 vwang Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

class TestSelfDelThread :public ACS::Thread
{
  public:
    TestSelfDelThread(const ACE_CString& name, 
		  const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime, 
		  const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime,
		  bool del=false) :
	ACS::Thread(name, responseTime, sleepTime, del)
	{
	    ACS_TRACE("TestSelfDelThread::TestSelfDelThread");
	}

    ~TestSelfDelThread() { ACS_TRACE("TestSelfDelThread::~TestSelfDelThread"); }

    // thread just writes (log) a message and exits
    virtual void run()
	{
	    ACS_TRACE("TestSelfDelThread::run");
	    ACS_LOG(LM_SOURCE_INFO,"TestSelfDelThread::run", (LM_INFO, "executed only once"));

	}

};

int main(int argc, char *argv[])
{
    LoggingProxy logger_m(0, 0, 31);
    LoggingProxy::init(&logger_m);

    ACS::ThreadManager tm;

    // let's create a thread that destroies the thread object when exits
    tm.create<TestSelfDelThread>("TestSelfDelThreadA",
				 ACS::ThreadBase::defaultResponseTime,
				 ACS::ThreadBase::defaultSleepTime,
				 (bool)true);
    tm.suspend("TestSelfDelThreadA");
    sleep(10);
     ACS_LOG(LM_SOURCE_INFO, "main", (LM_INFO, "Number of threads after execution: %d", tm.getThreadCount()));

// thread should be self destroied
/*
 // passing thread name and suspended flag
    TestACSThread *b = tm.create<TestACSThread>("TestThreadB");
    sleep(20);
    b->resume();
    sleep(20);

    // try if it is removed from the thread manager
    delete b;

 // passing thread name, suspended flag, respons and sleep time
    b = tm.create<TestACSThread>("TestThreadC", 100*1000*10, ACS::ThreadBase::defaultSleepTime);
    b->resume();
    sleep(20);

    // create thread that is never woken-up
    TestACSThread *forEverSleep = tm.create<TestACSThread>("SleepForEver");
    sleep(20);
*/
    LoggingProxy::done();
    sleep(5);

    return 0;

}








