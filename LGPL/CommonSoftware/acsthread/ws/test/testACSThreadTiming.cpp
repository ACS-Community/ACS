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
* "@(#) $Id: testACSThreadTiming.cpp,v 1.3 2006/03/24 12:42:31 vwang Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram   2005-02-15  created
* pcolomer 2015-04-23 upgraded code to check execution time of the thread
*/

// Uncomment this if you are using the VLT environment
// #include "vltPort.h"


#include "acsThreadManager.h"
#include "acsThreadTest.h"


static char *rcsId="@(#) $Id: testACSThreadTiming.cpp,v 1.3 2006/03/24 12:42:31 vwang Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);


static uint64_t timeStart = 0;
static uint64_t timeStop = 0;

uint64_t getTimeInMs()
{
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return t.tv_sec * 1000 + (t.tv_nsec / 1000000);
}

class TimingACSThread :public TestACSThread
{
  public:
    TimingACSThread(const ACE_CString& name, 
		  const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime, 
		  const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime,
		  bool del=false
	) :
	TestACSThread(name, responseTime, sleepTime, del)
	{
	}

    virtual ~TimingACSThread()
	{
	}

    virtual void onStart()
    {
        timeStart = getTimeInMs();
    }

    virtual void onStop()
    {
        timeStop = getTimeInMs();
    }

};


int main(int argc, char *argv[])
{
    LoggingProxy logger_m(0, 0, 31);
    LoggingProxy::init(&logger_m);

    ACS::ThreadManager tm;

    /*
     * Creates an autodelete thread that runs every 300 mseconds
     * and stops it after 10 iterations.
     */
    ACS::TimeInterval ti = 3000000;  /* 300ms */
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Creating thread with time interval %ld", ti));

    /*
     * Since the Thread is autodelete and
     * we do n access it,
     * we do not need to assign it to a variable
     */
    // TestACSThread *a = 
	tm.create<TimingACSThread>("TestThreadA", 
				 ti, ti,
				 true);
    tm.resume("TestThreadA");

    /**
     * We wait more than needed to ensure that the thread will be deleted before
     * manager's deletion
     */
    sleep(7);

    // Check execution time of the thread
    int64_t diff = timeStop - timeStart;
    if(diff >= 3400)
    {
        ACS_LOG(LM_SOURCE_INFO,"main",
            (LM_INFO, "Execution time of thread greater than 3400ms: %ld ms", diff));
    }

    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Done"));
    return 0;

}








