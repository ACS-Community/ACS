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
* "@(#) $Id: testACSThreadStop.cpp,v 1.3 2006/03/24 12:42:31 vwang Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2005-02-15  created
*/

// Uncomment this if you are using the VLT environment
// #include "vltPort.h"


#include "acsThreadManager.h"
#include "acsThreadTest.h"
#include <vector>

static char *rcsId="@(#) $Id: testACSThreadStop.cpp,v 1.3 2006/03/24 12:42:31 vwang Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

static uint32_t nLoops = 0;
static uint64_t timeStart = 0;
static uint64_t timeStop = 0;
static uint64_t timeDel = 0;
static std::vector<uint64_t> timeLoop;

uint64_t getTimeInMs()
{
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return t.tv_sec * 1000 + (t.tv_nsec / 1000000);
}

class StopACSThread :public TestACSThread
{
  public:
    StopACSThread(const ACE_CString& name, 
		  const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime, 
		  const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime,
		  bool del=false
	) :
	TestACSThread(name, responseTime, sleepTime, del)
	{
    }

    virtual ~StopACSThread()
	{
        timeDel = getTimeInMs();
	}

    virtual void onStart()
    {
        timeStart = getTimeInMs();
    }

    virtual void runLoop()
    {
        timeLoop.push_back(getTimeInMs());
        ++nLoops;
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
     * Creates an autodelete thread that runs every 5 seconds
     * and stops it after 18 seconds
     */
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Creating thread"));
    StopACSThread *a = 
	tm.create<StopACSThread>("TestThreadA", 
				 3 * 10 * 1000000 /* 3s */, 4 * 10 * 1000000 /* 4s */,
				 true);
    a->resume();
    sleep(12);
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Stopping thread"));

    a->stop();

    sleep(5);
    if(timeDel == 0)
    {
        ACS_LOG(LM_SOURCE_INFO,"main",
            (LM_INFO, "After 5s, Thread has not been deleted yet!"));
    }

    int64_t diff = timeDel - timeStop;
    if(diff > 100)
    {
        ACS_LOG(LM_SOURCE_INFO,"main",
            (LM_INFO, "Stopping the thread lasts more than 100ms: %ld ms", diff));
        for(uint32_t i = 1;i < timeLoop.size();++i)
        {
            int64_t tDiff = timeLoop[i] - timeLoop[i-1];
            ACS_LOG(LM_SOURCE_INFO,"main",
                (LM_INFO, "Time between loop %d and loop %d: %ld ms",i,i-1,tDiff));
        }
    }

    if(nLoops != 3 && nLoops != 4)
    {
        ACS_LOG(LM_SOURCE_INFO,"main",
            (LM_INFO, "Number of loops is different than 3 and 4: %d", nLoops));
    }


    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Done"));
    return 0;

}








